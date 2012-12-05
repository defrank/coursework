/* $Id: functions.pl,v 1.1 2012-12-04 22:20:42-08 - - $
 * Derek Frank, dmfrank@ucsc.edu
 * Assignment 5
 */

/*
 * Prolog version of not.
 */
not( X ) :- X, !, fail.
not( _ ).

/*
 * Function: Distance between airports.
 * Tested using SWI prolog.
 */

degmin_to_deg( degmin( Degrees, Minutes ), Degreesonly ) :-
    Degreesonly is Degrees + Minutes / 60.

pythagoras( X1, Y1, X2, Y2, Hypotenuse ) :-
    DeltaX is X1 - X2,
    DeltaY is Y1 - Y2,
    Hypotenuse is sqrt( DeltaX * DeltaX + DeltaY * DeltaY ).

distance( Airport1, Airport2, DistanceMiles ) :-
    airport( Airport1, _, Latitude1, Longitude1 ),
    airport( Airport2, _, Latitude2, Longitude2 ),
    degmin_to_deg( Latitude1, Latdegrees1 ),
    degmin_to_deg( Latitude2, Latdegrees2 ),
    degmin_to_deg( Longitude1, Longdegrees1 ),
    degmin_to_deg( Longitude2, Longdegrees2 ),
    pythagoras( Latdegrees1, Longdegrees1, Latdegrees2, Longdegrees2,
                DistanceDegrees ),
    DistanceMiles is 69 * DistanceDegrees.

hoursmins_to_hours( time( Hours, Mins ), Hoursonly ) :-
    Hoursonly is Hours + Mins / 60.

/*
 * Extra function to convert miles into hours when flying at 500 mph.
 */
miles_to_hours( Miles, Hours ) :-
    Hours is Miles / 500.

print_2digits( Digits ) :-
    Digits < 10, print( 0 ), print( Digits ).

print_2digits( Digits ) :-
    Digits >= 10, print( Digits ).

print_time( Hoursonly ) :-
    Minsonly is floor( Hoursonly * 60 ),
    Hours is Minsonly // 60,
    Mins is Minsonly mod 60,
    print_2digits( Hours ),
    print( ':' ),
    print_2digits( Mins ).


/*
* Find paths from a departure airport to a destination airport.
* The flight must finish before 24:00.  Also, there must be a
* 30 minute transfer time between flights.  The path with the
* shortest distance is chosen.  The list returned contains
* lists of the airports with their departure/arrival times.
*
*/
findpath( End, End, _, [End], _ ).
findpath( Curr, End, Visited, [[Curr, DepTime, ArrTime] | List],
          DepTimeInHM ) :-
    flight( Curr, End, DepTimeInHM ),
    not( member( End, Visited ) ),
    hoursmins_to_hours( DepTimeInHM, DepTime ),
    distance( Curr, End, DistanceMi ),
    miles_to_hours( DistanceMi, DeltaTime ),
    ArrTime is DepTime + DeltaTime,
    ArrTime < 24.0,
    findpath( End, End, [End | Visited], List, _).
findpath( Curr, End, Visited, [[Curr, DepTime, ArrTime] | List],
          DepTimeInHM ) :-
    flight( Curr, Next, DepTimeInHM ),
    not( member( Next, Visited ) ),

    hoursmins_to_hours( DepTimeInHM, DepTime ),
    distance( Curr, Next, DistanceMi ),
    miles_to_hours( DistanceMi, DeltaTime ),
    ArrTime is DepTime + DeltaTime,
    ArrTime < 24.0,

    flight( Next, _, NextDepTimeInHM ),
    hoursmins_to_hours( NextDepTimeInHM, NextDepTime ),
    TimeDiff is NextDepTime - ArrTime - 0.5,
    TimeDiff >= 0,

    findpath( Next, End, [Next | Visited], List, NextDepTimeInHM ).


/*
* Write the given list using a certain form given departs/arrives
* paired with times.
*/
writepath( [] ) :-
    nl.
writepath( [[Dep, DDTime, DATime], Arr | []] ) :-
    airport( Dep, Depart_name, _, _),
    airport( Arr, Arrive_name, _, _),
    write( '     ' ), write( 'depart  ' ),
    write( Dep ), write( '  ' ),
    write( Depart_name ),
    print_time( DDTime ), nl,

    write( '     ' ), write( 'arrive  ' ),
    write( Arr ), write( '  ' ),
    write( Arrive_name ),
    print_time( DATime ), nl,
    !, true.
writepath( [[Dep, DDTime, DATime], [Arr, ADTime, AATime] | Rest] ) :-
    airport( Dep, Depart_name, _, _),
    airport( Arr, Arrive_name, _, _),
    write( '     ' ), write( 'depart  ' ),
    write( Dep ), write( '  ' ),
    write( Depart_name ),
    print_time( DDTime ), nl,

    write( '     ' ), write( 'arrive  ' ),
    write( Arr ), write( '  ' ),
    write( Arrive_name ),
    print_time( DATime ), nl,
    !, writepath( [[Arr, ADTime, AATime] | Rest] ).


/*
* Error if there is a flight where the departure and destination
* are one in the same.
*/
fly( Depart, Depart ) :-
    write( 'Error: the departure and the destination are the same.' ),
    nl,
    !, fail.

/*
* Main case.
*/
fly( Depart, Arrive ) :-
    airport( Depart, _, _, _ ),
    airport( Arrive, _, _, _ ),

    findpath( Depart, Arrive, [Depart], List, _ ),
    !, nl,
    writepath( List ),
    true.

/*
* Print error if the flight specified does not follow the rules of
* the twilight zone or if the airports do not exist.
*/
fly( Depart, Arrive ) :-
    airport( Depart, _, _, _ ),
    airport( Arrive, _, _, _ ),
    write( 'Error: your flight is not possible in the twilight zone.' ),
    !, fail.
fly( _, _) :-
    write( 'Error: nonexistent airport(s).' ), nl,
    !, fail.
