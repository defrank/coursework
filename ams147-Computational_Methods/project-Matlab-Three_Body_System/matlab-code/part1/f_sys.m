function [z]=f_sys(w,t)
%
% This function calculates f_sys(w,t)
%
X1=[w(1), w(2)];	% position of body #1
X2=[w(3), w(4)];	% position of body #2
X3=[w(5), w(6)];    % position of body #3
%
z=zeros(1,12);
%
z(1)=w(7);
z(2)=w(8);
z(3)=w(9);
z(4)=w(10);
z(5)=w(11);
z(6)=w(12);
%
dX21=X2-X1;		% vector X2-X1
dX31=X3-X1;     % vecotr X3-X1
dX32=X3-X2;     % vecotr X3-X2
%
G21=dX21/norm(dX21)^3;	% gravitational force on body #1 from body #2
G12=-G21;		% gravitational force on body #2 from body #1
G31=dX31/norm(dX31)^3;	% gravitational force on body #1 from body 3
G13=-G31;		% gravitational force on body #3 from body #1
G32=dX32/norm(dX32)^3;	% gravitational force on body #2 from body #3
G23=-G32;		% gravitational force on body #3 from body #2
% Sum of forces acting on each body
P1=G21+G31;     % Forces acting on body 1
P2=G12+G32;     % Forces acting on body 2
P3=G13+G23;     % Forces acting on body 3
%
z(7)=P1(1);
z(8)=P1(2);
z(9)=P2(1);
z(10)=P2(2);
z(11)=P3(1);
z(12)=P3(2);