function watertank
tstart = 0;
tfinal = 100;
y0 = 0;
options = odeset('Events',@events1);
tout = tstart;
yout=0;
teout = [];
yeout = [];
ieout = [];
state = 1;
for i = 1:10
           if (state == 1),
              [t,y,te,ye,ie] = ode23(@f1,[tstart:0.1:tfinal],y0,options);
nt = length(t);
tout = [tout; t(2:nt)];
yout = [yout; y(2:nt)]
   teout = [teout; te];          % Events at tstart are never reported.
   yeout = [yeout; ye];
ieout = [ieout; ie];
state = 2;
options = odeset('Events',@events2);
else
   [t,y,te,ye,ie] = ode23(@f2,[tstart:0.1:tfinal],y0,options);
nt = length(t);
tout = [tout; t(2:nt)];
yout = [yout; y(2:nt)];
teout = [teout; te];          % Events at tstart are never reported.
yeout = [yeout; ye];
ieout = [ieout; ie];
state = 1;
options = odeset('Events',@events1);
    end
    tstart = t(nt);
y0 = y(nt);
end

plot(tout,yout,'r.-')
# ----------------------------------------------------------------------

function dydt = f1(t,y)
   dydt = 6;

# ----------------------------------------------------------------------

function dydt = f2(t,y)
   dydt = -4;

function [value,isterminal,direction] = events1(t,y)
   % Locate the time when height passes through zero
   value = y(1)-6;   % detect height = 0
   isterminal = 1;   % stop the integration
   direction =  1;   % positive direction

function [value,isterminal,direction] = events2(t,y)
   % Locate the time when height passes through zero
   value = y(1)-4;   % detect height = 0
   isterminal = 1;   % stop the integration
   direction = -1;   % negative direction
   
