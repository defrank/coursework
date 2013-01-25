# prob2.m
function prob2
clear
figure(1);
hold off
[r,theta]=meshgrid(-3:.2:3);
r_dot=(r.^3)-4.*r;
theta_dot=1;
x=r.*cos(theta);
y=r.*sin(theta);
x_dot=r_dot.*cos(theta)-theta_dot.*r.*sin(theta);
y_dot=r_dot.*sin(theta)+theta_dot.*r.*cos(theta);
h=quiver(x,y,x_dot,y_dot);
set(h,"autoscalefactor",2);
axis("tight");
title("Phase portrait");
xlabel("x: rcos(theta)");
ylabel("y: rsin(theta)");
fixAxes;
endfunction;
