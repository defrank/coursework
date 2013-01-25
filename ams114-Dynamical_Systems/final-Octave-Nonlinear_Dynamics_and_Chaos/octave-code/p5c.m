# p5c.m
function p5c
clear
figure(3)
clf()

mu=5;
[x,y]=meshgrid(-10:1:10);
x_dot=mu.*(y-(g(x)));
y_dot=(-(x./mu));
h=quiver(x,y,x_dot,y_dot);
set(h,"autoscalefactor",2);
axis("square");
title("Relaxation Oscillation, mu = 5 >> 1");
xlabel("x");
ylabel("y");
hold off

figure(4)
clf()

plot(x,x_dot,"r")
hold on
plot(y,y_dot,"b")
hold off
title("Relaxation Oscillation, mu = 5 >> 1");
xlabel("x'");
ylabel("y'");

figure(5)
clf()
hold on

[y1]=(f2(x));
plot(x,y1,"m*","markersize",1);
title("Relaxation Oscillation Nullclines, mu = 5 >> 1");
xlabel("x");
ylabel("y");
hold off

fixAxes;
endfunction
