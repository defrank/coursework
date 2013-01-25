# p5a.m
function p5a
clear
figure(1)
clf()

mu=5;
[x,y]=meshgrid(-2:.2:2);
x_dot=mu.*(y-(g(x)));
y_dot=(-(x./mu));
h=quiver(x,y,x_dot,y_dot);
set(h,"autoscalefactor",1);
axis("square");
title("Piecewise-Linear System, mu = 5 >> 1");
xlabel("x");
ylabel("y");
hold off

figure(2)
clf()

plot(x,x_dot,"r")
hold on
plot(y,y_dot,"b")
hold off
title("Piecewise-Linear System, mu = 1");
xlabel("x'");
ylabel("y'");

fixAxes;
endfunction
