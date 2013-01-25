# p5b.m
function p5b
clear
figure(3)
clf()

mu=1;
[x,y]=meshgrid(-2:.2:2);
x_dot=y;
y_dot=-x-(mu.*(f(x)).*y);
h=quiver(x,y,x_dot,y_dot);
set(h,"autoscalefactor",1);
axis("square");
title("Lienard System, mu = 1");
xlabel("x");
ylabel("y");
hold off

figure(4)
clf()

plot(x,x_dot,"r")
hold on
plot(y,y_dot,"b")
hold off
title("Lienard System, mu = 1");
xlabel("x'");
ylabel("y'");

fixAxes;
endfunction
