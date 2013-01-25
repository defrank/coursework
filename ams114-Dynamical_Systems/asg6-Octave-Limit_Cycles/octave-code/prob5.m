# prob5.m
function prob5
clear
figure(1);
hold off
[x,y]=meshgrid(-2:.1:2);
mu=1;
x_dot=y;
y_dot=-x-mu.*((x.^4)-1).*y;
h=quiver(x,y,x_dot,y_dot);
set(h,"autoscalefactor",2);
axis("tight");
title("mu = 1");
xlabel("x");
ylabel("y");
fixAxes;
endfunction
