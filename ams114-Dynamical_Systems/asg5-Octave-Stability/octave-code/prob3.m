# prob3.m
function prob3

clear
figure(1);

hold off
[x,y] = meshgrid(-8:.4:8);
f1=y+2.*x.*y;
f2=x+(x.^2)-(y.^2);
h=quiver(x,y,f1,f2);
set(h,"autoscalefactor",5);
axis("tight");
title("Phase Portrait");
xlabel("x");
ylabel("y");
fixAxes;

endfunction
