# prob1.m
function prob1
clear
figure(1);

hold off
[x,y] = meshgrid(0:.1:2.5);
f1=x.*(3-2.*x-2.*y);
f2=y.*(2-x-y);

#[dx1,dy1] = gradient(f1,.1,.1);
#[dx2,dy2] = gradient(f2,.1,.1);

###
# Not likely correct
#quiver(x,y,dx1,dy1)
#hold on
#quiver(x,y,dx2,dy2)
###

h=quiver(x,y,f1,f2);
set (h, "autoscalefactor", 5);
axis("tight");   
title("Rabbits versus Sheep");
xlabel("x: Rabbits");
ylabel("y: Sheep");

fixAxes;

endfunction
