# prob2.m
function prob2

clear
figure(1);

hold off
[r,th] = meshgrid(0:.1:5);
f1=r.*(1-r.^2).*(9-r.^2);
f2=1;
h=quiver(r,th,f1,f2);
set (h, "autoscalefactor", 5);
axis("tight");
title("Phase Portrait: Polar Coordinates");
xlabel("r");
ylabel("theta");
fixAxes;

clear
figure(2);

hold off
[x,y] = meshgrid(-3.5:.1:3.5);
r=sqrt((x.^2)+(y.^2));
f1=r.*(1-r.^2).*(9-r.^2);
g1=(r.*f1)./(x+(y.^2)./x);
g2=((r.^2)./x)+(y.*r.*f1)./(r.^2);
h=quiver(x,y,g1,g2);
set (h, "autoscalefactor", 5);
axis("tight");
title("Phase Portrait: Cartesian Coordinates");
xlabel("x");
ylabel("y");
fixAxes;

endfunction
