# loreq.m
function loreq
clear
close
figure(1);
hold off
lsode_options("absolute tolerance", 1e-3)
lsode_options("relative tolerance", 1e-4)
t=linspace(0,25,1e3);
x0_vec=[1,1,1.05];
[x_vec,t,MSG]=lsode(@loratt,x0_vec,t);
t
MSG
plot3(x_vec(:,1),x_vec(:,2),x_vec(:,3))
view(45,45)
title("Lorenz Equation Simulation\nx' = sigma*(y - x)\ny' = rx - y - xz\nz' = xy - bz\nsigma = 10, r = 28, b = 8/3\n(x_{0},y_{0},z_{0}) = (1, 1, 1.05)");
xlabel("x");
ylabel("y");
zlabel("z");
endfunction
