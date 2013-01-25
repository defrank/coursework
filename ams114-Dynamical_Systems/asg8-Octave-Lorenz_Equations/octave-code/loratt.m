# loratt.m
function x_dot_vec = loratt(x_vec)
   sigma=10; r=28; b=8/3;
   x_dot_vec=zeros(3,1);
   # x_dot = sigma.*(y-x)
   x_dot_vec(1)=sigma.*(x_vec(2)-x_vec(1));
   # y_dot = (r.*x)-(y)-(x.*z)
   x_dot_vec(2)=(r.*x_vec(1))-(x_vec(2))-(x_vec(1).*x_vec(3));
   # z_dot=(x.*y)-(b.*z)
   x_dot_vec(3)=(x_vec(1).*x_vec(2))-(b.*x_vec(3));
   return
endfunction
