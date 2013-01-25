# g.m
function [z] = g(x);
z=zeros(length(x));
for i = 1:length(x),
   if (x(i) < -1),
      z(i)=x(i)+2;
   elseif (x(i) <= 1 && x(i) >= -1),
      z(i)=(-x(i));
   elseif (x(i) > 1),
      z(i)=x(i)-2;
   endif
endfor
return
endfunction
