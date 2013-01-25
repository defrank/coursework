# f2.m
function [z] = f2(x);
z=zeros(length(x));
for i = 1:length(x),
   if (x(i) < 1 && x(i) > -1),
      z(i)=(-x(i));
   elseif (x(i) >= 1 || x(i) <= -1),
      z(i)=x(i);
   endif
endfor
return

ndfunction

