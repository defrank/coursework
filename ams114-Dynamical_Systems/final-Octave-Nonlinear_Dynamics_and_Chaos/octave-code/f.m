# f.m
function [z] = f(x);
z=zeros(length(x));
for i = 1:length(x),
   if (x(i) < 1 && x(i) > -1),
      z(i)=-1;
   elseif (x(i) >= 1 || x(i) <= -1),
      z(i)=1;
   endif
endfor
return
endfunction
