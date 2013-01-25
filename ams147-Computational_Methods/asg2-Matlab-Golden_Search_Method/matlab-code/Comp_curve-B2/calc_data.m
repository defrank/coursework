%
% Consider the non-linear equation sqrt(sum of (f(x(i))-y(i))^2), where, 
% depending on c, x(i) is an array of values produced by the fitting 
% funtion found in "dd.m" and y(i) is an array of values found in 
% "data1.txt".  Here "c" is a parameter in the equation. The distance of
% the equation varies with "c" and thus the distance is a function of "c". 
% This code calculates the distance for "c" in [0,1] and stores the data 
% in data1.mat. Later on the data is used in "plot_curve.m" to plot 
% the distance as a function of "c".
%
load -ascii data1.txt

% vector of x values found in "data1.txt"
x=data1(:,1);
% array/vector of y values found in "data1.txt"
y1=data1(:,2);
% array/vector of 100 c values between [0,1]
c_v=[0:.01:1];
% scalar number of c values in array/vector c_v
nc=size(c_v,2);
% initialization for array/vector of distance values
d_v=zeros(1,nc);

for i=1:nc,
  c=c_v(i);
  d(i)=dd(c,x,y1);
end
%
save data1 c_v y_v
%

