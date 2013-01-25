function [dist]=dd(T,td,yd)
%
t1=[0:0.1:8];
y1=spline(td,yd,t1);
t2=T+t1;
y2=spline(td,yd,t2);
dist=norm(y1-y2);