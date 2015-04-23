function t1diet
% Matlab example problem
%   Diet LP problem of Chvatal, 1983.
%
%   minimize           c'x
%   subject to   al <=  A*x
%                0  <=  x(1)  <=  4
%                0  <=  x(2)  <=  3
%                0  <=  x(3)  <=  2
%                0  <=  x(4)  <=  8
%                0  <=  x(5)  <=  2
%                0  <=  x(6)  <=  2
%
%   where
%     c  =  (   3   24   13    9   20   19 )
%     A  =  ( 110  205  160  160  420  260 )   al = ( 2000 )
%           (   4   32   13    8    4   14 )        (   55 )
%           (   2   12   54  285   22   80 )        (  800 )
%

dnScreen ('on');
dnPrint('t1diet.out');

t1diet.spc = which('t1diet.spc');
dnSpec ( t1diet.spc );

% Set up problem.
n  = 6;
x  = ones (n,1);
xl = zeros(n,1);
xu = [ 4
       3
       2
       8
       2
       2 ];

% Linear constraints
A  = [ 110  205  160  160  420  260;
         4   32   13    8    4   14;
         2   12   54  285   22   80 ];
al = [ 2000; 55; 800];
au = [ Inf; Inf; Inf];


% Solve the problem.
[x,obj,INFO] = dqopt('t1diet', @t1dietobj, x, xl, xu, A, al, au );

dnPrint ('off');
dnScreen ('off');
dnEnd;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [f,g] = t1dietobj(x)

% Compute objective and gradient.
c = [   3   24   13    9   20   19  ]';
f = c'*x;
g = c;
