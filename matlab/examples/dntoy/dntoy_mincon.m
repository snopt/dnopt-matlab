function dntoy_mincon
% Matlab example problem
% This example calls npsolve, which is based on fmincon.
%
%     Minimize      3*x(1) + (x(1) + x(2) + x(3))^2 + 5*x(4)
%     subject to
%                           -4*x(2)   - 2*x(3)                  <= 0
%
%                     x(1) +   x(2)^2 +   x(3)^2            - 2  = 0
%                              x(2)^4 +   x(3)^4   +   x(4) - 4  = 0
%
%                     x(1) >= 0,                       x(4)  >= 0.
%


dnScreen ('on');
dnPrint('dntoy.out');

dntoy.spc = which('dntoy.spc');
dnSpec (dntoy.spc);

dnSetInt ('Major Iteration limit', 250);

% Set up the problem.
x  = ones(4,1);
xl = [  0,-Inf,-Inf,   0]';
xu = [Inf, Inf, Inf, Inf]';

% Linear constraint
%   No equalities:
Aeq = [];  beq = [];

%   Inequalities:
A  = [ 0 -4 -2 0 ];  b  = [ 0 ];

% Solve the problem.
[x,obj,INFO,output,lambda] = dnSolve( @dntoyobj_mincon, x, A, b, Aeq, beq, ...
				      xl, xu, @dntoycon_mincon );

dnPrint ('off');
dnScreen ('off');
dnEnd;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [f,g] = dntoyobj_mincon(x)

% Compute the objective and its gradient
f = 3*x(1) + (x(1) + x(2) + x(3))^2 + 5*x(4);

g = [ 3 + 2*(x(1) + x(2) + x(3));
      2*(x(1) + x(2) + x(3));
      2*(x(1) + x(2) + x(3));
      5 ];

function [c,ceq,G,Geq] = dntoycon_mincon(x)

% Compute the nonlinear constraints and gradients

c = []; G = [];

ceq = [ x(1) + x(2)^2 + x(3)^2        - 2;
               x(2)^4 + x(3)^4 + x(4) - 4 ];

Geq = [      1         0;
        2*x(2)  4*x(2)^3;
        2*x(3)  4*x(3)^3;
             0         1 ];
