function dntoy
% Matlab example problem
%
%     Minimize      3*x(1) + (x(1) + x(2) + x(3))^2 + 5*x(4)
%     subject to             4*x(2)   + 2*x(3)               >= 0
%                     x(1) +   x(2)^2 +   x(3)^2              = 2
%                              x(2)^4 +   x(3)^4   +   x(4)   = 4
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
A  = [ 0 4 2 0 ];
al = [ 0 ]; au = [ Inf ];

% Nonlinear constraints
cl = [ 2; 4];
cu = [ 2; 4];

% Solve the problem.
[x,obj,INFO,output,lambda] = dnopt('dntoy', @dntoyobj, x, xl, xu, A, al, au, ...
				   @dntoycon, cl, cu);

dnPrint ('off');
dnScreen ('off');
dnEnd;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [f,g] = dntoyobj(x)

% Compute the objective and its gradient
f = 3*x(1) + (x(1) + x(2) + x(3))^2 + 5*x(4);

g = [ 3 + 2*(x(1) + x(2) + x(3));
      2*(x(1) + x(2) + x(3));
      2*(x(1) + x(2) + x(3));
      5 ];


function [c] = dntoycon(x)

% Compute the nonlinear constraints
%  Note, gradients are written below but not provided
%  to DNOPT to demonstrate finite differencing.
%  Derivative level is automatically set by the mex-file.
c = [ x(1) + x(2)^2 + x(3)^2;
             x(2)^4 + x(3)^4 + x(4) ];

% $$$ G = [      1         0;
% $$$       2*x(2)  4*x(2)^3;
% $$$       2*x(3)  4*x(3)^3;
% $$$            0         1 ];
