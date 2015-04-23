function t1diet_mincon
% Matlab example problem
% This example calls dnsolve, which is based on fmincon.
%   Diet LP problem of Chvatal, 1983.
%
%   minimize           c'x
%   subject to         -A*x   <= -b
%                0  <=  x(1)  <=  4
%                0  <=  x(2)  <=  3
%                0  <=  x(3)  <=  2
%                0  <=  x(4)  <=  8
%                0  <=  x(5)  <=  2
%                0  <=  x(6)  <=  2
%
%   where
%     c  =  (   3   24   13    9   20   19 )
%     A  =  ( 110  205  160  160  420  260 )   b = ( 2000 )
%           (   4   32   13    8    4   14 )       (   55 )
%           (   2   12   54  285   22   80 )       (  800 )
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
A = -A;
b = [ -2000; -55; -800];

Aeq = []; beq = [];


% Set OutputFcn function
options.OutputFcn = @stopFun;

% Solve the problem.
[x,obj,INFO] = dnSolve (@t1dietobj_mincon, x, A, b, Aeq, beq, xl, xu, ...
			@t1dietcon_mincon, options );

dnPrint ('off');
dnScreen ('off');
dnEnd;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [f,g] = t1dietobj_mincon(x)

% Compute the objective and gradient.
c = [   3   24   13    9   20   19  ]';
f = c'*x;
g = c;


function [c,ceq,G,Geq] = t1dietcon_mincon(x)

% No nonlinear constraints.
c = []; ceq = [];
G = []; Geq = [];


function STOP = stopFun(x)
% STOP function is called every major iteration.
% If, on exit, STOP /= 0, then the run terminates.

STOP = 0;

