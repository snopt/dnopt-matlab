function hs13_mincon
% Matlab example problem
% This example calls dnsolve, which is based on fmincon.
%
%   HS 13(modified so CQ holds)
%     Minimize       x(1)   + x(2)
%     subject to    -x(1)^3 + x(2) <= 0
%
%                            x(2) >= 1.
%

% Add path to DNOPT matlab files
addpath([pwd,'/../../'], '-end');

dnscreen('on');
dnprint('hs13_mincon.out');

hs13.spc = which('hs13.spc');
dnspec(hs13.spc);
dnsetint('Major Iteration limit', 250);


% Set up the problem.
x      = [ 3 1]';
xl     = [-Inf,  1 ]';
xu     = [ Inf, Inf]';

% No linear constraints
A   = []; b   = [];
Aeq = []; beq = [];

% Options output function.
options.name = 'hs13min';
options.stop = @stopFun;

% Solve the problem.
[x,obj,INFO,output,lambda] = dnsolve(@hs13obj, x, A, b, Aeq, beq, xl, ...
				     xu, @hs13con_mincon, options );

dnprint('off');
dnscreen('off');
dnend;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [f,g] = hs13obj(x)

% Compute objective and its gradient
f = x(1) + x(2);
g = [ 1; 1 ];


function [c,ceq,G,Geq] = hs13con_mincon(x)

% Compute the nonlinear constraints and gradients

% No equality constraints; only inequalities.
ceq = [];  Geq = [];

c(1) = -x(1)^3 + x(2);
G    = [ -3*x(1)^2;  1 ];


function STOP = stopFun(x)
% STOP function is called every major iteration.
% If, on exit, STOP /= 0, then the run terminates.

STOP = 0;
