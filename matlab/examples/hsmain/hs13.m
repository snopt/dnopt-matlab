function hs13
% Matlab example problem
% This example calls dnopt.
%
%   HS 13(modified so CQ holds)
%
%     Minimize      x(1) + x(2)
%     subject to    x(1)^3 - x(2) >= 0
%
%                            x(2) >= 1.
%
%

% Add path to DNOPT matlab files
addpath([pwd,'/../../'], '-end');

dnscreen('on');
dnprint('hs13.out');

hs13.spc = which('hs13.spc');
dnspec(hs13.spc);
dnsetint('Major Iteration limit', 250);


% Set up the problem.
x      = [ 3 1]';
xl     = [-Inf,  1 ]';
xu     = [ Inf, Inf]';

% Nonlinear constraint  x(1)^3 - x(2) >= 0
cl    = [ 0 ];
cu    = [ Inf ];

% Linear constraint x(2) >= 1
A     = [ 0 1 ];
al    = [ 1 ];
au    = [ Inf ];

% Solve the problem.
options.name = 'hs13';
options.stop = @stopFun;

[x,obj,INFO,lambda,output] = dnopt(@hs13obj, x, xl, xu, A, al, au, ...
				   @hs13con, cl, cu, options );
dnprint('off');
dnscreen('off');
dnend;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [f,g] = hs13obj(x)

% Compute objective and its gradient
f = x(1) + x(2);
g = [ 1; 1 ];


function [c,G] = hs13con(x)

% Compute the nonlinear constraints and gradients
c(1) = x(1)^3 - x(2);
G    = [  3*x(1)^2;  -1 ];


function STOP = stopFun(x)
% STOP function is called every major iteration.
% If, on exit, STOP /= 0, then the run terminates.

STOP = 0;
