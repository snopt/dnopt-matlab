function hs76_mincon
% Matlab example quadratic problem
% This example calls dqSolve.
%
%   HS 76
%
%           min  f'x + 1/2 * x'Hx
%
%   subject to    x(1) + 2*x(2) +   x(3) + x(4) <= 5
%               3*x(1) +   x(2) + 2*x(3) - x(4) <= 4
%                      -   x(2) - 4*x(3)        <= -1.5
%                  x >= 0
%

% Add path to DQOPT matlab files
addpath([pwd,'/../../'], '-end');

dqScreen ('on');
dqPrint('hs76_mincon.out');

hs76.spc = which('hs76.spc');
dqSpec (hs76.spc);
dqSetInt ('Major Iteration limit', 250);


% Set up the problem
m  = 3;
n  = 4;
x0 = zeros(n,1);
x0 = [ .5; .5; .5; .5 ];

% Hessian
H = [  2  0  -1  0  ;
       0  1   0  0  ;
      -1  0   2  1  ;
       0  0   1  1 ];

% Linear objective term
f = [ -1 -3 1 -1 ]';

% Linear inequality constraint matrix
A = [ 1  2  1  1 ;
      3  1  2 -1 ;
      0 -1 -4  0 ];

b = [ 5; 4; -1.5 ];

% No linear equality constraints
Aeq = []; beq = [];

% Lower and upper bounds on x >= 0
xl = zeros(n,1);
xu = [];

% Solve the problem.
[x,obj,INFO,output,lambda] = dqSolve( H, f, A, b, Aeq, beq, xl, xu, x0 );

dqPrint ('off');
dqScreen ('off');
dqEnd;


