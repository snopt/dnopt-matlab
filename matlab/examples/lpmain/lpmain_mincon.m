function lpmain_mincon
% Matlab example linear problem
% This example calls dqSolve.
%
%   LP main
%
%           min  f'x
%
%   subject to    A*x <= b   Aeq*x = beq   x >= 0
%
%   The structure of  A  for n = 8 is:
%     -inf     ( 1  -1                           )    0
%     -inf     (     1  -1                       )    0
%     -inf     (         1  -1                   )    0
%     -inf le  (             1  -1               ) le 0
%     -inf     (                 1  -1           )    0
%     -inf     (                     1  -1       )    0
%     -inf     (                         1  -1   )    0
%   The structure of  Aeq  for n = 8 is:
%        1 =   ( 1   1   1   1   1   1   1   1   ) =  1
%

% Add path to DQOPT matlab files
addpath([pwd,'/../../'], '-end');

dqScreen ('on');
dqPrint('lpmain_mincon.out');

lpmain.spc = which('lpmain.spc');
dqSpec (lpmain.spc);
dqSetInt ('Major Iteration limit', 250);


% Set up the problem
m  = 30;
n  = 30;

x0 = zeros(n,1);
for j = 1:n/2,
  x0(2*j-1) = -1;
  x0(2*j)   =  1;
end

% Hessian
H = [];

% Linear objective term
f = -0.5*ones(n,1);

% Linear inequality constraint matrix
A = eye(m-1,n);
for i = 1:m-1,
  A(i,i+1) = -1;
end
b = zeros(m-1,1);

% Linear equality constraints
Aeq = ones(1,n);
beq = [1];

% Lower and upper bounds on x >= 0
xl = zeros(n,1);
xu = [];

% Solve the problem.
[x,obj,INFO,output,lambda] = dqSolve( H, f, A, b, Aeq, beq, xl, xu, x0 );

dqPrint ('off');
dqScreen ('off');
dqEnd;


