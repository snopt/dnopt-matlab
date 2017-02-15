function lpmainN
% Matlab example linear problem
% This example calls dqSolveN.
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

dqscreen('on');
dqprint('lpmainN.out');

lpmain.spc = which('lpmain.spc');
dqspec(lpmain.spc);
dqsetint('Major Iteration limit', 250);


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

% Linear constraint matrix
A = [ eye(m-1,n) ];
for i = 1:m-1,
  A(i,i+1) = -1;
end

A = [ A;
      ones(1,n) ];
al = [ -inf*ones(m-1,1);
       1 ];
au = [ zeros(m-1,1);
       1 ];

% Lower and upper bounds on x >= 0
xl = zeros(n,1);
xu = [];

% Solve the problem.
options.name = 'lpmainN';
[x,obj,INFO,lambda,output] = dqsolve(, H, f, x0, xl, xu, [], A, al, au, ...
				     [], options );

dqprint('off');
dqscreen('off');
dqend;


