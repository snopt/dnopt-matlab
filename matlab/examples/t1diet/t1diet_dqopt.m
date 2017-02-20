function t1diet_dqopt
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

dqscreen('on');
dqprint('t1diet.out');

t1diet.spc = which('t1diet.spc');
dqspec(t1diet.spc);

% Set up problem.
n  = 6;
x  = ones(n,1);
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

% Objective term
c = [   3   24   13    9   20   19  ]';

% Solve the problem.
options.name  = 't1diet';
options.start = 'Cold';

[x,obj,INFO,output,lambda,states] = dqopt([], c, x, xl, xu, A, al, [], options);

options.start = 'Warm';
[x,obj,INFO] = dqopt([], c, x, xl, xu, A, al, [], lambda, states, options);

dqprint('off');
dqscreen('off');
dqend;
