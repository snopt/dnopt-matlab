function hsmain_mincon
% Matlab example problem
% This example calls dnsolve, which is based on fmincon.
%
%     Minimize    (x(1)-x(2))^2 + (x(2)-x(3))^3 + (x(3)-x(4))^4
%                                                 + (x(4)-x(5))^4
%
%     subject to
%                  x(1)      + x(2)^2 + x(3)^3         - 3     = 0
%                              x(2)   - x(3)^2  + x(4) - 1     = 0
%                  x(1)*x(5)                           - 1     = 0
%
%                                      -x(3)    - x(4) - x(5) <= 3
%                 -x(1)      - x(2)                           <= 1

dnScreen ('on');
dnPrint('hsmain_mincon.out');

hsmain.spc = which('hsmain.spc');
dnSpec (hsmain.spc);

% Set up the problem.
n = 5;
x = [ 2;
      sqrt(2) - 1 ;
      sqrt(2) - 1 ;
      2;
      0.5   ];
xl = -Inf*ones(n,1);
xu =  Inf*ones(n,1);

% Linear constraints:
%   Inequality constraints:
A   = [  0  0 -1 -1 -1;
	-1 -1  0  0  0 ];
b   = [ 3; 1 ];

%   No equality constraints:
Aeq = []; beq = [];


% Nonlinear constraints:
%   x(1)   + x(2)^2 + x(3)^3 = 3
%   x(2)   - x(3)^2  + x(4)  = 1
%   x(1)*x(5)                = 1


% Solve the problem.
[x,obj,INFO,output,lambda] = dnSolve( @hsmainobj_mincon, x, A, b, Aeq, beq, ...
				      xl, xu, @hsmaincon_mincon );

dnPrint ('off');
dnScreen ('off');
dnEnd;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [f,g] = hsmainobj_mincon(x)

f = (x(1)-x(2))^2 + (x(2)-x(3))^3 + (x(3)-x(4))^4;
g = [  2*(x(1)-x(2));
      -2*(x(1)-x(2)) + 3*(x(2)-x(3))^2;
      -3*(x(2)-x(3))^2 + 4*(x(3)-x(4))^3;
      -4*(x(3)-x(4))^3;
       0];


function [c,ceq,G,Geq] = hsmaincon_mincon(x)

c = [];  G = [];

ceq = [ x(1) + x(2)^2 + x(3)^3 - 3;
        x(2) - x(3)^2 + x(4)   - 1;
        x(1)*x(5)              - 1];

Geq = [ 1               0   x(5);
        2*x(2)          1   0;
        3*x(3)^2  -2*x(3)   0;
        0               1   0;
        0               0   x(1)];
