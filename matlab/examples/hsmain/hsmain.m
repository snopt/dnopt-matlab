function hsmain
% Matlab example problem
%     Minimize    (x(1)-x(2))^2 + (x(2)-x(3))^3 + (x(3)-x(4))^4
%                                                 + (x(4)-x(5))^4
%
%     subject to                        x(3)    + x(4) + x(5) >= 3
%                  x(1)      + x(2)^2 + x(3)^3                 = 3
%                  x(1)      + x(2)                           >= 1
%                              x(2)   - x(3)^2  + x(4)         = 1
%                  x(1)*x(5)                                   = 1

dnscreen('on');
dnprint('hsmain.out');

hsmain.spc = which('hsmain.spc');
dnspec(hsmain.spc);

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
%   x(3) + x(4) + x(5) >= 3
%   x(1) + x(2)        >= 1
A  = [ 0 0 1 1 1;
       1 1 0 0 0];
al = [ 3; 1 ];
au = [ Inf; Inf ];

% Nonlinear constraints:
%   x(1)   + x(2)^2 + x(3)^3 = 3
%   x(2)   - x(3)^2  + x(4)  = 1
%   x(1)*x(5)                = 1
cl = [ 3; 1; 1; ];
cu = [ 3; 1; 1; ];

% Solve the problem.
options.name = 'hsmain';
[x,obj,INFO,output,lambda] = dnopt(@hsmainobj, x, xl, xu, A, al, au, ...
				   @hsmaincon, cl, cu, options );

dnprint('off');
dnscreen('off');
dnend;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [f,g] = hsmainobj(x)

f =(x(1)-x(2))^2 +(x(2)-x(3))^3 +(x(3)-x(4))^4;
g = [  2*(x(1)-x(2));
      -2*(x(1)-x(2)) + 3*(x(2)-x(3))^2;
      -3*(x(2)-x(3))^2 + 4*(x(3)-x(4))^3;
      -4*(x(3)-x(4))^3;
       0];


function [c,G] = hsmaincon(x)

c = [ x(1) + x(2)^2 + x(3)^3;
      x(2) - x(3)^2 + x(4);
      x(1)*x(5)];

G = [ 1               0   x(5);
      2*x(2)          1   0;
      3*x(3)^2  -2*x(3)   0;
      0               1   0;
      0               0   x(1)];


