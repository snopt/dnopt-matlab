 function dnmain
% Matalb example problem
%
%   maximize  x_2 x_6 - x_1 x_7 + x_3 x_7 + x_5 x_8 - x_4 x_9 - x_3 x_8
%   subject to
%            xlow <=   x  <= xupp
%            Flow <= F(x) <= Fupp
%   where
%     F(1) =    x_1^2 + x_6^2
%     F(2) =  (x_2   - x_1)^2  + (x_7 - x_6)^2
%     F(3) =  (x_3   - x_1)^2  +   x_6^2
%     F(4) =  (x_1   - x_4)^2  + (x_6 - x_8)^2
%     F(5) =  (x_1   - x_5)^2  + (x_6 - x_9)^2
%     F(6) =    x_2^2 + x_7^2
%     F(7) =  (x_3   - x_2)^2  +   x_7^2
%     F(8) =  (x_4   - x_2)^2  + (x_8 - x_7)^2
%     F(9) =  (x_2   - x_5)^2  + (x_7 - x_9)^2
%     F(10) =  (x_4   - x_3)^2  +   x_8^2
%     F(11) =  (x_5   - x_3)^2  +   x_9^2
%     F(12) =    x_4^2 +  x_8^2
%     F(13) =  (x_4   - x_5)^2 +(x_9 - x_8)^2
%     F(14) =    x_5^2 + x_9^2
%
%     F(15) =   -x_1 + x_2
%     F(16) =         -x_2 + x_3
%     F(17) =                x_3 - x_4
%     F(18) =                      x_4 - x_5

dnscreen('on');
dnprint('dnmain.out');

dnmain.spc = which('dnmain.spc');
dnspec(dnmain.spc);

dnsetint('Major Iteration limit', 250);
dnset('Maximize');

% Set up the problem.
n   = 9;
x   = [ .1;
        .125;
        .666666;
        .142857;
        .111111;
        .2;
        .25;
       -.2;
       -.25 ];

xlow = -Inf*ones(n,1);
xupp =  Inf*ones(n,1);

xlow(1) =  0;
xlow(3) = -1;
xlow(5) =  0;
xlow(6) =  0;
xlow(7) =  0;

xupp(3) =  1;
xupp(8) =  0;
xupp(9) =  0;

% Linear constraints
%  0 <=  -x_1 + x_2
%  0 <=  -x_2 + x_3
%  0 <=         x_3 - x_4
%  0 <=               x_4 - x_5
nA = 4;
A  = [ 1  1  0  0  0  0  0  0  0;
       0 -1  1  0  0  0  0  0  0;
       0  0  1 -1  0  0  0  0  0;
       0  0  0  1 -1  0  0  0  0];
al = zeros(nA,1);
au = Inf*ones(nA,1);

% Nonlinear constraints
%    x_1^2 + x_6^2                    <= 1
%  (x_2   - x_1)^2  + (x_7 - x_6)^2   <= 1
%  (x_3   - x_1)^2  +   x_6^2         <= 1
%  (x_1   - x_4)^2  + (x_6 - x_8)^2   <= 1
%  (x_1   - x_5)^2  + (x_6 - x_9)^2   <= 1
%    x_2^2 + x_7^2                    <= 1
%  (x_3   - x_2)^2  +   x_7^2         <= 1
%  (x_4   - x_2)^2  + (x_8 - x_7)^2   <= 1
%  (x_2   - x_5)^2  + (x_7 - x_9)^2   <= 1
%  (x_4   - x_3)^2  +   x_8^2         <= 1
%  (x_5   - x_3)^2  +   x_9^2         <= 1
%    x_4^2 +  x_8^2                   <= 1
%  (x_4   - x_5)^2 +(x_9 - x_8)^2     <= 1
%    x_5^2 + x_9^2                    <= 1
nC = 14;
cl = -Inf*ones(nC,1);
cu =      ones(nC,1);

% Solve the problem.
options.name = 'dnmain';
[x,F,INFO,output,lambda,states,H] = dnopt('dnmainobj', x, xlow, xupp, A, al, au, ...
					@dnmaincon, cl, cu, options);

% Do a warm start and input the solution:
options.start = 'Warm';
[x,F,INFO] = dnopt('dnmainobj', x, xlow, xupp, A, al, au, ...
		   @dnmaincon, cl, cu, lambda, states, H, options);

dnprint('off');
dnscreen('off');
dnend;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% dnmainobj is passed as a string, so it must be in its own m-file
% dnmainobj.m
%
% function [f,g] = dnmainobj(x)
% % Compute the objective function and gradient
%
% f = x(2)*x(6) - x(1)*x(7) + x(3)*x(7) + x(5)*x(8) - x(4)*x(9) - x(3)*x(8);
%
% g = [ -x(7);
%        x(6);
%        x(7) - x(8);
%       -x(9);
%        x(8);
%        x(2);
%       -x(1) + x(3);
%        x(5) - x(3);
%       -x(4);];


function [c,G] = dnmaincon(x)
% Compute the nonlinear constraints and gradients
%  (x_1   - x_4)^2  + (x_6 - x_8)^2   <= 1
%  (x_1   - x_5)^2  + (x_6 - x_9)^2   <= 1
%    x_2^2 + x_7^2                      <= 1
%  (x_3   - x_2)^2  +   x_7^2          <= 1
%  (x_4   - x_2)^2  + (x_8 - x_7)^2   <= 1
%  (x_2   - x_5)^2  + (x_7 - x_9)^2   <= 1
%  (x_4   - x_3)^2  +   x_8^2          <= 1
%  (x_5   - x_3)^2  +   x_9^2          <= 1
%    x_4^2 +  x_8^2                     <= 1
%  (x_4   - x_5)^2 +(x_9 - x_8)^2     <= 1
%    x_5^2 + x_9^2                      <= 1

c = [    x(1)^2          +  x(6)^2;
       (x(2) - x(1))^2  + (x(7) - x(6))^2;
       (x(3) - x(1))^2  +  x(6)^2;
       (x(1) - x(4))^2  + (x(6) - x(8))^2;
       (x(1) - x(5))^2  + (x(6) - x(9))^2;
         x(2)^2          +  x(7)^2;
       (x(3) - x(2))^2  +  x(7)^2;;
       (x(4) - x(2))^2  + (x(8) - x(7))^2;
       (x(2) - x(5))^2  + (x(7) - x(9))^2;
       (x(4) - x(3))^2  +  x(8)^2;
       (x(5) - x(3))^2  +  x(9)^2;
         x(4)^2          +  x(8)^2;
       (x(4) - x(5))^2  + (x(9) - x(8))^2;
         x(5)^2          +  x(9)^2; ];

J = [  1,   1,     2*x(1);
       1,   6,     2*x(6);
       2,   1,    -2*(x(2) - x(1));
       2,   2,     2*(x(2) - x(1));
       2,   6,    -2*(x(7) - x(6));
       2,   7,     2*(x(7) - x(6));
       3,   1,    -2*(x(3) - x(1));
       3,   3,     2*(x(3) - x(1));
       3,   6,     2*x(6);
       4,   1,     2*(x(1) - x(4));
       4,   4,    -2*(x(1) - x(4));
       4,   6,     2*(x(6) - x(8));
       4,   8,    -2*(x(6) - x(8));
       5,   1,     2*(x(1) - x(5));
       5,   5,    -2*(x(1) - x(5));
       5,   6,     2*(x(6) - x(9));
       5,   9,    -2*(x(6) - x(9));
       6,   2,     2*x(2);
       6,   7,     2*x(7);
       7,   2,    -2*(x(3) - x(2));
       7,   3,     2*(x(3) - x(2));
       7,   7,     2*x(7);
       8,   2,    -2*(x(4) - x(2));
       8,   4,     2*(x(4) - x(2));
       8,   7,    -2*(x(8) - x(7));
       8,   8,     2*(x(8) - x(7));
       9,   2,     2*(x(2) - x(5));
       9,   5,    -2*(x(2) - x(5));
       9,   7,     2*(x(7) - x(9));
       9,   9,    -2*(x(7) - x(9));
      10,   3,    -2*(x(4) - x(3));
      10,   4,     2*(x(4) - x(3));
      10,   8,     2*x(8);
      11,   3,    -2*(x(5) - x(3));
      11,   5,     2*(x(5) - x(3));
      11,   9,     2*x(9);
      12,   4,     2*x(4);
      12,   8,     2*x(8);
      13,   4,     2*(x(4) - x(5));
      13,   5,    -2*(x(4) - x(5));
      13,   8,    -2*(x(9) - x(8));
      13,   9,     2*(x(9) - x(8));
      14,   5,     2*x(5);
      14,   9,     2*x(9) ];
G = sparse(J(:,2), J(:,1), J(:,3));
G = full(G);
