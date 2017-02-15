function [f,g] = dnmainobj(x)
% Compute the objective function and gradient

f = x(2)*x(6) - x(1)*x(7) + x(3)*x(7) + x(5)*x(8) - x(4)*x(9) - x(3)*x(8);

g = [ -x(7);
       x(6);
       x(7) - x(8);
      -x(9);
       x(8);
       x(2);
      -x(1) + x(3);
       x(5) - x(3);
      -x(4);];
