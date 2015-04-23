function [c,ceq,J,Jeq] = dncon ( nonlcon, x )
% Transpose the gradients of the constraints (to match fmincon interface).
% This is for dnsolve.m, which is based on fmincon and separates inequality
% and equality constraints.
%

if ( nargout(nonlcon) == 4 )
  [c,ceq,J,Jeq] = nonlcon(x);
  J = J';  Jeq = Jeq';

else
  [c,ceq] = nonlcon(x);
  J = []; Jeq = [];
end
