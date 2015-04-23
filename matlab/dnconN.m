function [c,J] = dnconN ( nonlcon, x )
% Transpose the gradients of the constraints (to match fmincon interface)
% This is for dnopt.m (the non-fmincon version of dnopt,
% which does not separate inequality and equality constraints.
%

if ( nargout(nonlcon) == 2 )
  [c,J] = nonlcon(x);
  J     = J';
else
  [c] = nonlcon(x);
  J   = [];
end
