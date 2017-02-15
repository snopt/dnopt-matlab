function [c,J] = dncon(nonlcon,x)
% Wrapper function for the user-defined function nonlcon.
% Transpose the gradients of the constraints (to match fmincon interface).
%

if nargout(nonlcon) == 2,
  % User provides constraint functions and derivatives
  [c,J] = nonlcon(x);
  J     = J';

elseif nargout(nonlcon) == 1,
  % User provides only constraint functions
  [c] = nonlcon(x);
  J   = [];

elseif nargout(nonlcon) == -1,
  % -1 should indicate the use of the wrapper "dnsolveCon" for the
  % fmincon-style interface.  No transpose, the wrapper function will do it
  [c,J] = nonlcon(x);

else
  error('DNOPT:InputArgs', 'nonlcon has %d output arguments',nargout(nonlcon));
end
