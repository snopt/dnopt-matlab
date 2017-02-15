function [f,g] = dnobj(objfun,x)

if nargout(objfun) == 2,
  [f,g] = objfun(x);
elseif nargout(objfun) == 1,
  f = objfun(x);
  g = [];
else
  error('DNOPT:InputArgs','Error in output arguments to obj');
end

