function [f,g] = dnobj ( objfun, x )

if ( nargout(objfun) == 2 )
  [f,g] = objfun(x);

else
  f = objfun(x);
  g = [];
end
