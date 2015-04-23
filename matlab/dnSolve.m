function [x,fval,exitFlag,output,lambda] = dnSolve ( obj, x0, A, b, varargin )
% function [x,fval,exitFlag,output,lambda] = dnSolve ( obj, x0, A, b, varargin )
%
% This function interface is based on the MATLAB function fmincon.
%
% Currently, the only field recognized in the 'option's argument is
%      options.OutputFcn,
% which is a handle for a function called at each major iteration.
% All others are ignored.  Please call the dnSet* or dnGet* routines to set
% or retrieve options for DNOPT.
%
%
% Calling sequences:
%  x = dnSolve ( obj, x0, A, b )
%  x = dnSolve ( obj, x0, A, b, Aeq, beq )
%  x = dnSolve ( obj, x0, A, b, Aeq, beq, lb, ub )
%  x = dnSolve ( obj, x0, A, b, Aeq, beq, lb, ub, nonlcon )
%  x = dnSolve ( obj, x0, A, b, Aeq, beq, lb, ub, nonlcon, options )
%
%  [x,fval]                        = dnSolve ( obj, x0, A, b, ... )
%  [x,fval,exitFlag]               = dnSolve ( obj, x0, A, b, ... )
%  [x,fval,exitFlag,output]        = dnSolve ( obj, x0, A, b, ... )
%  [x,fval,exitFlag,output,lambda] = dnSolve ( obj, x0, A, b, ... )
%
%
% Solve the given nonlinear problem:
%       minimize        f(x)
%     subject to   lb <=  x  <= ub
%                       c(x) <= 0
%                     ceq(x)  = 0
%                       A*x  <= b
%                     Aeq*x   = beq
%
%   INPUT:
%     obj      is the Matlab function that evaluates the objective function
%              f and computes the gradient of the objective function g at a
%              given point.
%              obj can be either a function handle or a string.  If it is a
%              string, then the function must be in its own m-file.
%              The function has the form:   [f,g] = obj(x)
%
%     x0       is the initial point x
%
%     A, b     contain the linear inequality constraints A*x <= b
%
%     Aeq, beq (optional) contain the lineaer equality constraints Aeq*x <= beq
%
%     lb, ub   (optional) are the lower and upper bounds of x
%
%     nonlcon  (optional) is the Matlab function that evaluates the nonlinear
%              inequality and equality constraints and the gradients of
%              the inequality and equality constraints at a given point.
%              nonlcon can be either a function handle or a string.  If it is
%              a string, then the function must be in its own m-file.
%
%              NOTE: nonlcon provides the gradients, NOT the Jacobian matrix,
%              which is the transpose of the gradients of the functions.
%              The function has the form: [c,ceq,G,Geq] = nonlcon(x)
%
%   OUTPUT:
%     x        is the final point
%
%     fval     is the final objective value
%
%     exitFlag is the exit flag returned by DNOPT
%
%     output   is a structure containing run information --
%              output.iterations is the total number of iterations
%              output.funcCount   is the total number of function evaluations
%
%     lambda   is a structure containing the multipliers
%              lambda.lb         are for the lower bounds
%              lambda.ub         are for the upper bounds
%              lambda.ineqlin    are for the linear inequality constraints
%              lambda.eqlin      are for the linear equality constraints
%              lambda.ineqnonlin are for the nonlinear inequality constraints
%              lambda.eqnonlin   are for the nonlinear equality constraints
%
%
solveOpt = 1;

yAeq = [];
yC   = [];
yCeq = [];

if ( ischar(obj) )
  objFun = str2func(obj);
else
  objFun = obj;
end

derSet = dnGet('Derivative level');
setDer = derSet < 0 || derSet > 3;

% Check if objective gradient is provided.
narg = nargout( objFun );
if     ( narg == 2 )
  lvlDer = 1;
elseif ( narg == 1 )
  lvlDer = 0;
else
  error('Wrong number of output arguments for obj function');
end


if ( nargin == 4 )
  if ( setDer )
    optString = [ 'Derivative level ' num2str(lvlDer) ];
    dnSet( optString );
  end

  [x,fval,exitFlag,itn,nEval,y,yA] = dnoptmex ( solveOpt, objFun, x0, A, b );

elseif ( nargin == 6 )
  Aeq = varargin{1};
  beq = varargin{2};

  if ( setDer )
    optString = [ 'Derivative level ' num2str(lvlDer) ];
    dnSet( optString );
  end

  [x,fval,exitFlag,itn,nEval,y,yA,yAeq] = dnoptmex ( solveOpt, objFun, x0, A, ...
   						     b, Aeq, beq );

elseif ( nargin == 8 )
  Aeq = varargin{1};
  beq = varargin{2};
  lb  = varargin{3};
  ub  = varargin{4};

  if ( setDer)
    optString = [ 'Derivative level ' num2str(lvlDer) ];
    dnSet( optString );
  end

  [x,fval,exitFlag,itn,nEval,y,yA,yAeq] = dnoptmex ( solveOpt, objFun, x0, A, ...
						     b, Aeq, beq, lb, ub );

elseif ( nargin == 9 )
  Aeq   = varargin{1};
  beq   = varargin{2};
  lb    = varargin{3};
  ub    = varargin{4};
  nonlc = varargin{5};

  if ( ischar(nonlc) )
    nonlcon = str2func(nonlc);
  else
    nonlcon = nonlc;
  end

  % Check if Jacobian for nonlinear constraints are provided.
  narg = nargout( nonlcon );
  if ( narg == 4 )
    [c,ceq,J,Jeq] = nonlcon(x0);
    J = J';  Jeq = Jeq';

    lvlDer = lvlDer + 2;

  elseif ( narg == 2 )
    [c,ceq]     = nonlcon(x0);
    [mc,nc]     = size(c);
    [mceq,nceq] = size(ceq);
    [nn,n0]     = size(x0);

    J = ones(mc,nn); Jeq = ones(mceq,nn);
  else
    error( 'Wrong number of output arguments for nonlcon function');
  end

  if ( setDer )
    optString = [ 'Derivative level ' num2str(lvlDer) ];
    dnSet( optString );
  end

  [x,fval,exitFlag,itn,nEval,y,yA,yAeq,yC,yCeq] = dnoptmex ( solveOpt, objFun, ...
						  x0, A, b, Aeq, beq, lb, ub, ...
						  nonlcon, J, Jeq );
elseif ( nargin == 10 )
  Aeq     = varargin{1};
  beq     = varargin{2};
  lb      = varargin{3};
  ub      = varargin{4};
  nonlc   = varargin{5};
  options = varargin{6};
  stopFun = getfield(options,'OutputFcn');

  if ( ischar(nonlc) )
    nonlcon = str2func(nonlc);
  else
    nonlcon = nonlc;
  end

  % Check if Jacobian for nonlinear constraints are provided.
  narg = nargout( nonlcon );
  if ( narg == 4 )
    [c,ceq,J,Jeq] = nonlcon(x0);
    J = J';  Jeq = Jeq';

    lvlDer = lvlDer + 2;

  elseif ( narg == 2 )
    [c,ceq]     = nonlcon(x0);
    [mc,nc]     = size(c);
    [mceq,nceq] = size(ceq);
    [nn,n0]     = size(x0);

    J = ones(mc,nn); Jeq = ones(mceq,nn);

  else
    error( 'Wrong number of output arguments for nonlcon function');
  end

  if ( setDer )
    optString = [ 'Derivative level ' num2str(lvlDer) ];
    dnSet( optString );
  end

  [x,fval,exitFlag,itn,nEval,y,yA,yAeq,yC,yCeq] = dnoptmex ( solveOpt, objFun, ...
						  x0, A, b, Aeq, beq, lb, ub, ...
						  nonlcon, J, Jeq, stopFun );
else
  error( 'Wrong number of input arguments for dnSolve');
end

if     ( nargout >= 4 )
  output.iterations = itn;
  output.funcCount  = nEval;
end

if ( nargout >= 5 )
  n    = size(x);
  zero = zeros(n);

  lambda.lb         = max(y,zero);
  lambda.ub         = min(y,zero);
  lambda.ineqlin    = yA;
  lambda.eqlin      = yAeq;
  lambda.ineqnonlin = yC;
  lambda.eqnonlin   = yCeq;
end
