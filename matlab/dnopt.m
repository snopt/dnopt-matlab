function [x,fval,exitFlag,output,lambda] = dnopt(name,obj,x0,xl,xu,varargin)
% function [x,fval,exitFlag,output,lambda] = dnopt (name,obj,x0,xl,xu,varargin)
%
%   Solve the given nonlinear problem:
%       minimize          f(x)
%     subject to   xl  <=  x   <= xu
%                  cl  <= c(x) <= cu
%                  al  <= A*x  <= au
%
%  Calling sequences:
%  [] = dnopt( name, obj, x0, xl, xu  )
%  [] = dnopt( name, obj, x0, xl, xu, stopFun )
%  [] = dnopt( name, obj, x0, xl, xu, A, al, au )
%  [] = dnopt( name, obj, x0, xl, xu, A, al, au, stopFun )
%  [] = dnopt( name, obj, x0, xl, xu, A, al, au, nonlcon, cl, cu )
%  [] = dnopt( name, obj, x0, xl, xu, A, al, au, nonlcon, cl, cu, stopFun )
%
%  [x,fval,exitFlag]               = dnopt( name, obj, ... )
%  [x,fval,exitFlag,output]        = dnopt( name, obj, ... )
%  [x,fval,exitFlag,output,lambda] = dnopt( name, obj, ... )
%
%   INPUT:
%     name     is the 8-character name of the problem
%
%     obj      is the Matlab function that evaluates the objective function
%              f and computes the gradient of the objective function g at a
%              given point.
%              obj can be either a function handle or a string.  If it is a
%              string, then the function must be in its own m-file.
%              The function has the form:   [f,g] = obj(x)
%
%     x0       is the initial point x
%
%     xl, xu   are the lower and upper bounds of x
%
%     A        is the linear constraint matrix
%
%     al, au   are the lower and upper bounds of the linear constraints
%
%     nonlcon  (optional) is the Matlab function that evaluates the nonlinear
%              inequality and equality constraints and the gradients of
%              the inequality and equality constraints at a given point.
%              nonlcon can be either a function handle or a string.  If it is
%              a string, then the function must be in its own m-file.
%
%              NOTE: The Jacobian is the transpose of the gradients.  This is
%              done to match the input for fmincon.
%              The function has the form: [c,G] = nonlcon(x)
%
%     cl, cu   are the lower and upper bounds on the nonlinear constraints
%
%     stopFun  is a Matlab function that will be called every major iteration
%              of the algorithm.
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

solveOpt = 17;

nEval    = 0;
yL       = [];
yN       = [];
stopFun  = 0;

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


if ( nargin == 5 )
  if ( setDer )
    optString = [ 'Derivative level ' num2str(lvlDer) ];
    dnSet( optString );
  end

  [x,fval,exitFlag,itn,nEval,y] = dnoptmex( solveOpt, stopFun, name, objFun, x0, xl, xu );

elseif ( nargin == 6 )
  stopFun = varargin{1};

  if ( setDer )
    optString = [ 'Derivative level ' num2str(lvlDer) ];
    dnSet( optString );
  end

  [x,fval,exitFlag,itn,nEval,y] = dnoptmex( solveOpt, stopFun, name, objFun, x0, xl, xu );

elseif ( nargin == 8 )
  A       = varargin{1};
  al      = varargin{2};
  au      = varargin{3};

  if ( setDer )
    optString = [ 'Derivative level ' num2str(lvlDer) ];
    dnSet( optString );
  end

  [x,fval,exitFlag,itn,nEval,y,yL] = dnoptmex( solveOpt, stopFun, name, objFun, ...
					       x0, xl, xu, A, al, au );
elseif ( nargin == 9 )
  A       = varargin{1};
  al      = varargin{2};
  au      = varargin{3};
  stopFun = varargin{4};

  if ( setDer )
    optString = [ 'Derivative level ' num2str(lvlDer) ];
    dnSet( optString );
  end

  [x,fval,exitFlag,itn,nEval,y,yL] = dnoptmex( solveOpt, stopFun, name, objFun, ...
					       x0, xl, xu, A, al, au );
elseif ( nargin == 11 )
  A     = varargin{1};
  al    = varargin{2};
  au    = varargin{3};
  nonlc = varargin{4};
  cl    = varargin{5};
  cu    = varargin{6};

  if ( ischar(nonlc) )
    nonlcon = str2func(nonlc);
  else
    nonlcon = nonlc;
  end

  % Check if Jacobian for nonlinear constraints are provided.
  narg = nargout( nonlcon );
  if ( narg == 2 )
    [c,J] = nonlcon(x0);
    J = J';

    lvlDer = lvlDer + 2;

  elseif ( narg == 1 )
    [c]     = nonlcon(x0);
    [mc,nc] = size(c);
    [nn,n0] = size(x0);
    J = zeros(mc,nn);

  else
    error( 'Wrong number of output arguments for nonlcon function');
  end

  if ( setDer )
    optString = [ 'Derivative level ' num2str(lvlDer) ];
    dnSet( optString );
  end

  [x,fval,exitFlag,itn,nEval,y,yL,yN] = dnoptmex( solveOpt, stopFun, name, objFun, x0, xl, xu, A, ...
						  al, au, nonlcon, cl, cu, J );

elseif ( nargin == 12 )
  A       = varargin{1};
  al      = varargin{2};
  au      = varargin{3};
  nonlc   = varargin{4};
  cl      = varargin{5};
  cu      = varargin{6};
  stopFun = varargin{7};

  if ( ischar(nonlc) )
    nonlcon = str2func(nonlc);
  else
    nonlcon = nonlc;
  end

  % Check if Jacobian for nonlinear constraints are provided.
  narg = nargout( nonlcon );
  if ( narg == 2 )
    [c,J] = nonlcon(x0);
    J = J';

    lvlDer = lvlDer + 2;

  elseif ( narg == 1 )
    [c]     = nonlcon(x0);
    [mc,nc] = size(c);
    [nn,n0] = size(x0);
    J = zeros(mc,nn);

  else
    error( 'Wrong number of output arguments for nonlcon function');
  end

  if ( setDer )
    optString = [ 'Derivative level ' num2str(lvlDer) ];
    dnSet( optString );
  end

  [x,fval,exitFlag,itn,nEval,y,yL,yN] = dnoptmex( solveOpt, stopFun, name, ...
						  objFun, x0, xl, xu, A, al, ...
						  au, nonlcon, cl, cu, J );

else
  error('Wrong number of input arguments for dnopt');
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
  lambda.linear     = yL;
  lambda.nonlin     = yN;
end
