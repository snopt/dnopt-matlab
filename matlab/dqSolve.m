function [x,fval,exitFlag,output,lambda] = dqSolve ( H, f, varargin )
% function [x,fval,exitFlag,output,lambda] = dqSolve ( H, f, varargin )
%
% This function interface is based on the MATLAB function quadprog.
% Please call the npSet* or npGet* routines to set or retrieve options.
%
% Calling sequences:
%  x = dqSolve ( H, f )
%  x = dqSolve ( H, f, A, b )
%  x = dqSolve ( H, f, A, b, Aeq, beq )
%  x = dqSolve ( H, f, A, b, Aeq, beq, lb, ub )
%  x = dqSolve ( H, f, A, b, Aeq, beq, lb, ub, x0 )
%
%  [x,fval]                        = dqSolve ( H, f, ... )
%  [x,fval,exitflag]               = dqSolve ( H, f, ... )
%  [x,fval,exitflag,output]        = dqSolve ( H, f, ... )
%  [x,fval,exitflag,output,lambda] = dqSolve ( H, f, ... )
%
%
% Solve the given quadratic problem:
%       minimize        q(x) = half*x'*H*x + f'*x
%     subject to   lb <=  x  <= ub
%                       A*x  <= b
%                     Aeq*x   = beq
%
%   INPUT:
%     H        is the Hessian matrix of the objective
%
%     f        is the linear term of the objective
%
%     A, b     contain the linear inequality constraints A*x <= b
%
%     Aeq, beq (optional) contain the lineaer equality constraints Aeq*x <= beq
%
%     lb, ub   (optional) are the lower and upper bounds of x
%
%     x0       is the initial point x
%
%
%   OUTPUT:
%     x        is the final point
%
%     fval     is the final objective value
%
%     exitFlag is the exit flag returned by DQOPT
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
%
%
solveOpt = 1;

if ( nargin == 2 )
  A   = [];  b   = [];
  Aeq = [];  beq = [];
  lb  = [];  ub  = [];
  x0  = [];

elseif ( nargin == 4 )
  A = varargin{1};
  b = varargin{2};

  Aeq = [];  beq = [];
  lb  = [];  ub  = [];
  x0  = [];


elseif ( nargin == 6 )
  A   = varargin{1};
  b   = varargin{2};
  Aeq = varargin{3};
  beq = varargin{4};

  Aeq = [];  beq = [];
  lb  = [];  ub  = [];
  x0  = [];

elseif ( nargin == 8 )
  A   = varargin{1};
  b   = varargin{2};
  Aeq = varargin{3};
  beq = varargin{4};
  lb  = varargin{5};
  ub  = varargin{6};

  x0  = [];

elseif ( nargin == 9 )
  A   = varargin{1};
  b   = varargin{2};
  Aeq = varargin{3};
  beq = varargin{4};
  lb  = varargin{5};
  ub  = varargin{6};

  x0  = varargin{7};

else
  error( 'Wrong number of input arguments for dqSolve');
end


[x,fval,exitFlag,itn,y,yA,yAeq] = dqoptmex ( solveOpt, H, f, A, b, Aeq, beq, ...
					     lb, ub, x0 );

if     ( nargout >= 4 )
  output.iterations = itn;
end

if ( nargout >= 5 )
  n    = size(x);
  zero = zeros(n);

  lambda.lb         = max(y,zero);
  lambda.ub         = min(y,zero);
  lambda.ineqlin    = yA;
  lambda.eqlin      = yAeq;
end
