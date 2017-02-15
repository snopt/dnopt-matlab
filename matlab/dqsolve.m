function [x,fval,exitFlag,output,lambda] = dqsolve(H, f, varargin)
% function [x,fval,exitFlag,output,lambda] = dqsolve(H, f, varargin)
%
% This function interface is based on the MATLAB function quadprog.
% Currently, the only field recognized in the 'option's arguments are
%                options.name,  options.start
% All others are ignored.  Please call the dqset* or dqget* routines to set
% or retrieve options for DQOPT.
%
%
% Calling sequences:
%  x = dqsolve(H, f)
%  x = dqsolve(H, f, A, b)
%  x = dqsolve(H, f, A, b, Aeq, beq)
%  x = dqsolve(H, f, A, b, Aeq, beq, lb, ub)
%  x = dqsolve(H, f, A, b, Aeq, beq, lb, ub, x0)
%  x = dqsolve(H, f, A, b, Aeq, beq, lb, ub, x0, options)
%
%  [x,fval]                        = dqsolve(H, f, ...)
%  [x,fval,exitflag]               = dqsolve(H, f, ...)
%  [x,fval,exitflag,output]        = dqsolve(H, f, ...)
%  [x,fval,exitflag,output,lambda] = dqsolve(H, f, ...)
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
%     Aeq, beq(optional) contain the lineaer equality constraints Aeq*x <= beq
%
%     lb, ub  (optional) are the lower and upper bounds of x
%
%     x0       is the initial point x
%
%     options  is a struct.
%              options.name   is the problem name
%              options.start  'Cold', 'Warm'
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

probName = '';
istart   = 0;

if nargin == 2,
  % dqsolve(H, f)
  A   = [];  b   = [];
  Aeq = [];  beq = [];
  lb  = [];  ub  = [];
  x0  = [];

elseif nargin == 4,
  % dqsolve(H, f, A, b)
  A = varargin{1};
  b = varargin{2};
  Aeq = [];  beq = [];
  lb  = [];  ub  = [];
  x0  = [];


elseif nargin == 6,
  % dqsolve(H, f, A, b, Aeq, beq)
  A   = varargin{1};
  b   = varargin{2};
  Aeq = varargin{3};
  beq = varargin{4};
  lb  = [];  ub  = [];
  x0  = [];

elseif nargin == 8,
  % dqsolve(H, f, A, b, Aeq, beq, lb, ub)
  A   = varargin{1};
  b   = varargin{2};
  Aeq = varargin{3};
  beq = varargin{4};
  lb  = varargin{5};
  ub  = varargin{6};
  x0  = [];

elseif nargin == 9,
  % dqsolve(H, f, A, b, Aeq, beq, lb, ub, x0)
  A   = varargin{1};
  b   = varargin{2};
  Aeq = varargin{3};
  beq = varargin{4};
  lb  = varargin{5};
  ub  = varargin{6};
  x0  = varargin{7};

elseif nargin == 10,
  % dqsolve(H, f, A, b, Aeq, beq, lb, ub, x0, options)
  A   = varargin{1};
  b   = varargin{2};
  Aeq = varargin{3};
  beq = varargin{4};
  lb  = varargin{5};
  ub  = varargin{6};
  x0  = varargin{7};

  % Deal with options.
  optionsLoc = 8;
  if isstruct(varargin{optionsLoc}),
    options = varargin{optionsLoc};
    % Name
    if isfield(options,'name'),
      probName = options.name;
    end

    % Start
    if isfield(options,'start'),
      if strcmp(options.start,'Warm'),
	istart = 1;
      end
    end

  else
    error('DQOPT:InputArgs','Options struct error');
  end

else
  error('DQOPT:InputArgs','Wrong number of input arguments for dqsolve');
end

ineq = size(A,1);
AA   = [                 A; Aeq ];
al   = [ -inf*ones(ineq,1); beq ];
au   = [                 b; beq ];

[x,fval,exitFlag,itn,y,state] = dqoptmex(solveOpt, istart, probName, ...
					 H, f, x0, lb, ub, [], [], ...
					 AA,  al, au, [], []);

% Set output
output.iterations = itn;

m    = size(AA,1);
n    = size(x0,1);
zero = zeros(n);

states.x      = state(1:n);
lambda.x      = y(1:n);
if m > 0,
  states.linear = state(n+1:n+m);
  lambda.linear = y(n+1:n+m);
end

lambda.lb         = max(y(1:n),zero);
lambda.ub         = min(y(1:n),zero);
lambda.ineqlin    = y(1+n:ineq+n);
lambda.eqlin      = y(1+ineq+n:n+m);
