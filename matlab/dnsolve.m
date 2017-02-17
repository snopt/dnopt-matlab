function [x,fval,exitFlag,output,lambda] = dnsolve(obj, x0, A, b, varargin)
% function [x,fval,exitFlag,output,lambda] = dnsolve(obj, x0, A, b, varargin)
%
% This function interface is based on the MATLAB function fmincon.
%
% Currently, the only field recognized in the 'option's arguments are
%                options.name,  options.stop,  options.start
% All others are ignored.  Please call the dnset* or dnget* routines to set
% or retrieve options for DNOPT.
%
%
% Calling sequences:
%  x = dnsolve(obj, x0, A, b)
%  x = dnsolve(obj, x0, A, b, options)
%  x = dnsolve(obj, x0, A, b, Aeq, beq)
%  x = dnsolve(obj, x0, A, b, Aeq, beq, options)
%  x = dnsolve(obj, x0, A, b, Aeq, beq, lb, ub)
%  x = dnsolve(obj, x0, A, b, Aeq, beq, lb, ub, options)
%  x = dnsolve(obj, x0, A, b, Aeq, beq, lb, ub, nonlcon)
%  x = dnsolve(obj, x0, A, b, Aeq, beq, lb, ub, nonlcon, options)
%
%  [x,fval]                        = dnsolve(obj, x0, A, b, ...)
%  [x,fval,exitFlag]               = dnsolve(obj, x0, A, b, ...)
%  [x,fval,exitFlag,output]        = dnsolve(obj, x0, A, b, ...)
%  [x,fval,exitFlag,output,lambda] = dnsolve(obj, x0, A, b, ...)
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
%     Aeq, beq(optional) contain the lineaer equality constraints Aeq*x <= beq
%
%     lb, ub  (optional) are the lower and upper bounds of x
%
%     nonlcon (optional) is the Matlab function that evaluates the nonlinear
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
solveOpt   = 1;

probName   = '';
istart     = 0;
stopFun    = 0;
optionsLoc = 0;

% Check obj
objFun = checkFun(obj,'DNOPT',[1 2]);


% Check derivative settings.  Is objective gradient provided?
derSet  = dnget('Derivative level');
setDer  = derSet < 0 || derSet > 3;
nargobj = nargout(obj);
if nargobj == 2,
  lvlDer = 1;
elseif nargobj == 1,
  lvlDer = 0;
else
  error('DNOPT:InputArgs','Wrong number of output arguments for %s',inputname(obj));
end

% Deal with options
if nargin == 5 || nargin == 7 || nargin == 9 || nargin == 10,
  optionsLoc = nargin - 4;
  if isstruct(varargin{optionsLoc}),
    options = varargin{optionsLoc};

    % Name
    if isfield(options,'name'),
      probName = options.name;
    end

    % Start
    if isfield(options,'start'),
      if strcmp(lower(options.start),'warm'),
	istart = 2;
      elseif strcmp(lower(options.start),'hot'),
	istart = 3;
      end
    end

    % Stop function
    if isfield(options,'stop'),
      if ischar(options.stop),
	stopFun = str2func(options.stop);
      elseif isa(options.stop,'function_handle'),
	stopFun = options.stop;
      else
	error('DNOPT:InputArgs','%s.stop should be a string or function handle',inputname(options));
      end
    end
  else
    optionsLoc = 0;
  end
end


n           = size(x0,1);
linear_ineq = size(A,1);
linear_eq   = 0;
nonlin_ineq = 0;
nonlin_eq   = 0;


if nargin == 4 || nargin == 5,
  % dnsolve(obj, x0, A, b)
  % dnsolve(obj, x0, A, b, options)

  if setDer,
    dnset(['Derivative level ' num2str(lvlDer) ]);
  end

  [x,fval,exitFlag,itn,nEval,y,istate] = dnoptmex(solveOpt, istart, stopFun, probName, ...
						  objFun, x0, [], [], [], [],...
						  A, [], b, [], []);

elseif nargin == 6 || nargin == 7,
  % dnsolve(obj, x0, A, b, Aeq, beq)
  % dnsolve(obj, x0, A, b, Aeq, beq, options)

  Aeq       = varargin{1};
  beq       = varargin{2};
  linear_eq = size(Aeq,1);

  if setDer,
    dnset(['Derivative level ' num2str(lvlDer)]);
  end

  [ineq,nn] = size(A);
  AA = [                 A; Aeq ];
  al = [ -inf*ones(ineq,1); beq ];
  au = [                 b; beq ];
  [x,fval,exitFlag,itn,nEval,y,istate] = dnoptmex(solveOpt, istart, stopFun, probName, ...
						  objFun, x0, [], [], [], [], ...
						  AA, al, au, [], []);

elseif nargin == 8 || (nargin == 9 && optionsLoc ~= 0),
  % dnsolve(obj, x0, A, b, Aeq, beq, lb, ub)
  % dnsolve(obj, x0, A, b, Aeq, beq, lb, ub, options)

  Aeq       = varargin{1};
  beq       = varargin{2};
  lb        = varargin{3};
  ub        = varargin{4};
  linear_eq = size(Aeq,1);

  if setDer,
    dnset(['Derivative level ' num2str(lvlDer)]);
  end

  AA = [                        A; Aeq ];
  al = [ -inf*ones(linear_ineq,1); beq ];
  au = [                        b; beq ];

  [x,fval,exitFlag,itn,nEval,y,istate] = dnoptmex(solveOpt, istart, stopFun, probName, ...
						  objFun, x0, lb, ub, [], [], ...
						  AA, al, au, [], []);

elseif nargin == 9 || nargin == 10,
  % dnsolve(obj, x0, A, b, Aeq, beq, lb, ub, nonlcon)
  % dnsolve(obj, x0, A, b, Aeq, beq, lb, ub, nonlcon, options)

  Aeq       = varargin{1};
  beq       = varargin{2};
  lb        = varargin{3};
  ub        = varargin{4};
  nonlc     = varargin{5};
  linear_eq = size(Aeq,1);

  nonlcon = checkFun(nonlc,'DNOPT');

  % Check if Jacobian for nonlinear constraints are provided.
  narg = nargout(nonlcon);
  if narg == 4,
    [c,ceq,J,Jeq] = nonlcon(x0);
    nonlin_ineq   = size(c,1);
    nonlin_eq     = size(ceq,1);

    J = J';  Jeq = Jeq';
    lvlDer = lvlDer + 2;

  elseif narg == 2,
    [c,ceq]     = nonlcon(x0);
    nonlin_ineq   = size(c,1);
    nonlin_eq     = size(ceq,1);

    J = ones(nonlin_ineq,n); Jeq = ones(nonlin_eq,n);
  else
    error('DNOPT:InputArgs','Wrong number of output arguments for %s',inputname(nonlc));
  end

  if setDer,
    dnset(['Derivative level ' num2str(lvlDer)]);
  end

  AA = [                        A; Aeq ];
  al = [ -inf*ones(linear_ineq,1); beq ];
  au = [                        b; beq ];

  JJ = [                        J; Jeq ];
  cl = [ -inf*ones(nonlin_ineq,1); zeros(nonlin_eq,1)];
  cu = [     zeros(nonlin_ineq,1); zeros(nonlin_eq,1)];

  [x,fval,exitFlag,itn,nEval,y,istate] = dnoptmex(solveOpt, istart, stopFun, probName, ...
						  objFun, x0, lb, ub, [], [], ...
						  AA, al, au, [], [], ...
						  @(x)dnsolveCon(x,nonlcon), ...
						  cl, cu, [], [], JJ);

else
  error('DNOPT:InputArgs','Wrong number of input arguments for dnsolve');
end

% Set output
zero              = zeros(n);

lambda.lb         = max(y(1:n),zero);
lambda.ub         = min(y(1:n),zero);

if nonlin_ineq > 0,
  i1 = n+1; i2 = i1-1 + nonlin_ineq;
  lambda.ineqnonlin = y(i1:i2);
else
  lambda.ineqnonlin = [];
end

if nonlin_eq > 0,
  i1 = n+nonlin_ineq; i2 = i1-1 + nonlin_eq;
  lambda.eqnonlin = y(i1:i2);
else
  lambda.eqnonlin = [];
end

if linear_ineq > 0,
  i1 = 1+n+nonlin_ineq + nonlin_eq; i2 = i1-1 + linear_ineq;
  lambda.ineqlin = y(i1:i2);
else
  lambda.ineqlin = [];
end

if linear_eq > 0,
  i1 = 1+n+nonlin_ineq + nonlin_eq + linear_ineq; i2 = i1-1 + linear_eq;
  lambda.eqlin = y(i1:i2);
else
  lambda.eqlin = [];
end

output.iterations = itn;
output.funcCount  = nEval;


function [cc,JJ] = dnsolveCon(x,nonlcon)
% Wrapper for fmincon-style user-defined constraint function nonlcon.
%

if nargout(nonlcon) == 4,
  [c,ceq,J,Jeq] = nonlcon(x);
  J = J';  Jeq = Jeq';
  cc = [c; ceq];
  JJ = [J; Jeq];

elseif nargout(nonlcon) == 2,
  [c,ceq] = nonlcon(x);
  cc = [c; ceq];
  JJ = [];
else
  error('DNOPT:InputArgs',['%s should have 2 or 4 output arguments, not' ...
		    ' %d', inputname(nonlcon),nargout(nonlcon)]);
end
