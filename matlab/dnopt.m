function [x,fval,exitFlag,output,lambda,states] = ...
    dnopt(obj, x0, xl, xu, varargin)
% function [x,fval,exitFlag,output,lambda] = dnopt(obj,x0,xl,xu,varargin)
%
%   Solve the given nonlinear problem:
%       minimize          f(x)
%     subject to   xl  <=  x   <= xu
%                  cl  <= c(x) <= cu
%                  al  <= A*x  <= au
%
%  Calling sequences:
%  [] = dnopt(obj, x, xl, xu )
%  [] = dnopt(obj, x, xl, xu, options )
%  [] = dnopt(obj, x, xl, xu, states, lambda )
%  [] = dnopt(obj, x, xl, xu, states, lambda, options)
%  [] = dnopt(obj, x, xl, xu, A, al, au)
%  [] = dnopt(obj, x, xl, xu, A, al, au, options)
%  [] = dnopt(obj, x, xl, xu, A, al, au, states, lambda)
%  [] = dnopt(obj, x, xl, xu, A, al, au, states, lambda, options)
%  [] = dnopt(obj, x, xl, xu, A, al, au, nonlcon, cl, cu)
%  [] = dnopt(obj, x, xl, xu, A, al, au, nonlcon, cl, cu, options)
%  [] = dnopt(obj, x, xl, xu, A, al, au, nonlcon, cl, cu, states, lambda)
%  [] = dnopt(obj, x, xl, xu, A, al, au, nonlcon, cl, cu, states, lambda, options)
%
%  [x,fval,exitFlag]                      = dnopt(...)
%  [x,fval,exitFlag,output]               = dnopt(...)
%  [x,fval,exitFlag,output,lambda]        = dnopt(...)
%  [x,fval,exitFlag,output,lambda,states] = dnopt(...)
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
%     xl, xu   are the lower and upper bounds of x
%
%     A        is the linear constraint matrix
%
%     al, au   are the lower and upper bounds of the linear constraints
%
%     nonlcon (optional) is the Matlab function that evaluates the nonlinear
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
%     states, lambda are structs containing the vectors of the initial
%              states and multipliers for the variables, nonlinear and
%              linear constraints.
%
%     options  is a struct.
%              options.name   is the problem name
%              options.stop   is the "dnSTOP" function called at every
%                             major iteration.
%              options.start  'Cold', 'Warm'
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
%              lambda.x          are for the variable bounds
%              lambda.nonlin     are for the nonlinear constraints
%              lambda.linear     are for the    linear constraints
%
%     states   is a structure containing the final states
%              states.x
%              states.nonlin
%              states.linear
%
%
solveOpt   = 1;

probName   = '';
istart     = 0;
stopFun    = 0;
optionsLoc = 0;

% Check objFun
if(ischar(obj))
  objFun = str2func(obj);
elseif isa(obj,'function_handle'),
  objFun = obj;
else
  error('DNOPT:InputArgs','obj should be a function handle or string');
end

nargobj = nargout(objFun);
if abs(nargobj) ~= 1 && abs(nargobj) ~= 2,
  error('DNOPT:InputArgs','objFun should return 1 or 2 arguments');
end


% Check derivative settings.  Is objective gradient provided?
derSet = dnget('Derivative level');
setDer = derSet < 0 || derSet > 3;
if nargobj == 2,
  lvlDer = 1;
elseif nargobj == 1,
  lvlDer = 0;
else
  error('DNOPT:InputArgs','Wrong number of output arguments for obj function');
end


% Deal with options first.
if nargin == 5 || nargin == 7 || nargin == 8 || nargin == 10 || ...
	   nargin == 11 || nargin == 13,
  optionsLoc = nargin - 4;
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

    % Stop function
    if isfield(options,'stop'),
      if(ischar(options.stop))
	stopFun = str2func(options.stop);
      else
	stopFun = options.stop;
      end
    end
  else
    optionsLoc = 0;
  end
end


n     = size(x0,1);
nlCon = 0;
nnCon = 0;

if nargin == 4 || nargin == 5,
  % dnopt(obj, x, xl, xu )
  % dnopt(obj, x, xl, xu, options )

  xstate = []; xmul = [];

  if setDer,
    dnset(['Derivative level ' num2str(lvlDer)]);
  end

  [x,fval,exitFlag,itn,nEval,y,istate] = dnoptmex(solveOpt, istart, stopFun, probName, ...
						  objFun, x0, xl, xu, xstate, xmul);

elseif nargin == 6 || (nargin == 7 && optionsLoc ~= 0),
  % dnopt(obj, x, xl, xu, states, lambda )
  % dnopt(obj, x, xl, xu, states, lambda, options)

  states = varargin{1};
  lambda = varargin{2};
  xstate = []; xmul = [];

  if isfield(states,'x'),
    xstate = states.x;
  end

  if isfield(lambda,'x'),
    xmul = lambda.x;
  end

  if setDer,
    dnset(['Derivative level ' num2str(lvlDer)]);
  end

  [x,fval,exitFlag,itn,nEval,y,istate] = dnoptmex(solveOpt, istart, stopFun, probName, ...
						  objFun, x0, xl, xu, xstate, xmul);

elseif nargin == 7 || nargin == 8,
  % dnopt(obj, x, xl, xu, A, al, au)
  % dnopt(obj, x, xl, xu, A, al, au, options)

  A      = varargin{1};
  al     = varargin{2};
  au     = varargin{3};
  xstate = []; xmul = [];
  astate = []; amul = [];
  nlCon  = size(A,1);

  if setDer,
    dnset(['Derivative level ' num2str(lvlDer)]);
  end

  [x,fval,exitFlag,itn,nEval,y,istate] = dnoptmex(solveOpt, istart, stopFun, probName, ...
						  objFun, x0, xl, xu, xstate, xmul,...
						  A, al, au, astate, amul);

elseif nargin == 9 || (nargin == 10 && optionsLoc ~= 0),
  % dnopt(obj, x, xl, xu, A, al, au, states, lambda)
  % dnopt(obj, x, xl, xu, A, al, au, states, lambda, options)

  A      = varargin{1};
  al     = varargin{2};
  au     = varargin{3};
  states = varargin{4};
  lambda = varargin{5};
  nlCon  = size(A,1);

  xstate = []; xmul = [];
  astate = []; amul = [];

  if isfield(states,'x'),
    xstate = states.x;
  end

  if isfield(states,'linear'),
    astate = states.linear;
  end

  if isfield(lambda,'x'),
    xmul = lambda.x;
  end

  if isfield(lambda,'linear'),
    amul = lambda.linear;
  end

  if setDer,
    dnset(['Derivative level ' num2str(lvlDer)]);
  end

  [x,fval,exitFlag,itn,nEval,y,istate] = dnoptmex(solveOpt, istart, stopFun, probName, ...
						  objFun, x0, xl, xu, xstate, xmul,...
						  A, al, au, astate, amul);

elseif nargin == 10 || nargin == 11,
  % dnopt(obj, x, xl, xu, A, al, au, nonlcon, cl, cu)
  % dnopt(obj, x, xl, xu, A, al, au, nonlcon, cl, cu, options)

  A      = varargin{1};
  al     = varargin{2};
  au     = varargin{3};
  nonlc  = varargin{4};
  cl     = varargin{5};
  cu     = varargin{6};
  nlCon  = size(A,1);

  xstate = []; xmul   = [];
  astate = []; amul   = [];
  cstate = []; cmul   = [];

  % Check nonlcon
  [nonlcon,c,J,nnCon] = checkCon(nonlc,x0,lvlDer,n);

  if setDer,
    dnset(['Derivative level ' num2str(lvlDer)]);
  end

  [x,fval,exitFlag,itn,nEval,y,istate] = dnoptmex(solveOpt, istart, stopFun, probName, ...
						  objFun, x0, xl, xu, xstate, xmul,...
						  A, al, au, astate, amul, ...
						  nonlcon, cl, cu, cstate, ...
						  cmul, J);

elseif nargin == 12 || nargin == 13,
  % dnopt(obj, x, xl, xu, A, al, au, nonlcon, cl, cu, states, lambda)
  % dnopt(obj, x, xl, xu, A, al, au, nonlcon, cl, cu, states, lambda, options)

  A      = varargin{1};
  al     = varargin{2};
  au     = varargin{3};
  nonlc  = varargin{4};
  cl     = varargin{5};
  cu     = varargin{6};
  states = varargin{7};
  lambda = varargin{8};
  nlCon  = size(A,1);

  xstate = []; xmul   = [];
  astate = []; amul   = [];
  cstate = []; cmul   = [];

  if isfield(states,'x'),
    xstate = states.x;
  end

  if isfield(states,'linear'),
    astate = states.linear;
  end

  if isfield(states,'nonlin'),
    cstate = states.nonlin;
  end

  if isfield(lambda,'x'),
    xmul = lambda.x;
  end

  if isfield(lambda,'linear'),
    amul = lambda.linear;
  end

  if isfield(lambda,'nonlin'),
    cmul = lambda.nonlin;
  end

  % Check nonlcon
  [nonlcon,c,J,nnCon] = checkCon(nonlc,x0,lvlDer,n);

  if setDer,
    dnset(['Derivative level ' num2str(lvlDer)]);
  end

  [x,fval,exitFlag,itn,nEval,y,istate] = dnoptmex(solveOpt, istart, stopFun, probName, ...
						  objFun, x0, xl, xu, xstate, xmul,...
						  A, al, au, astate, amul, ...
						  nonlcon, cl, cu, cstate, ...
						  cmul, J);

else
  error('DNOPT:InputArgs','Wrong number of input arguments for DNOPT');
end


% Set output
zero              = zeros(n);
states.x          = istate(1:n);
lambda.x          = y(1:n);

if nnCon > 0,
  states.nonlin   = istate(n+1:n+nnCon);
  lambda.nonlin   = y(n+1:n+nnCon);
else
  states.nonlin   = [];
  lambda.nonlin   = [];
end

if nlCon > 0,
  states.linear   = istate(n+nnCon+1:n+nnCon+nlCon);
  lambda.linear   = y(n+nnCon+1:n+nnCon+nlCon);
else
  states.linear   = [];
  lambda.linear   = [];
end

output.iterations = itn;
output.funcCount  = nEval;



function [nonlcon,c,J,nnCon] = checkCon(nonlc,x0,lvlDer,n)
% Check nonlcon has the right number of input arguments
% and the output has the right dimensions
%
if ischar(nonlc),
  nonlcon = str2func(nonlc);
elseif isa(nonlc,'function_handle'),
  nonlcon = nonlc;
else
  error('DNOPT:InputArgs','nonlcon should be a function handle or string');
end

narg = nargout(nonlcon);
if narg == 2,
  [c,J]  = nonlcon(x0);
  nnCon  = size(c,1);
  J      = J';

  if nnCon ~= size(J,1),
    error('DNOPT:InputArgs','Size of J in nonlcon is incorrect');
  end

  lvlDer = lvlDer + 2;
elseif narg == 1,
  [c]     = nonlcon(x0);
  nnCon  = size(c,1);
  J       = zeros(nnCon,n);
else
  error('DNOPT:InputArgs','Wrong number of output arguments for nonlcon function');
end

if nnCon == 0,
  error('DNOPT:InputArgs','No nonlinear constraints detected');
end
