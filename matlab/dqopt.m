function [x,fval,exitFlag,output,lambda,states] = dqopt(H, c, varargin)
% function [x,fval,exitFlag,output,lambda,states] = dqopt(H, c, varargin)
%
%   Solve the given quadratic problem:
%       minimize    c'x + half*x'Hx
%     subject to   xl  <=  x   <= xu
%                  al  <= A*x  <= au
%
%  Calling sequences:
%  [] = dqopt(H, c)
%  [] = dqopt(H, c, options)
%
%  [] = dqopt(H, c, x0, xl, xu)
%  [] = dqopt(H, c, x0, xl, xu, options)
%
%  [] = dqopt(H, c, x0, xl, xu, states, lambda)
%  [] = dqopt(H, c, x0, xl, xu, states, lambda, options)
%
%  [] = dqopt(H, c, x0, xl, xu, A, al, au)
%  [] = dqopt(H, c, x0, xl, xu, A, al, au, options)
%
%  [] = dqopt(H, c, x0, xl, xu, A, al, au, states, lambda)
%  [] = dqopt(H, c, x0, xl, xu, A, al, au, states, lambda, options)
%
%  [x,fval,exitFlag,output,lambda,states] = dqopt(H, c, ...)
%
%   INPUT:
%     H        is the Hessian matrix of the objective
%
%     c        is the linear term of the objective
%
%     x0       is the initial point x
%
%     xl, xu   are the lower and upper bounds of x
%
%     A        is the linear constraint matrix
%
%     al, au   are the lower and upper bounds of the linear constraints
%
%     states   is a struct of the initial states of the variables and
%              linear constraints
%
%     lambda   is a struct of the initial multipliers of the variables
%              and linear constraints
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
%              lambda.x          are for the variables
%              lambda.linear     are for the linear constraints
%
%     states   is a structure
%              states.x
%              states.linear
%
%

solveOpt = 1;

probName = '';
istart   = 0;

% Deal with options.
optionsLoc = 0;
if nargin == 3 || nargin == 6 || nargin == 8 || nargin == 9 || nargin == 11,
  optionsLoc = nargin - 2;
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

  else
    optionsLoc = 0;
  end
end

if     nargin == 3 || nargin == 4,
  % dqopt(H, c)
  % dqopt(H, c, options)
   A = []; al = []; au = []; astate = []; amul = [];
  x0 = []; xl = []; xu = []; xstate = []; xmul = [];

elseif nargin == 5 || nargin == 6,
  % dqopt(H, c, x0, xl, xu)
  % dqopt(H, c, x0, xl, xu, options)

  x0     = varargin{1};
  xl     = varargin{2};
  xu     = varargin{3};
  A      = [];
  al     = [];
  au     = [];

  xstate = []; xmul   = [];
  astate = []; amul   = [];

elseif nargin == 7 || (nargin == 8 && optionsLoc ~= 0),
  % dqopt(H, c, x0, xl, xu, states, lambda)
  % dqopt(H, c, x0, xl, xu, states, lambda, options)

  x0     = varargin{1};
  xl     = varargin{2};
  xu     = varargin{3};
  states = varargin{4};
  lambda = varargin{5};
  A      = [];
  al     = [];
  au     = [];

  xstate = []; xmul   = [];
  astate = []; amul   = [];

  if isfield(states,'x'),
    xstate = states.x;
  end

  if isfield(lambda,'x'),
    xmul = lambda.x;
  end

elseif nargin == 8 || nargin == 9,
  % dqopt(H, c, x0, xl, xu, A, al, au)
  % dqopt(H, c, x0, xl, xu, A, al, au, options)

  x0     = varargin{1};
  xl     = varargin{2};
  xu     = varargin{3};
  A      = varargin{4};
  al     = varargin{5};
  au     = varargin{6};

  xstate = []; xmul   = [];
  astate = []; amul   = [];

elseif nargin == 10 || nargin == 11,
  % dqopt(H, c, x0, xl, xu, A, al, au, states, lambda)
  % dqopt(H, c, x0, xl, xu, A, al, au, states, lambda, options)
  x0     = varargin{1};
  xl     = varargin{2};
  xu     = varargin{3};
  A      = varargin{4};
  al     = varargin{5};
  au     = varargin{6};
  states = varargin{7};
  lambda = varargin{8};

  xstate = []; xmul   = [];
  astate = []; amul   = [];

  if isfield(states,'x'),
    xstate = states.x;
  end

  if isfield(lambda,'x'),
    xmul = lambda.x;
  end

  if isfield(states,'linear'),
    astate = states.linear;
  end

  if isfield(lambda,'linear'),
    amul = lambda.linear;
  end

else
  error('DQOPT:InputArgs','Wrong number of input arguments for dqopt');
end

[x,fval,exitFlag,itn,y,state] = dqoptmex(solveOpt, istart, probName, ...
					 H, c, ...
					 x0, xl, xu, xstate, xmul, ...
					 A, al, au, astate, amul);

% Set output
output.iterations = itn;

m    = size(A,1);
n    = size(x,1);
zero = zeros(n);

states.x = state(1:n);
lambda.x = y(1:n);

if m > 0,
  states.linear = state(n+1:n+m);
  lambda.linear = y(n+1:n+m);
end
