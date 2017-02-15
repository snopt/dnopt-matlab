function [x,fval,exitFlag,output,lambda,states] = dlopt(c, varargin)
% function [x,fval,exitFlag,output,lambda,states] = dlopt(c, varargin)
%
%  Calling sequences:
%  [] = dlopt(c)
%  [] = dlopt(c, options)
%  [] = dlopt(c, x0, xl, xu)
%  [] = dlopt(c, x0, xl, xu, options)
%
%  [] = dlopt(c, x0, xl, xu, states, lambda)
%  [] = dlopt(c, x0, xl, xu, states, lambda, options)
%
%  [] = dlopt(c, x0, xl, xu, A, al, au)
%  [] = dlopt(c, x0, xl, xu, A, al, au, options)
%
%  [] = dlopt(c, x0, xl, xu, A, al, au, states, lambda)
%  [] = dlopt(c, x0, xl, xu, A, al, au, states, lambda, options)
%
%  [x,fval,exitFlag]               = dlopt(c, ...)
%  [x,fval,exitFlag,output]        = dlopt(c, ...)
%  [x,fval,exitFlag,output,lambda] = dlopt(c, ...)
%  [x,fval,exitFlag,output,lambda,states] = dlopt(c, ...)
%
%
%   Solve the given linear problem:
%       minimize    L(x) = c'x
%     subject to   xl  <=  x   <= xu
%                  al  <= A*x  <= au
%
%   INPUT:
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
  optionsLoc = nargin - 3;
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
    optionsLoc = 0;
  end
end


if nargin == 1 || nargin == 2,
  % dlopt(c)
  % dlopt(c, options)

  A  = []; al = []; au = []; astate = []; amul = [];
  x0 = []; xl = []; xu = []; xstate = []; xmul = [];

elseif nargin == 4 || nargin == 5,
  % dlopt(c, x0, xl, xu)
  % dlopt(c, x0, xl, xu, options)

  x0     = varargin{1};
  xl     = varargin{2};
  xu     = varargin{3};
  A  = []; al = []; au = [];

  astate = []; amul = [];
  xstate = []; xmul = [];

elseif nargin == 6 || (nargin == 7 && optionsLoc ~= 0),
  % dlopt(c, x0, xl, xu, states, lambda)
  % dlopt(c, x0, xl, xu, states, lambda, options)

  x0     = varargin{1};
  xl     = varargin{2};
  xu     = varargin{3};
  states = varargin{4};
  lambda = varargin{5};
  A  = []; al = []; au = [];

  astate = []; amul = [];
  xstate = []; xmul = [];

  if isfield(states,'x'),
    xstate = states.x;
  end

  if isfield(lambda,'x'),
    xmul = lambda.x;
  end

elseif nargin == 7 || nargin == 8,
  % dlopt(c, x0, xl, xu, A, al, au)
  % dlopt(c, x0, xl, xu, A, al, au, options)

  x0 = varargin{1};
  xl = varargin{2};
  xu = varargin{3};
  A  = varargin{4};
  al = varargin{5};
  au = varargin{6};

  astate = []; amul = [];
  xstate = []; xmul = [];

elseif nargin == 9 || nargin == 10,
  % dlopt(c, x0, xl, xu, A, al, au, states, lambda)
  % dlopt(c, x0, xl, xu, A, al, au, states, lambda, options)

  x0 = varargin{1};
  xl = varargin{2};
  xu = varargin{3};
  A  = varargin{4};
  al = varargin{5};
  au = varargin{6};

  astate = []; amul = [];
  xstate = []; xmul = [];

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
  error('DLOPT:InputArgs','Wrong number of input arguments for dlopt');
end

[x,fval,exitFlag,itn,y,state] = dqoptmex(solveOpt, istart, probName, ...
					 [], c, ...
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
