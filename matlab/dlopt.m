function [x,fval,exitFlag,output,lambda] = dlopt(name, c, varargin)
% function [x,fval,exitFlag,output,lambda] = dlopt(name, c, varargin)
%
%  Calling sequences:
%  [] = dlopt( name, c )
%  [] = dlopt( name, c, x0, xl, xu )
%  [] = dlopt( name, c, x0, xl, xu, A, al, au )
%
%  [x,fval,exitFlag]               = dlopt( name, c, ... )
%  [x,fval,exitFlag,output]        = dlopt( name, c, ... )
%  [x,fval,exitFlag,output,lambda] = dlopt( name, c, ... )
%
%
%   Solve the given linear problem:
%       minimize    L(x) = c'x
%     subject to   xl  <=  x   <= xu
%                  al  <= A*x  <= au
%
%   INPUT:
%     name     is the 8-character name of the problem
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
%              lambda.linear     are for the linear constraints
%
%

solveOpt = 17;

if ( nargin == 2 )
   A = []; al = []; au = [];
  x0 = []; xl = []; xu = [];

elseif ( nargin == 5 )

   A = []; al = []; au = [];
  x0 = varargin{1};
  xl = varargin{2};
  xu = varargin{3};

elseif ( nargin == 8 )
  x0 = varargin{1};
  xl = varargin{2};
  xu = varargin{3};
  A  = varargin{4};
  al = varargin{5};
  au = varargin{6};

else
  error('Wrong number of input arguments for dlopt');
end

H = [];
[x,fval,exitFlag,itn,y,yL] = dqoptmex( solveOpt, name, H, c, x0, xl, xu, A, al, au);


if     ( nargout >= 4 )
  output.iterations = itn;
end

if ( nargout >= 5 )
  n    = size(x);
  zero = zeros(n);

  lambda.lb         = max(y,zero);
  lambda.ub         = min(y,zero);
  lambda.linear     = yL;
end
