function inform = dqspec ( filename )
% function inform = dqspec ( filename )
%   Read 'filename' to set options.
%   Return 0 if successful.
%

specOpt = 9;
inform  = dqoptmex ( specOpt, filename );