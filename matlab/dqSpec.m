function inform = dqSpec ( filename )
% function inform = dqSpec ( filename )
%   Read 'filename' to set options.
%   Return 0 if successful.
%

specOpt = 9;
inform  = dqoptmex ( specOpt, filename );