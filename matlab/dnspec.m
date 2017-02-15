function inform = dnspec ( filename )
% function inform = dnspec ( filename )
%   Read 'filename' to set options.
%   Return 0 if successful.
%

specOpt = 9;
inform  = dnoptmex ( specOpt, filename );