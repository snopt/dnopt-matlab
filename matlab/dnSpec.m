function inform = dnSpec ( filename )
% function inform = dnSpec ( filename )
%   Read 'filename' to set options.
%   Return 0 if successful.
%

specOpt = 9;
inform  = dnoptmex ( specOpt, filename );