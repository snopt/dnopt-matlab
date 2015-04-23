function dnScreen ( filename )
% function dnscreen ( filename )
%   Regulates output to the terminal.  Will print major iteration
%   information identical to that printed to a summary file if set.
%   Thus options that effect summary output may affect information
%   printed to the screen.  Option is off by default.
%
%   To turn on the screen output, type:
%     >> dnscreen on
%   To turn the screen output back off, type:
%     >> dnscreen off
%

screenon  = 15;
screenoff = 16;

if strcmp( filename, 'on' )
  dnoptmex( screenon );
elseif strcmp( filename, 'off' )
  dnoptmex( screenoff );
end
