function dqScreen ( filename )
% function dqScreen ( filename )
%   Regulates output to the terminal.  Will print major iteration
%   information identical to that printed to a summary file if set.
%   Thus options that effect summary output may affect information
%   printed to the screen.  Option is off by default.
%
%   To turn on the screen output, type:
%     >> dqScreen on
%   To turn the screen output back off, type:
%     >> dqScreen off
%

screenon  = 15;
screenoff = 16;

if strcmp( filename, 'on' )
  dqoptmex( screenon );
elseif strcmp( filename, 'off' )
  dqoptmex( screenoff );
end
