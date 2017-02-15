function dqsummary ( filename )
% function dqsummary ( filename )
%   Set summary output to 'filename'.
%   If filename is 'off', then close.
%

openSum  = 11;
closeSum = 13;

if strcmp( filename, 'off')
  dqoptmex ( closeSum );
else
  dqoptmex ( openSum, filename );
end
