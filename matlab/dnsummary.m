function dnsummary ( filename )
% function dnsummary ( filename )
%   Set summary output to 'filename'.
%   If filename is 'off', then close.
%

openSum  = 11;
closeSum = 13;

if strcmp( filename, 'off')
  dnoptmex ( closeSum );
else
  dnoptmex ( openSum, filename );
end
