function dqPrint ( filename )
% function dqPrint ( filename )
%   Set print file name to 'filename'.
%   If filename is 'off', then close the file.
%   If filename is 'on', then open the file 'print.out'.
%

openPrint  = 10;
closePrint = 12;

if strcmp( filename, 'off')
  dqoptmex ( closePrint );
elseif strcmp( filename, 'on' )
  dqoptmex ( openPrint, 'print.out' );
else
  dqoptmex ( openPrint, filename );
end
