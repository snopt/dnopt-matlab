function dnprint ( filename )
% function dnPrint ( filename )
%   Set print file name to 'filename'.
%   If filename is 'off', then close the file.
%   If filename is 'on', then open the file 'print.out'.
%

openPrint  = 10;
closePrint = 12;

if strcmp( filename, 'off')
  dnoptmex ( closePrint );
elseif strcmp( filename, 'on' )
  dnoptmex ( openPrint, 'print.out' );
else
  dnoptmex ( openPrint, filename );
end
