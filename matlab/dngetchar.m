function getc = dngetchar( option )
% function getc = dnGetChar ( option )
%   The optional CHARACTER-valued parameter defined by the
%   string "option" is assigned to getc.
%

getOpt = 6;
getc   = dnoptmex( getOpt, option );
