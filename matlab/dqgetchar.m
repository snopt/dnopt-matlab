function getc = dqgetchar( option )
% function getc = dqgetchar ( option )
%   The optional CHARACTER-valued parameter defined by the
%   string "option" is assigned to getc.
%

getOpt = 6;
getc   = dqoptmex( getOpt, option );
