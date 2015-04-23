function getc = dqGetChar( option )
% function getc = dqGetChar ( option )
%   The optional CHARACTER-valued parameter defined by the
%   string "option" is assigned to getc.
%

getOpt = 6;
getc   = dqoptmex( getOpt, option );
