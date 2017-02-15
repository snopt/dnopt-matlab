function get = dnget( option )
% function get = dnGet ( option )
%     The  optional parameter defined by the
%     string "option" is assigned to get.
%

getOpt = 5;
get    = dnoptmex( getOpt, option );
