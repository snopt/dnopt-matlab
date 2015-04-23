function get = dqGet( option )
% function get = dqGet ( option )
%     The  optional parameter defined by the
%     string "option" is assigned to get.
%

getOpt = 5;
get    = dqoptmex( getOpt, option );
