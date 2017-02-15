function get = dqget( option )
% function get = dqget ( option )
%     The  optional parameter defined by the
%     string "option" is assigned to get.
%

getOpt = 5;
get    = dqoptmex( getOpt, option );
