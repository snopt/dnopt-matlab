function getr = dqgetreal ( option )
% function getr = dqgetreal( option )
%     The  optional REAL-valued parameter defined by the
%     string "option" is assigned to getr.
%

getOpt = 8;
getr   = dqoptmex( getOpt, option );
