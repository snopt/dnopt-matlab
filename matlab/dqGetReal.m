function getr = dqGetReal ( option )
% function getr = dqGetReal( option )
%     The  optional REAL-valued parameter defined by the
%     string "option" is assigned to getr.
%

getOpt = 8;
getr   = dqoptmex( getOpt, option );
