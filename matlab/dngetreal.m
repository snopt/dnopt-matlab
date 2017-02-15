function getr = dngetreal ( option )
% function getr = dnGetReal( option )
%     The  optional REAL-valued parameter defined by the
%     string "option" is assigned to getr.
%

getOpt = 8;
getr   = dnoptmex( getOpt, option );
