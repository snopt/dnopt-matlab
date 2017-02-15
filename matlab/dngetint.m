function geti = dngetint ( option )
% function geti = dnGetInt ( option )
%     The  optional INTEGER-valued parameter defined by the
%     string "option" is assigned to geti.
%

getOpt = 7;
geti   = dnoptmex( getOpt, option );
