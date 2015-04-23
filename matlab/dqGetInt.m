function geti = dqGetInt ( option )
% function geti = dqGetInt ( option )
%     The  optional INTEGER-valued parameter defined by the
%     string "option" is assigned to geti.
%

getOpt = 7;
geti   = dqoptmex( getOpt, option );
