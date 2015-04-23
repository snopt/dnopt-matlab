function dqSetInt ( option, ivalue )
% function dqSetInt ( option, ivalue )
%   Set INTEGER-valued option to ivalue.
%

setOpt = 3;
dqoptmex ( setOpt, option, ivalue );
