function dqsetint ( option, ivalue )
% function dqsetint ( option, ivalue )
%   Set INTEGER-valued option to ivalue.
%

setOpt = 3;
dqoptmex ( setOpt, option, ivalue );
