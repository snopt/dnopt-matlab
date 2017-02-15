function dqsetreal ( option, rvalue )
% function dqsetreal ( option, rvalue )
%   Set REAL-valued option to rvalue.
%

setOpt = 4;
dqoptmex ( setOpt, option, rvalue );
