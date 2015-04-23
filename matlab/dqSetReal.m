function dqSetReal ( option, rvalue )
% function dqSetReal ( option, rvalue )
%   Set REAL-valued option to rvalue.
%

setOpt = 4;
dqoptmex ( setOpt, option, rvalue );
