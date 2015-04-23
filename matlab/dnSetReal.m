function dnSetReal ( option, rvalue )
% function dnSetReal ( option, rvalue )
%   Set REAL-valued option to rvalue.
%

setOpt = 4;
dnoptmex ( setOpt, option, rvalue );
