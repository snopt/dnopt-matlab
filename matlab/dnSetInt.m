function dnSetInt ( option, ivalue )
% function dnSetInt ( option, ivalue )
%   Set INTEGER-valued option to ivalue.
%

setOpt = 3;
dnoptmex ( setOpt, option, ivalue );
