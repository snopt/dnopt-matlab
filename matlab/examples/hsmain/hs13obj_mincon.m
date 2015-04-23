function [f,g] = hs13obj_mincon(x)

% Compute objective and its gradient
f = x(1) + x(2);
g = [ 1; 1 ];
