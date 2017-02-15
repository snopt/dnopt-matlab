%Test Script.

format compact;

addpath([pwd,'/examples'       ], '-end');
addpath([pwd,'/examples/hs76'  ], '-end');
addpath([pwd,'/examples/lpmain'], '-end');

dqscreen on;

fprintf('\n=============================================================== ');
fprintf('\n hs76: Solving quadratic hs76 using dqsolve (quadprog-style)...\n');
hs76_mincon;

fprintf('\n======================================================================== ');
fprintf('\n lpmain: Solving linear program lpmain using dqsolve (quadprog-style)...\n');
lpmain_mincon;

fprintf('\n======================================================================== ');
fprintf('\n lpmain: Solving linear program lpmain using dlopt (dqopt-style)...\n');
lpmain;

dqscreen off;
