%Test Script.

format compact;

addpath([pwd,'/examples'       ], '-end');
addpath([pwd,'/examples/t1diet'], '-end');
addpath([pwd,'/examples/hs76'  ], '-end');
addpath([pwd,'/examples/lpmain'], '-end');

dqscreen on;

fprintf('\n=============================================================');
fprintf('\n hsmain: Solving t1diet using dqopt ...\n');
t1diet_dqopt;

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
