%Test Script.

format compact;

addpath([pwd,'/examples'       ],'-end');
addpath([pwd,'/examples/t1diet'],'-end');
addpath([pwd,'/examples/dntoy' ],'-end');
addpath([pwd,'/examples/dnmain'],'-end');
addpath([pwd,'/examples/hsmain'],'-end');
addpath([pwd,'/examples/hs76'  ],'-end');
addpath([pwd,'/examples/lpmain'],'-end');

dnscreen on;

fprintf('\n=============================================================');
fprintf('\n hsmain: Solving hs13 using dnopt ...\n');
hs13;

fprintf('\n=============================================================');
fprintf('\n hsmain: Solving hs13_mincon using dnsolve (fmincon-style)...\n');
hs13_mincon;

fprintf('\n=============================================================');
fprintf('\n hsmain: Solving hsmain using dnopt ...\n');
hsmain;

fprintf('\n===============================================================');
fprintf('\n hsmain: Solving hsmain_mincon using dnsolve (fmincon-style)...\n');
hsmain_mincon;

fprintf('\n=============================================================');
fprintf('\n dnmain: Solving dnmain using dnopt ...\n');
dnmain;

fprintf('\n=============================================================');
fprintf('\n dntoy: Solving dntoy using dnopt ...\n');
dntoy;

fprintf('\n=============================================================');
fprintf('\n dntoy: Solving dntoy_mincon using dnSolve (fmincon-style)...\n');
dntoy_mincon;

dnscreen off;
