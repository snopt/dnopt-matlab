%Test Script.

format compact;

addpath([pwd,'/examples'       ], '-end');
addpath([pwd,'/examples/t1diet'], '-end');
addpath([pwd,'/examples/dntoy' ], '-end');
addpath([pwd,'/examples/dnmain'], '-end');
addpath([pwd,'/examples/hsmain'], '-end');
addpath([pwd,'/examples/hs76'  ], '-end');
addpath([pwd,'/examples/lpmain'], '-end');

dnScreen on;

fprintf('\n============================================================= ');
fprintf('\n hsmain: Solving hs13 using dnopt ... ');
hs13;

fprintf('\n============================================================= ');
fprintf('\n hsmain: Solving hs13_mincon using dnSolve (fmincon-style)... ');
hs13_mincon;

fprintf('\n============================================================= ');
fprintf('\n hsmain: Solving hsmain using dnopt ... ');
hsmain;

fprintf('\n=============================================================== ');
fprintf('\n hsmain: Solving hsmain_mincon using dnSolve (fmincon-style)... ');
hsmain_mincon;

fprintf('\n============================================================= ');
fprintf('\n dnmain: Solving dnmain using dnopt ... ');
dnmain;

fprintf('\n============================================================= ');
fprintf('\n dntoy: Solving dntoy using dnopt ... ');
dntoy;

fprintf('\n============================================================= ');
fprintf('\n dntoy: Solving dntoy_mincon using dnSolve (fmincon-style)... ');
dntoy_mincon;

dnScreen off;
