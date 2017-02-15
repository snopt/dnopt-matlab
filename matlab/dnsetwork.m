function dnsetwork( leniw, lenrw )
%     Modify the initial amount of workspace for DNOPT.
%     Values must be at least 500.
%

setoptionI = 14;
dnoptmex( setoptionI, leniw, lenrw );
