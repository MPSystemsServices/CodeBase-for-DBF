@echo off
set clist=
set llist=
if not "%1" == "" goto compile

echo Microsoft Visual C++ compile and link command file
echo.
echo   Usage:    c4ap   {source_file}  [compiler options]
echo.
goto end

:compile
set clist=%clist% %1
shift
if exist %1.c goto compile
if exist c4class.lib set llist=c4class.lib

@echo on
cl -DWIN32 -O2 -I..\..\source -I. -MT -W1 -Zp1 %1 %2 %3 %4 %clist% /link C4LIB.LIB %llist% USER32.LIB WSOCK32.LIB GDI32.LIB COMDLG32.LIB
@echo off

:end
set clist=
set llist=
