@echo off
set clist=
set llist=
if not "%1" == "" goto start

echo Microsoft Visual C++ compile and link command file for Windows applications
echo.
echo   Usage:    c4ap_win   {source_file}  [{options}]
echo.
goto end

:start
set clist=%clist% %1
shift
if exist %1 goto start
if exist c4class.lib set llist=c4class.lib

@echo on
cl %1 %2 %3 %4 -DS4DLL -DWIN32 -I..\..\source -I. -O2 -MT -W1 -Zp1 %clist% /link C4DLL.LIB %llist% USER32.LIB GDI32.LIB COMDLG32.LIB
@echo off

:end
set clist=
set llist=
