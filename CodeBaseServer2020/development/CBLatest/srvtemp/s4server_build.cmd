@echo off

if "%CMDEXTVERSION%" == "" goto ERRCmdExt

if not "%1" == "/?" goto BEGIN
echo Build the CodeBase server executables.
echo.
echo s4server [/D] [/O dir] [/S dir]
echo.
echo   /O        Specifies where all output files will be placed. If not
echo             provided, output files are placed in the same directory as the
echo             source directory.
echo   /S        Specifies the directory where the CodeBase source files are
echo             located. If not provided, source files are assumed to be in
echo             the current directory.
echo   /COMPILE  Specifies an option to be passed directly to the compiler.
echo.
echo This command assumes that the import library for the server DLL, S4DLL.LIB, is
echo located in the output directory.
echo Errors and warnings from the build is stored in the file 'OUT' located in the
echo output directory.
echo.
echo Exit codes:
echo   0 Success
echo   1 Unable to build. Check the file called 'out' for errors.
goto :EOF

:BEGIN
setlocal
echo Preparing to build...

REM set defaults
set COMPOPTS=
set COMP_DEB_OPT=-O2
set DEBUG=
set OUTPUT=
set SOURCE=%cd%

:ReadParameters
if /I "%1" == "/D" (
   set COMP_DEB_OPT=-Od -Zi
   set DEBUG=/DEBUG
   shift
   goto ReadParameters
)
if /I "%1" == "/O" (
   set OUTPUT=%~f2
   shift
   shift
   goto ReadParameters
)
if /I "%1" == "/S" (
   set SOURCE=%~f2
   shift
   shift
   goto ReadParameters
)
if /I "%1" == "/COMPILE" (
   set COMPOPTS=%COMPOPTS% %~2
   shift
   shift
   goto ReadParameters
)
if not "%1" == "" (
   echo Unrecognized parameter: %1
   exit /b 1
)
REM end of parameter checking

if not defined OUTPUT set OUTPUT=%SOURCE%

cd /d %SOURCE%

@echo on

set STDLIBS=USER32.LIB GDI32.LIB COMDLG32.LIB ADVAPI32.LIB

@rem build regular server executable
cl -DS4FOX -DS4SERVER -DS4DLL -DWIN32 %1 %2 %3 %4 %5 %6 %7 %8 %9 -nologo -c -MT %COMPOPTS% %COMP_DEB_OPT% -W3 -Zp1 -Tp d4srvApp.c > %OUTPUT%\out
@if exist s4server.exe erase s4server.exe
rc s4server >> %OUTPUT%\out
link %DEBUG% /OUT:%OUTPUT%\s4server.exe d4srvApp S4SERVER.RES %OUTPUT%\S4DLL.LIB %STDLIBS% >> %OUTPUT%\out

@rem build service
cl -DS4FOX -DS4SERVER -DS4DLL -DWIN32 %1 %2 %3 %4 %5 %6 %7 %8 %9 -nologo -MT %COMPOPTS% %COMP_DEB_OPT% -W3 -Zp1 -Tp d4service.c /link %DEBUG% /out:%OUTPUT%\s4service.exe %OUTPUT%\S4DLL.LIB %STDLIBS% S4SERVER.RES >> %OUTPUT%\out

@echo off

del s4server.res 2> nul
del d4srvApp.obj 2> nul
del d4service.obj 2> nul

exit /b 0
