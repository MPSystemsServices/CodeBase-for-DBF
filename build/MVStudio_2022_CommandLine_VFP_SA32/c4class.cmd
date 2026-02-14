@echo off

if "%CMDEXTVERSION%" == "" goto ERRCmdExt

if not "%1" == "/?" goto BEGIN
echo Build c4class.lib.
echo.
echo c4class [/D] [/O dir] [/S dir]
echo.
echo   /D       Generate debugging information.
echo   /I       Specifies where all intermediate files (e.g. *.obj) will be
echo            placed. If not provided, intermediate files are placed in the
echo            same directory as the source directory.
echo   /O       Specifies where all output files will be placed. If not
echo            provided, output files are placed in the same directory as the
echo            source directory.
echo   /S       Specifies the directory where the CodeBase source files are
echo            located. If not provided, source files are assumed to be in
echo            the current directory.
echo   /compile An option passed directly to the compiler.
echo.
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
set CDEBUG=-O2
set LDEBUG=
set COMPOPTS=
set OUTPUT=
set INTEREDIATE=
set SOURCE=%cd%

:ReadParameters
if /I "%1" == "/D" (
   set CDEBUG=-Zi -Od
   set LDEBUG=/DEBUG
   shift
   goto ReadParameters
)
if /I "%1" == "/I" (
   set INTERMEDIATE=%~f2
   shift
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

if not defined INTERMEDIATE set INTERMEDIATE=%SOURCE%
if not defined OUTPUT set OUTPUT=%SOURCE%

cd /d "%INTERMEDIATE%"

echo on
cl -DS4CBPP -DS4LIB_BUILD -DWIN32 -c %CDEBUG% %COMPOPTS% -Zp1 -YX "%SOURCE%\f4info_p.cpp" "%SOURCE%\f4seq.cpp" "%SOURCE%\t4info.cpp" "%SOURCE%\d4data_p.cpp" "%SOURCE%\s4str.cpp" "%SOURCE%\s4string.cpp"  > "%OUTPUT%\OUT"
@if exist c4class.lib del c4class.lib
@cd /d "%OUTPUT%"
lib %LDEBUG% /OUT:c4class.lib "%INTERMEDIATE%\f4info_p.obj" "%INTERMEDIATE%\f4seq.obj" "%INTERMEDIATE%\d4data_p.obj" "%INTERMEDIATE%\t4info.obj" "%INTERMEDIATE%\s4str.obj" "%INTERMEDIATE%\s4string.obj" >> "%OUTPUT%\OUT"
