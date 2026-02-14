@echo off

if "%CMDEXTVERSION%" == "" goto ERRCmdExt

if not "%1" == "/?" goto BEGIN
echo Compile and link the source files for the 32-bit CodeBase DLL, 'C4DLL.DLL'.
echo.
echo c4dll [/C] [/D] [I dir] [/N] [/NOLINK] [/O dir] [/S dir]
echo       [/COMPILE opt [/COMPILE opt  [...]]] [/LINK opt [/LINK opt  [...]]]
echo.
echo   /C       If any source file could not be compiled, continue compiling the
echo            remaining source files. By default, this command stops compiling
echo            when any source file cannot be compiled.
echo   /D       Generate debugging information.
echo   /N       Specifies name of the output DLL. If not provided, c4dll.dll
echo            is used.
echo   /I       Specifies where all intermediate files (e.g. *.obj) will be
echo            placed. If not provided, intermediate files are placed in the
echo            same directory as the source directory.
echo   /NOLINK  Compile only. Do not link.
echo   /O       Specifies where all output files will be placed. If not
echo            provided, output files are placed in the same directory as the
echo            source directory.
echo   /S       Specifies the directory where the CodeBase source files are
echo            located. If not provided, source files are assumed to be in
echo            ..\..\source\.
echo   /COMPILE Specifies an option to be passed directly to the compiler.
echo   /LINK    Specifies an option to be passed directly to the linker.
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

if not defined s4echo set s4echo=off
echo %s4echo%

REM set defaults
set CONTINUE=
set DEBUG=
set INTERMEDIATE=
set DLLNAME=c4dll.dll
set OUTPUT=
set SOURCE=..\..\source
set DOLINK=T
set COMPOPTS=
set LINKOPTS=

:ReadParameters
if /I "%1" == "/C" (
   set CONTINUE=%1
   shift
   goto ReadParameters
)
if /I "%1" == "/D" (
   set DEBUG=%1
   shift
   goto ReadParameters
)
if /I "%1" == "/I" (
   set INTERMEDIATE=%~f2
   shift
   shift
   goto ReadParameters
)
if /I "%~1" == "/N" (
   set DLLNAME=%~2
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
if /I "%1" == "/NOLINK" (
   set DOLINK=
   shift
   goto ReadParameters
)
if /I "%1" == "/COMPILE" (
   set COMPOPTS=%COMPOPTS% %1 %~2
   shift
   shift
   goto ReadParameters
)
if /I "%1" == "/LINK" (
   set LINKOPTS=%LINKOPTS% %1 %~2
   shift
   shift
   goto ReadParameters
)
if not "%1" == "" (
   echo Unrecognized parameter: %1
   exit /b 1
)

if not defined INTERMEDIATE set INTERMEDIATE=%SOURCE%
if not defined OUTPUT set OUTPUT=%SOURCE%

if exist "%OUTPUT%\out" del "%OUTPUT%\out"

REM *** compile
call c4dll_compile %CONTINUE% %DEBUG% /O "%INTERMEDIATE%" /S "%SOURCE%" %COMPOPTS% @c4source.lst
echo %s4echo%

set SAVE_EL=%ERRORLEVEL%
move /y "%INTERMEDIATE%\out" "%OUTPUT%\." > nul

if %SAVE_EL% NEQ 0 exit /b %SAVE_EL%

REM *** link
if not defined DOLINK exit /b 0

move /y "%OUTPUT%\out" "%OUTPUT%\c4dll_compile.out"
call c4dll_link %DEBUG% /I "%INTERMEDIATE%" /N "%DLLNAME%" /O "%OUTPUT%" %LINKOPTS% @c4source.lst
echo %s4echo%

set SAVE_EL=%ERRORLEVEL%

move /y "%OUTPUT%\out" "%OUTPUT%\c4dll_link.out"
copy "%OUTPUT%\c4dll_compile.out"+"%OUTPUT%\c4dll_link.out" "%OUTPUT%\out" > nul
del "%OUTPUT%\c4dll_compile.out"
del "%OUTPUT%\c4dll_link.out"

if %SAVE_EL% NEQ 0 exit /b %SAVE_EL%

exit /b 0


:ERRCmdExt
   echo Unable to run. Command extensions are not enabled.
