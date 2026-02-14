@echo off

if "%CMDEXTVERSION%" == "" goto ERRCmdExt

if not "%1" == "/?" goto BEGIN
echo Compile and link the source files for the 32-bit CodeBase static library.
echo.
echo c4lib [/C] [/D] [/I dir] [/N] [/NOLINK] [/O dir] [/S dir]
echo       [/COMPILE opt [/COMPILE opt  [...]]]
echo.
echo   /C       If any source file could not be compiled, continue compiling the
echo            remaining source files. By default, this command stops compiling
echo            when any source file cannot be compiled.
echo   /D       Generate debugging information.
echo   /N       Specifies name of the output library file. If not provided,
echo            c4lib.lib is used.
echo   /I       Specifies where all intermediate files (e.g. *.obj) will be
echo            placed. If not provided, intermediate files are placed in the
echo            same directory as the source directory.
echo   /NOLINK  Compile only. Do not create the library.
echo   /O       Specifies where all output files will be placed. If not
echo            provided, output files are placed in the same directory as the
echo            source directory.
echo   /S       Specifies the directory where the CodeBase source files are
echo            located. If not provided, source files are assumed to be in
echo            ..\..\source\.
echo   /COMPILE Specifies an option to be passed directly to the compiler.
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
set LIBNAME=c4lib.lib
set OUTPUT=
set SOURCE=..\..\source
set DOLINK=T
set COMPOPTS=

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
   set LIBNAME=%~2
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
if not "%1" == "" (
   echo Unrecognized parameter: %1
   exit /b 1
)

if not defined INTERMEDIATE set INTERMEDIATE=%SOURCE%
if not defined OUTPUT set OUTPUT=%SOURCE%

if exist "%OUTPUT%\out" del "%OUTPUT%\out"

set listfile=c4source.lst
if not exist %listfile% (
   set listfile=%SOURCE%\%listfile%
)
if not exist %listfile% (
   echo %listfile% does not exist.
   exit /b 1
)

REM *** compile
call c4lib_compile %CONTINUE% %DEBUG% /O "%INTERMEDIATE%" /S "%SOURCE%" %COMPOPTS% @%listfile%
echo %s4echo%

set SAVE_EL=%ERRORLEVEL%
move /y "%INTERMEDIATE%\out" "%OUTPUT%\." > nul

if %SAVE_EL% NEQ 0 exit /b %SAVE_EL%

REM *** link
if not defined DOLINK exit /b 0

move /y "%OUTPUT%\out" "%OUTPUT%\c4lib_compile.out"
call c4lib_link %DEBUG% /I "%INTERMEDIATE%" /N "%LIBNAME%" /O "%OUTPUT%" @%listfile%
echo %s4echo%

set SAVE_EL=%ERRORLEVEL%

move /y "%OUTPUT%\out" "%OUTPUT%\c4lib_link.out"
copy "%OUTPUT%\c4lib_compile.out"+"%OUTPUT%\c4lib_link.out" "%OUTPUT%\out" > nul
del "%OUTPUT%\c4lib_compile.out"
del "%OUTPUT%\c4lib_link.out"

if %SAVE_EL% NEQ 0 exit /b %SAVE_EL%

exit /b 0


:ERRCmdExt
   echo Unable to run. Command extensions are not enabled.
