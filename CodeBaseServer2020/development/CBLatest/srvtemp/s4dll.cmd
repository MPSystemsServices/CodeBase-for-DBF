@echo off

if "%CMDEXTVERSION%" == "" goto ERRCmdExt

if not "%1" == "/?" goto BEGIN
echo Compile and link the CodeBase server DLL, 'S4DLL.DLL'.
echo.
echo s4dll [/C] [/D] [I dir] [/NOLINK] [/O dir] [/S dir]
echo       [/COMPILE opt [/COMPILE opt  [...]]] [/LINK opt [/LINK opt  [...]]]
echo.
if /I "%2" == "full" (
echo   /C       If any source file could not be compiled, continue compiling the
echo            remaining source files. By default, this command stops compiling
echo            when any source file cannot be compiled.
echo   /D       Generate debugging information.
echo   /I       Specifies where all intermediate files ^(e.g. *.obj^) will be
echo            placed. If not provided, intermediate files are placed in the
echo            same directory as the source directory.
echo   /NOLINK  Compile only. Do not link.
echo   /O       Specifies where all output files will be placed. If not
echo            provided, output files are placed in the same directory as the
echo            source directory.
echo   /S       Specifies the directory where the CodeBase source files are
echo            located. If not provided, source files are assumed to be in
echo            the current directory.
echo   /COMPILE Specifies an option to be passed directly to the compiler.
echo   /LINK    Specifies an option to be passed directly to the linker.
) else (
echo   /C       Continue on error.
echo   /D       Generate debugging information.
echo   /I       Specifies the intermediate directory. Default is source.
echo   /NOLINK  Compile only. Do not link.
echo   /O       Specifies the output directory. Default is source.
echo   /S       Specifies the source directory. Default is current directory.
echo   /COMPILE Specifies an option to be passed directly to the compiler.
echo   /LINK    Specifies an option to be passed directly to the linker.
echo.
echo For complete documentation, enter s4dll /? full
)
echo.
echo Errors and warnings from the build is stored in the file 'OUT' located in the
echo output directory.
echo.
echo Exit codes:
echo   0 Success
echo   1 Unable to build. Check the file called 'out' for errors.
exit /b 0

:BEGIN
setlocal
echo Preparing to build...

REM set defaults
set CONTINUE=
set DEBUG=
set INTERMEDIATE=
set OUTPUT=
set SOURCE=%cd%
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
REM AS Dec 31/02 - input has changed to be " /COMPILE" not "/COMPILE" unknown why...
if /I "%1" == " /COMPILE" (
   set COMPOPTS=%COMPOPTS% %1 %~2
   shift
   shift
   goto ReadParameters
)
REM AS Dec 31/02 - sometimes input doesn't have the space, unknown why...
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
REM end of parameter checking

if not defined INTERMEDIATE set INTERMEDIATE=%SOURCE%
if not defined OUTPUT set OUTPUT=%SOURCE%

if exist %OUTPUT%\out del %OUTPUT%\out

REM *** compile
call s4dll_compile %CONTINUE% %DEBUG% /O "%INTERMEDIATE%" /S "%SOURCE%" %COMPOPTS%

set SAVE_EL=%ERRORLEVEL%
move /y "%INTERMEDIATE%\out" "%OUTPUT%\." > nul

if %SAVE_EL% NEQ 0 exit /b %SAVE_EL%

REM *** link
if not defined DOLINK exit /b 0

move /y "%OUTPUT%\out" "%OUTPUT%\s4dll_compile.out"
call s4dll_link %DEBUG% /I "%INTERMEDIATE%" /O "%OUTPUT%" %LINKOPTS%

set SAVE_EL=%ERRORLEVEL%

move /y "%OUTPUT%\out" "%OUTPUT%\s4dll_link.out"
copy "%OUTPUT%\s4dll_compile.out"+"%OUTPUT%\s4dll_link.out" "%OUTPUT%\out" > nul
del "%OUTPUT%\s4dll_compile.out"
del "%OUTPUT%\s4dll_link.out"

if %SAVE_EL% NEQ 0 exit /b %SAVE_EL%

exit /b 0


:ERRCmdExt
   echo Unable to run. Command extensions are not enabled.
