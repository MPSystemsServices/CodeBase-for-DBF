@echo off

if "%CMDEXTVERSION%" == "" goto ERRCmdExt

if not "%1" == "/?" goto BEGIN
echo Link the object files for the CodeBase DLL, 'S4DLL.DLL'.
echo.
echo s4dll_link [/D] [/I dir] [/O dir] [/link opt [/link opt [...]]]
echo.
echo   /D          Generate debugging information.
echo   /I          Specifies the directory where the intermediate files
echo               (e.g. *.obj) are located. If not provided, intermediate files
echo               are assumed to be in the current directory.
echo   /O          Specifies where all output files will be placed. If not
echo               provided, output files are placed in the same directory as the
echo               intermediate directory.
echo   /link       An options passed directly to the linker.
echo.
echo Errors and warnings from the link are placed in the file 'OUT' located in the
echo output directory.
echo.
echo Exit codes:
echo   0 Success
echo   1 The link could not be performed. Check the file called 'out' for errors.
goto :EOF

:BEGIN
setlocal
echo Preparing to link...

REM set defaults
set DEBUG=
set INTERMEDIATE=%cd%
set OUTPUT=
set LINKOPTS=

:ReadParameters
if /I "%1" == "/D" (
   set DEBUG="T"
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
if /I "%1" == "/LINK" (
   set LINKOPTS=%LINKOPTS% %~2
   shift
   shift
   goto ReadParameters
)
if not "%1" == "" (
   echo Invalid parameter: %~1
   exit /b 1
)
REM end of parameter checking

if defined DEBUG (
   set DEBOPTS=/DEBUG
) else (
   set DEBOPTS=
)

set STARTINGDIRECTORY=%cd%

if not defined OUTPUT set OUTPUT=%INTERMEDIATE%

REM change to output directory
cd /d "%OUTPUT%"
if ERRORLEVEL 1 exit /b %ERRORLEVEL%

set LINKLIST=L%RANDOM%.lst
del %LINKLIST% 2>nul
for /F "usebackq eol=# tokens=*" %%f in ("%STARTINGDIRECTORY%\s4source.lst") do (
   call :ToList %%f %LINKLIST%
)

@echo on
link %DEBOPTS% /DLL /OUT:s4dll.dll /PROFILE /NODEFAULTLIB:LIBC %LINKOPTS% "/LIBPATH:%INTERMEDIATE%" "@%STARTINGDIRECTORY%\s4dll_lib.lst" @%LINKLIST% > out
@echo off

del %LINKLIST%

if not exist s4dll.dll (
   echo *** Unable to build s4dll.dll
   exit /b 1
)

goto :EOF
REM end of main script body


:ToList
   @REM Description: given a source file name, append
   @REM    the corresponding object file name to a list file.
   @REM %1 The name of the source file.
   @REM %2 The name of the list file.
   echo "%INTERMEDIATE%\%~n1.obj" >> %2
   goto :EOF
REM end ToList


:ERRCmdExt
   echo Unable to run. Command extensions are not enabled.
