@echo off

if "%CMDEXTVERSION%" == "" goto ERRCmdExt

if not "%1" == "/?" goto BEGIN
echo Compile source files for the 32-bit CodeBase static library.
echo.
echo c4lib_compile [/C] [/D] [/O dir] [/S dir] [/compile opt [/compile opt [...]]]
echo               ^{@source.lst ^| source.c^}
echo.
echo   /C          If any source file could not be compiled, continue compiling the
echo               remaining source files. By default, this command stops compiling
echo               when any source file cannot be compiled.
echo   /D          Generate debugging information.
echo   /O          Specifies where all output files (e.g. *.obj) will be placed. If
echo               not provided, output files are placed in the same directory as
echo               the source directory.
echo   /S          Specifies the directory where the CodeBase source files are
echo               located. If not provided, source files are assumed to be in
echo               the current directory.
echo   /compile    An option passed directly to the compiler.
echo   @source.lst A text file containing a list of source files to compile, one
echo               per line. Lines beginning with # are ignored.
echo   source.c    A single source file to compile.
echo.
echo Errors and warnings from the compile are placed in the file 'OUT' located in
echo the output directory.
echo You can provide as many @source.lst and source.c parameters as required.
echo.
echo Exit codes:
echo   0 Success
echo   1 One or more source files could not be compiled. Check the file called
echo     'out' for errors.
goto :EOF

:BEGIN
setlocal
echo Preparing to compile...

REM set defaults
set CONTINUE=
set DEBUG=
set OUTPUT=
set SOURCE=%cd%
set COMPOPTS=
set COMPILELIST=%TEMP%\C%RANDOM%.tmp
if exist %COMPILELIST% del %COMPILELIST%
set PCH=-Ycd4all.h

:ReadParameters
if /I "%1" == "/C" (
   set CONTINUE=T
   shift
   goto ReadParameters
)
if /I "%1" == "/D" (
   set DEBUG=T
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
set PERCENT1=%~1
if "%PERCENT1:~0,1%" == "@" (
   @REM The parameter is a list file name.
   if exist %COMPILELIST% (
      copy %COMPILELIST%+%PERCENT1:~1% %COMPILELIST% > nul
   ) else (
      copy /y %PERCENT1:~1% %COMPILELIST% > nul
   )
   shift
   goto ReadParameters
)
if not "%1" == "" (
   @REM The parameter must be a file name
   echo %~1 >> %COMPILELIST%
   shift
   goto ReadParameters
)
REM end of parameter checking

if not exist %COMPILELIST% (
   echo No source files provided.
   exit /b 1
)


if defined DEBUG (
   set DEBOPTS=-Od -Zi
) else (
   set DEBOPTS=-O2
)

if not defined OUTPUT set OUTPUT=%SOURCE%

REM change to output directory
cd /d "%OUTPUT%"
if ERRORLEVEL 1 exit /b %ERRORLEVEL%

if exist out del out
set FAILCOUNT=0


REM for each source file, compile it
for /F "eol=# tokens=*" %%f in (%COMPILELIST%) do (
   call :CompileOne %%f
   if errorlevel 1 (
      del %COMPILELIST%
      if not defined CONTINUE (
         exit /b 1
      )
   )
)

REM delete the temporary file list
del %COMPILELIST%

if %FAILCOUNT% NEQ 0 (
   echo Error compiling %FAILCOUNT% files.
   exit /b 1
)

goto :EOF
REM end of main script body



:CompileOne
   if exist "%~n1.obj" del "%~n1.obj"

   @echo on
   cl -DS4LIB_BUILD -DWIN32 -c -nologo -MT -W1 -Zp1 -Zl -O2 %DEBOPTS% %COMPOPTS% %PCH% -Tp "%SOURCE%\%~1" >> OUT
   @echo off
   set PCH=-Yud4all.h

   if not exist "%~n1.obj" (
      echo *** Unable to build %~1
      set /a FAILCOUNT+=1
      exit /b 1
   )
   goto :EOF
REM end CompileOne



:ERRCmdExt
   echo Unable to run. Command extensions are not enabled.
