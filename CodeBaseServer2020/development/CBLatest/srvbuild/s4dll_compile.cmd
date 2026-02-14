@echo off

if "%CMDEXTVERSION%" == "" goto ERRCmdExt

if not "%1" == "/?" goto BEGIN
echo Compile source files for the 32-bit CodeBase Server DLL, 'S4DLL.DLL'.
echo.
echo s4dll_compile [/C] [/D] [/O dir] [/S dir] [/compile opt [/compile opt [...]]]
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
echo.
echo Errors and warnings from the compile are placed in the file 'OUT' located in
echo the output directory.
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
set PCH_CB=
set PCH_OLEDB=

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
if not "%1" == "" (
   echo Invalid parameter: %~1
   exit /b 1
)
REM end of parameter checking


REM DEBOPTS contains the debug and optimization switches for most source files.
REM DEBOPTS_NOOPT contains the debug and optimization switches for the select
REM          source files that should not use optimization.
if defined DEBUG (
   set DEBOPTS=-Od -Zi
   set DEBOPTS_NOOPT=-Od -Zi
) else (
   set DEBOPTS=-O2
   set DEBOPTS_NOOPT=-Od
)

set CURRENTDIRECTORY=%cd%

if not defined OUTPUT set OUTPUT=%SOURCE%

REM change to output directory
cd /d %OUTPUT%
if ERRORLEVEL 1 exit /b %ERRORLEVEL%

if exist out del out
set FAILCOUNT=0

if not exist %CURRENTDIRECTORY%\s4source.lst (
   echo s4source.lst not found
   exit /b 1
)

REM for each source file, compile it
for /F "eol=# tokens=*" %%f in (%CURRENTDIRECTORY%\s4source.lst) do (
   call :CompileOne %%f
   if errorlevel 1 (
      if not defined CONTINUE (
         exit /b 1
      )
   )
)

if errorlevel 1 exit /b 1

if %FAILCOUNT% NEQ 0 (
   echo Error compiling %FAILCOUNT% files.
   exit /b 1
)

if "%SOURCE%" NEQ "%OUTPUT%" (
   REM Copy files which do not get compiled but are needed for link.
   if exist "%SOURCE%\c4dll.res" (
      copy /y "%SOURCE%\c4dll.res" "%OUTPUT%\." > nul
   )
   if exist "%SOURCE%\c4dll32.def" (
      copy /y "%SOURCE%\c4dll32.def" "%OUTPUT%\." > nul
   )
   if exist "%SOURCE%\zlib.lib" (
      copy /y "%SOURCE%\zlib.lib" "%OUTPUT%\." > nul
   )
)

goto :EOF
REM end of main script body



:CompileOne
   del "%~n1.obj" 2>nul

   call :SetPCH %~1

   REM For these select source files, disable optimization and precompiled header.
   set NOOPT=
   if /i "%~nx1" EQU "f4file.c"   set NOOPT=TRUE
   if /i "%~nx1" EQU "f4filese.c" set NOOPT=TRUE
   if /i "%~nx1" EQU "r4reinde.c" set NOOPT=TRUE
   if /i "%~nx1" EQU "r4reindx.c" set NOOPT=TRUE

   set DEBOPTS_LOCAL=%DEBOPTS%
   set PCH_LOCAL=%PCH%
   if defined NOOPT (
      set DEBOPTS_LOCAL=%DEBOPTS_NOOPT%
      set PCH_LOCAL=
   )

   @echo on
   cl -DS4DLL_BUILD -DS4SERVER -DS4JAVA -DS4HALFJAVA -DWIN32 -c -nologo -GX -MT -W3 -Zp1 %DEBOPTS_LOCAL% %COMPOPTS% %PCH_LOCAL% -Tp "%SOURCE%\%~1" >> OUT
   @echo off

   if not exist "%~n1.obj" (
      echo *** Unable to build %~1
      set /a FAILCOUNT+=1
      exit /b 1
   )
   goto :EOF
REM end CompileOne



:SetPCH
   @REM Analize the file name and determine which header to precompile.
   @REM First compile with the -Yc option. Then compile with the -Yu option.
   if /I "%~x1" == ".c" goto SetPCH_CB
   if /I "%~1" == "hash4.cpp" goto SetPCH_CB
   if /I "%~1" == "LOG5.cpp" goto SetPCH_CB
   if /I "%~1" == "f4find.cpp" goto SetPCH_CB

   if /I "%~1" == "util5.cpp" (
      set PCH=
      goto :EOF
   )

   if not defined PCH_OLEDB (
      set PCH_OLEDB=-Ycoledb5.hpp
   ) else (
      set PCH_OLEDB=-Yuoledb5.hpp
   )
   set PCH=%PCH_OLEDB%
   goto :EOF

   :SetPCH_CB
   if not defined PCH_CB (
      set PCH_CB=-Ycd4all.h
   ) else (
      set PCH_CB=-Yud4all.h
   )
   set PCH=%PCH_CB%
   goto :EOF
REM end SetPCH


:ERRCmdExt
   echo Unable to run. Command extensions are not enabled.
