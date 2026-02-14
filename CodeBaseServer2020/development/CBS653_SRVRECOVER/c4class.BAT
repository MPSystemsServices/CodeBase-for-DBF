@echo off
if not %1.== batch. if not %1. == BATCH. goto USER
shift
goto COMPILE

:USER
cls
echo :  This batch file builds the Stand-alone version of the CodeBase 6
echo :  C++ interface library C4CLASS.LIB, using the Microsoft Visual C++
echo :  compiler and places it in the SOURCE directory.
echo :
echo :  Compile output can be viewed in the  file 'OUT' located
echo :  in the SOURCE directory. This compilation may produce some
echo :  warnings that will not appear with other compile options.
echo :
echo :  This batch file can be run in batch mode so that no user
echo :  interaction is required provided that the files 'C4CLASS.BAT'
echo :  and 'C4CLASS.LST' are in the same directory as the
echo :  source and the 'BATCH' command line option is specified.
echo :
echo :  Warning:  Install CodeBase 6 before running this batch file.
echo :
echo :  Press Ctrl C or Break to abort.
echo :
pause

copy c4class.lst ..\..\source\.
chdir ..\..\source

:COMPILE
echo on
cl -DS4CBPP -DS4LIB_BUILD -DWIN32 %1 %2 %3 %4 %5 %6 %7 %8 %9 -c -O2 -Zl -Zp1 -YX F4INFO_P.CPP F4SEQ.CPP T4INFO.CPP D4DATA_P.CPP S4STR.CPP S4STRING.CPP  > OUT
if exist c4class.lib erase c4class.lib
lib @c4class.lst >> OUT
