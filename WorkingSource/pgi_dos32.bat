@echo off
set PGI=D:\APPS64\PGI114~1
set PATH=C:\Program Files\Java\jre6\bin;%PATH%
set PATH=D:\APPS64\PGI114\flexlm;%PATH%
set PATH=%PGI%\win32\2011\cuda\3.1\bin;%PATH%
set PATH=%PGI%\win32\2011\cuda\3.2\bin;%PATH%
set PATH=D:\APPS64\PGI114\Microsoft Open Tools 10\bin;%PATH%
set PATH=%PGI%\win32\11.4\bin;%PATH%
set PATH=%PATH%;.
set FLEXLM_BATCH=1
title PGI Workstation 11.4
set TMP=C:\temp
set PS1=PGI$ 
echo PGI Workstation 11.4

copy D:\APPS64\PGI114-32\win32\11.4\REDIST\pgc.dll pgc.dll
copy D:\APPS64\PGI114-32\win32\11.4\REDIST\pg.dll pg.dll
copy D:\APPS64\PGI114-32\win32\11.4\REDIST\pgftnrtl.dll pgftnrtl.dll

copy "D:\APPS64\PGI114\Microsoft Open Tools 10\PlatformSDK\lib\user32.lib"
copy "D:\APPS64\PGI114\Microsoft Open Tools 10\PlatformSDK\lib\kernel32.lib"

cmd

