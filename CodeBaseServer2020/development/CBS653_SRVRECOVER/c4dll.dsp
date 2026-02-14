# Microsoft Developer Studio Project File - Name="c4dll" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=c4dll - Win32 Release
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "C4dll.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "C4dll.mak" CFG="c4dll - Win32 Release"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "c4dll - Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "c4dll - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "c4dll - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir ".\c4dll___"
# PROP BASE Intermediate_Dir ".\c4dll___"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "."
# PROP Intermediate_Dir "..\source"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /Zp1 /MT /W3 /GX /O2 /I "." /I "..\source" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "S4DLL_BUILD" /YX /FD /c /Tp
# ADD BASE MTL /nologo /D "NDEBUG" /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib /nologo /subsystem:windows /dll /machine:I386
# ADD LINK32 wsock32.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib /nologo /subsystem:windows /dll /machine:I386

!ELSEIF  "$(CFG)" == "c4dll - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir ".\c4dll__0"
# PROP BASE Intermediate_Dir ".\c4dll__0"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "."
# PROP Intermediate_Dir "..\source"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MTd /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /c
# ADD CPP /nologo /Zp1 /MTd /W3 /Gm /GX /ZI /Od /I "." /I "..\source" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "S4DLL_BUILD" /YX /FD /c /Tp
# ADD BASE MTL /nologo /D "_DEBUG" /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib /nologo /subsystem:windows /dll /debug /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib wsock32.lib /nologo /subsystem:windows /dll /debug /machine:I386

!ENDIF 

# Begin Target

# Name "c4dll - Win32 Release"
# Name "c4dll - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;hpj;bat;for;f90"
# Begin Source File

SOURCE=..\..\Source\B4block.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\b4node.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\C4.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\c4baspas.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\C4bcd.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\C4BUFFER.C
# End Source File
# Begin Source File

SOURCE=..\..\Source\C4code.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\C4com.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\C4com.h
# End Source File
# Begin Source File

SOURCE=..\..\Source\C4COM2.C
# End Source File
# Begin Source File

SOURCE=..\..\Source\C4coml.h
# End Source File
# Begin Source File

SOURCE=..\..\Source\C4comws.h
# End Source File
# Begin Source File

SOURCE=..\..\Source\C4CONECT.C
# End Source File
# Begin Source File

SOURCE=..\..\Source\C4CONLOW.C
# End Source File
# Begin Source File

SOURCE=..\..\Source\C4const.c
# End Source File
# Begin Source File

SOURCE=..\..\source\c4dll32.def
# End Source File
# Begin Source File

SOURCE=..\..\Source\C4hook.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\c4send.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\c4set.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\C4THREAD.C
# End Source File
# Begin Source File

SOURCE=..\..\Source\C4trans.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\C4trans.h
# End Source File
# Begin Source File

SOURCE=..\..\Source\C4UTIL.C
# End Source File
# Begin Source File

SOURCE=..\..\Source\D4all.h
# End Source File
# Begin Source File

SOURCE=..\..\Source\D4append.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\D4bottom.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\D4close.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\D4create.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\D4data.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\D4data.h
# End Source File
# Begin Source File

SOURCE=..\..\Source\D4date.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\D4declar.h
# End Source File
# Begin Source File

SOURCE=..\..\Source\D4defs.h
# End Source File
# Begin Source File

SOURCE=..\..\Source\D4extra.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\D4field.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\D4file.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\D4flush.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\D4fresh.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\D4go.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\D4index.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\D4info.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\D4inline.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\D4inline.h
# End Source File
# Begin Source File

SOURCE=..\..\Source\D4lock.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\D4MODIFY.C
# End Source File
# Begin Source File

SOURCE=..\..\Source\D4open.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\D4opt.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\D4pack.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\D4positi.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\D4remove.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\D4seek.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\D4skip.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\D4tag.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\D4top.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\D4unlock.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\D4write.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\D4zap.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\Df4lock.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\Df4unlok.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\E4calc.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\E4calc_2.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\E4defs.h
# End Source File
# Begin Source File

SOURCE=..\..\Source\E4error.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\E4error.h
# End Source File
# Begin Source File

SOURCE=..\..\Source\E4EVENT.C
# End Source File
# Begin Source File

SOURCE=..\..\Source\E4expr.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\E4expr.h
# End Source File
# Begin Source File

SOURCE=..\..\Source\E4functi.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\E4not_s.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\E4parse.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\E4string.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\E4string.h
# End Source File
# Begin Source File

SOURCE=..\..\Source\F4ass_f.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\F4char.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\F4close.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\F4create.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\F4double.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\F4field.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\F4file.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\F4filese.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\f4find.cpp
# End Source File
# Begin Source File

SOURCE=..\..\Source\F4flag.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\F4flag.h
# End Source File
# Begin Source File

SOURCE=..\..\Source\F4flush.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\F4info.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\F4int.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\F4lock.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\F4long.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\F4memo.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\F4open.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\F4opt.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\F4ptr.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\F4str.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\F4temp.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\F4true.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\F4write.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\F4WSTR.C
# End Source File
# Begin Source File

SOURCE=..\..\Source\I4add.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\I4addtag.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\I4check.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\i4conv.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\I4create.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\I4dump.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\I4index.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\I4info.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\I4init.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\I4key.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\I4lock.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\I4ntag.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\I4positi.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\I4remove.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\I4tag.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\I4THREAD.C
# End Source File
# Begin Source File

SOURCE=..\..\Source\L4link.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\L4lock_c.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\L4MUTEX.C
# End Source File
# Begin Source File

SOURCE=..\..\Source\M4check.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\M4create.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\M4file.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\M4map.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\M4memo.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\M4memory.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\O4opt.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\O4opt.h
# End Source File
# Begin Source File

SOURCE=..\..\Source\R4area.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\R4bit.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\R4drive2.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\R4driver.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\R4group.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\R4log.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\R4obj.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\R4regs.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\R4reinde.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\R4reinde.h
# End Source File
# Begin Source File

SOURCE=..\..\Source\R4reindx.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\R4relate.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\R4relate.h
# End Source File
# Begin Source File

SOURCE=..\..\Source\R4report.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\R4report.h
# End Source File
# Begin Source File

SOURCE=..\..\Source\R4save.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\R4save_2.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\R4style.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\R4total.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\S4init.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\S4initfr.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\S4next.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\S4quick.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\S4SIGNAL.C
# End Source File
# Begin Source File

SOURCE=..\..\Source\S4sort.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\S4sort.h
# End Source File
# Begin Source File

SOURCE=..\..\Source\SEMA4.C
# End Source File
# Begin Source File

SOURCE=..\..\Source\U4name.c
# End Source File
# Begin Source File

SOURCE=..\..\Source\U4util.c
# End Source File
# Begin Source File

SOURCE=..\..\source\zlib.lib
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl;fi;fd"
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;cnt;rtf;gif;jpg;jpeg;jpe"
# Begin Source File

SOURCE=..\..\Source\c4dll.res
# End Source File
# End Group
# End Target
# End Project
