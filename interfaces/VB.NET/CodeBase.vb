Option Strict Off
Option Explicit On

Module CodeBase
    ' codebase.vb  (c)Copyright Sequiter Software Inc., 1988-2011.  All rights reserved.

    'Data Types Used by CodeBase

    Structure FIELD4INFOCB
        Dim fName As Integer
        Dim ftype As Short
        Dim flength As Short
        Dim fdecimals As Short
        Dim fnulls As Short
    End Structure

    Structure FIELD4INFO
        Dim fName As String
        <VBFixedString(1), System.Runtime.InteropServices.MarshalAs(System.Runtime.InteropServices.UnmanagedType.ByValTStr, SizeConst:=1)> Dim ftype As String
        Dim flength As Short
        Dim fdecimals As Short
        Dim fnulls As Short
    End Structure

    Structure TAG4INFOCB
        Dim name As Integer
        Dim expression As Integer
        Dim filter_Renamed As Integer
        Dim unique As Short
        Dim descending As Short
    End Structure

    Structure TAG4INFO
        Dim name As String
        Dim expression As String
        Dim filter_Renamed As String
        Dim unique As Short
        Dim descending As Short
    End Structure

    '===================================================================================
    '
    '     CODE4 Access  function prototypes
    '
    '===================================================================================
#If USE_D4DLL Then
    Declare Sub c4setAcceptTimeOut Lib "d4dll.dll" (ByVal c4 As Integer, ByVal value As Integer)
    Declare Function code4actionCode Lib "d4dll.dll" (ByVal c4 As Integer) As Short
    Declare Function code4accessMode Lib "d4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Sub code4autoIncrementStart Lib "d4dll.dll" (ByVal c4 As Integer, ByVal value As Double)
    Declare Function code4autoOpen Lib "d4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4calcCreate Lib "d4dll.dll" (ByVal c4 As Integer, ByVal expr4 As Integer, ByVal fcnName As String) As Short
    Declare Sub code4calcReset Lib "d4dll.dll" (ByVal c4 As Integer)
    Declare Function code4codePage Lib "d4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4collatingSequence Lib "d4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4collate Lib "d4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4compatibility Lib "d4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4compress Lib "d4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4connect Lib "d4dll.dll" (ByVal c4 As Integer, ByVal serverId As String, ByVal processId As String, ByVal userName As String, ByVal password As String, ByVal protocol As String) As Short
    Declare Function code4close Lib "d4dll.dll" (ByVal c4 As Integer) As Short
    Declare Function code4createTemp Lib "d4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4data Lib "d4dll.dll" (ByVal c4 As Integer, ByVal AliasName As String) As Integer
    Declare Function code4dateFormatVB Lib "d4dll.dll" Alias "code4dateFormat" (ByVal c4 As Integer) As Integer
    Declare Function code4dateFormatSet Lib "d4dll.dll" (ByVal c4 As Integer, ByVal fmt As String) As Short
    Declare Function code4errCreate Lib "d4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4errDefaultUnique Lib "d4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4errorCode Lib "d4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4errExpr Lib "d4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4errFieldName Lib "d4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4errGo Lib "d4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4errSkip Lib "d4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4errTagName Lib "d4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Sub code4exit Lib "d4dll.dll" (ByVal c4 As Integer)
    Declare Function code4fileFlush Lib "d4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4flush Lib "d4dll.dll" (ByVal c4 As Integer) As Short
    Declare Function code4hInst Lib "d4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4indexBlockSize Lib "d4dll.dll" (ByVal c4 As Integer) As Short
    Declare Function code4indexBlockSizeSet Lib "d4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4indexExtensionVB Lib "d4dll.dll" Alias "code4indexExtension" (ByVal c4 As Integer) As Integer
    Declare Function code4hWnd Lib "d4dll.dll" (ByVal c4 As Integer, ByVal value As Integer) As Integer
    Declare Function code4allocDll Lib "d4dll.dll" (ByVal dllName As String) As Integer
    Declare Function code4initUndo Lib "d4dll.dll" (ByVal c4 As Integer) As Short
    Declare Sub code4largeOn Lib "d4dll.dll" (ByVal c4 As Integer)
    Declare Function code4lock Lib "d4dll.dll" (ByVal c4 As Integer) As Short
    Declare Function code4lockAttempts Lib "d4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4lockAttemptsSingle Lib "d4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Sub code4lockClear Lib "d4dll.dll" (ByVal c4 As Integer)
    Declare Function code4lockDelay Lib "d4dll.dll" (ByVal c4 As Integer, ByVal value As Integer) As Integer
    Declare Function code4lockEnforce Lib "d4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4lockFileNameVB Lib "d4dll.dll" Alias "code4lockFileName" (ByVal c4 As Integer) As Integer
    Declare Function code4lockItem Lib "d4dll.dll" (ByVal c4 As Integer) As Integer
    Declare Function code4lockNetworkIdVB Lib "d4dll.dll" Alias "code4lockNetworkId" (ByVal c4 As Integer) As Integer
    Declare Function code4lockUserIdVB Lib "d4dll.dll" Alias "code4lockUserId" (ByVal c4 As Integer) As Integer
    Declare Function code4log Lib "d4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4logCreate Lib "d4dll.dll" (ByVal c4 As Integer, ByVal logName As String, ByVal userId As String) As Short
    Declare Function code4logFileNameVB Lib "d4dll.dll" (ByVal c4 As Integer) As Integer
    Declare Function code4logOpen Lib "d4dll.dll" (ByVal c4 As Integer, ByVal logName As String, ByVal userId As String) As Short
    Declare Sub code4logOpenOff Lib "d4dll.dll" (ByVal c4 As Integer)
    Declare Function code4memExpandBlock Lib "d4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4memExpandData Lib "d4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4memExpandIndex Lib "d4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4memExpandLock Lib "d4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4memExpandTag Lib "d4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4memSizeBlock Lib "d4dll.dll" (ByVal c4 As Integer, ByVal value As Integer) As Integer
    Declare Function code4memSizeBuffer Lib "d4dll.dll" (ByVal c4 As Integer, ByVal value As Integer) As Integer
    Declare Function code4memSizeMemo Lib "d4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4memSizeMemoExpr Lib "d4dll.dll" (ByVal c4 As Integer, ByVal value As Integer) As Integer
    Declare Function code4memSizeSortBuffer Lib "d4dll.dll" (ByVal c4 As Integer, ByVal value As Integer) As Integer
    Declare Function code4memSizeSortPool Lib "d4dll.dll" (ByVal c4 As Integer, ByVal value As Integer) As Integer
    Declare Function code4memStartBlock Lib "d4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4memStartData Lib "d4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4memStartIndex Lib "d4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4memStartLock Lib "d4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4memStartMax Lib "d4dll.dll" (ByVal c4 As Integer, ByVal value As Integer) As Integer
    Declare Function code4memStartTag Lib "d4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4errOff Lib "d4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4errOpen Lib "d4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4optAll Lib "d4dll.dll" (ByVal c4 As Integer) As Short
    Declare Function code4optimize Lib "d4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4optStart Lib "d4dll.dll" (ByVal c4 As Integer) As Short
    Declare Function code4optSuspend Lib "d4dll.dll" (ByVal c4 As Integer) As Short
    Declare Function code4optimizeWrite Lib "d4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4readLock Lib "d4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4readOnly Lib "d4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4errRelate Lib "d4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4safety Lib "d4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4singleOpen Lib "d4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4serverOS Lib "d4dll.dll" (ByVal c4 As Integer) As Integer
    Declare Function code4timeout Lib "d4dll.dll" (ByVal c4 As Integer) As Integer
    Declare Sub code4timeoutSet Lib "d4dll.dll" (ByVal c4 As Integer, ByVal value As Integer)
    Declare Function code4tranStart Lib "d4dll.dll" (ByVal c4 As Integer) As Short
    Declare Function code4tranStatus Lib "d4dll.dll" Alias "code4tranStatusCB" (ByVal c4 As Integer) As Short
    Declare Function code4tranCommit Lib "d4dll.dll" (ByVal c4 As Integer) As Short
    Declare Function code4tranRollback Lib "d4dll.dll" (ByVal c4 As Integer) As Short
    Declare Function code4unlock Lib "d4dll.dll" (ByVal c4 As Integer) As Short
    Declare Function code4unlockAuto Lib "d4dll.dll" Alias "code4unlockAutoCB" (ByVal c4 As Integer) As Short
    Declare Function code4useGeneralTagsInRelate Lib "d4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Sub code4unlockAutoSet Lib "d4dll.dll" Alias "code4unlockAutoSetCB" (ByVal c4 As Integer, ByVal value As Short)
    Declare Sub code4verifySet Lib "d4dll.dll" (ByVal c4 As Integer, ByVal value As String)
#Else
    Declare Sub c4setAcceptTimeOut Lib "c4dll.dll" (ByVal c4 As Integer, ByVal value As Integer)
    Declare Function code4actionCode Lib "c4dll.dll" (ByVal c4 As Integer) As Short
    Declare Function code4accessMode Lib "c4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Sub code4autoIncrementStart Lib "c4dll.dll" (ByVal c4 As Integer, ByVal value As Double)
    Declare Function code4autoOpen Lib "c4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4calcCreate Lib "c4dll.dll" (ByVal c4 As Integer, ByVal expr4 As Integer, ByVal fcnName As String) As Short
    Declare Sub code4calcReset Lib "c4dll.dll" (ByVal c4 As Integer)
    Declare Function code4codePage Lib "c4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4collatingSequence Lib "c4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4collate Lib "c4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4compatibility Lib "c4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4compress Lib "c4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4connect Lib "c4dll.dll" (ByVal c4 As Integer, ByVal serverId As String, ByVal processId As String, ByVal userName As String, ByVal password As String, ByVal protocol As String) As Short
    Declare Function code4close Lib "c4dll.dll" (ByVal c4 As Integer) As Short
    Declare Function code4createTemp Lib "c4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4data Lib "c4dll.dll" (ByVal c4 As Integer, ByVal AliasName As String) As Integer
    Declare Function code4dateFormatVB Lib "c4dll.dll" Alias "code4dateFormat" (ByVal c4 As Integer) As Integer
    Declare Function code4dateFormatSet Lib "c4dll.dll" (ByVal c4 As Integer, ByVal fmt As String) As Short
    Declare Sub code4encryptFile Lib "c4dll.dll" (ByVal c4 As Integer, ByVal value As Short)
    Declare Function code4encryptInit Lib "c4dll.dll" (ByVal c4 As Integer, ByVal key As String, ByVal length As Short) As Short
    Declare Function code4errCreate Lib "c4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4errDefaultUnique Lib "c4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4errorCode Lib "c4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4errExpr Lib "c4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4errFieldName Lib "c4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4errGo Lib "c4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4errSkip Lib "c4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4errTagName Lib "c4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Sub code4exit Lib "c4dll.dll" (ByVal c4 As Integer)
    Declare Function code4fileFlush Lib "c4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4flush Lib "c4dll.dll" (ByVal c4 As Integer) As Short
    Declare Function code4hInst Lib "c4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4indexBlockSize Lib "c4dll.dll" (ByVal c4 As Integer) As Short
    Declare Function code4indexBlockSizeSet Lib "c4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4indexExtensionVB Lib "c4dll.dll" Alias "code4indexExtension" (ByVal c4 As Integer) As Integer
    Declare Function code4hWnd Lib "c4dll.dll" (ByVal c4 As Integer, ByVal value As Integer) As Integer
    Declare Function code4init Lib "c4dll.dll" Alias "code4initVB" () As Integer
    Declare Function code4initUndo Lib "c4dll.dll" (ByVal c4 As Integer) As Short
    Declare Sub code4largeOn Lib "c4dll.dll" (ByVal c4 As Integer)
    Declare Sub code4limitKeySizeSet Lib "c4dll.dll" (ByVal c4 As Integer, ByVal value As Short)
    Declare Function code4lock Lib "c4dll.dll" (ByVal c4 As Integer) As Short
    Declare Function code4lockAttempts Lib "c4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4lockAttemptsSingle Lib "c4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Sub code4lockClear Lib "c4dll.dll" (ByVal c4 As Integer)
    Declare Function code4lockDelay Lib "c4dll.dll" (ByVal c4 As Integer, ByVal value As Integer) As Integer
    Declare Function code4lockEnforce Lib "c4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4lockFileNameVB Lib "c4dll.dll" Alias "code4lockFileName" (ByVal c4 As Integer) As Integer
    Declare Function code4lockItem Lib "c4dll.dll" (ByVal c4 As Integer) As Integer
    Declare Function code4lockNetworkIdVB Lib "c4dll.dll" Alias "code4lockNetworkId" (ByVal c4 As Integer) As Integer
    Declare Function code4lockUserIdVB Lib "c4dll.dll" Alias "code4lockUserId" (ByVal c4 As Integer) As Integer
    Declare Function code4log Lib "c4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4logCreate Lib "c4dll.dll" (ByVal c4 As Integer, ByVal logName As String, ByVal userId As String) As Short
    Declare Function code4logFileNameVB Lib "c4dll.dll" (ByVal c4 As Integer) As Integer
    Declare Function code4logOpen Lib "c4dll.dll" (ByVal c4 As Integer, ByVal logName As String, ByVal userId As String) As Short
    Declare Sub code4logOpenOff Lib "c4dll.dll" (ByVal c4 As Integer)
    Declare Function code4memExpandBlock Lib "c4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4memExpandData Lib "c4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4memExpandIndex Lib "c4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4memExpandLock Lib "c4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4memExpandTag Lib "c4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4memoCompress Lib "c4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Integer
    Declare Function code4memSizeBlock Lib "c4dll.dll" (ByVal c4 As Integer, ByVal value As Integer) As Integer
    Declare Function code4memSizeBuffer Lib "c4dll.dll" (ByVal c4 As Integer, ByVal value As Integer) As Integer
    Declare Function code4memSizeMemo Lib "c4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4memSizeMemoExpr Lib "c4dll.dll" (ByVal c4 As Integer, ByVal value As Integer) As Integer
    Declare Function code4memSizeSortBuffer Lib "c4dll.dll" (ByVal c4 As Integer, ByVal value As Integer) As Integer
    Declare Function code4memSizeSortPool Lib "c4dll.dll" (ByVal c4 As Integer, ByVal value As Integer) As Integer
    Declare Function code4memStartBlock Lib "c4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4memStartData Lib "c4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4memStartIndex Lib "c4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4memStartLock Lib "c4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4memStartMax Lib "c4dll.dll" (ByVal c4 As Integer, ByVal value As Integer) As Integer
    Declare Function code4memStartTag Lib "c4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4errOff Lib "c4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4errOpen Lib "c4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4optAll Lib "c4dll.dll" (ByVal c4 As Integer) As Short
    Declare Function code4optimize Lib "c4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4optStart Lib "c4dll.dll" (ByVal c4 As Integer) As Short
    Declare Function code4optSuspend Lib "c4dll.dll" (ByVal c4 As Integer) As Short
    Declare Function code4optimizeWrite Lib "c4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4ping Lib "c4dll.dll" (ByVal c4 As Integer) As Short
    Declare Function code4readLock Lib "c4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4readOnly Lib "c4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4errRelate Lib "c4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4safety Lib "c4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4singleOpen Lib "c4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Function code4serverOS Lib "c4dll.dll" (ByVal c4 As Integer) As Integer
    Declare Function code4timeout Lib "c4dll.dll" (ByVal c4 As Integer) As Integer
    Declare Sub code4timeoutSet Lib "c4dll.dll" (ByVal c4 As Integer, ByVal value As Integer)
    Declare Function code4tranStart Lib "c4dll.dll" (ByVal c4 As Integer) As Short
    Declare Function code4tranStatus Lib "c4dll.dll" Alias "code4tranStatusCB" (ByVal c4 As Integer) As Short
    Declare Function code4tranCommit Lib "c4dll.dll" (ByVal c4 As Integer) As Short
    Declare Function code4tranRollback Lib "c4dll.dll" (ByVal c4 As Integer) As Short
    Declare Function code4unlock Lib "c4dll.dll" (ByVal c4 As Integer) As Short
    Declare Function code4unlockAuto Lib "c4dll.dll" Alias "code4unlockAutoCB" (ByVal c4 As Integer) As Short
    Declare Function code4useGeneralTagsInRelate Lib "c4dll.dll" (ByVal c4 As Integer, ByVal value As Short) As Short
    Declare Sub code4unlockAutoSet Lib "c4dll.dll" Alias "code4unlockAutoSetCB" (ByVal c4 As Integer, ByVal value As Short)
    Declare Sub code4verifySet Lib "c4dll.dll" (ByVal c4 As Integer, ByVal value As String)
#End If
    '===============================================================================================
    '
    '                                 Data File Functions' Prototypes
    '
    '-----------------------------------------------------------------------------------------------
#If USE_D4DLL Then
    Declare Function d4aliasCB Lib "d4dll.dll"  Alias "d4alias"(ByVal d4 As Integer) As Integer
    Declare Sub d4aliasSet Lib "d4dll.dll" (ByVal d4 As Integer, ByVal AliasValue As String)
    Declare Function d4append Lib "d4dll.dll" (ByVal d4 As Integer) As Short
    Declare Function d4appendBlank Lib "d4dll.dll" (ByVal d4 As Integer) As Short
    Declare Function d4appendStart Lib "d4dll.dll" (ByVal d4 As Integer, ByVal UseMemoEntries As Short) As Short
    Declare Sub d4blank Lib "d4dll.dll" (ByVal d4 As Integer)
    Declare Function d4bof Lib "d4dll.dll" (ByVal d4 As Integer) As Short
    Declare Function d4bottom Lib "d4dll.dll" (ByVal d4 As Integer) As Short
    Declare Function d4changed Lib "d4dll.dll" (ByVal d4 As Integer, ByVal intFlag As Short) As Short
    Declare Function d4check Lib "d4dll.dll" (ByVal d4 As Integer) As Short
    Declare Function d4close Lib "d4dll.dll" (ByVal d4 As Integer) As Short
    Declare Function d4codePage Lib "d4dll.dll" (ByVal d4 As Integer) As Short
    Declare Function d4createCB Lib "d4dll.dll"  Alias "d4create"(ByVal c4 As Integer, ByVal DbfName As String, ByVal fieldinfo As Integer, ByVal tagInfo As Integer) As Integer
    Declare Function d4createLow Lib "d4dll.dll" Alias "d4create" (ByVal c4 As Integer, ByVal DbfName As String, ByRef fieldinfo As FIELD4INFOCB, ByRef tagInfo As TAG4INFOCB) As Integer
    Declare Function d4createLow Lib "d4dll.dll" Alias "d4create" (ByVal c4 As Integer, ByVal DbfName As String, ByRef fieldinfo As FIELD4INFOCB, Optional ByVal tagInfo As Integer = 0) As Integer
    Declare Sub d4delete Lib "d4dll.dll" (ByVal d4 As Integer)
    Declare Function d4deleted Lib "d4dll.dll" (ByVal d4 As Integer) As Short
    Declare Function d4eof Lib "d4dll.dll" (ByVal d4 As Integer) As Short
    Declare Function d4field Lib "d4dll.dll" (ByVal d4 As Integer, ByVal FieldName As String) As Integer
    Declare Function d4fieldInfo Lib "d4dll.dll" (ByVal d4 As Integer) As Integer
    Declare Function d4fieldJ Lib "d4dll.dll" (ByVal d4 As Integer, ByVal JField As Short) As Integer
    Declare Function d4fieldNumber Lib "d4dll.dll" (ByVal d4 As Integer, ByVal FieldName As String) As Short
    Declare Function d4fieldsAddCB Lib "d4dll.dll" Alias "d4fieldsAdd" (ByVal d4 As Integer, ByVal nFields As Short, ByRef fieldinfo As FIELD4INFOCB) As Integer
    Declare Function d4fieldsRemoveCB Lib "d4dll.dll" Alias "d4fieldsRemove" (ByRef d4 As Integer, ByVal nFields As Short, ByRef fieldNames As Integer) As Integer
    Declare Function d4fileNameCB Lib "d4dll.dll"  Alias "d4fileName"(ByVal d4 As Integer) As Integer
    Declare Function d4flush Lib "d4dll.dll" (ByVal d4 As Integer) As Short
    Declare Function d4freeBlocks Lib "d4dll.dll" (ByVal d4 As Integer) As Short
    Declare Function d4goLow Lib "d4dll.dll" (ByVal d4 As Integer, ByVal RecNum As Integer, ByVal goForWrite As Short) As Short
    Declare Function d4goBof Lib "d4dll.dll" (ByVal d4 As Integer) As Short
    Declare Function d4goEof Lib "d4dll.dll" (ByVal d4 As Integer) As Short
    Declare Function d4index Lib "d4dll.dll" (ByVal d4 As Integer, ByVal IndexName As String) As Integer
    Declare Function d4log Lib "d4dll.dll"  Alias "d4logVB"(ByVal d4 As Integer, ByVal logging As Short) As Short
    Declare Function d4lock Lib "d4dll.dll"  Alias "d4lockVB"(ByVal d4 As Integer, ByVal recordNum As Integer) As Short
    Declare Function d4lockAdd Lib "d4dll.dll" (ByVal d4 As Integer, ByVal recordNum As Integer) As Short
    Declare Function d4lockAddAll Lib "d4dll.dll" (ByVal d4 As Integer) As Short
    Declare Function d4lockAddAppend Lib "d4dll.dll" (ByVal d4 As Integer) As Short
    Declare Function d4lockAddFile Lib "d4dll.dll" (ByVal d4 As Integer) As Short
    Declare Function d4lockAll Lib "d4dll.dll"  Alias "d4lockAllVB"(ByVal d4 As Integer) As Short
    Declare Function d4lockAppend Lib "d4dll.dll"  Alias "d4lockAppendVB"(ByVal d4 As Integer) As Short
    Declare Function d4lockFile Lib "d4dll.dll"  Alias "d4lockFileVB"(ByVal d4 As Integer) As Short
    Declare Function d4logStatus Lib "d4dll.dll"  Alias "d4logStatusCB"(ByVal d4 As Integer) As Short
    Declare Function d4memoCompress Lib "d4dll.dll" (ByVal d4 As Integer) As Short
    Declare Function d4numFields Lib "d4dll.dll" (ByVal d4 As Integer) As Short
    Declare Function d4open Lib "d4dll.dll" (ByVal c4 As Integer, ByVal DbfName As String) As Integer
    Declare Function d4openClone Lib "d4dll.dll" (ByVal d4 As Integer) As Integer
    Declare Function d4optimize Lib "d4dll.dll"  Alias "d4optimizeVB"(ByVal d4 As Integer, ByVal OptFlag As Short) As Short
    Declare Function d4optimizeWrite Lib "d4dll.dll"  Alias "d4optimizeWriteVB"(ByVal d4 As Integer, ByVal OptFlag As Short) As Short
    Declare Function d4pack Lib "d4dll.dll" (ByVal d4 As Integer) As Short
    Declare Function d4packData Lib "d4dll.dll" (ByVal d4 As Integer) As Short
    Declare Function d4position Lib "d4dll.dll" (ByVal d4 As Integer) As Double
    Declare Function d4positionSet Lib "d4dll.dll" (ByVal d4 As Integer, ByVal Percentage As Double) As Short
    Declare Function d4readBuffer Lib "d4dll.dll" (ByVal d4 As Integer, ByVal numRecsToBuf As Integer, ByVal doMemos As Short) As Integer
    Declare Sub d4recall Lib "d4dll.dll" (ByVal d4 As Integer)
    Declare Function d4recCount Lib "d4dll.dll"  Alias "d4recCountDo"(ByVal d4 As Integer) As Integer
    Declare Function d4recNo Lib "d4dll.dll"  Alias "d4recNoLow"(ByVal d4 As Integer) As Integer
    Declare Function d4record Lib "d4dll.dll"  Alias "d4recordLow"(ByVal d4 As Integer) As Integer
    Declare Function d4recWidth Lib "d4dll.dll"  Alias "d4recWidth_v"(ByVal d4 As Integer) As Integer
    Declare Function d4remove Lib "d4dll.dll" (ByVal d4 As Integer) As Short
    Declare Function d4refresh Lib "d4dll.dll" (ByVal d4 As Integer) As Short
    Declare Function d4refreshRecord Lib "d4dll.dll" (ByVal d4 As Integer) As Short
    Declare Function d4reindex Lib "d4dll.dll" (ByVal d4 As Integer) As Short
    Delegate Function d4reindexWithProgressDelegate(ByVal percent As Double) As Short
    Declare Function d4reindexWithProgress Lib "d4dll.dll" (ByVal d4 As Integer, ByVal callback As d4reindexWithProgressDelegate, ByVal milliseconds As Integer) As Short
    Declare Function d4seek Lib "d4dll.dll" (ByVal d4 As Integer, ByVal seekValue As String) As Short
    Declare Function d4seekDouble Lib "d4dll.dll" (ByVal d4 As Integer, ByVal value As Double) As Short
    Declare Function d4seekN Lib "d4dll.dll" (ByVal d4 As Integer, ByVal seekValue As String, ByVal seekLen As Short) As Short
    Declare Function d4seekNext Lib "d4dll.dll" (ByVal d4 As Integer, ByVal seekValue As String) As Short
    Declare Function d4seekNextDouble Lib "d4dll.dll" (ByVal d4 As Integer, ByVal seekValue As Double) As Short
    Declare Function d4seekNextN Lib "d4dll.dll" (ByVal d4 As Integer, ByVal seekValue As String, ByVal seekLen As Short) As Short
    Declare Function d4skip Lib "d4dll.dll" (ByVal d4 As Integer, ByVal NumberRecords As Integer) As Short
    Declare Function d4tag Lib "d4dll.dll" (ByVal d4 As Integer, ByVal TagName As String) As Integer
    Declare Function d4tagDefault Lib "d4dll.dll" (ByVal d4 As Integer) As Integer
    Declare Function d4tagNext Lib "d4dll.dll" (ByVal d4 As Integer, ByVal TagOn As Integer) As Integer
    Declare Function d4tagPrev Lib "d4dll.dll" (ByVal d4 As Integer, ByVal TagOn As Integer) As Integer
    Declare Sub d4tagSelect Lib "d4dll.dll" (ByVal d4 As Integer, ByVal tPtr As Integer)
    Declare Function d4tagSelected Lib "d4dll.dll" (ByVal d4 As Integer) As Integer
    Declare Function d4tagSync Lib "d4dll.dll" (ByVal d4 As Integer, ByVal tPtr As Integer) As Short
    Declare Function d4top Lib "d4dll.dll" (ByVal d4 As Integer) As Short
    Declare Function d4unlock Lib "d4dll.dll" (ByVal d4 As Integer) As Short
    Declare Function d4unlockFiles Lib "d4dll.dll"  Alias "code4unlock"(ByVal d4 As Integer) As Short
    Declare Function d4write Lib "d4dll.dll"  Alias "d4writeVB"(ByVal d4 As Integer, ByVal RecNum As Integer) As Short
    Declare Function d4writeBuffer Lib "d4dll.dll" (ByVal r4 As Integer, ByVal numRecs As Integer) As Integer
    Declare Function d4zap Lib "d4dll.dll" (ByVal d4 As Integer, ByVal StartRecord As Integer, ByVal EndRecord As Integer) As Short
#Else
    Declare Function d4aliasCB Lib "c4dll.dll" Alias "d4alias" (ByVal d4 As Integer) As Integer
    Declare Sub d4aliasSet Lib "c4dll.dll" (ByVal d4 As Integer, ByVal AliasValue As String)
    Declare Function d4append Lib "c4dll.dll" (ByVal d4 As Integer) As Short
    Declare Function d4appendBlank Lib "c4dll.dll" (ByVal d4 As Integer) As Short
    Declare Function d4appendStart Lib "c4dll.dll" (ByVal d4 As Integer, ByVal UseMemoEntries As Short) As Short
    Declare Sub d4blank Lib "c4dll.dll" (ByVal d4 As Integer)
    Declare Function d4bof Lib "c4dll.dll" (ByVal d4 As Integer) As Short
    Declare Function d4bottom Lib "c4dll.dll" (ByVal d4 As Integer) As Short
    Declare Function d4changed Lib "c4dll.dll" (ByVal d4 As Integer, ByVal intFlag As Short) As Short
    Declare Function d4check Lib "c4dll.dll" (ByVal d4 As Integer) As Short
    Declare Function d4close Lib "c4dll.dll" (ByVal d4 As Integer) As Short
    Declare Function d4codePage Lib "c4dll.dll" (ByVal d4 As Integer) As Short
    Declare Function d4compress Lib "c4dll.dll" (ByVal d4 As Integer, ByVal compressedName As String, ByVal blockSize As Short) As Integer
    Declare Function d4createCB Lib "c4dll.dll" Alias "d4create" (ByVal c4 As Integer, ByVal DbfName As String, ByVal fieldinfo As Integer, ByVal tagInfo As Integer) As Integer
    Declare Function d4createLow Lib "c4dll.dll" Alias "d4create" (ByVal c4 As Integer, ByVal DbfName As String, ByRef fieldinfo As FIELD4INFOCB, ByRef tagInfo As TAG4INFOCB) As Integer
    Declare Function d4createLow Lib "c4dll.dll" Alias "d4create" (ByVal c4 As Integer, ByVal DbfName As String, ByRef fieldinfo As FIELD4INFOCB, Optional ByVal tagInfo As Integer = 0) As Integer
    Declare Sub d4delete Lib "c4dll.dll" (ByVal d4 As Integer)
    Declare Function d4deleted Lib "c4dll.dll" (ByVal d4 As Integer) As Short
    Declare Function d4eof Lib "c4dll.dll" (ByVal d4 As Integer) As Short
    Declare Function d4field Lib "c4dll.dll" (ByVal d4 As Integer, ByVal FieldName As String) As Integer
    Declare Function d4fieldInfo Lib "c4dll.dll" (ByVal d4 As Integer) As Integer
    Declare Function d4fieldJ Lib "c4dll.dll" (ByVal d4 As Integer, ByVal JField As Short) As Integer
    Declare Function d4fieldNumber Lib "c4dll.dll" (ByVal d4 As Integer, ByVal FieldName As String) As Short
    Declare Function d4fieldsAddCB Lib "c4dll.dll" Alias "d4fieldsAdd" (ByVal d4 As Integer, ByVal nFields As Short, ByRef fieldinfo As FIELD4INFOCB) As Integer
    Declare Function d4fieldsRemoveCB Lib "c4dll.dll" Alias "d4fieldsRemove" (ByRef d4 As Integer, ByVal nFields As Short, ByRef fieldNames As Integer) As Integer
    Declare Function d4fileNameCB Lib "c4dll.dll" Alias "d4fileName" (ByVal d4 As Integer) As Integer
    Declare Function d4flush Lib "c4dll.dll" (ByVal d4 As Integer) As Short
    Declare Function d4freeBlocks Lib "c4dll.dll" (ByVal d4 As Integer) As Short
    Declare Function d4goLow Lib "c4dll.dll" (ByVal d4 As Integer, ByVal RecNum As Integer, ByVal goForWrite As Short) As Short
    Declare Function d4goBof Lib "c4dll.dll" (ByVal d4 As Integer) As Short
    Declare Function d4goEof Lib "c4dll.dll" (ByVal d4 As Integer) As Short
    Declare Function d4index Lib "c4dll.dll" (ByVal d4 As Integer, ByVal IndexName As String) As Integer
    Declare Function d4log Lib "c4dll.dll" Alias "d4logVB" (ByVal d4 As Integer, ByVal logging As Short) As Short
    Declare Function d4lock Lib "c4dll.dll" Alias "d4lockVB" (ByVal d4 As Integer, ByVal recordNum As Integer) As Short
    Declare Function d4lockAdd Lib "c4dll.dll" (ByVal d4 As Integer, ByVal recordNum As Integer) As Short
    Declare Function d4lockAddAll Lib "c4dll.dll" (ByVal d4 As Integer) As Short
    Declare Function d4lockAddAppend Lib "c4dll.dll" (ByVal d4 As Integer) As Short
    Declare Function d4lockAddFile Lib "c4dll.dll" (ByVal d4 As Integer) As Short
    Declare Function d4lockAll Lib "c4dll.dll" Alias "d4lockAllVB" (ByVal d4 As Integer) As Short
    Declare Function d4lockAppend Lib "c4dll.dll" Alias "d4lockAppendVB" (ByVal d4 As Integer) As Short
    Declare Function d4lockFile Lib "c4dll.dll" Alias "d4lockFileVB" (ByVal d4 As Integer) As Short
    Declare Function d4logStatus Lib "c4dll.dll" Alias "d4logStatusCB" (ByVal d4 As Integer) As Short
    Declare Function d4memoCompress Lib "c4dll.dll" (ByVal d4 As Integer) As Short
    Declare Function d4numFields Lib "c4dll.dll" (ByVal d4 As Integer) As Short
    Declare Function d4open Lib "c4dll.dll" (ByVal c4 As Integer, ByVal DbfName As String) As Integer
    Declare Function d4openClone Lib "c4dll.dll" (ByVal d4 As Integer) As Integer
    Declare Function d4optimize Lib "c4dll.dll" Alias "d4optimizeVB" (ByVal d4 As Integer, ByVal OptFlag As Short) As Short
    Declare Function d4optimizeWrite Lib "c4dll.dll" Alias "d4optimizeWriteVB" (ByVal d4 As Integer, ByVal OptFlag As Short) As Short
    Declare Function d4pack Lib "c4dll.dll" (ByVal d4 As Integer) As Short
    Declare Function d4packData Lib "c4dll.dll" (ByVal d4 As Integer) As Short
    Delegate Function d4packWithProgressDelegate(ByVal percent As Double) As Short
    Declare Function d4packWithProgress Lib "c4dll.dll" (ByVal d4 As Integer, ByVal callback As d4packWithProgressDelegate, ByVal milliseconds As Integer) As Short
    Declare Function d4position Lib "c4dll.dll" (ByVal d4 As Integer) As Double
    Declare Function d4positionSet Lib "c4dll.dll" (ByVal d4 As Integer, ByVal Percentage As Double) As Short
    Declare Function d4readBuffer Lib "c4dll.dll" (ByVal d4 As Integer, ByVal numRecsToBuf As Integer, ByVal doMemos As Short) As Integer
    Declare Sub d4recall Lib "c4dll.dll" (ByVal d4 As Integer)
    Declare Function d4recCount Lib "c4dll.dll" Alias "d4recCountDo" (ByVal d4 As Integer) As Integer
    Declare Function d4recNo Lib "c4dll.dll" Alias "d4recNoLow" (ByVal d4 As Integer) As Integer
    Declare Function d4record Lib "c4dll.dll" Alias "d4recordLow" (ByVal d4 As Integer) As Integer
    Declare Function d4recWidth Lib "c4dll.dll" Alias "d4recWidth_v" (ByVal d4 As Integer) As Integer
    Declare Function d4remove Lib "c4dll.dll" (ByVal d4 As Integer) As Short
    Declare Function d4refresh Lib "c4dll.dll" (ByVal d4 As Integer) As Short
    Declare Function d4refreshRecord Lib "c4dll.dll" (ByVal d4 As Integer) As Short
    Declare Function d4reindex Lib "c4dll.dll" (ByVal d4 As Integer) As Short
    Delegate Function d4reindexWithProgressDelegate(ByVal percent As Double) As Short
    Declare Function d4reindexWithProgress Lib "c4dll.dll" (ByVal d4 As Integer, ByVal callback As d4reindexWithProgressDelegate, ByVal milliseconds As Integer) As Short
    Declare Function d4seek Lib "c4dll.dll" (ByVal d4 As Integer, ByVal seekValue As String) As Short
    Declare Function d4seekDouble Lib "c4dll.dll" (ByVal d4 As Integer, ByVal value As Double) As Short
    Declare Function d4seekN Lib "c4dll.dll" (ByVal d4 As Integer, ByVal seekValue As String, ByVal seekLen As Short) As Short
    Declare Function d4seekNext Lib "c4dll.dll" (ByVal d4 As Integer, ByVal seekValue As String) As Short
    Declare Function d4seekNextDouble Lib "c4dll.dll" (ByVal d4 As Integer, ByVal seekValue As Double) As Short
    Declare Function d4seekNextN Lib "c4dll.dll" (ByVal d4 As Integer, ByVal seekValue As String, ByVal seekLen As Short) As Short
    Declare Function d4skip Lib "c4dll.dll" (ByVal d4 As Integer, ByVal NumberRecords As Integer) As Short
    Declare Function d4tag Lib "c4dll.dll" (ByVal d4 As Integer, ByVal TagName As String) As Integer
    Declare Function d4tagDefault Lib "c4dll.dll" (ByVal d4 As Integer) As Integer
    Declare Function d4tagNext Lib "c4dll.dll" (ByVal d4 As Integer, ByVal TagOn As Integer) As Integer
    Declare Function d4tagPrev Lib "c4dll.dll" (ByVal d4 As Integer, ByVal TagOn As Integer) As Integer
    Declare Sub d4tagSelect Lib "c4dll.dll" (ByVal d4 As Integer, ByVal tPtr As Integer)
    Declare Function d4tagSelected Lib "c4dll.dll" (ByVal d4 As Integer) As Integer
    Declare Function d4tagSync Lib "c4dll.dll" (ByVal d4 As Integer, ByVal tPtr As Integer) As Short
    Declare Function d4top Lib "c4dll.dll" (ByVal d4 As Integer) As Short
    Declare Function d4unlock Lib "c4dll.dll" (ByVal d4 As Integer) As Short
    Declare Function d4unlockFiles Lib "c4dll.dll" Alias "code4unlock" (ByVal d4 As Integer) As Short
    Declare Function d4write Lib "c4dll.dll" Alias "d4writeVB" (ByVal d4 As Integer, ByVal RecNum As Integer) As Short
    Declare Function d4writeBuffer Lib "c4dll.dll" (ByVal r4 As Integer, ByVal numRecs As Integer) As Integer
    Declare Function d4zap Lib "c4dll.dll" (ByVal d4 As Integer, ByVal StartRecord As Integer, ByVal EndRecord As Integer) As Short
#End If

    '===============================================================================================
    '
    '                                   Date Functions' Prototypes
    '
    '-----------------------------------------------------------------------------------------------
#If USE_D4DLL Then
    Declare Sub date4assignLow Lib "d4dll.dll" (ByVal dateForm As String, ByVal julianDay As Integer, ByVal isOle As Short)
    Declare Function date4cdowCB Lib "d4dll.dll"  Alias "date4cdow"(ByVal dateForm As String) As Integer
    Declare Function date4cmonthCB Lib "d4dll.dll"  Alias "date4cmonth"(ByVal dateForm As String) As Integer
    Declare Function date4day Lib "d4dll.dll"  Alias "date4day_v"(ByVal dateForm As String) As Short
    Declare Function date4dow Lib "d4dll.dll" (ByVal dateForm As String) As Short
    Declare Sub date4formatCB Lib "d4dll.dll"  Alias "date4format"(ByVal dateForm As String, ByVal Result As String, ByVal pic As String)
    Declare Sub date4initCB Lib "d4dll.dll"  Alias "date4init"(ByVal dateForm As String, ByVal value As String, ByVal pic As String)
    Declare Function date4isLeap Lib "d4dll.dll" (ByVal dateForm As String) As Short
    Declare Function date4long Lib "d4dll.dll" (ByVal dateForm As String) As Integer
    Declare Function date4month Lib "d4dll.dll"  Alias "date4month_v"(ByVal dateForm As String) As Short
    Declare Sub date4timeNow Lib "d4dll.dll" (ByVal TimeForm As String)
    Declare Sub date4todayCB Lib "d4dll.dll"  Alias "date4today"(ByVal dateForm As String)
    Declare Function date4year Lib "d4dll.dll"  Alias "date4year_v"(ByVal dateForm As String) As Short
#Else
    Declare Sub date4assignLow Lib "c4dll.dll" (ByVal dateForm As String, ByVal julianDay As Integer, ByVal isOle As Short)
    Declare Function date4cdowCB Lib "c4dll.dll" Alias "date4cdow" (ByVal dateForm As String) As Integer
    Declare Function date4cmonthCB Lib "c4dll.dll" Alias "date4cmonth" (ByVal dateForm As String) As Integer
    Declare Function date4day Lib "c4dll.dll" Alias "date4day_v" (ByVal dateForm As String) As Short
    Declare Function date4dow Lib "c4dll.dll" (ByVal dateForm As String) As Short
    Declare Sub date4formatCB Lib "c4dll.dll" Alias "date4format" (ByVal dateForm As String, ByVal Result As String, ByVal pic As String)
    Declare Sub date4initCB Lib "c4dll.dll" Alias "date4init" (ByVal dateForm As String, ByVal value As String, ByVal pic As String)
    Declare Function date4isLeap Lib "c4dll.dll" (ByVal dateForm As String) As Short
    Declare Function date4long Lib "c4dll.dll" (ByVal dateForm As String) As Integer
    Declare Function date4month Lib "c4dll.dll" Alias "date4month_v" (ByVal dateForm As String) As Short
    Declare Sub date4timeNow Lib "c4dll.dll" (ByVal TimeForm As String)
    Declare Sub date4todayCB Lib "c4dll.dll" Alias "date4today" (ByVal dateForm As String)
    Declare Function date4year Lib "c4dll.dll" Alias "date4year_v" (ByVal dateForm As String) As Short
#End If

    '===============================================================================================
    '
    '                          Error  Functions' Prototypes
    '
    '-----------------------------------------------------------------------------------------------
#If USE_D4DLL Then
    Declare Function error4 Lib "d4dll.dll"  Alias "error4VB"(ByVal c4 As Integer, ByVal errCode As Short, ByVal extraInfo As Integer) As Short
    Declare Sub error4exitTest Lib "d4dll.dll" (ByVal c4 As Integer)
    Declare Function error4describe Lib "d4dll.dll"  Alias "error4describeVB"(ByVal c4 As Integer, ByVal errCode As Short, ByVal extraInfo As Integer, ByVal desc1 As String, ByVal desc2 As String, ByVal desc3 As String) As Short
    Declare Function error4file Lib "d4dll.dll" (ByVal c4 As Integer, ByVal fileName As String, ByVal overwrite As Short) As Short
    Declare Function error4lastDescription Lib "d4dll.dll" (ByVal c4 As Integer) As String
    Declare Function error4set Lib "d4dll.dll" (ByVal c4 As Integer, ByVal errCode As Short) As Short
    Declare Function error4textCB Lib "d4dll.dll"  Alias "error4text"(ByVal c4 As Integer, ByVal errCode As Integer) As Integer
#Else
    Declare Function error4 Lib "c4dll.dll" Alias "error4VB" (ByVal c4 As Integer, ByVal errCode As Short, ByVal extraInfo As Integer) As Short
    Declare Sub error4exitTest Lib "c4dll.dll" (ByVal c4 As Integer)
    Declare Function error4describe Lib "c4dll.dll" Alias "error4describeVB" (ByVal c4 As Integer, ByVal errCode As Short, ByVal extraInfo As Integer, ByVal desc1 As String, ByVal desc2 As String, ByVal desc3 As String) As Short
    Declare Function error4file Lib "c4dll.dll" (ByVal c4 As Integer, ByVal fileName As String, ByVal overwrite As Short) As Short
    Declare Function error4lastDescription Lib "c4dll.dll" (ByVal c4 As Integer) As String
    Declare Function error4set Lib "c4dll.dll" (ByVal c4 As Integer, ByVal errCode As Short) As Short
    Declare Function error4textCB Lib "c4dll.dll" Alias "error4text" (ByVal c4 As Integer, ByVal errCode As Integer) As Integer
#End If

    '===============================================================================================
    '
    '                          Expression Evaluation Functions' Prototypes
    '
    '-----------------------------------------------------------------------------------------------
#If USE_D4DLL Then
    Declare Function expr4data Lib "d4dll.dll"  Alias "expr4dataCB"(ByVal exprPtr As Integer) As Integer
    Declare Function expr4double Lib "d4dll.dll" (ByVal exprPtr As Integer) As Double
    Declare Sub expr4free Lib "d4dll.dll"  Alias "expr4freeCB"(ByVal exprPtr As Integer)
    Declare Function expr4len Lib "d4dll.dll"  Alias "expr4lenCB"(ByVal exprPtr As Integer) As Integer
    Declare Function expr4nullLow Lib "d4dll.dll" (ByVal exprPtr As Integer, ByVal forAdd As Short) As Short
    Declare Function expr4parse Lib "d4dll.dll"  Alias "expr4parseCB"(ByVal d4 As Integer, ByVal expression As String) As Integer
    Declare Function expr4sourceCB Lib "d4dll.dll"  Alias "expr4source"(ByVal exprPtr As Integer) As Integer
    Declare Function expr4strCB Lib "d4dll.dll"  Alias "expr4str"(ByVal exprPtr As Integer) As Integer
    Declare Function expr4true Lib "d4dll.dll" (ByVal exprPtr As Integer) As Short
    Declare Function expr4typeCB Lib "d4dll.dll" (ByVal exprPtr As Integer) As Short
#Else
    Declare Function expr4data Lib "c4dll.dll" Alias "expr4dataCB" (ByVal exprPtr As Integer) As Integer
    Declare Function expr4double Lib "c4dll.dll" (ByVal exprPtr As Integer) As Double
    Declare Sub expr4free Lib "c4dll.dll" Alias "expr4freeCB" (ByVal exprPtr As Integer)
    Declare Function expr4len Lib "c4dll.dll" Alias "expr4lenCB" (ByVal exprPtr As Integer) As Integer
    Declare Function expr4nullLow Lib "c4dll.dll" (ByVal exprPtr As Integer, ByVal forAdd As Short) As Short
    Declare Function expr4parse Lib "c4dll.dll" Alias "expr4parseCB" (ByVal d4 As Integer, ByVal expression As String) As Integer
    Declare Function expr4sourceCB Lib "c4dll.dll" Alias "expr4source" (ByVal exprPtr As Integer) As Integer
    Declare Function expr4strCB Lib "c4dll.dll" Alias "expr4str" (ByVal exprPtr As Integer) As Integer
    Declare Function expr4true Lib "c4dll.dll" (ByVal exprPtr As Integer) As Short
    Declare Function expr4typeCB Lib "c4dll.dll" (ByVal exprPtr As Integer) As Short
#End If

    '===============================================================================================
    '
    '                            Field Functions' Prototypes
    '
    '-----------------------------------------------------------------------------------------------
#If USE_D4DLL Then
    Declare Sub f4assignBinaryVB Lib "d4dll.dll"  Alias "f4assignN"(ByVal fPtr As Integer, ByRef value As Byte, ByVal length As Short)
    Declare Sub f4assignChar Lib "d4dll.dll"  Alias "f4assignCharVB"(ByVal fPtr As Integer, ByVal Char_Renamed As Short)
    Declare Sub f4assignCurrency Lib "d4dll.dll" (ByVal fPtr As Integer, ByVal value As String)
    Declare Sub f4assignDateTime Lib "d4dll.dll" (ByVal fPtr As Integer, ByVal value As String)
    Declare Sub f4assignDouble Lib "d4dll.dll" (ByVal fPtr As Integer, ByVal value As Double)
    Declare Sub f4assignField Lib "d4dll.dll" (ByVal fPtrTo As Integer, ByVal fPtrFrom As Integer)
    Declare Sub f4assignIntVB Lib "d4dll.dll" (ByVal fPtr As Integer, ByVal value As Short)
    Declare Sub f4assignLong Lib "d4dll.dll" (ByVal fPtr As Integer, ByVal value As Integer)
    Declare Sub f4assignN Lib "d4dll.dll"  Alias "f4assignNVB"(ByVal fPtr As Integer, ByVal value As String, ByVal length As Short)
    Declare Sub f4assignNull Lib "d4dll.dll" (ByVal fPtr As Integer)
    Declare Sub f4assignUnicodeVB Lib "d4dll.dll" Alias "f4assignUnicode" (ByVal fPtr As Integer, ByRef value As Byte)
    Declare Sub f4blank Lib "d4dll.dll" (ByVal fPtr As Integer)
    Declare Function f4char Lib "d4dll.dll" (ByVal fPtr As Integer) As Short
    Declare Function f4currencyCB Lib "d4dll.dll"  Alias "f4currency"(ByVal fPtr As Integer, ByVal numDec As Short) As Integer
    Declare Function f4data Lib "d4dll.dll" (ByVal fPtr As Integer) As Integer
    Declare Function f4dateTimeCB Lib "d4dll.dll"  Alias "f4dateTime"(ByVal fPtr As Integer) As Integer
    Declare Function f4decimals Lib "d4dll.dll" (ByVal fPtr As Integer) As Short
    Declare Function f4double Lib "d4dll.dll" (ByVal fPtr As Integer) As Double
    Declare Function f4int Lib "d4dll.dll" (ByVal fPtr As Integer) As Short
    Declare Function f4len Lib "d4dll.dll"  Alias "f4len_v"(ByVal fPtr As Integer) As Short
    Declare Function f4long Lib "d4dll.dll" (ByVal fPtr As Integer) As Integer
    Declare Function f4memoAssign Lib "d4dll.dll" (ByVal fPtr As Integer, ByVal value As String) As Short
    Declare Function f4memoAssignBinaryVB Lib "d4dll.dll" Alias "f4memoAssignNVB" (ByVal fPtr As Integer, ByRef value As Byte, ByVal length As Short) As Short
    Declare Function f4memoAssignN Lib "d4dll.dll" (ByVal fPtr As Integer, ByVal value As String, ByVal length As Integer) As Short
    Declare Sub f4memoAssignUnicodeVB Lib "d4dll.dll" Alias "f4memoAssignUnicode" (ByVal fPtr As Integer, ByRef value As String)
    Declare Sub f4memoFree Lib "d4dll.dll" (ByVal fPtr As Integer)
    Declare Function f4memoLen Lib "d4dll.dll" (ByVal fPtr As Integer) As Integer
    Declare Function f4memoNcpy Lib "d4dll.dll" (ByVal fPtr As Integer, ByVal memPtr As String, ByVal memLen As Short) As Short
    Declare Function f4memoNcpyBinary Lib "d4dll.dll" Alias "f4memoNcpy" (ByVal fPtr As Integer, ByRef memPtr As Byte, ByVal memLen As Short) As Short
    Declare Function f4memoPtr Lib "d4dll.dll" (ByVal fPtr As Integer) As Integer
    Declare Function f4nameCB Lib "d4dll.dll"  Alias "f4name"(ByVal fPtr As Integer) As Integer
    Declare Function f4ncpyBinary Lib "d4dll.dll" Alias "f4ncpy" (ByVal fPtr As Integer, ByRef memPtr As Byte, ByVal memLength As Short) As Short
    Declare Function f4ncpyCB Lib "d4dll.dll"  Alias "f4ncpy"(ByVal fPtr As Integer, ByVal memPtr As String, ByVal memLength As Short) As Short
    Declare Function f4number Lib "d4dll.dll" (ByVal fPtr As Integer) As Short
    Declare Function f4null Lib "d4dll.dll" (ByVal fPtr As Integer) As Short
    Declare Function f4ptr Lib "d4dll.dll" (ByVal fPtr As Integer) As Integer
    Declare Function f4strCB Lib "d4dll.dll"  Alias "f4str"(ByVal fPtr As Integer) As Integer
    Declare Function f4true Lib "d4dll.dll" (ByVal fPtr As Integer) As Short
    Declare Function f4type Lib "d4dll.dll" (ByVal fPtr As Integer) As Short
#Else
    Declare Sub f4assignBinaryVB Lib "c4dll.dll" Alias "f4assignN" (ByVal fPtr As Integer, ByRef value As Byte, ByVal length As Short)
    Declare Sub f4assignChar Lib "c4dll.dll" Alias "f4assignCharVB" (ByVal fPtr As Integer, ByVal Char_Renamed As Short)
    Declare Sub f4assignCurrency Lib "c4dll.dll" (ByVal fPtr As Integer, ByVal value As String)
    Declare Sub f4assignDateTime Lib "c4dll.dll" (ByVal fPtr As Integer, ByVal value As String)
    Declare Sub f4assignDouble Lib "c4dll.dll" (ByVal fPtr As Integer, ByVal value As Double)
    Declare Sub f4assignField Lib "c4dll.dll" (ByVal fPtrTo As Integer, ByVal fPtrFrom As Integer)
    Declare Sub f4assignIntVB Lib "c4dll.dll" (ByVal fPtr As Integer, ByVal value As Short)
    Declare Sub f4assignLong Lib "c4dll.dll" (ByVal fPtr As Integer, ByVal value As Integer)
    Declare Sub f4assignN Lib "c4dll.dll" Alias "f4assignNVB" (ByVal fPtr As Integer, ByVal value As String, ByVal length As Short)
    Declare Sub f4assignNull Lib "c4dll.dll" (ByVal fPtr As Integer)
    Declare Sub f4assignUnicodeVB Lib "c4dll.dll" Alias "f4assignUnicode" (ByVal fPtr As Integer, ByRef value As Byte)
    Declare Sub f4blank Lib "c4dll.dll" (ByVal fPtr As Integer)
    Declare Function f4char Lib "c4dll.dll" (ByVal fPtr As Integer) As Short
    Declare Function f4currencyCB Lib "c4dll.dll" Alias "f4currency" (ByVal fPtr As Integer, ByVal numDec As Short) As Integer
    Declare Function f4data Lib "c4dll.dll" (ByVal fPtr As Integer) As Integer
    Declare Function f4dateTimeCB Lib "c4dll.dll" Alias "f4dateTime" (ByVal fPtr As Integer) As Integer
    Declare Function f4decimals Lib "c4dll.dll" (ByVal fPtr As Integer) As Short
    Declare Function f4double Lib "c4dll.dll" (ByVal fPtr As Integer) As Double
    Declare Function f4int Lib "c4dll.dll" (ByVal fPtr As Integer) As Short
    Declare Function f4len Lib "c4dll.dll" Alias "f4len_v" (ByVal fPtr As Integer) As Short
    Declare Function f4long Lib "c4dll.dll" (ByVal fPtr As Integer) As Integer
    Declare Function f4memoAssign Lib "c4dll.dll" (ByVal fPtr As Integer, ByVal value As String) As Short
    Declare Function f4memoAssignBinaryVB Lib "c4dll.dll" Alias "f4memoAssignNVB" (ByVal fPtr As Integer, ByRef value As Byte, ByVal length As Short) As Short
    Declare Function f4memoAssignN Lib "c4dll.dll" (ByVal fPtr As Integer, ByVal value As String, ByVal length As Integer) As Short
    Declare Sub f4memoAssignUnicodeVB Lib "c4dll.dll" Alias "f4memoAssignUnicode" (ByVal fPtr As Integer, ByRef value As String)
    Declare Sub f4memoFree Lib "c4dll.dll" (ByVal fPtr As Integer)
    Declare Function f4memoLen Lib "c4dll.dll" (ByVal fPtr As Integer) As Integer
    Declare Function f4memoNcpy Lib "c4dll.dll" (ByVal fPtr As Integer, ByVal memPtr As String, ByVal memLen As Short) As Short
    Declare Function f4memoNcpyBinary Lib "c4dll.dll" Alias "f4memoNcpy" (ByVal fPtr As Integer, ByRef memPtr As Byte, ByVal memLen As Short) As Short
    Declare Function f4memoPtr Lib "c4dll.dll" (ByVal fPtr As Integer) As Integer
    Declare Function f4nameCB Lib "c4dll.dll" Alias "f4name" (ByVal fPtr As Integer) As Integer
    Declare Function f4ncpyBinary Lib "c4dll.dll" Alias "f4ncpy" (ByVal fPtr As Integer, ByRef memPtr As Byte, ByVal memLength As Short) As Short
    Declare Function f4ncpyCB Lib "c4dll.dll" Alias "f4ncpy" (ByVal fPtr As Integer, ByVal memPtr As String, ByVal memLength As Short) As Short
    Declare Function f4number Lib "c4dll.dll" (ByVal fPtr As Integer) As Short
    Declare Function f4null Lib "c4dll.dll" (ByVal fPtr As Integer) As Short
    Declare Function f4ptr Lib "c4dll.dll" (ByVal fPtr As Integer) As Integer
    Declare Function f4strCB Lib "c4dll.dll" Alias "f4str" (ByVal fPtr As Integer) As Integer
    Declare Function f4true Lib "c4dll.dll" (ByVal fPtr As Integer) As Short
    Declare Function f4type Lib "c4dll.dll" (ByVal fPtr As Integer) As Short
#End If

    '===============================================================================================
    '
    '                             Index Functions' Prototypes
    '
    '-----------------------------------------------------------------------------------------------
#If USE_D4DLL Then
    Declare Function i4close Lib "d4dll.dll" (ByVal i4 As Integer) As Short
    Declare Function i4createCB Lib "d4dll.dll"  Alias "i4create"(ByVal d4 As Integer, ByVal fileName As String, ByRef tagInfo As TAG4INFOCB) As Integer
    Declare Function i4fileNameCB Lib "d4dll.dll"  Alias "i4fileName"(ByVal i4 As Integer) As Integer
    Declare Function i4openCB Lib "d4dll.dll" Alias "i4open" (ByVal d4 As Integer, ByVal fileName As String) As Integer
    Declare Function i4reindex Lib "d4dll.dll" (ByVal i4 As Integer) As Short
    Declare Function i4tag Lib "d4dll.dll" (ByVal i4 As Integer, ByVal fileName As String) As Integer
    Declare Function i4tagInfo Lib "d4dll.dll" (ByVal i4 As Integer) As Integer
    Declare Function i4tagAddCB Lib "d4dll.dll"  Alias "i4tagAdd"(ByVal i4 As Integer, ByRef tagInfo As TAG4INFOCB) As Short
    Declare Function i4tagRemove Lib "d4dll.dll" (ByVal t4 As Integer) As Short
#Else
    Declare Function i4close Lib "c4dll.dll" (ByVal i4 As Integer) As Short
    Declare Function i4createCB Lib "c4dll.dll" Alias "i4create" (ByVal d4 As Integer, ByVal fileName As String, ByRef tagInfo As TAG4INFOCB) As Integer
    Declare Function i4fileNameCB Lib "c4dll.dll" Alias "i4fileName" (ByVal i4 As Integer) As Integer
    Declare Function i4openCB Lib "c4dll.dll" Alias "i4open" (ByVal d4 As Integer, ByVal fileName As String) As Integer
    Declare Function i4reindex Lib "c4dll.dll" (ByVal i4 As Integer) As Short
    Declare Function i4tag Lib "c4dll.dll" (ByVal i4 As Integer, ByVal fileName As String) As Integer
    Declare Function i4tagInfo Lib "c4dll.dll" (ByVal i4 As Integer) As Integer
    Declare Function i4tagAddCB Lib "c4dll.dll" Alias "i4tagAdd" (ByVal i4 As Integer, ByRef tagInfo As TAG4INFOCB) As Short
    Declare Function i4tagRemove Lib "c4dll.dll" (ByVal t4 As Integer) As Short
#End If

    '===============================================================================================
    '
    '                            Relate Functions' Prototypes
    '
    '-----------------------------------------------------------------------------------------------
#If USE_D4DLL Then
    Declare Function relate4bottom Lib "d4dll.dll" (ByVal r4 As Integer) As Short
    Declare Sub relate4changed Lib "d4dll.dll" (ByVal r4 As Integer)
    Declare Function relate4createSlave Lib "d4dll.dll" (ByVal r4 As Integer, ByVal d4 As Integer, ByVal mExpr As String, ByVal t4 As Integer) As Integer
    Declare Function relate4count Lib "d4dll.dll" (ByVal r4 As Integer) As Integer
    Declare Function relate4data Lib "d4dll.dll"  Alias "relate4dataCB"(ByVal r4 As Integer) As Integer
    Declare Function relate4dataTag Lib "d4dll.dll"  Alias "relate4dataTagCB"(ByVal r4 As Integer) As Integer
    Declare Function relate4doAll Lib "d4dll.dll" (ByVal r4 As Integer) As Short
    Declare Function relate4doOne Lib "d4dll.dll" (ByVal r4 As Integer) As Short
    Declare Function relate4eof Lib "d4dll.dll" (ByVal r4 As Integer) As Short
    Declare Function relate4errorAction Lib "d4dll.dll"  Alias "relate4errorActionVB"(ByVal r4 As Integer, ByVal ErrAction As Short) As Short
    Declare Function relate4free Lib "d4dll.dll"  Alias "relate4freeVB"(ByVal r4 As Integer, ByVal CloseFlag As Short) As Short
    Declare Function relate4init Lib "d4dll.dll" (ByVal d4 As Integer) As Integer
    Declare Function relate4lockAdd Lib "d4dll.dll" (ByVal r4 As Integer) As Short
    Declare Function relate4master Lib "d4dll.dll"  Alias "relate4masterCB"(ByVal r4 As Integer) As Integer
    Declare Function relate4masterExprCB Lib "d4dll.dll" (ByVal r4 As Integer) As Integer
    Declare Function relate4matchLen Lib "d4dll.dll"  Alias "relate4matchLenVB"(ByVal r4 As Integer, ByVal length As Short) As Short
    Declare Function relate4next Lib "d4dll.dll" (ByRef r4 As Integer) As Short
    Declare Function relate4optimizeable Lib "d4dll.dll" (ByVal r4 As Integer) As Short
    Declare Function relate4querySet Lib "d4dll.dll" (ByVal r4 As Integer, ByVal expr As String) As Short
    Declare Function relate4readBuffer Lib "d4dll.dll" (ByVal r4 As Integer, ByVal numRecsToBuf As Integer, ByVal doMemos As Short) As Integer
    Declare Function relate4retain Lib "d4dll.dll" (ByVal r4 As Integer, ByVal value As Short) As Short
    Declare Function relate4retrieve Lib "d4dll.dll" (ByVal c4 As Integer, ByVal fileName As String, ByVal openFiles As Short, ByVal dataPathName As String) As Integer
    Declare Function relate4save Lib "d4dll.dll" (ByVal rel4 As Integer, ByVal fileName As String, ByVal savePathNames As Short) As Short
    Declare Function relate4skip Lib "d4dll.dll" (ByVal r4 As Integer, ByVal NumRecs As Integer) As Short
    Declare Function relate4skipEnable Lib "d4dll.dll" Alias "relate4skipEnableVB" (ByVal r4 As Integer, ByVal DoEnable As Short) As Short
    Declare Function relate4skipMaster Lib "d4dll.dll" (ByVal r4 As Integer, ByVal numSkip As Integer) As Short
    Declare Function relate4sortSet Lib "d4dll.dll" (ByVal r4 As Integer, ByVal expr As String) As Short
    Declare Function relate4top Lib "d4dll.dll" (ByVal r4 As Integer) As Short
    Declare Function relate4topMaster Lib "d4dll.dll" (ByVal r4 As Integer) As Integer
    Declare Function relate4type Lib "d4dll.dll" Alias "relate4typeVB" (ByVal r4 As Integer, ByVal rType As Short) As Short
#Else
    Declare Function relate4bottom Lib "c4dll.dll" (ByVal r4 As Integer) As Short
    Declare Sub relate4changed Lib "c4dll.dll" (ByVal r4 As Integer)
    Declare Function relate4createSlave Lib "c4dll.dll" (ByVal r4 As Integer, ByVal d4 As Integer, ByVal mExpr As String, ByVal t4 As Integer) As Integer
    Declare Function relate4count Lib "c4dll.dll" (ByVal r4 As Integer) As Integer
    Declare Function relate4data Lib "c4dll.dll" Alias "relate4dataCB" (ByVal r4 As Integer) As Integer
    Declare Function relate4dataTag Lib "c4dll.dll" Alias "relate4dataTagCB" (ByVal r4 As Integer) As Integer
    Declare Function relate4doAll Lib "c4dll.dll" (ByVal r4 As Integer) As Short
    Declare Function relate4doOne Lib "c4dll.dll" (ByVal r4 As Integer) As Short
    Declare Function relate4eof Lib "c4dll.dll" (ByVal r4 As Integer) As Short
    Declare Function relate4errorAction Lib "c4dll.dll" Alias "relate4errorActionVB" (ByVal r4 As Integer, ByVal ErrAction As Short) As Short
    Declare Function relate4free Lib "c4dll.dll" Alias "relate4freeVB" (ByVal r4 As Integer, ByVal CloseFlag As Short) As Short
    Declare Function relate4init Lib "c4dll.dll" (ByVal d4 As Integer) As Integer
    Declare Function relate4lockAdd Lib "c4dll.dll" (ByVal r4 As Integer) As Short
    Declare Function relate4master Lib "c4dll.dll" Alias "relate4masterCB" (ByVal r4 As Integer) As Integer
    Declare Function relate4masterExprCB Lib "c4dll.dll" (ByVal r4 As Integer) As Integer
    Declare Function relate4matchLen Lib "c4dll.dll" Alias "relate4matchLenVB" (ByVal r4 As Integer, ByVal length As Short) As Short
    Declare Function relate4next Lib "c4dll.dll" (ByRef r4 As Integer) As Short
    Declare Function relate4optimizeable Lib "c4dll.dll" (ByVal r4 As Integer) As Short
    Declare Function relate4querySet Lib "c4dll.dll" (ByVal r4 As Integer, ByVal expr As String) As Short
    Declare Function relate4readBuffer Lib "c4dll.dll" (ByVal r4 As Integer, ByVal numRecsToBuf As Integer, ByVal doMemos As Short) As Integer
    Declare Function relate4retain Lib "c4dll.dll" (ByVal r4 As Integer, ByVal value As Short) As Short
    Declare Function relate4retrieve Lib "c4dll.dll" (ByVal c4 As Integer, ByVal fileName As String, ByVal openFiles As Short, ByVal dataPathName As String) As Integer
    Declare Function relate4save Lib "c4dll.dll" (ByVal rel4 As Integer, ByVal fileName As String, ByVal savePathNames As Short) As Short
    Declare Function relate4skip Lib "c4dll.dll" (ByVal r4 As Integer, ByVal NumRecs As Integer) As Short
    Declare Function relate4skipEnable Lib "c4dll.dll" Alias "relate4skipEnableVB" (ByVal r4 As Integer, ByVal DoEnable As Short) As Short
    Declare Function relate4skipMaster Lib "c4dll.dll" (ByVal r4 As Integer, ByVal numSkip As Integer) As Short
    Declare Function relate4sortSet Lib "c4dll.dll" (ByVal r4 As Integer, ByVal expr As String) As Short
    Declare Function relate4top Lib "c4dll.dll" (ByVal r4 As Integer) As Short
    Declare Function relate4topMaster Lib "c4dll.dll" (ByVal r4 As Integer) As Integer
    Declare Function relate4type Lib "c4dll.dll" Alias "relate4typeVB" (ByVal r4 As Integer, ByVal rType As Short) As Short
#End If

    '===============================================================================================
    '
    '  Report function prototypes
    '
    '================================================================================================
#If USE_D4DLL Then
    Declare Function report4caption Lib "d4dll.dll" (ByVal r4 As Integer, ByVal caption As String) As Short
    Declare Function report4currency Lib "d4dll.dll" (ByVal r4 As Integer, ByVal currncy As String) As Short
    Declare Function report4dateFormat Lib "d4dll.dll" (ByVal r4 As Integer, ByVal dateFmt As String) As Short
    Declare Function report4decimal Lib "d4dll.dll"  Alias "report4decimal_v"(ByVal r4 As Integer, ByVal decChar As String) As Short
    Declare Function report4do Lib "d4dll.dll"  Alias "report4doCB"(ByVal r4 As Integer) As Short
    Declare Sub report4freeLow Lib "d4dll.dll" (ByVal r4 As Integer, ByVal freeRelate As Short, ByVal closeFiles As Short, ByVal doPrinterFree As Short)
    Declare Function report4margins Lib "d4dll.dll" (ByVal r4 As Integer, ByVal mLeft As Integer, ByVal mRight As Integer, ByVal mTop As Integer, ByVal mBottom As Integer, ByVal uType As Short) As Short
    Declare Function report4pageSize Lib "d4dll.dll" (ByVal r4 As Integer, ByVal pHeight As Integer, ByVal pWidth As Integer, ByVal uType As Short) As Short
    Declare Function report4parent32 Lib "d4dll.dll" Alias "report4parent" (ByVal r4 As Integer, ByVal hWnd As IntPtr) As Short
    Declare Sub report4printerSelect Lib "d4dll.dll" (ByVal r4 As Integer)
    Declare Function report4querySet Lib "d4dll.dll" (ByVal r4 As Integer, ByVal queryExpr As String) As Short
    Declare Function report4relate Lib "d4dll.dll" (ByVal r4 As Integer) As Integer
    Declare Function report4retrieve Lib "d4dll.dll" (ByVal c4 As Integer, ByVal fileName As String, ByVal openFiles As Short, ByVal dataPath As String) As Integer
    Declare Function report4save Lib "d4dll.dll" (ByVal r4 As Integer, ByVal fileName As String, ByVal savePaths As Short) As Short
    Declare Function report4screenBreaks Lib "d4dll.dll" (ByVal r4 As Integer, ByVal value As Short) As Short
    Declare Function report4separator Lib "d4dll.dll"  Alias "report4separator_v"(ByVal r4 As Integer, ByVal separator As String) As Short
    Declare Function report4sortSet Lib "d4dll.dll" (ByVal r4 As Integer, ByVal sortExpr As String) As Short
    Declare Function report4toScreen Lib "d4dll.dll" (ByVal r4 As Integer, ByVal toScreen As Short) As Short
#Else
    Declare Function report4caption Lib "c4dll.dll" (ByVal r4 As Integer, ByVal caption As String) As Short
    Declare Function report4currency Lib "c4dll.dll" (ByVal r4 As Integer, ByVal currncy As String) As Short
    Declare Function report4dateFormat Lib "c4dll.dll" (ByVal r4 As Integer, ByVal dateFmt As String) As Short
    Declare Function report4decimal Lib "c4dll.dll" Alias "report4decimal_v" (ByVal r4 As Integer, ByVal decChar As String) As Short
    Declare Function report4do Lib "c4dll.dll" Alias "report4doCB" (ByVal r4 As Integer) As Short
    Declare Sub report4freeLow Lib "c4dll.dll" (ByVal r4 As Integer, ByVal freeRelate As Short, ByVal closeFiles As Short, ByVal doPrinterFree As Short)
    Declare Function report4margins Lib "c4dll.dll" (ByVal r4 As Integer, ByVal mLeft As Integer, ByVal mRight As Integer, ByVal mTop As Integer, ByVal mBottom As Integer, ByVal uType As Short) As Short
    Declare Function report4pageSize Lib "c4dll.dll" (ByVal r4 As Integer, ByVal pHeight As Integer, ByVal pWidth As Integer, ByVal uType As Short) As Short
    Declare Function report4parent32 Lib "c4dll.dll" Alias "report4parent" (ByVal r4 As Integer, ByVal hWnd As IntPtr) As Short
    Declare Sub report4printerSelect Lib "c4dll.dll" (ByVal r4 As Integer)
    Declare Function report4querySet Lib "c4dll.dll" (ByVal r4 As Integer, ByVal queryExpr As String) As Short
    Declare Function report4relate Lib "c4dll.dll" (ByVal r4 As Integer) As Integer
    Declare Function report4retrieve Lib "c4dll.dll" (ByVal c4 As Integer, ByVal fileName As String, ByVal openFiles As Short, ByVal dataPath As String) As Integer
    Declare Function report4save Lib "c4dll.dll" (ByVal r4 As Integer, ByVal fileName As String, ByVal savePaths As Short) As Short
    Declare Function report4screenBreaks Lib "c4dll.dll" (ByVal r4 As Integer, ByVal value As Short) As Short
    Declare Function report4separator Lib "c4dll.dll" Alias "report4separator_v" (ByVal r4 As Integer, ByVal separator As String) As Short
    Declare Function report4sortSet Lib "c4dll.dll" (ByVal r4 As Integer, ByVal sortExpr As String) As Short
    Declare Function report4toScreen Lib "c4dll.dll" (ByVal r4 As Integer, ByVal toScreen As Short) As Short
#End If

    '===============================================================================================
    '
    '                            Tag Functions' Prototypes
    '
    '-----------------------------------------------------------------------------------------------
#If USE_D4DLL Then
    Declare Function t4aliasCB Lib "d4dll.dll"  Alias "t4alias"(ByVal t4 As Integer) As Integer
    Declare Function t4close Lib "d4dll.dll" (ByVal t4 As Integer) As Short
    Declare Function t4descending Lib "d4dll.dll"  Alias "tfile4isDescending"(ByVal t4 As Integer) As Short
    Declare Function t4exprCB Lib "d4dll.dll" (ByVal t4 As Integer) As Integer
    Declare Function t4filterCB Lib "d4dll.dll" (ByVal t4 As Integer) As Integer
    Declare Function t4open Lib "d4dll.dll" Alias "t4openCB" (ByVal dbPtr As Integer, ByVal IndexName As String) As Integer
    Declare Function t4seekN Lib "d4dll.dll" (ByVal t4 As Integer, ByVal seekValue As String, ByVal inputKeyLen As Short, ByVal doDataPosition As Short) As Short
    Declare Function t4unique Lib "d4dll.dll" (ByVal t4 As Integer) As Short
    Declare Function t4uniqueSet Lib "d4dll.dll"  Alias "t4uniqueSetVB"(ByVal t4 As Integer, ByVal value As Short) As Short
#Else
    Declare Function t4aliasCB Lib "c4dll.dll" Alias "t4alias" (ByVal t4 As Integer) As Integer
    Declare Function t4close Lib "c4dll.dll" (ByVal t4 As Integer) As Short
    Declare Function t4descending Lib "c4dll.dll" Alias "tfile4isDescending" (ByVal t4 As Integer) As Short
    Declare Function t4exprCB Lib "c4dll.dll" (ByVal t4 As Integer) As Integer
    Declare Function t4filterCB Lib "c4dll.dll" (ByVal t4 As Integer) As Integer
    Declare Function t4open Lib "c4dll.dll" Alias "t4openCB" (ByVal dbPtr As Integer, ByVal IndexName As String) As Integer
    Declare Function t4seekN Lib "c4dll.dll" (ByVal t4 As Integer, ByVal seekValue As String, ByVal inputKeyLen As Short, ByVal doDataPosition As Short) As Short
    Declare Function t4unique Lib "c4dll.dll" (ByVal t4 As Integer) As Short
    Declare Function t4uniqueSet Lib "c4dll.dll" Alias "t4uniqueSetVB" (ByVal t4 As Integer, ByVal value As Short) As Short
#End If

    '=======================================================================================
    '
    '                Utility function prototypes
    '
    '-----------------------------------------------------------------------------------------------
#If USE_D4DLL Then
    Declare Function u4alloc Lib "d4dll.dll"  Alias "u4allocDefault"(ByVal amt As Integer) As Integer
    Declare Function u4allocFree Lib "d4dll.dll"  Alias "u4allocFreeDefault"(ByVal c4 As Integer, ByVal amt As Integer) As Integer
    Declare Sub u4free Lib "d4dll.dll"  Alias "u4freeDefault"(ByVal memPtr As Integer)

    '16-Bit versions
    Declare Function u4ncpy Lib "d4dll.dll" (ByVal MemPtr1 As String, ByVal memptr2 As Integer, ByVal memLength As Short) As Short
    Declare Function u4ncpy2 Lib "d4dll.dll"  Alias "u4ncpy"(ByVal MemPtr1 As Integer, ByVal memptr2 As String, ByVal memLength As Short) As Short

    '32-Bit versions
    'Declare Function u4ncpy& Lib "d4dll.dll" (ByVal MemPtr1$, ByVal memptr2&, ByVal memLength&)
    'Declare Function u4ncpy2& Lib "d4dll.dll" Alias "u4ncpy" (ByVal MemPtr1&, ByVal memptr2$, ByVal memLength&)

    Declare Sub u4memCpy Lib "d4dll.dll" (ByVal dest As String, ByVal source As Integer, ByVal numCopy As Integer)
    Declare Function u4switch Lib "d4dll.dll" () As Integer
#Else
    Declare Function u4alloc Lib "c4dll.dll" Alias "u4allocDefault" (ByVal amt As Integer) As Integer
    Declare Function u4allocFree Lib "c4dll.dll" Alias "u4allocFreeDefault" (ByVal c4 As Integer, ByVal amt As Integer) As Integer
    Declare Sub u4free Lib "c4dll.dll" Alias "u4freeDefault" (ByVal memPtr As Integer)

    '16-Bit versions
    Declare Function u4ncpy Lib "c4dll.dll" (ByVal MemPtr1 As String, ByVal memptr2 As Integer, ByVal memLength As Short) As Short
    Declare Function u4ncpy2 Lib "c4dll.dll" Alias "u4ncpy" (ByVal MemPtr1 As Integer, ByVal memptr2 As String, ByVal memLength As Short) As Short

    '32-Bit versions
    'Declare Function u4ncpy& Lib "c4dll.dll" (ByVal MemPtr1$, ByVal memptr2&, ByVal memLength&)
    'Declare Function u4ncpy2& Lib "c4dll.dll" Alias "u4ncpy" (ByVal MemPtr1&, ByVal memptr2$, ByVal memLength&)

    Declare Sub u4memCpy Lib "c4dll.dll" (ByVal dest As String, ByVal source As Integer, ByVal numCopy As Integer)
    Declare Function u4switch Lib "c4dll.dll" () As Integer
#End If
    '=======================================================================================
    '
    '                Misc. function prototypes
    '
    '========================================================================================
#If USE_D4DLL Then
    Declare Function v4Cstring Lib "d4dll.dll" (ByVal s As String) As Integer
    Declare Sub v4Cstringfree Lib "d4dll.dll" (ByVal s As Integer)
#Else
    Declare Function v4Cstring Lib "c4dll.dll" (ByVal s As String) As Integer
    Declare Sub v4Cstringfree Lib "c4dll.dll" (ByVal s As Integer)
#End If

    Private Declare Function lstrcat Lib "kernel32" Alias "lstrcatA" (ByVal dest As String, ByVal src As String) As Integer

    'CodeBase Return Code Constants

    Public Const r4success As Short = 0
    Public Const r4same As Short = 0
    Public Const r4found As Short = 1
    Public Const r4down As Short = 1
    Public Const r4after As Short = 2
    Public Const r4complete As Short = 2
    Public Const r4eof As Short = 3
    Public Const r4bof As Short = 4
    Public Const r4entry As Short = 5
    Public Const r4descending As Short = 10
    Public Const r4unique As Short = 20
    Public Const r4uniqueContinue As Short = 25
    Public Const r4locked As Short = 50
    Public Const r4noCreate As Short = 60
    Public Const r4noOpen As Short = 70
    Public Const r4notag As Short = 80
    Public Const r4terminate As Short = 90
    Public Const r4inactive As Short = 110
    Public Const r4active As Short = 120
    Public Const r4authorize As Short = 140
    Public Const r4connected As Short = 150
    Public Const r4logOpen As Short = 170
    Public Const r4logOff As Short = 180
    Public Const r4null As Short = 190
    Public Const r4timeout As Short = 225

    Public Const relate4filterRecord As Short = 101
    Public Const relate4doRemove As Short = 102
    Public Const relate4skipped As Short = 104
    Public Const relate4blank As Short = 105
    Public Const relate4skipRec As Short = 106
    Public Const relate4terminate As Short = 107
    Public Const relate4exact As Short = 108
    Public Const relate4scan As Short = 109
    Public Const relate4approx As Short = 110
    Public Const relate4sortSkip As Short = 120
    Public Const relate4sortDone As Short = 121

    'CodeBasic Field Definition Constants
    Public Const r4logLen As Short = 1
    Public Const r4dateLen As Short = 8
    Public Const r4memoLen As Short = 10
    Public Const r4bin As String = "B" ' Binary
    Public Const r4str As String = "C" ' Character
    Public Const r4charBin As String = "Z" ' Character (binary)
    Public Const r4currency As String = "Y" ' Currency
    Public Const r4date As String = "D" ' Date
    Public Const r4dateTime As String = "T" ' DateTime
    Public Const r4double As String = "B" ' Double
    Public Const r4float As String = "F" ' Float
    Public Const r4gen As String = "G" ' General
    Public Const r4int As String = "I" ' Integer
    Public Const r4log As String = "L" ' Logical
    Public Const r4memo As String = "M" ' Memo
    Public Const r4memoBin As String = "X" ' Memo (binary)
    Public Const r4num As String = "N" ' Numeric
    Public Const r4dateDoub As String = "d" ' Date as Double
    Public Const r4numDoub As String = "n" ' Numeric as Double
    Public Const r4unicode As String = "W" ' Unicode character (same as r5wstr)

    Public Const r4system As String = "0" ' used by FoxPro for null field value field
    Public Const r5wstrLen As String = "O"
    Public Const r5ui4 As String = "P"
    Public Const r5i2 As String = "Q"
    Public Const r5ui2 As String = "R"
    Public Const r5guid As String = "V"
    Public Const r5wstr As String = "W"
    Public Const r5i8 As String = "1" ' 8-byte long signed value (LONGLONG)
    Public Const r5dbDate As String = "2" ' struct DBDATE (6 bytes)
    Public Const r5dbTime As String = "3" ' struct DBTIME (6 bytes)
    Public Const r5dbTimeStamp As String = "4" ' struct DBTIMESTAMP (16 bytes)
    Public Const r5date As String = "5"

    'Other CodeBase Constants
    Public Const cp0 As Short = 0 'code4.codePage constant
    Public Const cp437 As Short = 1
    Public Const cp850 As Short = 2 ' LY Jun 24/04
    Public Const cp1252 As Short = 3
    Public Const LOCK4OFF As Short = 0
    Public Const LOCK4ALL As Short = 1
    Public Const LOCK4DATA As Short = 2
    Public Const LOG4TRANS As Short = 0
    Public Const LOG4ON As Short = 1
    Public Const LOG4ALWAYS As Short = 2
    Public Const OPEN4DENY_NONE As Short = 0
    Public Const OPEN4DENY_RW As Short = 1
    Public Const OPEN4DENY_WRITE As Short = 2
    Public Const OPT4EXCLUSIVE As Short = -1
    Public Const OPT4OFF As Short = 0
    Public Const OPT4ALL As Short = 1
    Public Const r4check As Short = -5
    Public Const r4maxVBStringLen As Integer = 65500
    Public Const r4maxVBStrFunction As Short = 32767
    Public Const collate4machine As Short = 1
    Public Const collate4general As Short = 1001
    Public Const collate4special As Short = 1002
    Public Const sort4machine As Short = 0 'code4.collatingSequence constant
    Public Const sort4general As Short = 1
    Public Const WAIT4EVER As Short = -1

    ' Constants for code4serverOS
    Public Const OS4UNKNOWN As Short = &H0S
    Public Const OS4WIN32 As Short = &H1S
    Public Const OS4UNIX As Short = &H2S


    'CodeBasic Error Code Constants
    Public Const e4close As Short = -10
    Public Const e4create As Short = -20
    Public Const e4len As Short = -30
    Public Const e4lenSet As Short = -40
    Public Const e4lock As Short = -50
    Public Const e4open As Short = -60
    Public Const e4permiss As Short = -61
    Public Const e4access As Short = -62
    Public Const e4numFiles As Short = -63
    Public Const e4fileFind As Short = -64
    Public Const e4instance As Short = -69
    Public Const e4read As Short = -70
    Public Const e4remove As Short = -80
    Public Const e4rename As Short = -90
    Public Const e4seek As Short = -250
    Public Const e4unlock As Short = -110
    Public Const e4write As Short = -120
    Public Const e4data As Short = -200
    Public Const e4fieldName As Short = -210
    Public Const e4fieldType As Short = -220
    Public Const e4recordLen As Short = -230
    Public Const e4append As Short = -240
    Public Const e4entry As Short = -300
    Public Const e4index As Short = -310
    Public Const e4tagName As Short = -330
    Public Const e4unique As Short = -340
    Public Const e4tagInfo As Short = -350
    Public Const e4commaExpected As Short = -400
    Public Const e4complete As Short = -410
    Public Const e4dataName As Short = -420
    Public Const e4lengthErr As Short = -422
    Public Const e4notConstant As Short = -425
    Public Const e4numParms As Short = -430
    Public Const e4overflow As Short = -440
    Public Const e4rightMissing As Short = -450
    Public Const e4typeSub As Short = -460
    Public Const e4unrecFunction As Short = -470
    Public Const e4unrecOperator As Short = -480
    Public Const e4unrecValue As Short = -490
    Public Const e4undetermined As Short = -500
    Public Const e4tagExpr As Short = -510
    Public Const e4opt As Short = -610
    Public Const e4optSuspend As Short = -620
    Public Const e4optFlush As Short = -630
    Public Const e4relate As Short = -710
    Public Const e4lookupErr As Short = -720
    Public Const e4relateRefer As Short = -730
    Public Const e4info As Short = -910
    Public Const e4memory As Short = -920
    Public Const e4parm As Short = -930
    Public Const e4parmNull As Short = -935
    Public Const e4demo As Short = -940
    Public Const e4result As Short = -950
    Public Const e4verify As Short = -960
    Public Const e4struct As Short = -970
    Public Const e4notSupported As Short = -1090
    Public Const e4version As Short = -1095
    Public Const e4memoCorrupt As Short = -1110
    Public Const e4memoCreate As Short = -1120
    Public Const e4transViolation As Short = -1200
    Public Const e4trans As Short = -1210
    Public Const e4rollback As Short = -1220
    Public Const e4commit As Short = -1230
    Public Const e4transAppend As Short = -1240
    Public Const e4corrupt As Short = -1300
    Public Const e4connection As Short = -1310
    Public Const e4socket As Short = -1320
    Public Const e4net As Short = -1330
    Public Const e4loadlib As Short = -1340
    Public Const e4timeOut As Short = -1350
    Public Const e4message As Short = -1360
    Public Const e4packetLen As Short = -1370
    Public Const e4packet As Short = -1380
    Public Const e4max As Short = -1400
    Public Const e4codeBase As Short = -1410
    Public Const e4name As Short = -1420
    Public Const e4authorize As Short = -1430
    Public Const e4server As Short = -2100
    Public Const e4config As Short = -2110
    Public Const e4cat As Short = -2120

    Public Const ACTION4NONE As Short = 0
    Public Const ACTION4REINDEX As Short = 1
    Public Const ACTION4INITIALIZING As Short = 32767

    'ADO Constants
    Public Const e5badBinding As Short = 200
    Public Const e5conversion As Short = 210
    Public Const e5delete As Short = 220
    Public Const e5property As Short = 230

    Function b4String(ByRef p As Integer) As String
        'This is a utility function for copying a 'C' string to a VB string.

        Dim s As String = New String(CChar(" "), 256)
        Dim rc As Short

        If p <> 0 Then
            rc = u4ncpy(s, p, 256)
        End If

        b4String = Left(s, rc)
    End Function

    Function code4dateFormat(ByRef c4Ptr As Integer) As String
        'This function returns the CODE4.dateFormat member
        code4dateFormat = b4String(code4dateFormatVB(c4Ptr))
    End Function

    Function code4indexExtension(ByRef c4Ptr As Integer) As String
        'This function returns the CodeBase DLL index format
        code4indexExtension = b4String(code4indexExtensionVB(c4Ptr))
    End Function

    Function code4lockFileName(ByRef c4Ptr As Integer) As String
        'This function returns the locked file name
        code4lockFileName = b4String(code4lockFileNameVB(c4Ptr))
    End Function

    Function code4lockNetworkId(ByRef c4Ptr As Integer) As String
        'This function returns the user's network id
        'who has locked the current file
        code4lockNetworkId = b4String(code4lockNetworkIdVB(c4Ptr))
    End Function

    Function code4lockUserId(ByRef c4Ptr As Integer) As String
        'This function returns the user's name
        'who has locked the current file
        code4lockUserId = b4String(code4lockUserIdVB(c4Ptr))
    End Function

    Function code4logFileName(ByRef c4Ptr As Integer) As String
        'This function returns the locked file name
        code4logFileName = b4String(code4lockFileNameVB(c4Ptr))
    End Function

    Function d4alias(ByRef dbPtr As Integer) As String
        'This function returns the data file alias
        d4alias = b4String(d4aliasCB(dbPtr))
    End Function

    Function d4create(ByVal cb As Integer, ByRef dbname As String, ByRef D() As FIELD4INFO, ByRef n() As TAG4INFO) As Integer

        ' d4create calls d4createLow() to create a new database.
        ' This function is the same as d4createData() except that
        ' it requires an additional parameter which it uses to
        ' create tag information for a database.
        '
        ' Variable n is an array of type TAG4INFO which corresponds
        ' to TAG4INFOCB, a structure that can be used by d4create.
        ' The difference once again is merely the difference in the
        ' representation of strings between C and Basic.
        ' d4create takes the contents from the TAG4INFO structure
        ' and builds a TAG4INFOCB structure which it passes to d4createLow().
        ' Note: the TAG4INFOCB array is one size larger than the TAG4INFO
        ' array.  The extra empty (zero filled) array element is the
        ' way that d4createLow() detects the end of the array.


        Dim i As Short

        Dim flb As Short
        Dim fub As Short
        Dim fs As Short

        Dim tlb As Short
        Dim tub As Short
        Dim ts As Short


        flb = LBound(D)
        fub = UBound(D)
        ' LY Apr 13/04 : changed from fs = fub - 1, since Microsoft docs wrong about
        ' array sizes (i.e. dim array(N) contains elements 0 to N, not 0 to N - 1)
        fs = fub


        Dim f((fub + 1)) As FIELD4INFOCB
        i = flb
        Do While D(i).fName <> "" And i <= fs
            f(i).fName = v4Cstring(D(i).fName) ' note: this function allocates memory
            f(i).ftype = Asc(D(i).ftype)
            f(i).flength = D(i).flength
            f(i).fdecimals = D(i).fdecimals
            f(i).fnulls = D(i).fnulls
            i = i + 1
        Loop

        tlb = LBound(n)
        tub = UBound(n)
        ' LY Apr 13/04 : changed from ts = tub - 1, since Microsoft docs wrong about
        ' array sizes (i.e. dim array(N) contains elements 0 to N, not 0 to N - 1)
        ts = tub
        Dim t((tub + 1)) As TAG4INFOCB
        i = tlb
        Do While n(i).name <> "" And i <= ts
            t(i).name = v4Cstring(n(i).name)
            t(i).expression = v4Cstring(n(i).expression)
            t(i).filter_Renamed = v4Cstring(n(i).filter_Renamed)
            t(i).unique = n(i).unique
            t(i).descending = n(i).descending
            i = i + 1
        Loop
        d4create = d4createLow(cb, (dbname), f(0), t(0))

        ' Since v4Cstring allocates memory for the storage of
        ' C strings, we must free the memory after it has been
        ' used.
        ' LY Jul 14/04 : changed from "1 to fs" to "flb to fs - 1"
        For i = flb To fs - 1
            Call v4Cstringfree(f(i).fName)
        Next i

        ' LY Jul 14/04 : changed from "1 to ts" to "tlb to ts - 1"
        For i = tlb To ts - 1
            Call v4Cstringfree(t(i).name)
            Call v4Cstringfree(t(i).expression)
            Call v4Cstringfree(t(i).filter_Renamed)
        Next i

    End Function

    Function d4createData(ByVal cb As Integer, ByRef dbname As String, ByRef D() As FIELD4INFO) As Integer

        ' d4createData() calls d4createLow() to create a new database.
        ' d4create() builds the FIELD4INFOCB array which is
        ' the one recognized by d4create (note that the only difference
        ' is that the fname field is a string in type FIELD4INFO
        ' and type long in FIELD4INFOCB which is how strings are represented
        ' in C).  Furthermore, the size of f (our FIELD4INFOCB array) is one
        ' larger than the size s of FIELD4INFO d.  This is because
        ' d4create doesn't know the size of the array f and therefore it stops
        ' when it reaches an array element that is filled with zeros which
        ' the extra (s+1)'th element of f provides.

        Dim i As Short

        ' Make this array 1 larger than the one passed in.
        Dim f(1 + UBound(D) - LBound(D)) As FIELD4INFOCB

        i = 0
        For Each fielddef As FIELD4INFO In D
            If Len(fielddef.fName) = 0 Then
                Exit For
            End If
            f(i).fName = v4Cstring(fielddef.fName) ' note: this function allocates memory
            f(i).ftype = Asc(fielddef.ftype)
            f(i).flength = fielddef.flength
            f(i).fdecimals = fielddef.fdecimals
            f(i).fnulls = fielddef.fnulls
            i = i + 1
        Next fielddef

        d4createData = d4createLow(cb, (dbname), f(0), 0)

        ' Since v4Cstring allocates memory for the storage of
        ' C strings, we must free the memory after it has been
        ' used.
        ' LY Jul 14/04 : changed from "lb to s" to "lb to s - 1"
        For Each cf As FIELD4INFOCB In f
            Call v4Cstringfree(cf.fName)
        Next cf
    End Function

    Function d4encodeHandle(ByRef temp As Integer) As Object
        Dim EncodedString As String
        EncodedString = "#" & Str(temp)
        d4encodeHandle = EncodedString
    End Function

    Function d4fieldsAdd(ByRef DATA4 As Integer, ByRef fields() As FIELD4INFO) As Integer
        Dim i As Short
        Dim ub As Short

        ub = UBound(fields)
        Dim f(ub + 1) As FIELD4INFOCB
        For i = LBound(fields) To ub
            f(i).fName = v4Cstring(fields(i).fName) ' note: this function allocates memory
            f(i).ftype = Asc(fields(i).ftype)
            f(i).flength = fields(i).flength
            f(i).fdecimals = fields(i).fdecimals
            f(i).fnulls = fields(i).fnulls
        Next i

        d4fieldsAdd = d4fieldsAddCB(DATA4, ub, f(LBound(f)))
        If d4fieldsAdd <> 0 Then
            DATA4 = d4fieldsAdd
        End If

        ' Since v4Cstring allocates memory for the storage of
        ' C strings, we must free the memory after it has been
        ' used.
        For i = LBound(f) To UBound(f)
            Call v4Cstringfree(f(i).fName)
        Next i
    End Function

    Function d4fieldsRemove(ByRef DATA4 As Integer, ByRef fieldNames() As String) As Integer
        Dim addrs() As Integer
        Dim i As Short
        ReDim addrs(UBound(fieldNames))
        For i = LBound(fieldNames) To UBound(fieldNames)
            addrs(i) = v4Cstring(fieldNames(i))
        Next i

        d4fieldsRemove = d4fieldsRemoveCB(DATA4, UBound(fieldNames) - LBound(fieldNames) + 1, addrs(LBound(addrs)))

        For i = LBound(addrs) To UBound(addrs)
            Call v4Cstring(CStr(addrs(i)))
        Next i
    End Function

    Function d4fileName(ByRef dbfPtr As Integer) As String
        d4fileName = b4String(d4fileNameCB(dbfPtr))
    End Function

    Function d4go(ByRef DATA4 As Integer, ByRef recordNumber As Integer) As Short
        d4go = d4goLow(DATA4, recordNumber, 1)
    End Function

    Sub date4assign(ByRef dateString_Renamed As String, ByRef julianDay As Integer)
        'This functions converts the julian day into standard format
        'and puts the result in dateString

        'Size dateString$
        dateString_Renamed = Space(8 + 1)

        Call date4assignLow(dateString_Renamed, julianDay, 0)
        dateString_Renamed = Left(dateString_Renamed, 8)
    End Sub

    Function date4cdow(ByRef dateString_Renamed As String) As String
        'This function returns the day of the week in a character
        'string based on the value in 'DateString'

        ' CS 2006/05/12 Fix so function always returns a value.
        If Len(dateString_Renamed) < 8 Then
            date4cdow = ""
        Else
            Dim datePtr As Integer
            datePtr = date4cdowCB(dateString_Renamed) 'Get pointer to day
            If datePtr <> 0 Then
                date4cdow = b4String(datePtr)
            Else
                date4cdow = ""
            End If
        End If
    End Function

    Function date4cmonth(ByRef dateString_Renamed As String) As String
        'This function returns the month in 'DateString' as a
        'character string

        ' CS 2006/05/12 Fix so function always returns a value.
        If Len(dateString_Renamed) < 8 Then
            date4cmonth = ""
        Else
            Dim datePtr As Integer
            datePtr = date4cmonthCB(dateString_Renamed) 'Get pointer to month
            If datePtr <> 0 Then
                date4cmonth = b4String(datePtr) 'Return month name
            Else
                date4cmonth = ""
            End If
        End If
    End Function

    Sub date4format(ByRef dateString_Renamed As String, ByRef Result As String, ByRef pic As String)
        'This function formats Result$ using the date value
        ' in 'dateString$' and the format info. in 'Pic$'

        'Size Result$
        Result = Space(Len(pic) + 1)

        Call date4formatCB(dateString_Renamed, Result, pic)
        Result = Left(Result, Len(pic))
    End Sub

    Sub date4init(ByRef Result As String, ByRef dateString_Renamed As String, ByRef pic As String)
        'This function copies the date, specified by dateString,
        'and formatted according to pic, into Result. The date copied
        'will be in standard dBASE format,

        'Size Result$
        Result = Space(9)

        Call date4initCB(Result, dateString_Renamed, pic)
        Result = Left(Result, 8)
    End Sub

    Sub date4today(ByRef dateS As String)
        If Len(dateS) < 8 Then dateS = Space(8)
        Call date4todayCB(dateS)
    End Sub

    Function error4text(ByRef c4 As Integer, ByRef errCode As Integer) As String
        'This function returns the error message string
        error4text = b4String(error4textCB(c4, errCode))
    End Function

    Function expr4null(ByRef exPtr As Integer) As Short
        expr4null = expr4nullLow(exPtr, 1)
    End Function

    Function expr4source(ByRef exPtr As Integer) As String
        'This function returns a copy of the original
        'dBASE expression string
        expr4source = b4String(expr4sourceCB(exPtr))
    End Function

    Function expr4str(ByRef exPtr As Integer) As String
        'This function returns the parsed string

        'Get pointer to alias string
        Dim exprPtr As Integer = expr4strCB(exPtr)

        If exprPtr = 0 Then
            Return ""
        Else
            Return Left(b4String(exprPtr), expr4len(exPtr))
        End If
    End Function

    Function expr4type(ByRef exPtr As Integer) As String
        'This function returns the type of the parsed string

        Dim exprType As Short

        ' CS 2006/05/12 Fix so function always returns a value.
        'Get ASCII value of type
        exprType = expr4typeCB(exPtr)
        If exprType <> 0 Then
            expr4type = Chr(exprType)
        Else
            expr4type = ""
        End If
    End Function

    Sub f4assign(ByRef fPtr As Integer, ByRef fStr As String)
        Call f4assignN(fPtr, fStr, Len(fStr))
    End Sub

    Sub f4assignBinary(ByRef fPtr As Integer, ByRef value() As Byte)
        Call f4assignBinaryVB(fPtr, value(LBound(value)), UBound(value) - LBound(value))
    End Sub

    Sub f4assignUnicode(ByRef fPtr As Integer, ByRef value As String)
        Dim bArray() As Byte
        bArray = System.Text.UnicodeEncoding.Unicode.GetBytes(value & ControlChars.NullChar)
        Call f4assignUnicodeVB(fPtr, bArray(0))
    End Sub

    Sub f4assignInt(ByRef fldPtr As Integer, ByRef fldVal As Short)
        Call f4assignIntVB(fldPtr, fldVal)
    End Sub

    Function f4binary(ByRef field As Integer) As Object
        Dim fLen As Integer
        fLen = f4len(field)
        Dim buffer() As Byte
        If fLen = 0 Then
            ReDim buffer(0)
        Else
            ReDim buffer((fLen + 1)) ' 1 greater because f4ncpy null-terminates the buffer
            fLen = f4ncpyBinary(field, buffer(0), fLen + 1)
            ReDim Preserve buffer(fLen - 1)
        End If
        f4binary = buffer
    End Function

    Function f4currency(ByRef field As Integer, ByRef numDec As Short) As String
        'This function returns the contents of a field
        f4currency = b4String(f4currencyCB(field, numDec))
    End Function

    Function f4dateTime(ByRef field As Integer) As String
        'This function returns the contents of a field
        f4dateTime = b4String(f4dateTimeCB(field))
    End Function

    Function f4memoAssignBinary(ByRef fPtr As Integer, ByRef value() As Byte) As Short
        f4memoAssignBinary = f4memoAssignBinaryVB(fPtr, value(LBound(value)), UBound(value) - LBound(value) + 1)
    End Function

    Sub f4memoAssignUnicode(ByRef fPtr As Integer, ByRef value As String)
        Dim bArray() As Byte
        bArray = System.Text.UnicodeEncoding.Unicode.GetBytes(value & ControlChars.NullChar)
        Call f4memoAssignUnicodeVB(fPtr, bArray(0))
    End Sub

    Function f4memoBinary(ByRef field As Integer) As Object
        Dim fLen As Integer
        fLen = f4memoLen(field)
        Dim buffer() As Byte
        If fLen = 0 Then
            ReDim buffer(0)
        Else
            ' CS 2007/04/23 Fixes because Lbound is always 0.
            ReDim buffer(fLen)
            fLen = f4memoNcpyBinary(field, buffer(0), fLen + 1)
            ReDim Preserve buffer(fLen - 1)
        End If
        f4memoBinary = buffer
    End Function

    Function f4memoStr(ByRef fPtr As Integer) As String
        'This function returns a string corresponding to the memo
        'field pointer argument.
        Dim MemoLen, MemoPtr As Integer

        MemoLen = f4memoLen(fPtr) 'Get memo length

        If MemoLen > &H7FFFFFFF Then
            MsgBox("Error #: -910" & ControlChars.CrLf & "Unexpected Information" & vbCrLf & "Memo entry too long to return in a Visual Basic string." & vbCrLf & "Field Name:" & vbCrLf & f4name(fPtr), 16, "CodeBase Error")
            Return ""
        End If

        MemoPtr = f4memoPtr(fPtr)
        If MemoPtr = 0 Then Return ""

        Dim MemoString As String
        MemoString = New String(" ", MemoLen)

        'Copy 'MemoPtr' to VB string 'MemoString'
        u4memCpy(MemoString, MemoPtr, MemoLen)

        f4memoStr = MemoString
    End Function

    Sub f4memoStr64(ByRef fPtr As Integer, ByRef src As String)

        'This function copies a large memo entry (32K-64K)
        'into a user supplied string

        Dim r4line As String
        r4line = Chr(10) & Chr(13)

        Dim MemoLen, MemoPtr As Integer

        MemoLen = f4memoLen(fPtr) 'Get memo length

        ' 'r4maxVBStringLen' defined in 'Constants' section of this file
        If MemoLen > r4maxVBStringLen Then
            MsgBox("Error #: -910" & r4line & "Unexpected Information" & r4line & "Memo entry too long to retrieve into VB string." & r4line & "Field Name:" & r4line & f4name(fPtr), 16, "CodeBasic Error")
            Exit Sub
        End If

        MemoPtr = f4memoPtr(fPtr)
        If MemoPtr = 0 Then Exit Sub

        src = New String(" ", MemoLen)

        'Copy 'MemoPtr' to VB string 'src'
        u4memCpy(src, MemoPtr, MemoLen)

    End Sub

    Function f4name(ByRef fPtr As Integer) As String
        'This function returns the name of a field

        Dim FldNamePtr As Integer 'Pointer to field name
        FldNamePtr = f4nameCB(fPtr) 'Get pointer

        f4name = b4String(FldNamePtr)
    End Function

    Function f4nCpy(ByRef field As Integer, ByRef s As String, ByRef slen As Short) As Short
        'This function copies the fields contents into a string
        s = Space(slen) 'Make s$ one byte longer for null character that u4ncpy adds
        Dim fPtr As Integer
        fPtr = f4ptr(field)

        If fPtr = 0 Then Exit Function

        u4memCpy(s, fPtr, slen)

        f4nCpy = Len(s)
    End Function

    Function f4str(ByRef field As Integer) As String
        'This function returns the contents of a field
        Dim s As String
        Dim fPtr As Integer
        Dim fLen As Short

        fPtr = f4ptr(field)
        If fPtr = 0 Then Return ""

        fLen = f4len(field) 'Get field length
        s = Space(fLen) 'Make s$ one byte longer for null character that u4ncpy adds

        u4memCpy(s, fPtr, fLen)

        f4str = s
    End Function

    Function f4strUnicode(ByRef field As Integer) As String
        'This function returns the contents of a Unicode field
        Dim bArray() As Byte
        bArray = f4binary(field)
        f4strUnicode = RTrimNulls(Left(System.Text.UnicodeEncoding.Unicode.GetString(bArray), f4len(field)))
    End Function

    Function f4memoStrUnicode(ByRef field As Integer) As String
        'This function returns the contents of a Unicode memo field
        Dim bArray() As Byte
        bArray = f4memoBinary(field)
        f4memoStrUnicode = RTrimNulls(Left(System.Text.UnicodeEncoding.Unicode.GetString(bArray), f4memoLen(field)))
    End Function

    Function i4create(ByVal dbPtr As Integer, ByRef IndexName As String, ByRef n() As TAG4INFO) As Integer
        ' i4create() calls i4createCB() to create a new
        ' index file. Variable n is an array of type TAG4INFO
        ' which corresponds to TAG4INFOCB, a structure that
        ' can be used by i4createCB(). The difference once
        ' again is merely the difference in the representation
        ' of strings between C and Basic.
        '
        ' i4create() takes the contents from the TAG4INFO
        ' structure and builds a TAG4INFOCB structure which
        ' it passes to i4createCB(). Note: the TAG4INFOCB
        ' arrary is one size larger than the TAG4INFO array.
        ' The extra empty (zero filled) array element is the
        ' way that i4create detects the end of the array.
        '
        ' Note also, that if 'IndexName' is an empty string,
        ' the index file that is created will become a
        ' "production" index file. i.e. it will be opened every
        ' time the corresponding data file is opened.

        Dim i As Short

        ' CS 2006/06/21 Improve array handling.
        Dim c_tag_array(UBound(n) - LBound(n)) As TAG4INFOCB  ' Array size needs to be one more than VB array for null terminator.
        i = 0
        For Each tag As TAG4INFO In n
            c_tag_array(i).name = v4Cstring(tag.name)
            c_tag_array(i).expression = v4Cstring(tag.expression)
            c_tag_array(i).filter_Renamed = v4Cstring(tag.filter_Renamed)
            c_tag_array(i).unique = tag.unique
            c_tag_array(i).descending = tag.descending
            i = i + 1
        Next tag

        ' LY Apr 18/05 : see LY Apr 18/05 in i4create.c
        i4create = i4createCB(dbPtr, IndexName, c_tag_array(0))

        ' Since v4Cstring allocates memory for the storage of
        ' C strings, we must free the memory after it has been
        ' used.

        For Each tag As TAG4INFOCB In c_tag_array
            If tag.name <> 0 Then Call v4Cstringfree(tag.name)
            If tag.expression <> 0 Then Call v4Cstringfree(tag.expression)
            If tag.filter_Renamed <> 0 Then Call v4Cstringfree(tag.filter_Renamed)
        Next tag
    End Function

    Function i4fileName(ByRef iPtr As Integer) As String
        'This function returns the file name of an index tag
        i4fileName = b4String(i4fileNameCB(iPtr))
    End Function

    Function i4open(ByRef d4 As Integer, ByRef fName As String) As Integer
        If fName = "" Then
            i4open = i4openCB(d4, 0) 'Use data file name
        Else
            i4open = i4openCB(d4, fName) 'Use supplied name
        End If
    End Function

    Function i4tagAdd(ByVal i4Ptr As Integer, ByRef n() As TAG4INFO) As Short
        ' i4tagAdd adds additional tags to an existing
        ' index.

        ' i4tagAdd takes the contents from the TAG4INFO
        ' structure and builds a TAG4INFOCB structure which
        ' is passed to i4tagAddCB.

        Dim i As Short

        Dim tlb As Short
        Dim tub As Short
        Dim ts As Short


        tlb = LBound(n)
        tub = UBound(n)
        ts = tub - tlb + 1
        Dim t((ts + 1)) As TAG4INFOCB
        For i = 1 To ts
            t(i).name = v4Cstring(n((tlb - 1) + i).name)
            t(i).expression = v4Cstring(n((tlb - 1) + i).expression)
            t(i).filter_Renamed = v4Cstring(n((tlb - 1) + i).filter_Renamed)
            t(i).unique = n((tlb - 1) + i).unique
            t(i).descending = n((tlb - 1) + i).descending
        Next i

        i4tagAdd = i4tagAddCB(i4Ptr, t(1))

    End Function

    Function relate4masterExpr(ByRef r4Ptr As Integer) As String
        'This function returns the Relations expression string
        relate4masterExpr = b4String(relate4masterExprCB(r4Ptr))
    End Function

    Function report4parent(ByVal r4 As Integer, ByVal hWnd As IntPtr) As Short
        report4parent = report4parent32(r4, hWnd)
    End Function

    Sub report4free(ByRef pReport As Integer, ByRef freeRelate As Short, ByRef closeFiles As Short)
        Call report4freeLow(pReport, freeRelate, closeFiles, 1)
    End Sub

    Function t4Alias(ByRef tPtr As Integer) As String
        t4Alias = b4String(t4aliasCB(tPtr))
    End Function

    Function t4expr(ByRef tPtr As Integer) As String
        'This function returns the original tag expression
        t4expr = b4String(t4exprCB(tPtr))
    End Function

    Function t4filter(ByRef tPtr As Integer) As String

        'This function returns the tag filter expression

        Dim FilterPtr As Integer

        'Get pointer to parsed filter expression
        FilterPtr = t4filterCB(tPtr)

        If FilterPtr = 0 Then
            t4filter = ""
            Exit Function 'No filter
        End If

        t4filter = b4String(FilterPtr)

    End Function

    Function u4descend(ByRef charString As String) As String
        Dim Result As String = ""
        Dim i As Short

        For i = 1 To Len(charString)
            Result = Result & Chr(128 And Asc(Mid(charString, i, 1)))
        Next

        u4descend = Result
    End Function

    Private Function RTrimNulls(ByRef str_Renamed As String) As String
        ' Returns a String containing a copy
        ' of str without trailing nulls.
        Dim curLength, newLength As Integer
        curLength = Len(str_Renamed)
        If curLength = 0 Then Return ""

        For newLength = 1 To curLength
            If Mid(str_Renamed, newLength, 1) = ControlChars.NullChar Then
                RTrimNulls = Left(str_Renamed, newLength - 1)
                Exit Function
            End If
        Next

        RTrimNulls = str_Renamed
    End Function
End Module
