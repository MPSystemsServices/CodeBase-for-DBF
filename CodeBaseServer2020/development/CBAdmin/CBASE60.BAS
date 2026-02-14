Attribute VB_Name = "CodeBase"
' codebase.bas  (c)Copyright Sequiter Software Inc., 1988-2001.  All rights reserved.

'Data Types Used by CodeBase

Type FIELD4INFOCB
    fName As Long        ' C string (which is different than a Basic String)
    ftype As Integer
    flength As Integer
    fdecimals As Integer
    fnulls As Integer
End Type

Type FIELD4INFO          ' Corresponding Basic structure
    fName As String
    ftype As String * 1
    flength As Integer
    fdecimals As Integer
    fnulls As Integer
End Type

Type TAG4INFOCB
    name As Long         ' C string
    expression As Long   ' C string
    filter As Long       ' C string
    unique As Integer
    descending As Integer
End Type

Type TAG4INFO
    name As String
    expression As String
    filter As String
    unique As Integer
    descending As Integer
End Type

'===================================================================================
'
'     CODE4 Access  function prototypes
'
'===================================================================================
' LY 2001/09/25 : in order to use the dual DLL support, USE_D4DLL = 1 should be
'  added to the Condition Compilation setting under VB's Project|Properties|Make
#If USE_D4DLL Then
    Declare Sub c4setAcceptTimeOut Lib "d4dll.dll" (ByVal c4&, ByVal timeout&)
    Declare Function code4actionCode% Lib "d4dll.dll" (ByVal c4&)
    Declare Function code4accessMode% Lib "d4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4allocDll& Lib "d4dll.dll" (ByVal dllName$)
    Declare Function code4autoOpen% Lib "d4dll.dll" (ByVal c4&, ByVal value%)
    Declare Sub code4autoIncrementStart Lib "d4dll.dll" (ByVal c4&, ByVal value#)
    Declare Function code4calcCreate% Lib "d4dll.dll" (ByVal c4&, ByVal expr4&, ByVal fcnName$)
    Declare Sub code4calcReset Lib "d4dll.dll" (ByVal c4&)
    Declare Function code4codePage% Lib "d4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4collatingSequence% Lib "d4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4collate% Lib "d4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4compatibility% Lib "d4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4compress% Lib "d4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4connect% Lib "d4dll.dll" (ByVal c4&, ByVal serverId$, ByVal processId$, ByVal userName$, ByVal password$, ByVal protocol$)
    Declare Function code4close% Lib "d4dll.dll" (ByVal c4&)
    Declare Function code4createTemp% Lib "d4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4data& Lib "d4dll.dll" (ByVal c4&, ByVal AliasName$)
    Declare Function code4dateFormatVB& Lib "d4dll.dll" Alias "code4dateFormat" (ByVal c4&)
    Declare Function code4dateFormatSet% Lib "d4dll.dll" (ByVal c4&, ByVal fmt$)
    Declare Function code4errCreate% Lib "d4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4errDefaultUnique% Lib "d4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4errorCode% Lib "d4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4errExpr% Lib "d4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4errFieldName% Lib "d4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4errGo% Lib "d4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4errSkip% Lib "d4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4errTagName% Lib "d4dll.dll" (ByVal c4&, ByVal value%)
    Declare Sub code4exit Lib "d4dll.dll" (ByVal c4&)
    Declare Function code4fileFlush% Lib "d4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4flush% Lib "d4dll.dll" (ByVal c4&)
    Declare Function code4hInst% Lib "d4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4indexBlockSize% Lib "d4dll.dll" (ByVal c4&)
    Declare Function code4indexBlockSizeSet% Lib "d4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4indexExtensionVB& Lib "d4dll.dll" Alias "code4indexExtension" (ByVal c4&)
    Declare Function code4hWnd& Lib "d4dll.dll" (ByVal c4&, ByVal value&)
    Declare Function code4initUndo% Lib "d4dll.dll" (ByVal c4&)
    Declare Function code4initUndoLow% Lib "d4dll.dll" (ByVal c4&)
    Declare Sub code4largeOn Lib "d4dll.dll" (ByVal c4&)
    Declare Function code4lock% Lib "d4dll.dll" (ByVal c4&)
    Declare Function code4lockAttempts% Lib "d4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4lockAttemptsSingle% Lib "d4dll.dll" (ByVal c4&, ByVal value%)
    Declare Sub code4lockClear Lib "d4dll.dll" (ByVal c4&)
    Declare Function code4lockDelay& Lib "d4dll.dll" (ByVal c4&, ByVal value&)
    Declare Function code4lockEnforce% Lib "d4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4lockFileNameVB& Lib "d4dll.dll" Alias "code4lockFileName" (ByVal c4&)
    Declare Function code4lockItem& Lib "d4dll.dll" (ByVal c4&)
    Declare Function code4lockNetworkIdVB& Lib "d4dll.dll" Alias "code4lockNetworkId" (ByVal c4&)
    Declare Function code4lockUserIdVB& Lib "d4dll.dll" Alias "code4lockUserId" (ByVal c4&)
    Declare Function code4log% Lib "d4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4logCreate% Lib "d4dll.dll" (ByVal c4&, ByVal logName$, ByVal userId$)
    Declare Function code4logFileNameVB& Lib "d4dll.dll" (ByVal c4&)
    Declare Function code4logOpen% Lib "d4dll.dll" (ByVal c4&, ByVal logName$, ByVal userId$)
    Declare Sub code4logOpenOff Lib "d4dll.dll" (ByVal c4&)
    Declare Function code4memExpandBlock% Lib "d4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4memExpandData% Lib "d4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4memExpandIndex% Lib "d4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4memExpandLock% Lib "d4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4memExpandTag% Lib "d4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4memoCompress% Lib "d4dll.dll" (ByVal cb&, ByVal flag%)
    Declare Function code4memSizeBlock& Lib "d4dll.dll" (ByVal c4&, ByVal value&)
    Declare Function code4memSizeBuffer& Lib "d4dll.dll" (ByVal c4&, ByVal value&)
    Declare Function code4memSizeMemo% Lib "d4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4memSizeMemoExpr& Lib "d4dll.dll" (ByVal c4&, ByVal value&)
    Declare Function code4memSizeSortBuffer& Lib "d4dll.dll" (ByVal c4&, ByVal value&)
    Declare Function code4memSizeSortPool& Lib "d4dll.dll" (ByVal c4&, ByVal value&)
    Declare Function code4memStartBlock% Lib "d4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4memStartData% Lib "d4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4memStartIndex% Lib "d4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4memStartLock% Lib "d4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4memStartMax& Lib "d4dll.dll" (ByVal c4&, ByVal value&)
    Declare Function code4memStartTag% Lib "d4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4errOff% Lib "d4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4errOffLow% Lib "d4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4errOpen% Lib "d4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4optAll% Lib "d4dll.dll" (ByVal c4&)
    Declare Function code4optimize% Lib "d4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4optStart% Lib "d4dll.dll" (ByVal c4&)
    Declare Function code4optSuspend% Lib "d4dll.dll" (ByVal c4&)
    Declare Function code4optimizeWrite% Lib "d4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4readLock% Lib "d4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4readOnly% Lib "d4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4errRelate% Lib "d4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4safety% Lib "d4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4singleOpen% Lib "d4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4serverOS& Lib "d4dll.dll" (ByVal c4&)
    Declare Function code4timeout& Lib "d4dll.dll" (ByVal c4&)
    Declare Sub code4timeoutSet Lib "d4dll.dll" (ByVal c4&, ByVal value&)
    Declare Function code4tranStart% Lib "d4dll.dll" (ByVal c4&)
    Declare Function code4tranStatus% Lib "d4dll.dll" Alias "code4tranStatusCB" (ByVal c4&)
    Declare Function code4tranCommit% Lib "d4dll.dll" (ByVal c4&)
    Declare Function code4tranRollback% Lib "d4dll.dll" (ByVal c4&)
    Declare Function code4unlock% Lib "d4dll.dll" (ByVal c4&)
    Declare Function code4unlockAuto% Lib "d4dll.dll" Alias "code4unlockAutoCB" (ByVal c4&)
    Declare Function code4useGeneralTagsInRelate% Lib "d4dll.dll" (ByVal c4&, ByVal value%)
    Declare Sub code4unlockAutoSet Lib "d4dll.dll" Alias "code4unlockAutoSetCB" (ByVal c4&, ByVal value%)
    Declare Sub code4verifySet Lib "d4dll.dll" (ByVal c4&, ByVal value$)
#Else
    Declare Sub c4setAcceptTimeOut Lib "c4dll.dll" (ByVal c4&, ByVal timeout&)
    Declare Function code4actionCode% Lib "c4dll.dll" (ByVal c4&)
    Declare Function code4accessMode% Lib "c4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4autoOpen% Lib "c4dll.dll" (ByVal c4&, ByVal value%)
    Declare Sub code4autoIncrementStart Lib "c4dll.dll" (ByVal c4&, ByVal value#)
    Declare Function code4calcCreate% Lib "c4dll.dll" (ByVal c4&, ByVal expr4&, ByVal fcnName$)
    Declare Sub code4calcReset Lib "c4dll.dll" (ByVal c4&)
    Declare Function code4codePage% Lib "c4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4collatingSequence% Lib "c4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4collate% Lib "c4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4compatibility% Lib "c4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4compress% Lib "c4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4connect% Lib "c4dll.dll" (ByVal c4&, ByVal serverId$, ByVal processId$, ByVal userName$, ByVal password$, ByVal protocol$)
    Declare Function code4close% Lib "c4dll.dll" (ByVal c4&)
    Declare Function code4createTemp% Lib "c4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4data& Lib "c4dll.dll" (ByVal c4&, ByVal AliasName$)
    Declare Function code4dateFormatVB& Lib "c4dll.dll" Alias "code4dateFormat" (ByVal c4&)
    Declare Function code4dateFormatSet% Lib "c4dll.dll" (ByVal c4&, ByVal fmt$)
    Declare Sub code4encryptFile Lib "c4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4encryptInit% Lib "c4dll.dll" (ByVal c4&, ByVal key$, ByVal length%)
    Declare Function code4errCreate% Lib "c4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4errDefaultUnique% Lib "c4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4errorCode% Lib "c4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4errExpr% Lib "c4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4errFieldName% Lib "c4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4errGo% Lib "c4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4errSkip% Lib "c4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4errTagName% Lib "c4dll.dll" (ByVal c4&, ByVal value%)
    Declare Sub code4exit Lib "c4dll.dll" (ByVal c4&)
    Declare Function code4fileFlush% Lib "c4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4flush% Lib "c4dll.dll" (ByVal c4&)
    Declare Function code4hInst% Lib "c4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4indexBlockSize% Lib "c4dll.dll" (ByVal c4&)
    Declare Function code4indexBlockSizeSet% Lib "c4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4indexExtensionVB& Lib "c4dll.dll" Alias "code4indexExtension" (ByVal c4&)
    Declare Function code4hWnd& Lib "c4dll.dll" (ByVal c4&, ByVal value&)
    Declare Function code4init& Lib "c4dll.dll" Alias "code4initVB" ()
    Declare Function code4initUndo% Lib "c4dll.dll" (ByVal c4&)
    Declare Sub code4largeOn Lib "c4dll.dll" (ByVal c4&)
    Declare Function code4lock% Lib "c4dll.dll" (ByVal c4&)
    Declare Function code4lockAttempts% Lib "c4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4lockAttemptsSingle% Lib "c4dll.dll" (ByVal c4&, ByVal value%)
    Declare Sub code4lockClear Lib "c4dll.dll" (ByVal c4&)
    Declare Function code4lockDelay& Lib "c4dll.dll" (ByVal c4&, ByVal value&)
    Declare Function code4lockEnforce% Lib "c4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4lockFileNameVB& Lib "c4dll.dll" Alias "code4lockFileName" (ByVal c4&)
    Declare Function code4lockItem& Lib "c4dll.dll" (ByVal c4&)
    Declare Function code4lockNetworkIdVB& Lib "c4dll.dll" Alias "code4lockNetworkId" (ByVal c4&)
    Declare Function code4lockUserIdVB& Lib "c4dll.dll" Alias "code4lockUserId" (ByVal c4&)
    Declare Function code4log% Lib "c4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4logCreate% Lib "c4dll.dll" (ByVal c4&, ByVal logName$, ByVal userId$)
    Declare Function code4logFileNameVB& Lib "c4dll.dll" (ByVal c4&)
    Declare Function code4logOpen% Lib "c4dll.dll" (ByVal c4&, ByVal logName$, ByVal userId$)
    Declare Sub code4logOpenOff Lib "c4dll.dll" (ByVal c4&)
    Declare Function code4memExpandBlock% Lib "c4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4memExpandData% Lib "c4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4memExpandIndex% Lib "c4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4memExpandLock% Lib "c4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4memExpandTag% Lib "c4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4memoCompress% Lib "c4dll.dll" (ByVal cb&, ByVal flag%)
    Declare Function code4memSizeBlock& Lib "c4dll.dll" (ByVal c4&, ByVal value&)
    Declare Function code4memSizeBuffer& Lib "c4dll.dll" (ByVal c4&, ByVal value&)
    Declare Function code4memSizeMemo% Lib "c4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4memSizeMemoExpr& Lib "c4dll.dll" (ByVal c4&, ByVal value&)
    Declare Function code4memSizeSortBuffer& Lib "c4dll.dll" (ByVal c4&, ByVal value&)
    Declare Function code4memSizeSortPool& Lib "c4dll.dll" (ByVal c4&, ByVal value&)
    Declare Function code4memStartBlock% Lib "c4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4memStartData% Lib "c4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4memStartIndex% Lib "c4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4memStartLock% Lib "c4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4memStartMax& Lib "c4dll.dll" (ByVal c4&, ByVal value&)
    Declare Function code4memStartTag% Lib "c4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4errOff% Lib "c4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4errOpen% Lib "c4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4optAll% Lib "c4dll.dll" (ByVal c4&)
    Declare Function code4optimize% Lib "c4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4optStart% Lib "c4dll.dll" (ByVal c4&)
    Declare Function code4optSuspend% Lib "c4dll.dll" (ByVal c4&)
    Declare Function code4optimizeWrite% Lib "c4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4ping% Lib "c4dll.dll" (ByVal c4&)
    Declare Function code4readLock% Lib "c4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4readOnly% Lib "c4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4errRelate% Lib "c4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4safety% Lib "c4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4singleOpen% Lib "c4dll.dll" (ByVal c4&, ByVal value%)
    Declare Function code4serverOS& Lib "c4dll.dll" (ByVal c4&)
    Declare Function code4timeout& Lib "c4dll.dll" (ByVal c4&)
    Declare Sub code4timeoutSet Lib "c4dll.dll" (ByVal c4&, ByVal value&)
    Declare Function code4tranStart% Lib "c4dll.dll" (ByVal c4&)
    Declare Function code4tranStatus% Lib "c4dll.dll" Alias "code4tranStatusCB" (ByVal c4&)
    Declare Function code4tranCommit% Lib "c4dll.dll" (ByVal c4&)
    Declare Function code4tranRollback% Lib "c4dll.dll" (ByVal c4&)
    Declare Function code4unlock% Lib "c4dll.dll" (ByVal c4&)
    Declare Function code4unlockAuto% Lib "c4dll.dll" Alias "code4unlockAutoCB" (ByVal c4&)
    Declare Function code4useGeneralTagsInRelate% Lib "c4dll.dll" (ByVal c4&, ByVal value%)
    Declare Sub code4unlockAutoSet Lib "c4dll.dll" Alias "code4unlockAutoSetCB" (ByVal c4&, ByVal value%)
    Declare Sub code4verifySet Lib "c4dll.dll" (ByVal c4&, ByVal value$)
#End If

'===============================================================================================
'
'                                 Data File Functions' Prototypes
'
'-----------------------------------------------------------------------------------------------
#If USE_D4DLL Then
    Declare Function d4aliasCB& Lib "d4dll.dll" Alias "d4alias" (ByVal d4&)
    Declare Sub d4aliasSet Lib "d4dll.dll" (ByVal d4&, ByVal AliasValue$)
    Declare Function d4append% Lib "d4dll.dll" (ByVal d4&)
    Declare Function d4appendBlank% Lib "d4dll.dll" (ByVal d4&)
    Declare Function d4appendStart% Lib "d4dll.dll" (ByVal d4&, ByVal UseMemoEntries%)
    Declare Sub d4blank Lib "d4dll.dll" (ByVal d4&)
    Declare Function d4bof% Lib "d4dll.dll" (ByVal d4&)
    Declare Function d4bottom% Lib "d4dll.dll" (ByVal d4&)
    Declare Function d4changed% Lib "d4dll.dll" (ByVal d4&, ByVal intFlag%)
    Declare Function d4check% Lib "d4dll.dll" (ByVal d4&)
    Declare Function d4close% Lib "d4dll.dll" (ByVal d4&)
    Declare Function d4codePage% Lib "d4dll.dll" (ByVal d4&)
    Declare Function d4createCB& Lib "d4dll.dll" Alias "d4create" (ByVal c4&, ByVal DbfName$, ByVal fieldinfo&, ByVal tagInfo&)
    Declare Function d4createLow& Lib "d4dll.dll" Alias "d4create" (ByVal c4&, ByVal DbfName$, fieldinfo As Any, tagInfo As Any)
    Declare Sub d4delete Lib "d4dll.dll" (ByVal d4&)
    Declare Function d4deleted% Lib "d4dll.dll" (ByVal d4&)
    Declare Function d4eof% Lib "d4dll.dll" (ByVal d4&)
    Declare Function d4field& Lib "d4dll.dll" (ByVal d4&, ByVal FieldName$)
    Declare Function d4fieldInfo& Lib "d4dll.dll" (ByVal d4&)
    Declare Function d4fieldJ& Lib "d4dll.dll" (ByVal d4&, ByVal JField%)
    Declare Function d4fieldNumber% Lib "d4dll.dll" (ByVal d4&, ByVal FieldName$)
    Declare Function d4fieldsAddCB& Lib "d4dll.dll" Alias "d4fieldsAdd" (ByVal d4&, ByVal nFields%, fieldinfo As Any)
    Declare Function d4fieldsRemoveCB& Lib "d4dll.dll" Alias "d4fieldsRemove" (ByRef d4&, ByVal nFields%, fieldNames As Any)
    Declare Function d4fileNameCB& Lib "d4dll.dll" Alias "d4fileName" (ByVal d4&)
    Declare Function d4flush% Lib "d4dll.dll" (ByVal d4&)
    Declare Function d4freeBlocks% Lib "d4dll.dll" (ByVal d4&)
    Declare Function d4goLow% Lib "d4dll.dll" (ByVal d4&, ByVal RecNum&, ByVal goForWrite%)
    Declare Function d4goBof% Lib "d4dll.dll" (ByVal d4&)
    Declare Function d4goEof% Lib "d4dll.dll" (ByVal d4&)
    Declare Function d4index& Lib "d4dll.dll" (ByVal d4&, ByVal IndexName$)
    Declare Function d4log% Lib "d4dll.dll" Alias "d4logVB" (ByVal d4&, ByVal logging%)
    Declare Function d4lock% Lib "d4dll.dll" Alias "d4lockVB" (ByVal d4&, ByVal recordNum&)
    Declare Function d4lockAdd% Lib "d4dll.dll" (ByVal d4&, ByVal recordNum&)
    Declare Function d4lockAddAll% Lib "d4dll.dll" (ByVal d4&)
    Declare Function d4lockAddAppend% Lib "d4dll.dll" (ByVal d4&)
    Declare Function d4lockAddFile% Lib "d4dll.dll" (ByVal d4&)
    Declare Function d4lockAll% Lib "d4dll.dll" Alias "d4lockAllVB" (ByVal d4&)
    Declare Function d4lockAppend% Lib "d4dll.dll" Alias "d4lockAppendVB" (ByVal d4&)
    Declare Function d4lockFile% Lib "d4dll.dll" Alias "d4lockFileVB" (ByVal d4&)
    Declare Function d4logStatus% Lib "d4dll.dll" Alias "d4logStatusCB" (ByVal d4&)
    Declare Function d4memoCompress% Lib "d4dll.dll" (ByVal d4&)
    Declare Function d4numFields% Lib "d4dll.dll" (ByVal d4&)
    Declare Function d4open& Lib "d4dll.dll" (ByVal c4&, ByVal DbfName$)
    Declare Function d4openClone& Lib "d4dll.dll" (ByVal d4&)
    Declare Function d4optimize% Lib "d4dll.dll" Alias "d4optimizeVB" (ByVal d4&, ByVal OptFlag%)
    Declare Function d4optimizeWrite% Lib "d4dll.dll" Alias "d4optimizeWriteVB" (ByVal d4&, ByVal OptFlag%)
    Declare Function d4pack% Lib "d4dll.dll" (ByVal d4&)
    Declare Function d4packData% Lib "d4dll.dll" (ByVal d4&)
    Declare Function d4position# Lib "d4dll.dll" (ByVal d4&)
    Declare Function d4positionSet% Lib "d4dll.dll" (ByVal d4&, ByVal Percentage#)
    Declare Function d4readBuffer& Lib "d4dll.dll" (ByVal d4&, ByVal numRecsToBuf&, ByVal doMemos%)
    Declare Sub d4recall Lib "d4dll.dll" (ByVal d4&)
    Declare Function d4recCount& Lib "d4dll.dll" Alias "d4recCountDo" (ByVal d4&)
    Declare Function d4recNo& Lib "d4dll.dll" Alias "d4recNoLow" (ByVal d4&)
    Declare Function d4record& Lib "d4dll.dll" Alias "d4recordLow" (ByVal d4&)
    Declare Function d4recWidth& Lib "d4dll.dll" Alias "d4recWidth_v" (ByVal d4&)
    Declare Function d4remove% Lib "d4dll.dll" (ByVal d4&)
    Declare Function d4refresh% Lib "d4dll.dll" (ByVal d4&)
    Declare Function d4refreshRecord% Lib "d4dll.dll" (ByVal d4&)
    Declare Function d4reindex% Lib "d4dll.dll" (ByVal d4&)
    Declare Function d4reindexWithProgress% Lib "d4dll.dll" (ByVal d4&, ByVal callback&, ByVal milliseconds&)
    Declare Function d4seek% Lib "d4dll.dll" (ByVal d4&, ByVal seekValue$)
    Declare Function d4seekDouble% Lib "d4dll.dll" (ByVal d4&, ByVal value#)
    Declare Function d4seekN% Lib "d4dll.dll" (ByVal d4&, ByVal seekValue$, ByVal seekLen%)
    Declare Function d4seekNext% Lib "d4dll.dll" (ByVal d4&, ByVal seekValue$)
    Declare Function d4seekNextDouble% Lib "d4dll.dll" (ByVal d4&, ByVal seekValue#)
    Declare Function d4seekNextN% Lib "d4dll.dll" (ByVal d4&, ByVal seekValue$, ByVal seekLen%)
    Declare Function d4skip% Lib "d4dll.dll" (ByVal d4&, ByVal NumberRecords&)
    Declare Function d4tag& Lib "d4dll.dll" (ByVal d4&, ByVal TagName$)
    Declare Function d4tagDefault& Lib "d4dll.dll" (ByVal d4&)
    Declare Function d4tagNext& Lib "d4dll.dll" (ByVal d4&, ByVal TagOn&)
    Declare Function d4tagPrev& Lib "d4dll.dll" (ByVal d4&, ByVal TagOn&)
    Declare Sub d4tagSelect Lib "d4dll.dll" (ByVal d4&, ByVal tPtr&)
    Declare Function d4tagSelected& Lib "d4dll.dll" (ByVal d4&)
    Declare Function d4tagSync% Lib "d4dll.dll" (ByVal d4&, ByVal tPtr&)
    Declare Function d4top% Lib "d4dll.dll" (ByVal d4&)
    Declare Function d4unlock% Lib "d4dll.dll" (ByVal d4&)
    Declare Function d4unlockFiles% Lib "d4dll.dll" Alias "code4unlock" (ByVal d4&)
    Declare Function d4write% Lib "d4dll.dll" Alias "d4writeVB" (ByVal d4&, ByVal RecNum&)
    Declare Function d4writeBuffer& Lib "d4dll.dll" (ByVal d4&, ByVal numRecsToBuf&)
    Declare Function d4zap% Lib "d4dll.dll" (ByVal d4&, ByVal StartRecord&, ByVal EndRecord&)
#Else
    Declare Function d4aliasCB& Lib "c4dll.dll" Alias "d4alias" (ByVal d4&)
    Declare Sub d4aliasSet Lib "c4dll.dll" (ByVal d4&, ByVal AliasValue$)
    Declare Function d4append% Lib "c4dll.dll" (ByVal d4&)
    Declare Function d4appendBlank% Lib "c4dll.dll" (ByVal d4&)
    Declare Function d4appendStart% Lib "c4dll.dll" (ByVal d4&, ByVal UseMemoEntries%)
    Declare Sub d4blank Lib "c4dll.dll" (ByVal d4&)
    Declare Function d4bof% Lib "c4dll.dll" (ByVal d4&)
    Declare Function d4bottom% Lib "c4dll.dll" (ByVal d4&)
    Declare Function d4changed% Lib "c4dll.dll" (ByVal d4&, ByVal intFlag%)
    Declare Function d4check% Lib "c4dll.dll" (ByVal d4&)
    Declare Function d4close% Lib "c4dll.dll" (ByVal d4&)
    Declare Function d4codePage% Lib "c4dll.dll" (ByVal d4&)
    Declare Function d4createCB& Lib "c4dll.dll" Alias "d4create" (ByVal c4&, ByVal DbfName$, ByVal fieldinfo&, ByVal tagInfo&)
    Declare Function d4createLow& Lib "c4dll.dll" Alias "d4create" (ByVal c4&, ByVal DbfName$, fieldinfo As Any, tagInfo As Any)
    Declare Sub d4delete Lib "c4dll.dll" (ByVal d4&)
    Declare Function d4deleted% Lib "c4dll.dll" (ByVal d4&)
    Declare Function d4eof% Lib "c4dll.dll" (ByVal d4&)
    Declare Function d4field& Lib "c4dll.dll" (ByVal d4&, ByVal FieldName$)
    Declare Function d4fieldInfo& Lib "c4dll.dll" (ByVal d4&)
    Declare Function d4fieldJ& Lib "c4dll.dll" (ByVal d4&, ByVal JField%)
    Declare Function d4fieldNumber% Lib "c4dll.dll" (ByVal d4&, ByVal FieldName$)
    Declare Function d4fieldsAddCB& Lib "c4dll.dll" Alias "d4fieldsAdd" (ByVal d4&, ByVal nFields%, fieldinfo As Any)
    Declare Function d4fieldsRemoveCB& Lib "c4dll.dll" Alias "d4fieldsRemove" (ByRef d4&, ByVal nFields%, fieldNames As Any)
    Declare Function d4fileNameCB& Lib "c4dll.dll" Alias "d4fileName" (ByVal d4&)
    Declare Function d4flush% Lib "c4dll.dll" (ByVal d4&)
    Declare Function d4freeBlocks% Lib "c4dll.dll" (ByVal d4&)
    Declare Function d4goLow% Lib "c4dll.dll" (ByVal d4&, ByVal RecNum&, ByVal goForWrite%)
    Declare Function d4goBof% Lib "c4dll.dll" (ByVal d4&)
    Declare Function d4goEof% Lib "c4dll.dll" (ByVal d4&)
    Declare Function d4index& Lib "c4dll.dll" (ByVal d4&, ByVal IndexName$)
    Declare Function d4log% Lib "c4dll.dll" Alias "d4logVB" (ByVal d4&, ByVal logging%)
    Declare Function d4lock% Lib "c4dll.dll" Alias "d4lockVB" (ByVal d4&, ByVal recordNum&)
    Declare Function d4lockAdd% Lib "c4dll.dll" (ByVal d4&, ByVal recordNum&)
    Declare Function d4lockAddAll% Lib "c4dll.dll" (ByVal d4&)
    Declare Function d4lockAddAppend% Lib "c4dll.dll" (ByVal d4&)
    Declare Function d4lockAddFile% Lib "c4dll.dll" (ByVal d4&)
    Declare Function d4lockAll% Lib "c4dll.dll" Alias "d4lockAllVB" (ByVal d4&)
    Declare Function d4lockAppend% Lib "c4dll.dll" Alias "d4lockAppendVB" (ByVal d4&)
    Declare Function d4lockFile% Lib "c4dll.dll" Alias "d4lockFileVB" (ByVal d4&)
    Declare Function d4logStatus% Lib "c4dll.dll" Alias "d4logStatusCB" (ByVal d4&)
    Declare Function d4memoCompress% Lib "c4dll.dll" (ByVal d4&)
    Declare Function d4numFields% Lib "c4dll.dll" (ByVal d4&)
    Declare Function d4open& Lib "c4dll.dll" (ByVal c4&, ByVal DbfName$)
    Declare Function d4openClone& Lib "c4dll.dll" (ByVal d4&)
    Declare Function d4optimize% Lib "c4dll.dll" Alias "d4optimizeVB" (ByVal d4&, ByVal OptFlag%)
    Declare Function d4optimizeWrite% Lib "c4dll.dll" Alias "d4optimizeWriteVB" (ByVal d4&, ByVal OptFlag%)
    Declare Function d4pack% Lib "c4dll.dll" (ByVal d4&)
    Declare Function d4packData% Lib "c4dll.dll" (ByVal d4&)
    Declare Function d4packWithProgress% Lib "c4dll.dll" (ByVal d4&, ByVal callback&, ByVal milliseconds&)
    Declare Function d4position# Lib "c4dll.dll" (ByVal d4&)
    Declare Function d4positionSet% Lib "c4dll.dll" (ByVal d4&, ByVal Percentage#)
    Declare Sub d4recall Lib "c4dll.dll" (ByVal d4&)
    Declare Function d4readBuffer& Lib "c4dll.dll" (ByVal d4&, ByVal numRecsToBuf&, ByVal doMemos%)
    Declare Function d4recCount& Lib "c4dll.dll" Alias "d4recCountDo" (ByVal d4&)
    Declare Function d4recNo& Lib "c4dll.dll" Alias "d4recNoLow" (ByVal d4&)
    Declare Function d4record& Lib "c4dll.dll" Alias "d4recordLow" (ByVal d4&)
    Declare Function d4recWidth& Lib "c4dll.dll" Alias "d4recWidth_v" (ByVal d4&)
    Declare Function d4remove% Lib "c4dll.dll" (ByVal d4&)
    Declare Function d4refresh% Lib "c4dll.dll" (ByVal d4&)
    Declare Function d4refreshRecord% Lib "c4dll.dll" (ByVal d4&)
    Declare Function d4reindex% Lib "c4dll.dll" (ByVal d4&)
    Declare Function d4reindexWithProgress% Lib "c4dll.dll" (ByVal d4&, ByVal callback&, ByVal milliseconds&)
    Declare Function d4seek% Lib "c4dll.dll" (ByVal d4&, ByVal seekValue$)
    Declare Function d4seekDouble% Lib "c4dll.dll" (ByVal d4&, ByVal value#)
    Declare Function d4seekN% Lib "c4dll.dll" (ByVal d4&, ByVal seekValue$, ByVal seekLen%)
    Declare Function d4seekNext% Lib "c4dll.dll" (ByVal d4&, ByVal seekValue$)
    Declare Function d4seekNextDouble% Lib "c4dll.dll" (ByVal d4&, ByVal seekValue#)
    Declare Function d4seekNextN% Lib "c4dll.dll" (ByVal d4&, ByVal seekValue$, ByVal seekLen%)
    Declare Function d4skip% Lib "c4dll.dll" (ByVal d4&, ByVal NumberRecords&)
    Declare Function d4tag& Lib "c4dll.dll" (ByVal d4&, ByVal TagName$)
    Declare Function d4tagDefault& Lib "c4dll.dll" (ByVal d4&)
    Declare Function d4tagNext& Lib "c4dll.dll" (ByVal d4&, ByVal TagOn&)
    Declare Function d4tagPrev& Lib "c4dll.dll" (ByVal d4&, ByVal TagOn&)
    Declare Sub d4tagSelect Lib "c4dll.dll" (ByVal d4&, ByVal tPtr&)
    Declare Function d4tagSelected& Lib "c4dll.dll" (ByVal d4&)
    Declare Function d4tagSync% Lib "c4dll.dll" (ByVal d4&, ByVal tPtr&)
    Declare Function d4top% Lib "c4dll.dll" (ByVal d4&)
    Declare Function d4unlock% Lib "c4dll.dll" (ByVal d4&)
    Declare Function d4unlockFiles% Lib "c4dll.dll" Alias "code4unlock" (ByVal d4&)
    Declare Function d4versionNumber& Lib "c4dll.dll" (ByVal d4&)
    Declare Function d4write% Lib "c4dll.dll" Alias "d4writeVB" (ByVal d4&, ByVal RecNum&)
    Declare Function d4writeBuffer& Lib "c4dll.dll" (ByVal d4&, ByVal numRecsToBuf&)
    Declare Function d4zap% Lib "c4dll.dll" (ByVal d4&, ByVal StartRecord&, ByVal EndRecord&)
#End If

'===============================================================================================
'
'                                   Date Functions' Prototypes
'
'-----------------------------------------------------------------------------------------------
#If USE_D4DLL Then
    Declare Sub date4assignLow Lib "d4dll.dll" (ByVal dateForm$, ByVal julianDay&, ByVal isOle%)
    Declare Function date4cdowCB& Lib "d4dll.dll" Alias "date4cdow" (ByVal dateForm$)
    Declare Function date4cmonthCB& Lib "d4dll.dll" Alias "date4cmonth" (ByVal dateForm$)
    Declare Function date4day% Lib "d4dll.dll" Alias "date4day_v" (ByVal dateForm$)
    Declare Function date4dow% Lib "d4dll.dll" (ByVal dateForm$)
    Declare Sub date4formatCB Lib "d4dll.dll" Alias "date4format" (ByVal dateForm$, ByVal Result$, ByVal pic$)
    Declare Sub date4initCB Lib "d4dll.dll" Alias "date4init" (ByVal dateForm$, ByVal value$, ByVal pic$)
    Declare Function date4isLeap% Lib "d4dll.dll" (ByVal dateForm$)
    Declare Function date4long& Lib "d4dll.dll" (ByVal dateForm$)
    Declare Function date4month% Lib "d4dll.dll" Alias "date4month_v" (ByVal dateForm$)
    Declare Sub date4timeNow Lib "d4dll.dll" (ByVal TimeForm$)
    Declare Sub date4todayCB Lib "d4dll.dll" Alias "date4today" (ByVal dateForm$)
    Declare Function date4year% Lib "d4dll.dll" Alias "date4year_v" (ByVal dateForm$)
#Else
    Declare Sub date4assignLow Lib "c4dll.dll" (ByVal dateForm$, ByVal julianDay&, ByVal isOle%)
    Declare Function date4cdowCB& Lib "c4dll.dll" Alias "date4cdow" (ByVal dateForm$)
    Declare Function date4cmonthCB& Lib "c4dll.dll" Alias "date4cmonth" (ByVal dateForm$)
    Declare Function date4day% Lib "c4dll.dll" Alias "date4day_v" (ByVal dateForm$)
    Declare Function date4dow% Lib "c4dll.dll" (ByVal dateForm$)
    Declare Sub date4formatCB Lib "c4dll.dll" Alias "date4format" (ByVal dateForm$, ByVal Result$, ByVal pic$)
    Declare Sub date4initCB Lib "c4dll.dll" Alias "date4init" (ByVal dateForm$, ByVal value$, ByVal pic$)
    Declare Function date4isLeap% Lib "c4dll.dll" (ByVal dateForm$)
    Declare Function date4long& Lib "c4dll.dll" (ByVal dateForm$)
    Declare Function date4month% Lib "c4dll.dll" Alias "date4month_v" (ByVal dateForm$)
    Declare Sub date4timeNow Lib "c4dll.dll" (ByVal TimeForm$)
    Declare Sub date4todayCB Lib "c4dll.dll" Alias "date4today" (ByVal dateForm$)
    Declare Function date4year% Lib "c4dll.dll" Alias "date4year_v" (ByVal dateForm$)
#End If

'===============================================================================================
'
'                          Error  Functions' Prototypes
'
'-----------------------------------------------------------------------------------------------
#If USE_D4DLL Then
    Declare Function error4% Lib "d4dll.dll" Alias "error4VB" (ByVal c4&, ByVal errCode%, ByVal extraInfo&)
    Declare Sub error4callback Lib "d4dll.dll" (ByVal c4&, ByVal callback&)
    Declare Sub error4exitTest Lib "d4dll.dll" (ByVal c4&)
    Declare Function error4describe% Lib "d4dll.dll" Alias "error4describeVB" (ByVal c4&, ByVal errCode%, ByVal extraInfo&, ByVal desc1$, ByVal desc2$, ByVal desc3$)
    Declare Function error4file% Lib "d4dll.dll" (ByVal c4&, ByVal fileName$, ByVal overwrite%)
    Declare Function error4lastDescriptionVB& Lib "d4dll.dll" Alias "error4lastDescription" (ByVal c4&)
    Declare Function error4lastDescriptionVBLow& Lib "d4dll.dll" Alias "error4lastDescriptionLow" (ByVal c4&)
    Declare Function error4set% Lib "d4dll.dll" (ByVal c4&, ByVal errCode%)
    Declare Function error4textCB& Lib "d4dll.dll" Alias "error4text" (ByVal c4&, ByVal errCode&)
#Else
    Declare Function error4% Lib "c4dll.dll" Alias "error4VB" (ByVal c4&, ByVal errCode%, ByVal extraInfo&)
    Declare Sub error4callback Lib "c4dll.dll" (ByVal c4&, ByVal callback&)
    Declare Sub error4exitTest Lib "c4dll.dll" (ByVal c4&)
    Declare Function error4describe% Lib "c4dll.dll" Alias "error4describeVB" (ByVal c4&, ByVal errCode%, ByVal extraInfo&, ByVal desc1$, ByVal desc2$, ByVal desc3$)
    Declare Function error4file% Lib "c4dll.dll" (ByVal c4&, ByVal fileName$, ByVal overwrite%)
    Declare Function error4lastDescriptionVB& Lib "c4dll.dll" Alias "error4lastDescription" (ByVal c4&)
    Declare Function error4set% Lib "c4dll.dll" (ByVal c4&, ByVal errCode%)
    Declare Function error4textCB& Lib "c4dll.dll" Alias "error4text" (ByVal c4&, ByVal errCode&)
#End If

'===============================================================================================
'
'                          Expression Evaluation Functions' Prototypes
'
'-----------------------------------------------------------------------------------------------
#If USE_D4DLL Then
    Declare Function expr4data& Lib "d4dll.dll" Alias "expr4dataCB" (ByVal exprPtr&)
    Declare Function expr4double# Lib "d4dll.dll" (ByVal exprPtr&)
    Declare Sub expr4free Lib "d4dll.dll" Alias "expr4freeCB" (ByVal exprPtr&)
    Declare Function expr4len& Lib "d4dll.dll" Alias "expr4lenCB" (ByVal exprPtr&)
    Declare Function expr4nullLow% Lib "d4dll.dll" (ByVal exprPtr&, ByVal forAdd%)
    Declare Function expr4parse& Lib "d4dll.dll" Alias "expr4parseCB" (ByVal d4&, ByVal expression$)
    Declare Function expr4sourceCB& Lib "d4dll.dll" Alias "expr4source" (ByVal exprPtr&)
    Declare Function expr4strCB& Lib "d4dll.dll" Alias "expr4str" (ByVal exprPtr&)
    Declare Function expr4true% Lib "d4dll.dll" (ByVal exprPtr&)
    Declare Function expr4typeCB% Lib "d4dll.dll" (ByVal exprPtr&)
#Else
    Declare Function expr4data& Lib "c4dll.dll" Alias "expr4dataCB" (ByVal exprPtr&)
    Declare Function expr4double# Lib "c4dll.dll" (ByVal exprPtr&)
    Declare Sub expr4free Lib "c4dll.dll" Alias "expr4freeCB" (ByVal exprPtr&)
    Declare Function expr4len& Lib "c4dll.dll" Alias "expr4lenCB" (ByVal exprPtr&)
    Declare Function expr4nullLow% Lib "c4dll.dll" (ByVal exprPtr&, ByVal forAdd%)
    Declare Function expr4parse& Lib "c4dll.dll" Alias "expr4parseCB" (ByVal d4&, ByVal expression$)
    Declare Function expr4sourceCB& Lib "c4dll.dll" Alias "expr4source" (ByVal exprPtr&)
    Declare Function expr4strCB& Lib "c4dll.dll" Alias "expr4str" (ByVal exprPtr&)
    Declare Function expr4true% Lib "c4dll.dll" (ByVal exprPtr&)
    Declare Function expr4typeCB% Lib "c4dll.dll" (ByVal exprPtr&)
#End If

'===============================================================================================
'
'                            Field Functions' Prototypes
'
'-----------------------------------------------------------------------------------------------
#If USE_D4DLL Then
    Declare Sub f4assignBinaryVB Lib "d4dll.dll" Alias "f4assignN" (ByVal fPtr&, ByRef value As Any, ByVal length%)
    Declare Sub f4assignChar Lib "d4dll.dll" Alias "f4assignCharVB" (ByVal fPtr&, ByVal char%)
    Declare Sub f4assignCurrency Lib "d4dll.dll" (ByVal fPtr&, ByVal value$)
    Declare Sub f4assignDateTime Lib "d4dll.dll" (ByVal fPtr&, ByVal value$)
    Declare Sub f4assignDouble Lib "d4dll.dll" (ByVal fPtr&, ByVal value#)
    Declare Sub f4assignField Lib "d4dll.dll" (ByVal fPtrTo&, ByVal fPtrFrom&)
    Declare Sub f4assignIntVB Lib "d4dll.dll" (ByVal fPtr&, ByVal value%)
    Declare Sub f4assignLong Lib "d4dll.dll" (ByVal fPtr&, ByVal value&)
    Declare Sub f4assignN Lib "d4dll.dll" Alias "f4assignNVB" (ByVal fPtr&, ByVal value$, ByVal length%)
    Declare Sub f4assignNull Lib "d4dll.dll" (ByVal fPtr&)
    Declare Sub f4assignUnicodeVB Lib "d4dll.dll" Alias "f4assignUnicode" (ByVal fPtr&, ByRef value As Any)
    Declare Sub f4blank Lib "d4dll.dll" (ByVal fPtr&)
    Declare Function f4char% Lib "d4dll.dll" (ByVal fPtr&)
    Declare Function f4currencyCB& Lib "d4dll.dll" Alias "f4currency" (ByVal fPtr&, ByVal numDec%)
    Declare Function f4data& Lib "d4dll.dll" (ByVal fPtr&)
    Declare Function f4dateTimeCB& Lib "d4dll.dll" Alias "f4dateTime" (ByVal fPtr&)
    Declare Function f4decimals% Lib "d4dll.dll" (ByVal fPtr&)
    Declare Function f4double# Lib "d4dll.dll" (ByVal fPtr&)
    Declare Function f4int% Lib "d4dll.dll" (ByVal fPtr&)
    Declare Function f4len% Lib "d4dll.dll" Alias "f4len_v" (ByVal fPtr&)
    Declare Function f4long& Lib "d4dll.dll" (ByVal fPtr&)
    Declare Function f4memoAssign% Lib "d4dll.dll" (ByVal fPtr&, ByVal value$)
    Declare Function f4memoAssignBinaryVB% Lib "d4dll.dll" Alias "f4memoAssignNVB" (ByVal fPtr&, ByRef value As Any, ByVal length&)
    #If Win32 Then
        Declare Function f4memoAssignN% Lib "d4dll.dll" (ByVal fPtr&, ByVal value$, ByVal length&)
    #Else
        Declare Function f4memoAssignN% Lib "d4dll.dll" (ByVal fPtr&, ByVal value$, ByVal length%)
    #End If
    Declare Sub f4memoAssignUnicodeVB Lib "d4dll.dll" Alias "f4memoAssignUnicode" (ByVal fPtr&, ByRef value As Any)
    Declare Sub f4memoFree Lib "d4dll.dll" (ByVal fPtr&)
    Declare Function f4memoLen& Lib "d4dll.dll" (ByVal fPtr&)
    Declare Function f4memoNcpy& Lib "d4dll.dll" (ByVal fPtr&, ByVal memPtr$, ByVal memLen&)
    Declare Function f4memoNcpyBinary& Lib "d4dll.dll" Alias "f4memoNcpy" (ByVal fPtr&, ByRef memPtr As Any, ByVal memLen&)
    Declare Function f4memoPtr& Lib "d4dll.dll" (ByVal fPtr&)
    Declare Function f4nameCB& Lib "d4dll.dll" Alias "f4name" (ByVal fPtr&)
    Declare Function f4ncpyBinary% Lib "d4dll.dll" Alias "f4ncpy" (ByVal fPtr&, ByRef memPtr As Any, ByVal memLength%)
    Declare Function f4ncpyCB% Lib "d4dll.dll" Alias "f4ncpy" (ByVal fPtr&, ByVal memPtr$, ByVal memLength%)
    Declare Function f4number% Lib "d4dll.dll" (ByVal fPtr&)
    Declare Function f4null% Lib "d4dll.dll" (ByVal fPtr&)
    Declare Function f4ptr& Lib "d4dll.dll" (ByVal fPtr&)
    Declare Function f4strCB& Lib "d4dll.dll" Alias "f4str" (ByVal fPtr&)
    Declare Function f4true% Lib "d4dll.dll" (ByVal fPtr&)
    Declare Function f4type% Lib "d4dll.dll" (ByVal fPtr&)
#Else
    Declare Sub f4assignBinaryVB Lib "c4dll.dll" Alias "f4assignN" (ByVal fPtr&, ByRef value As Any, ByVal length%)
    Declare Sub f4assignChar Lib "c4dll.dll" Alias "f4assignCharVB" (ByVal fPtr&, ByVal char%)
    Declare Sub f4assignCurrency Lib "c4dll.dll" (ByVal fPtr&, ByVal value$)
    Declare Sub f4assignDateTime Lib "c4dll.dll" (ByVal fPtr&, ByVal value$)
    Declare Sub f4assignDouble Lib "c4dll.dll" (ByVal fPtr&, ByVal value#)
    Declare Sub f4assignField Lib "c4dll.dll" (ByVal fPtrTo&, ByVal fPtrFrom&)
    Declare Sub f4assignIntVB Lib "c4dll.dll" (ByVal fPtr&, ByVal value%)
    Declare Sub f4assignLong Lib "c4dll.dll" (ByVal fPtr&, ByVal value&)
    Declare Sub f4assignN Lib "c4dll.dll" Alias "f4assignNVB" (ByVal fPtr&, ByVal value$, ByVal length%)
    Declare Sub f4assignNull Lib "c4dll.dll" (ByVal fPtr&)
    Declare Sub f4assignUnicodeVB Lib "c4dll.dll" Alias "f4assignUnicode" (ByVal fPtr&, ByRef value As Any)
    Declare Sub f4blank Lib "c4dll.dll" (ByVal fPtr&)
    Declare Function f4char% Lib "c4dll.dll" (ByVal fPtr&)
    Declare Function f4currencyCB& Lib "c4dll.dll" Alias "f4currency" (ByVal fPtr&, ByVal numDec%)
    Declare Function f4data& Lib "c4dll.dll" (ByVal fPtr&)
    Declare Function f4dateTimeCB& Lib "c4dll.dll" Alias "f4dateTime" (ByVal fPtr&)
    Declare Function f4decimals% Lib "c4dll.dll" (ByVal fPtr&)
    Declare Function f4double# Lib "c4dll.dll" (ByVal fPtr&)
    Declare Function f4int% Lib "c4dll.dll" (ByVal fPtr&)
    Declare Function f4len% Lib "c4dll.dll" Alias "f4len_v" (ByVal fPtr&)
    Declare Function f4long& Lib "c4dll.dll" (ByVal fPtr&)
    Declare Function f4memoAssign% Lib "c4dll.dll" (ByVal fPtr&, ByVal value$)
    Declare Function f4memoAssignBinaryVB% Lib "c4dll.dll" Alias "f4memoAssignNVB" (ByVal fPtr&, ByRef value As Any, ByVal length&)
    #If Win32 Then
        Declare Function f4memoAssignN% Lib "c4dll.dll" (ByVal fPtr&, ByVal value$, ByVal length&)
    #Else
        Declare Function f4memoAssignN% Lib "c4dll.dll" (ByVal fPtr&, ByVal value$, ByVal length%)
    #End If
    Declare Sub f4memoAssignUnicodeVB Lib "c4dll.dll" Alias "f4memoAssignUnicode" (ByVal fPtr&, ByRef value As Any)
    Declare Sub f4memoFree Lib "c4dll.dll" (ByVal fPtr&)
    Declare Function f4memoLen& Lib "c4dll.dll" (ByVal fPtr&)
    Declare Function f4memoNcpy& Lib "c4dll.dll" (ByVal fPtr&, ByVal memPtr$, ByVal memLen&)
    Declare Function f4memoNcpyBinary& Lib "c4dll.dll" Alias "f4memoNcpy" (ByVal fPtr&, ByRef memPtr As Any, ByVal memLen&)
    Declare Function f4memoPtr& Lib "c4dll.dll" (ByVal fPtr&)
    Declare Function f4nameCB& Lib "c4dll.dll" Alias "f4name" (ByVal fPtr&)
    Declare Function f4ncpyBinary% Lib "c4dll.dll" Alias "f4ncpy" (ByVal fPtr&, ByRef memPtr As Any, ByVal memLength%)
    Declare Function f4ncpyCB% Lib "c4dll.dll" Alias "f4ncpy" (ByVal fPtr&, ByVal memPtr$, ByVal memLength%)
    Declare Function f4number% Lib "c4dll.dll" (ByVal fPtr&)
    Declare Function f4null% Lib "c4dll.dll" (ByVal fPtr&)
    Declare Function f4ptr& Lib "c4dll.dll" (ByVal fPtr&)
    Declare Function f4strCB& Lib "c4dll.dll" Alias "f4str" (ByVal fPtr&)
    Declare Function f4true% Lib "c4dll.dll" (ByVal fPtr&)
    Declare Function f4type% Lib "c4dll.dll" (ByVal fPtr&)
#End If

'===============================================================================================
'
'                             Index Functions' Prototypes
'
'-----------------------------------------------------------------------------------------------
#If USE_D4DLL Then
    Declare Function i4close% Lib "d4dll.dll" (ByVal i4&)
    Declare Function i4createCB& Lib "d4dll.dll" Alias "i4create" (ByVal d4&, ByVal fileName As Any, tagInfo As TAG4INFOCB)
    Declare Function i4fileNameCB& Lib "d4dll.dll" Alias "i4fileName" (ByVal i4&)
    Declare Function i4openCB& Lib "d4dll.dll" Alias "i4open" (ByVal d4&, ByVal fileName As Any)
    Declare Function i4reindex% Lib "d4dll.dll" (ByVal i4&)
    Declare Function i4tag& Lib "d4dll.dll" (ByVal i4&, ByVal fileName$)
    Declare Function i4tagInfo& Lib "d4dll.dll" (ByVal i4&)
    Declare Function i4tagAddCB% Lib "d4dll.dll" Alias "i4tagAdd" (ByVal i4&, tagInfo As TAG4INFOCB)
    Declare Function i4tagRemove% Lib "d4dll.dll" (ByVal t4&)
#Else
    Declare Function i4close% Lib "c4dll.dll" (ByVal i4&)
    Declare Function i4createCB& Lib "c4dll.dll" Alias "i4create" (ByVal d4&, ByVal fileName As Any, tagInfo As TAG4INFOCB)
    Declare Function i4fileNameCB& Lib "c4dll.dll" Alias "i4fileName" (ByVal i4&)
    Declare Function i4openCB& Lib "c4dll.dll" Alias "i4open" (ByVal d4&, ByVal fileName As Any)
    Declare Function i4reindex% Lib "c4dll.dll" (ByVal i4&)
    Declare Function i4tag& Lib "c4dll.dll" (ByVal i4&, ByVal fileName$)
    Declare Function i4tagInfo& Lib "c4dll.dll" (ByVal i4&)
    Declare Function i4tagAddCB% Lib "c4dll.dll" Alias "i4tagAdd" (ByVal i4&, tagInfo As TAG4INFOCB)
    Declare Function i4tagRemove% Lib "c4dll.dll" (ByVal t4&)
#End If

'===============================================================================================
'
'                            Relate Functions' Prototypes
'
'-----------------------------------------------------------------------------------------------
#If USE_D4DLL Then
    Declare Function relate4bottom% Lib "d4dll.dll" (ByVal r4&)
    Declare Sub relate4changed Lib "d4dll.dll" (ByVal r4&)
    Declare Function relate4count& Lib "d4dll.dll" (ByVal r4&)
    Declare Function relate4createSlave& Lib "d4dll.dll" (ByVal r4&, ByVal d4&, ByVal mExpr$, ByVal t4 As Any)
    Declare Function relate4data& Lib "d4dll.dll" Alias "relate4dataCB" (ByVal r4&)
    Declare Function relate4dataTag& Lib "d4dll.dll" Alias "relate4dataTagCB" (ByVal r4&)
    Declare Function relate4doAll% Lib "d4dll.dll" (ByVal r4&)
    Declare Function relate4doOne% Lib "d4dll.dll" (ByVal r4&)
    Declare Function relate4eof% Lib "d4dll.dll" (ByVal r4&)
    Declare Function relate4errorAction% Lib "d4dll.dll" Alias "relate4errorActionVB" (ByVal r4&, ByVal ErrAction%)
    Declare Function relate4free% Lib "d4dll.dll" Alias "relate4freeVB" (ByVal r4&, ByVal CloseFlag%)
    Declare Function relate4init& Lib "d4dll.dll" (ByVal d4&)
    Declare Function relate4lockAdd% Lib "d4dll.dll" (ByVal r4&)
    Declare Function relate4master& Lib "d4dll.dll" Alias "relate4masterCB" (ByVal r4&)
    Declare Function relate4masterExprCB& Lib "d4dll.dll" (ByVal r4&)
    Declare Function relate4matchLen% Lib "d4dll.dll" Alias "relate4matchLenVB" (ByVal r4&, ByVal length%)
    Declare Function relate4next% Lib "d4dll.dll" (r4&)
    Declare Function relate4optimizeable% Lib "d4dll.dll" (ByVal r4&)
    Declare Function relate4querySet% Lib "d4dll.dll" (ByVal r4&, ByVal expr As String)
    Declare Function relate4readBuffer& Lib "d4dll.dll" (ByVal rel4&, ByVal numRecsToBuf&, ByVal doMemos as Integer)
    Declare Function relate4retain% Lib "d4dll.dll" (ByVal r4&, ByVal flag%)
    Declare Function relate4retrieve& Lib "d4dll.dll" (ByVal c4&, ByVal fileName$, ByVal openFiles%, ByVal dataPathName$)
    Declare Function relate4save% Lib "d4dll.dll" (ByVal rel4&, ByVal fileName$, ByVal savePathNames%)
    Declare Function relate4skip% Lib "d4dll.dll" (ByVal r4&, ByVal NumRecs&)
    Declare Function relate4skipEnable% Lib "d4dll.dll" Alias "relate4skipEnableVB" (ByVal r4&, ByVal DoEnable%)
    Declare Function relate4skipMaster% Lib "d4dll.dll" (ByVal r4&, ByVal NumRecs&)
    Declare Function relate4sortSet% Lib "d4dll.dll" (ByVal r4&, ByVal expr As String)
    Declare Function relate4top% Lib "d4dll.dll" (ByVal r4&)
    Declare Function relate4topMaster& Lib "d4dll.dll" (ByVal r4&)
    Declare Function relate4type% Lib "d4dll.dll" Alias "relate4typeVB" (ByVal r4&, ByVal rType%)
#Else
    Declare Function relate4bottom% Lib "c4dll.dll" (ByVal r4&)
    Declare Sub relate4changed Lib "c4dll.dll" (ByVal r4&)
    Declare Function relate4count& Lib "c4dll.dll" (ByVal r4&)
    Declare Function relate4createSlave& Lib "c4dll.dll" (ByVal r4&, ByVal d4&, ByVal mExpr$, ByVal t4 As Any)
    Declare Function relate4data& Lib "c4dll.dll" Alias "relate4dataCB" (ByVal r4&)
    Declare Function relate4dataTag& Lib "c4dll.dll" Alias "relate4dataTagCB" (ByVal r4&)
    Declare Function relate4doAll% Lib "c4dll.dll" (ByVal r4&)
    Declare Function relate4doOne% Lib "c4dll.dll" (ByVal r4&)
    Declare Function relate4eof% Lib "c4dll.dll" (ByVal r4&)
    Declare Function relate4errorAction% Lib "c4dll.dll" Alias "relate4errorActionVB" (ByVal r4&, ByVal ErrAction%)
    Declare Function relate4free% Lib "c4dll.dll" Alias "relate4freeVB" (ByVal r4&, ByVal CloseFlag%)
    Declare Function relate4init& Lib "c4dll.dll" (ByVal d4&)
    Declare Function relate4lockAdd% Lib "c4dll.dll" (ByVal r4&)
    Declare Function relate4master& Lib "c4dll.dll" Alias "relate4masterCB" (ByVal r4&)
    Declare Function relate4masterExprCB& Lib "c4dll.dll" (ByVal r4&)
    Declare Function relate4matchLen% Lib "c4dll.dll" Alias "relate4matchLenVB" (ByVal r4&, ByVal length%)
    Declare Function relate4next% Lib "c4dll.dll" (r4&)
    Declare Function relate4optimizeable% Lib "c4dll.dll" (ByVal r4&)
    Declare Function relate4querySet% Lib "c4dll.dll" (ByVal r4&, ByVal expr As String)
    Declare Function relate4readBuffer& Lib "c4dll.dll" (ByVal rel4&, ByVal numRecsToBuf&, ByVal doMemos as Integer)
    Declare Function relate4retain% Lib "c4dll.dll" (ByVal rel4&, ByVal flag%)
    Declare Function relate4retrieve& Lib "c4dll.dll" (ByVal c4&, ByVal fileName$, ByVal openFiles%, ByVal dataPathName$)
    Declare Function relate4save% Lib "c4dll.dll" (ByVal rel4&, ByVal fileName$, ByVal savePathNames%)
    Declare Function relate4skip% Lib "c4dll.dll" (ByVal r4&, ByVal NumRecs&)
    Declare Function relate4skipEnable% Lib "c4dll.dll" Alias "relate4skipEnableVB" (ByVal r4&, ByVal DoEnable%)
    Declare Function relate4skipMaster% Lib "c4dll.dll" (ByVal r4&, ByVal NumRecs&)
    Declare Function relate4sortSet% Lib "c4dll.dll" (ByVal r4&, ByVal expr As String)
    Declare Function relate4top% Lib "c4dll.dll" (ByVal r4&)
    Declare Function relate4topMaster& Lib "c4dll.dll" (ByVal r4&)
    Declare Function relate4type% Lib "c4dll.dll" Alias "relate4typeVB" (ByVal r4&, ByVal rType%)
#End If

'===============================================================================================
'
'  Report function prototypes
'
'================================================================================================
Declare Function report4caption% Lib "c4dll.dll" (ByVal r4&, ByVal caption$)
Declare Function report4currency% Lib "c4dll.dll" (ByVal r4&, ByVal currncy$)
Declare Function report4dataDo% Lib "c4dll.dll" (ByVal r4&)
Declare Function report4dataFileSet% Lib "c4dll.dll" (ByVal r4&, ByVal destFile$)
Declare Function report4dateFormat% Lib "c4dll.dll" (ByVal r4&, ByVal dateFmt$)
Declare Function report4decimal% Lib "c4dll.dll" Alias "report4decimal_v" (ByVal r4&, ByVal decChar$)
Declare Function report4do% Lib "c4dll.dll" Alias "report4doCB" (ByVal r4&)
Declare Sub report4freeLow Lib "c4dll.dll" (ByVal r4&, ByVal freeRelate%, ByVal closeFiles%, ByVal doPrinterFree%)
Declare Function report4margins% Lib "c4dll.dll" (ByVal r4&, ByVal mLeft&, ByVal mRight&, ByVal mTop&, ByVal mBottom&, ByVal uType%)
Declare Function report4pageSize% Lib "c4dll.dll" (ByVal r4&, ByVal pHeight&, ByVal pWidth&, ByVal uType%)
#If Win16 Then
   Declare Function report4parent16% Lib "c4dll.dll" Alias "report4parent" (ByVal r4&, ByVal hWnd%)
#End If
#If Win32 Then
   Declare Function report4parent32% Lib "c4dll.dll" Alias "report4parent" (ByVal r4&, ByVal hWnd&)
#End If
Declare Sub report4printerSelect Lib "c4dll.dll" (ByVal r4&)
Declare Function report4querySet% Lib "c4dll.dll" (ByVal r4&, ByVal queryExpr$)
Declare Function report4relate& Lib "c4dll.dll" (ByVal r4&)
Declare Function report4retrieve& Lib "c4dll.dll" (ByVal c4&, ByVal fileName$, ByVal openFiles%, ByVal dataPath$)
Declare Function report4save% Lib "c4dll.dll" (ByVal r4&, ByVal fileName$, ByVal savePaths%)
Declare Function report4screenBreaks% Lib "c4dll.dll" (ByVal r4&, ByVal value%)
Declare Function report4separator% Lib "c4dll.dll" Alias "report4separator_v" (ByVal r4&, ByVal separator$)
Declare Function report4sortSet% Lib "c4dll.dll" (ByVal r4&, ByVal sortExpr$)
Declare Function report4toScreen% Lib "c4dll.dll" (ByVal r4&, ByVal toScreen%)


'===============================================================================================
'
'                            Tag Functions' Prototypes
'
'-----------------------------------------------------------------------------------------------
#If USE_D4DLL Then
    Declare Function t4aliasCB& Lib "d4dll.dll" Alias "t4alias" (ByVal t4&)
    Declare Function t4close% Lib "d4dll.dll" (ByVal t4&)
    Declare Function t4descending% Lib "d4dll.dll" (ByVal t4&)
    Declare Function t4exprCB& Lib "d4dll.dll" (ByVal t4&)
    Declare Function t4filterCB& Lib "d4dll.dll" (ByVal t4&)
    Declare Function t4open& Lib "d4dll.dll" Alias "t4openCB" (ByVal dbPtr&, ByVal IndexName$)
    Declare Function t4seekN% Lib "d4dll.dll" (ByVal t4&, ByVal seekValue$, ByVal seekLen%, ByVal doDataPosition%)
    Declare Function t4unique% Lib "d4dll.dll" (ByVal t4&)
    Declare Function t4uniqueSet% Lib "d4dll.dll" Alias "t4uniqueSetVB" (ByVal t4&, ByVal value%)
#Else
    Declare Function t4aliasCB& Lib "c4dll.dll" Alias "t4alias" (ByVal t4&)
    Declare Function t4close% Lib "c4dll.dll" (ByVal t4&)
    Declare Function t4descending% Lib "c4dll.dll" (ByVal t4&)
    Declare Function t4exprCB& Lib "c4dll.dll" (ByVal t4&)
    Declare Function t4filterCB& Lib "c4dll.dll" (ByVal t4&)
    Declare Function t4open& Lib "c4dll.dll" Alias "t4openCB" (ByVal dbPtr&, ByVal IndexName$)
    Declare Function t4seekN% Lib "c4dll.dll" (ByVal t4&, ByVal seekValue$, ByVal seekLen%, ByVal doDataPosition%)
    Declare Function t4unique% Lib "c4dll.dll" (ByVal t4&)
    Declare Function t4uniqueSet% Lib "c4dll.dll" Alias "t4uniqueSetVB" (ByVal t4&, ByVal value%)
#End If

'=======================================================================================
'
'                Utility function prototypes
'
'-----------------------------------------------------------------------------------------------
#If USE_D4DLL Then
    Declare Function u4alloc& Lib "d4dll.dll" Alias "u4allocDefault" (ByVal amt&)
    Declare Function u4allocFree& Lib "d4dll.dll" Alias "u4allocFreeDefault" (ByVal c4&, ByVal amt&)
    Declare Sub u4free Lib "d4dll.dll" Alias "u4freeDefault" (ByVal memPtr&)

    Declare Function u4ncpy& Lib "d4dll.dll" (ByVal MemPtr1$, ByVal memptr2&, ByVal memLength&)
    Declare Function u4ncpy2& Lib "d4dll.dll" Alias "u4ncpy" (ByVal MemPtr1&, ByVal memptr2$, ByVal memLength&)

    Declare Sub u4memCpy Lib "d4dll.dll" (ByVal dest$, ByVal source&, ByVal numCopy&)
    Declare Function u4switch& Lib "d4dll.dll" ()
#Else
    Declare Function u4alloc& Lib "c4dll.dll" Alias "u4allocDefault" (ByVal amt&)
    Declare Function u4allocFree& Lib "c4dll.dll" Alias "u4allocFreeDefault" (ByVal c4&, ByVal amt&)
    Declare Sub u4free Lib "c4dll.dll" Alias "u4freeDefault" (ByVal memPtr&)

    Declare Function u4ncpy& Lib "c4dll.dll" (ByVal MemPtr1$, ByVal memptr2&, ByVal memLength&)
    Declare Function u4ncpy2& Lib "c4dll.dll" Alias "u4ncpy" (ByVal MemPtr1&, ByVal memptr2$, ByVal memLength&)

    Declare Sub u4memCpy Lib "c4dll.dll" (ByVal dest$, ByVal source&, ByVal numCopy&)
    Declare Function u4switch& Lib "c4dll.dll" ()
#End If

'=======================================================================================
'
'                Misc. function prototypes
'
'========================================================================================

#If USE_D4DLL Then
    Declare Function v4Cstring& Lib "d4dll.dll" (ByVal s$)
    Declare Sub v4Cstringfree Lib "d4dll.dll" (ByVal s&)
#Else
    Declare Function v4Cstring& Lib "c4dll.dll" (ByVal s$)
    Declare Sub v4Cstringfree Lib "c4dll.dll" (ByVal s&)
#End If

Private Declare Function lstrcat Lib "kernel32" Alias "lstrcatA" (ByVal dest As String, ByVal src As String) As Long

'CodeBase Return Code Constants

Global Const r4success% = 0
Global Const r4same = 0
Global Const r4found% = 1
Global Const r4down = 1
Global Const r4after = 2
Global Const r4complete = 2
Global Const r4eof = 3
Global Const r4bof = 4
Global Const r4entry = 5
Global Const r4descending = 10
Global Const r4unique = 20
Global Const r4uniqueContinue = 25
Global Const r4locked = 50
Global Const r4noCreate = 60
Global Const r4noOpen = 70
Global Const r4notag = 80
Global Const r4terminate = 90
Global Const r4inactive = 110
Global Const r4active = 120
Global Const r4authorize = 140
Global Const r4connected = 150
Global Const r4logOpen = 170
Global Const r4logOff = 180
Global Const r4null = 190
Global Const r4autoIncrement = 195
Global Const r4timeout = 225
Global Const r4connectTimeOut = 300

Global Const relate4filterRecord = 101
Global Const relate4doRemove = 102
Global Const relate4skipped = 104
Global Const relate4blank = 105
Global Const relate4skipRec = 106
Global Const relate4terminate = 107
Global Const relate4exact = 108
Global Const relate4scan = 109
Global Const relate4approx = 110
Global Const relate4sortSkip = 120
Global Const relate4sortDone = 121

'CodeBasic Field Definition Constants
Global Const r4logLen = 1
Global Const r4dateLen = 8
Global Const r4memoLen = 10
Global Const r4bin = "B"        ' Binary
Global Const r4str$ = "C"       ' Character
Global Const r4charBin$ = "Z"   ' Character (binary)
Global Const r4currency$ = "Y"  ' Currency
Global Const r4date$ = "D"      ' Date
Global Const r4dateTime$ = "T"  ' DateTime
Global Const r4double$ = "B"    ' Double
Global Const r4float$ = "F"     ' Float
Global Const r4gen$ = "G"       ' General
Global Const r4int$ = "I"       ' Integer
Global Const r4log$ = "L"       ' Logical
Global Const r4memo$ = "M"      ' Memo
Global Const r4memoBin$ = "X"   ' Memo (binary)
Global Const r4num$ = "N"       ' Numeric
Global Const r4dateDoub$ = "d"  ' Date as Double
Global Const r4numDoub$ = "n"   ' Numeric as Double
Global Const r4unicode$ = "W"   ' Unicode character (same as r5wstr)

Global Const r4system$ = "0"    ' used by FoxPro for null field value field
Global Const r5wstrLen$ = "O"
Global Const r5ui4$ = "P"
Global Const r5i2$ = "Q"
Global Const r5ui2$ = "R"
Global Const r5guid$ = "V"
Global Const r5wstr$ = "W"
Global Const r5i8$ = "1"        ' 8-byte long signed value (LONGLONG)
Global Const r5dbDate$ = "2"    ' struct DBDATE (6 bytes)
Global Const r5dbTime$ = "3"    ' struct DBTIME (6 bytes)
Global Const r5dbTimeStamp$ = "4" ' struct DBTIMESTAMP (16 bytes)
Global Const r5date$ = "5"

'Other CodeBase Constants
Global Const cp0 = 0          'code4.codePage constant
Global Const cp437 = 1
'Global Const cp850 = 2        ' CS 2006/03/14 not supported; remove
Global Const cp1250 = -56       ' LY 2003/01/08 : Eastern European; CS 2006/03/14 change from 4 to -56
Global Const cp1252 = 3
Global Const LOCK4OFF = 0
Global Const LOCK4ALL = 1
Global Const LOCK4DATA = 2
Global Const LOG4TRANS = 0
Global Const LOG4ON = 1
Global Const LOG4ALWAYS = 2
Global Const OPEN4DENY_NONE = 0
Global Const OPEN4DENY_RW = 1
Global Const OPEN4DENY_WRITE = 2
Global Const OPT4EXCLUSIVE = -1
Global Const OPT4OFF = 0
Global Const OPT4ALL = 1
Global Const r4check = -5
Global Const r4maxVBStringLen = 65500
Global Const r4maxVBStrFunction = 32767
Global Const collate4machine = 1
Global Const collate4general = 1001
Global Const collate4special = 1002
Global Const collate4spanishCp1252 = 9
Global Const collate4spanishCp850 = 10
Global Const sort4machine = 0 'code4.collatingSequence constant
Global Const sort4general = 1
Global Const sort4croatian = 2   ' LY 2003/01/08 : Croatian ordering
Global Const sort4croatianUpper = 3 ' LY 2003/01/08 : case-insensitive Croatian ordering
Global Const WAIT4EVER = -1

' Constants for code4serverOS
Global Const OS4UNKNOWN = &H0
Global Const OS4WIN32 = &H1
Global Const OS4UNIX = &H2


'CodeBasic Error Code Constants
Global Const e4close = -10
Global Const e4create = -20
Global Const e4len = -30
Global Const e4lenSet = -40
Global Const e4lock = -50
Global Const e4open = -60
Global Const e4permiss = -61
Global Const e4access = -62
Global Const e4numFiles = -63
Global Const e4fileFind = -64
Global Const e4instance = -69
Global Const e4read = -70
Global Const e4remove = -80
Global Const e4rename = -90
Global Const e4seek = -250
Global Const e4unlock = -110
Global Const e4write = -120
Global Const e4data = -200
Global Const e4fieldName = -210
Global Const e4fieldType = -220
Global Const e4recordLen = -230
Global Const e4append = -240
Global Const e4entry = -300
Global Const e4index = -310
Global Const e4tagName = -330
Global Const e4unique = -340
Global Const e4tagInfo = -350
Global Const e4commaExpected = -400
Global Const e4complete = -410
Global Const e4dataName = -420
Global Const e4lengthErr = -422
Global Const e4notConstant = -425
Global Const e4numParms = -430
Global Const e4overflow = -440
Global Const e4rightMissing = -450
Global Const e4typeSub = -460
Global Const e4unrecFunction = -470
Global Const e4unrecOperator = -480
Global Const e4unrecValue = -490
Global Const e4undetermined = -500
Global Const e4tagExpr = -510
Global Const e4opt = -610
Global Const e4optSuspend = -620
Global Const e4optFlush = -630
Global Const e4relate = -710
Global Const e4lookupErr = -720
Global Const e4relateRefer = -730
Global Const e4info = -910
Global Const e4memory = -920
Global Const e4parm = -930
Global Const e4parmNull = -935
Global Const e4demo = -940
Global Const e4result = -950
Global Const e4verify = -960
Global Const e4struct = -970
Global Const e4notSupported = -1090
Global Const e4version = -1095
Global Const e4memoCorrupt = -1110
Global Const e4memoCreate = -1120
Global Const e4transViolation = -1200
Global Const e4trans = -1210
Global Const e4rollback = -1220
Global Const e4commit = -1230
Global Const e4transAppend = -1240
Global Const e4corrupt = -1300
Global Const e4connection = -1310
Global Const e4socket = -1320
Global Const e4net = -1330
Global Const e4loadlib = -1340
Global Const e4timeOut = -1350
Global Const e4message = -1360
Global Const e4packetLen = -1370
Global Const e4packet = -1380
Global Const e4max = -1400
Global Const e4codeBase = -1410
Global Const e4name = -1420
Global Const e4authorize = -1430
Global Const e4server = -2100
Global Const e4config = -2110
Global Const e4cat = -2120

Global Const ACTION4NONE = 0
Global Const ACTION4REINDEX = 1
Global Const ACTION4INITIALIZING = 32767

'ADO Constants
Global Const e5badBinding = 200
Global Const e5conversion = 210
Global Const e5delete = 220
Global Const e5property = 230

'CodeControls Constants
Global Const CB_TOP = 1
Global Const CB_PREV = 2
Global Const CB_SEARCH = 3
Global Const CB_NEXT = 4
Global Const CB_BOTTOM = 5
Global Const CB_APPEND = 6
Global Const CB_DEL = 7
Global Const CB_UNDO = 8
Global Const CB_FLUSH = 9
Global Const CB_GO = 10

'=======================================================================================
'
'                CodeControls function prototypes
'
'========================================================================================


'CodeControls Constants

Global Const MASTER4NODATA% = 1
Global Const MASTER4NOTAG% = 2
Global Const MASTER4BADEXPR% = 3
Global Const CTRL4BADFIELD% = 4
Global Const CTRL4NOTAG% = 5
Global Const CTRL4BADEXPR% = 6
Function b4String$(p&)
    'This is a utility function for copying a 'C' string to a VB string.

    Dim s As String * 256
    Dim rc As Integer

    s$ = ""

    If p& <> 0 Then
       rc = u4ncpy(s, p, 256)
    End If

    b4String$ = Left$(s, rc)
End Function

Function code4dateFormat$(c4Ptr&)
    'This function returns the CODE4.dateFormat member
    code4dateFormat = b4String(code4dateFormatVB(c4Ptr&))
End Function

Function code4indexExtension$(c4Ptr&)
    'This function returns the CodeBase DLL index format
    code4indexExtension = b4String(code4indexExtensionVB(c4Ptr&))
End Function

Function code4lockFileName$(c4Ptr&)
'This function returns the locked file name
    code4lockFileName = b4String(code4lockFileNameVB(c4Ptr&))
End Function

Function code4lockNetworkId$(c4Ptr&)
    'This function returns the user's network id
    'who has locked the current file
    code4lockNetworkId = b4String(code4lockNetworkIdVB(c4Ptr&))
End Function

Function code4lockUserId$(c4Ptr&)
    'This function returns the user's name
    'who has locked the current file
    code4lockUserId = b4String(code4lockUserIdVB(c4Ptr&))
End Function

Function code4logFileName$(c4Ptr&)
    'This function returns the locked file name
    code4logFileName = b4String(code4lockFileNameVB(c4Ptr&))
End Function

Function d4alias$(dbPtr&)
    'This function returns the data file alias
   d4alias = b4String(d4aliasCB(dbPtr))
End Function

Function d4create&(ByVal cb&, dbname$, D() As FIELD4INFO, n() As TAG4INFO)

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


    Dim i%

    Dim flb%
    Dim fub%
    Dim fs%

    Dim tlb%
    Dim tub%
    Dim ts%


    flb = LBound(D)
    fub = UBound(D)
    fs = fub - flb + 1


    ReDim f(1 To (fs + 1)) As FIELD4INFOCB
    For i = 1 To fs
        f(i).fName = v4Cstring(D((flb - 1) + i).fName)    ' note: this function allocates memory
        f(i).ftype = Asc(D((flb - 1) + i).ftype)
        f(i).flength = D((flb - 1) + i).flength
        f(i).fdecimals = D((flb - 1) + i).fdecimals
        f(i).fnulls = D((flb - 1) + i).fnulls
    Next i

    tlb = LBound(n)
    tub = UBound(n)
    ts = tub - tlb + 1
    ReDim t(1 To (ts + 1)) As TAG4INFOCB
    For i = 1 To ts
        t(i).name = v4Cstring(n((tlb - 1) + i).name)
        t(i).expression = v4Cstring(n((tlb - 1) + i).expression)
        t(i).filter = v4Cstring(n((tlb - 1) + i).filter)
        t(i).unique = n((tlb - 1) + i).unique
        t(i).descending = n((tlb - 1) + i).descending
    Next i
    d4create = d4createLow(cb&, ByVal (dbname$), f(1), t(1))

    ' Since v4Cstring allocates memory for the storage of
    ' C strings, we must free the memory after it has been
    ' used.
    For i = 1 To fs
         Call v4Cstringfree(f(i).fName)
    Next i

    For i = 1 To ts
        Call v4Cstringfree(t(i).name)
        Call v4Cstringfree(t(i).expression)
        Call v4Cstringfree(t(i).filter)
    Next i

End Function

Function d4createData&(ByVal cb&, dbname$, D() As FIELD4INFO)

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

    Dim i%
    Dim lb%
    Dim ub%
    Dim s%

    lb = LBound(D)
    ub = UBound(D)
    s = ub - lb + 1


    ReDim f(1 To (s + 1)) As FIELD4INFOCB
    For i = 1 To s
        f(i).fName = v4Cstring(D((lb - 1) + i).fName) ' note: this function allocates memory
        f(i).ftype = Asc(D((lb - 1) + i).ftype)
        f(i).flength = D((lb - 1) + i).flength
        f(i).fdecimals = D((lb - 1) + i).fdecimals
        f(i).fnulls = D((lb - 1) + i).fnulls
    Next i

    d4createData = d4createLow(cb&, ByVal (dbname$), f(1), ByVal (0&))

    ' Since v4Cstring allocates memory for the storage of
    ' C strings, we must free the memory after it has been
    ' used.
    For i = 1 To s
      Call v4Cstringfree(f(i).fName)
    Next i

End Function

Function d4encodeHandle(temp As Long)
    Dim EncodedString As String
    EncodedString = "#" + str$(temp)
    d4encodeHandle = EncodedString
End Function

Function d4fieldsAdd&(DATA4&, fields() As FIELD4INFO)
    Dim i%

    ReDim f(LBound(fields) To UBound(fields)) As FIELD4INFOCB
    For i = LBound(fields) To UBound(fields)
        f(i).fName = v4Cstring(fields(i).fName)  ' note: this function allocates memory
        f(i).ftype = Asc(fields(i).ftype)
        f(i).flength = fields(i).flength
        f(i).fdecimals = fields(i).fdecimals
        f(i).fnulls = fields(i).fnulls
    Next i

    d4fieldsAdd = d4fieldsAddCB(DATA4, UBound(f) - LBound(f) + 1, f(LBound(f)))
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

Function d4fieldsRemove&(DATA4&, fieldNames() As String)
    Dim addrs() As Long, i%
    ReDim addrs(LBound(fieldNames) To UBound(fieldNames)) As Long
    For i = LBound(fieldNames) To UBound(fieldNames)
        addrs(i) = v4Cstring(fieldNames(i))
    Next i

    d4fieldsRemove = d4fieldsRemoveCB(DATA4, UBound(fieldNames) - LBound(fieldNames) + 1, addrs(LBound(addrs)))

    For i = LBound(addrs) To UBound(addrs)
        Call v4Cstring(addrs(i))
    Next i
End Function

Function d4fileName$(dbfPtr&)
   d4fileName$ = b4String(d4fileNameCB(dbfPtr))
End Function

Function d4go%(DATA4&, recordNumber&)
   d4go = d4goLow(DATA4, recordNumber, 1)
End Function

Sub date4assign(dateString$, julianDay&)
'This functions converts the julian day into standard format
'and puts the result in dateString

   'Size dateString$
   dateString$ = Space$(8 + 1)

   Call date4assignLow(dateString, julianDay, 0)
   dateString$ = Left$(dateString$, 8)
End Sub

Function date4cdow$(dateString$)

'This function returns the day of the week in a character
'string based on the value in 'DateString'

    'Validate "dateString"
   If dateString = "" Or Len(dateString) < 8 Then Exit Function

   Dim datePtr&
   datePtr& = date4cdowCB(dateString)    'Get pointer to day
   If datePtr = 0 Then Exit Function     'Illegal date

   date4cdow = b4String(datePtr)

End Function

Function date4cmonth$(dateString$)

'This function returns the month in 'DateString' as a
'character string

   'Validate "DateString"
   If dateString = "" Or Len(dateString) < 8 Then Exit Function

   Dim datePtr&
   datePtr& = date4cmonthCB(dateString)    'Get pointer to month
   If datePtr = 0 Then Exit Function       'Illegal date

   date4cmonth = b4String(datePtr)                 'Return month name

End Function

Sub date4format(dateString$, Result$, pic$)
    'This function formats Result$ using the date value
    ' in 'dateString$' and the format info. in 'Pic$'

    'Size Result$
    Result$ = Space$(Len(pic$) + 1)

    Call date4formatCB(dateString$, Result$, pic$)
    Result$ = Left$(Result$, Len(pic$))
End Sub

Sub date4init(Result$, dateString$, pic$)
    'This function copies the date, specified by dateString,
    'and formatted according to pic, into Result. The date copied
    'will be in standard dBASE format,

    'Size Result$
    Result$ = Space$(9)

    Call date4initCB(Result$, dateString$, pic$)
    Result$ = Left$(Result$, 8)
End Sub

Sub date4today(dateS As String)
   If Len(dateS) < 8 Then dateS = Space$(8)
   Call date4todayCB(dateS)
End Sub

Function error4lastDescription$(c4&)
    'This function returns the last error description string
    error4lastDescription = b4String(error4lastDescriptionVB(c4))
End Function

#If USE_D4DLL Then
Function error4lastDescriptionLow$(c4&)
    'This function returns the last error description string, given the c4***.dll CODE4
    error4lastDescriptionLow = b4String(error4lastDescriptionVBLow(c4))
End Function
#End If

Function error4text$(c4&, errCode&)
    'This function returns the error message string
    error4text = b4String(error4textCB(c4, errCode))
End Function

Function expr4null%(exPtr&)
    expr4null = expr4nullLow(exPtr, 1)
End Function

Function expr4source$(exPtr&)
    'This function returns a copy of the original
    'dBASE expression string
    expr4source = b4String(expr4sourceCB(exPtr))
End Function

Function expr4str$(exPtr&)
'This function returns the parsed string

   Dim exprPtr&
   Dim buf As String

   'Get pointer to alias string
   exprPtr& = expr4strCB(exPtr)

   If exprPtr& = 0 Then Exit Function

   expr4str = Left$(b4String(exprPtr), expr4len(exPtr))
End Function

Function expr4type$(exPtr&)
'This function returns the type of the parsed string

   Dim exprType%

   'Get ASCII value of type
   exprType = expr4typeCB(exPtr)

   If exprType = 0 Then Exit Function

   expr4type = Chr$(exprType)

End Function

Sub f4assign(fPtr As Long, fStr As String)
   Call f4assignN(fPtr, fStr, Len(fStr))
End Sub

Sub f4assignBinary(fPtr As Long, value() As Byte)
    Call f4assignBinaryVB(fPtr, value(LBound(value)), UBound(value) - LBound(value) + 1)
End Sub

Sub f4assignUnicode(fPtr As Long, value As String)
    Dim bArray() As Byte
    bArray = value & vbNullChar
    Call f4assignUnicodeVB(fPtr, bArray(0))
End Sub

Sub f4assignInt(fldPtr&, fldVal%)
    Call f4assignIntVB(fldPtr, fldVal)
End Sub

Function f4binary(field&)
    Dim fLen&
    fLen = f4len(field)
    Dim buffer() As Byte
    If fLen > 0 Then
        ReDim buffer(1 To (fLen + 1)) As Byte  ' 1 greater because f4ncpy null-terminates the buffer
        fLen = f4ncpyBinary(field, buffer(1), fLen + 1)
        ReDim Preserve buffer(1 To fLen) As Byte
    End If
    f4binary = buffer
End Function

Function f4currency$(field&, numDec%)
    'This function returns the contents of a field
    f4currency = b4String(f4currencyCB(field, numDec))
End Function

Function f4dateTime$(field&)
    'This function returns the contents of a field
    f4dateTime = b4String(f4dateTimeCB(field))
End Function

Function f4memoAssignBinary%(fPtr As Long, value() As Byte)
    f4memoAssignBinary = f4memoAssignBinaryVB(fPtr, value(LBound(value)), UBound(value) - LBound(value) + 1)
End Function

Sub f4memoAssignUnicode(fPtr As Long, value As String)
    Dim bArray() As Byte
    bArray = value & vbNullChar
    Call f4memoAssignUnicodeVB(fPtr, bArray(0))
End Sub

Function f4memoBinary(field&)
    Dim fLen&
    fLen = f4memoLen(field)
    Dim buffer() As Byte
    If fLen > 0 Then
        ReDim buffer(1 To (fLen + 1)) As Byte  ' 1 greater because f4ncpy null-terminates the buffer
        fLen = f4memoNcpyBinary(field, buffer(1), fLen + 1)
        ReDim Preserve buffer(1 To fLen) As Byte
    End If
    f4memoBinary = buffer
End Function

Function f4memoStr$(fPtr&)
'This function returns a string corresponding to the memo
'field pointer argument.
    Dim MemoLen&, MemoPtr&

    MemoLen& = f4memoLen(fPtr)          'Get memo length

    If MemoLen > &H7FFFFFFF Then
        MsgBox "Error #: -910" & vbCrLf & "Unexpected Information" + r4line + "Memo entry too long to return in a Visual Basic string." + r4line + "Field Name:" + r4line + f4name(fPtr), 16, "CodeBase Error"
        Exit Function
    End If

    MemoPtr& = f4memoPtr(fPtr)
    If MemoPtr& = 0 Then Exit Function

    Dim MemoString$
    MemoString = String$(MemoLen&, " ")

    'Copy 'MemoPtr' to VB string 'MemoString'
    u4memCpy MemoString, MemoPtr, MemoLen

    f4memoStr = MemoString
End Function

Sub f4memoStr64(fPtr As Long, src As String)

    'This function copies a large memo entry (32K-64K)
    'into a user supplied string

    Dim r4line$
    r4line = Chr$(10) + Chr$(13)

    Dim MemoLen&, MemoPtr&

    MemoLen& = f4memoLen(fPtr)          'Get memo length

    ' 'r4maxVBStringLen' defined in 'Constants' section of this file
    If MemoLen > r4maxVBStringLen Then
        MsgBox "Error #: -910" + r4line + "Unexpected Information" + r4line + "Memo entry too long to retrieve into VB string." + r4line + "Field Name:" + r4line + f4name(fPtr), 16, "CodeBasic Error"
        Exit Sub
    End If

    MemoPtr& = f4memoPtr(fPtr)
    If MemoPtr& = 0 Then Exit Sub

    src = String$(MemoLen&, " ")

    'Copy 'MemoPtr' to VB string 'src'
    u4memCpy src, MemoPtr, MemoLen

End Sub

Function f4name$(fPtr&)

'This function returns the name of a field

    Dim FldNamePtr&                     'Pointer to field name
    Dim FldName As String * 11          'String to hold info
    FldNamePtr& = f4nameCB(fPtr)        'Get pointer

         f4name = b4String(FldNamePtr)

End Function

Function f4nCpy(field&, s$, slen%)
    'This function copies the fields contents into a string
    s = Space$(slen)                    'Make s$ one byte longer for null character that u4ncpy adds
    Dim fPtr&
    fPtr& = f4ptr(field)

    If fPtr& = 0 Then Exit Function

    u4memCpy s, fPtr, slen

    f4nCpy = Len(s)
End Function

Function f4str$(field&)
    'This function returns the contents of a field
    Dim s$, fPtr&, fLen%

    fPtr& = f4ptr(field)
    If fPtr& = 0 Then Exit Function

    fLen = f4len(field)                 'Get field length
    s = Space$(fLen)                    'Make s$ one byte longer for null character that u4ncpy adds

    u4memCpy s, fPtr, fLen

    f4str = s
End Function

Function f4strUnicode$(field&)
    'This function returns the contents of a Unicode field
    Dim bArray() As Byte
    bArray = f4binary(field)
    f4strUnicode = RTrimNulls(Left(bArray, f4len(field)))
End Function

Function f4memoStrUnicode$(field&)
    'This function returns the contents of a Unicode memo field
    Dim bArray() As Byte
    bArray = f4memoBinary(field)
    f4memoStrUnicode = RTrimNulls(Left(bArray, f4memoLen(field)))
End Function

Function i4create&(ByVal dbPtr&, IndexName$, n() As TAG4INFO)
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

    Dim i%

    Dim tlb%
    Dim tub%
    Dim ts%


    tlb = LBound(n)
    tub = UBound(n)
    ts = tub - tlb + 1
    ReDim t(1 To (ts + 1)) As TAG4INFOCB
    For i = 1 To ts
        t(i).name = v4Cstring(n((tlb - 1) + i).name)
        t(i).expression = v4Cstring(n((tlb - 1) + i).expression)
        t(i).filter = v4Cstring(n((tlb - 1) + i).filter)
        t(i).unique = n((tlb - 1) + i).unique
        t(i).descending = n((tlb - 1) + i).descending
    Next i

    If IndexName$ = "" Then    'User wants production index file
        i4create = i4createCB(dbPtr&, ByVal 0&, t(1))
    Else
        i4create = i4createCB(dbPtr&, IndexName$, t(1))
    End If

    ' Since v4Cstring allocates memory for the storage of
    ' C strings, we must free the memory after it has been
    ' used.

    For i = 1 To ts
         Call v4Cstringfree(t(i).name)
         Call v4Cstringfree(t(i).expression)
         Call v4Cstringfree(t(i).filter)
    Next i
End Function

Function i4fileName$(iPtr&)
    'This function returns the file name of an index tag
    i4fileName = b4String(i4fileNameCB(iPtr))
End Function

Function i4open&(d4&, fName$)
   If fName = "" Then
      i4open = i4openCB(d4&, ByVal 0&)   'Use data file name
   Else
      i4open = i4openCB(d4&, fName$)     'Use supplied name
   End If
End Function

Function i4tagAdd%(ByVal i4Ptr&, n() As TAG4INFO)
    ' i4tagAdd adds additional tags to an existing
    ' index.

    ' i4tagAdd takes the contents from the TAG4INFO
    ' structure and builds a TAG4INFOCB structure which
    ' is passed to i4tagAddCB.

    Dim i%

    Dim tlb%
    Dim tub%
    Dim ts%


    tlb = LBound(n)
    tub = UBound(n)
    ts = tub - tlb + 1
    ReDim t(1 To (ts + 1)) As TAG4INFOCB
    For i = 1 To ts
        t(i).name = v4Cstring(n((tlb - 1) + i).name)
        t(i).expression = v4Cstring(n((tlb - 1) + i).expression)
        t(i).filter = v4Cstring(n((tlb - 1) + i).filter)
        t(i).unique = n((tlb - 1) + i).unique
        t(i).descending = n((tlb - 1) + i).descending
    Next i

    i4tagAdd = i4tagAddCB(i4Ptr&, t(1))

End Function

Function relate4masterExpr$(r4Ptr&)
    'This function returns the Relations expression string
    relate4masterExpr = b4String(relate4masterExprCB(r4Ptr&))
End Function

Function report4parent%(ByVal r4&, ByVal hWnd&)
   #If Win16 Then
      report4parent = report4parent16(r4, hWnd)
   #End If
   #If Win32 Then
      report4parent = report4parent32(r4, hWnd)
   #End If
End Function

Sub report4free(pReport&, freeRelate%, closeFiles%)
    Call report4freeLow(pReport, freeRelate, closeFiles, 1)
End Sub

Function t4Alias$(tPtr&)
    t4Alias = b4String(t4aliasCB(tPtr))
End Function

Function t4expr$(tPtr&)
    'This function returns the original tag expression
    t4expr = b4String(t4exprCB(tPtr))
End Function

Function t4filter$(tPtr&)

'This function returns the tag filter expression

    Dim FilterPtr&
    Dim filter As String * 255

    'Get pointer to parsed filter expression
    FilterPtr& = t4filterCB(tPtr&)

    If FilterPtr& = 0 Then
        t4filter = ""
        Exit Function         'No filter
    End If

   t4filter = b4String(FilterPtr)

End Function

Function u4descend$(charString$)
   Dim Result$, i%

   For i = 1 To Len(charString)
      Result = Result + Chr$(128 And Asc(Mid$(charString, i, 1)))
   Next

   u4descend = Result
End Function

Private Function RTrimNulls$(str$)
    ' Returns a String containing a copy
    ' of str without trailing nulls.
    Dim curLength&, newLength&
    curLength = Len(str)
    If curLength = 0 Then Exit Function

    For newLength = 1 To curLength
        If Mid(str, newLength, 1) = vbNullChar Then
            RTrimNulls = Left(str, newLength - 1)
            Exit Function
        End If
    Next

    RTrimNulls = str
End Function
