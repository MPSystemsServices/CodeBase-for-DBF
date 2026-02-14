Attribute VB_Name = "Module1"
' codebase.bas  (c)Copyright Sequiter Software Inc., 1988-1996. All rights reserved.

'Data Types Used by CodeBasic

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

Declare Function code4accessMode% Lib "cb6demo.dll" (ByVal c4&, ByVal Value%)
Declare Function code4autoOpen% Lib "cb6demo.dll" (ByVal c4&, ByVal Value%)
Declare Function code4calcCreate% Lib "cb6demo.dll" (ByVal c4&, ByVal expr4&, ByVal fcnName$)
Declare Sub code4calcReset Lib "cb6demo.dll" (ByVal c4&)
Declare Function code4codePage% Lib "cb6demo.dll" (ByVal c4&, ByVal Value%)
Declare Function code4collatingSequence% Lib "cb6demo.dll" (ByVal c4&, ByVal Value%)
Declare Function code4connect% Lib "cb6demo.dll" (ByVal c4&, ByVal serverId$, ByVal processId$, ByVal userName$, ByVal password$, ByVal protocol$)
Declare Function code4close% Lib "cb6demo.dll" (ByVal c4&)
Declare Function code4createTemp% Lib "cb6demo.dll" (ByVal c4&, ByVal Value%)
Declare Function code4data& Lib "cb6demo.dll" (ByVal c4&, ByVal AliasName$)
Declare Function code4dateFormatVB& Lib "cb6demo.dll" (ByVal c4&)
Declare Function code4dateFormatSet% Lib "cb6demo.dll" (ByVal c4&, ByVal fmt$)
Declare Function code4errCreate% Lib "cb6demo.dll" (ByVal c4&, ByVal Value%)
Declare Function code4errDefaultUnique% Lib "cb6demo.dll" (ByVal c4&, ByVal Value%)
Declare Function code4errorCode% Lib "cb6demo.dll" (ByVal c4&, ByVal Value%)
Declare Function code4errExpr% Lib "cb6demo.dll" (ByVal c4&, ByVal Value%)
Declare Function code4errFieldName% Lib "cb6demo.dll" (ByVal c4&, ByVal Value%)
Declare Function code4errGo% Lib "cb6demo.dll" (ByVal c4&, ByVal Value%)
Declare Function code4errSkip% Lib "cb6demo.dll" (ByVal c4&, ByVal Value%)
Declare Function code4errTagName% Lib "cb6demo.dll" (ByVal c4&, ByVal Value%)
Declare Sub code4exit Lib "cb6demo.dll" (ByVal c4&)
Declare Function code4fileFlush% Lib "cb6demo.dll" (ByVal c4&, ByVal Value%)
Declare Function code4flush% Lib "cb6demo.dll" (ByVal c4&)
Declare Function code4hInst% Lib "cb6demo.dll" (ByVal c4&, ByVal Value%)
Declare Function code4indexExtensionVB& Lib "cb6demo.dll" (ByVal c4&)
Declare Function code4hWnd& Lib "cb6demo.dll" (ByVal c4&, ByVal Value&)
Declare Function code4init& Lib "cb6demo.dll" Alias "d4init_v" ()
Declare Function code4initUndo% Lib "cb6demo.dll" Alias "d4initUndo" (ByVal c4&)
Declare Function code4lock% Lib "cb6demo.dll" (ByVal c4&)
Declare Function code4lockAttempts% Lib "cb6demo.dll" (ByVal c4&, ByVal Value%)
Declare Function code4lockAttemptsSingle% Lib "cb6demo.dll" (ByVal c4&, ByVal Value%)
Declare Sub code4lockClear Lib "cb6demo.dll" (ByVal c4&)
Declare Function code4lockDelay& Lib "cb6demo.dll" (ByVal c4&, ByVal Value&)
Declare Function code4lockEnforce% Lib "cb6demo.dll" (ByVal c4&, ByVal Value%)
Declare Function code4lockFileNameVB& Lib "cb6demo.dll" (ByVal c4&)
Declare Function code4lockItem& Lib "cb6demo.dll" (ByVal c4&)
Declare Function code4lockNetworkIdVB& Lib "cb6demo.dll" (ByVal c4&)
Declare Function code4lockUserIdVB& Lib "cb6demo.dll" (ByVal c4&)
Declare Function code4log% Lib "cb6demo.dll" (ByVal c4&, ByVal Value%)
Declare Function code4logCreate% Lib "cb6demo.dll" (ByVal c4&, ByVal logName$, ByVal userId$)
Declare Function code4logFileNameVB& Lib "cb6demo.dll" (ByVal c4&)
Declare Function code4logOpen% Lib "cb6demo.dll" (ByVal c4&, ByVal logName$, ByVal userId$)
Declare Sub code4logOpenOff Lib "cb6demo.dll" (ByVal c4&)
Declare Function code4memExpandBlock% Lib "cb6demo.dll" (ByVal c4&, ByVal Value%)
Declare Function code4memExpandData% Lib "cb6demo.dll" (ByVal c4&, ByVal Value%)
Declare Function code4memExpandIndex% Lib "cb6demo.dll" (ByVal c4&, ByVal Value%)
Declare Function code4memExpandLock% Lib "cb6demo.dll" (ByVal c4&, ByVal Value%)
Declare Function code4memExpandTag% Lib "cb6demo.dll" (ByVal c4&, ByVal Value%)
Declare Function code4memSizeBlock& Lib "cb6demo.dll" (ByVal c4&, ByVal Value&)
Declare Function code4memSizeBuffer& Lib "cb6demo.dll" (ByVal c4&, ByVal Value&)
Declare Function code4memSizeMemo% Lib "cb6demo.dll" (ByVal c4&, ByVal Value%)
Declare Function code4memSizeMemoExpr& Lib "cb6demo.dll" (ByVal c4&, ByVal Value&)
Declare Function code4memSizeSortBuffer& Lib "cb6demo.dll" (ByVal c4&, ByVal Value&)
Declare Function code4memSizeSortPool& Lib "cb6demo.dll" (ByVal c4&, ByVal Value&)
Declare Function code4memStartBlock% Lib "cb6demo.dll" (ByVal c4&, ByVal Value%)
Declare Function code4memStartData% Lib "cb6demo.dll" (ByVal c4&, ByVal Value%)
Declare Function code4memStartIndex% Lib "cb6demo.dll" (ByVal c4&, ByVal Value%)
Declare Function code4memStartLock% Lib "cb6demo.dll" (ByVal c4&, ByVal Value%)
Declare Function code4memStartMax& Lib "cb6demo.dll" (ByVal c4&, ByVal Value&)
Declare Function code4memStartTag% Lib "cb6demo.dll" (ByVal c4&, ByVal Value%)
Declare Function code4errOff% Lib "cb6demo.dll" (ByVal c4&, ByVal Value%)
Declare Function code4errOpen% Lib "cb6demo.dll" (ByVal c4&, ByVal Value%)
Declare Function code4optAll% Lib "cb6demo.dll" (ByVal c4&)
Declare Function code4optimize% Lib "cb6demo.dll" (ByVal c4&, ByVal Value%)
Declare Function code4optStart% Lib "cb6demo.dll" (ByVal c4&)
Declare Function code4optSuspend% Lib "cb6demo.dll" (ByVal c4&)
Declare Function code4optimizeWrite% Lib "cb6demo.dll" (ByVal c4&, ByVal Value%)
Declare Function code4readLock% Lib "cb6demo.dll" (ByVal c4&, ByVal Value%)
Declare Function code4readOnly% Lib "cb6demo.dll" (ByVal c4&, ByVal Value%)
Declare Function code4errRelate% Lib "cb6demo.dll" (ByVal c4&, ByVal Value%)
Declare Function code4safety% Lib "cb6demo.dll" (ByVal c4&, ByVal Value%)
Declare Function code4singleOpen% Lib "cb6demo.dll" (ByVal c4&, ByVal Value%)
Declare Function code4timeout& Lib "cb6demo.dll" (ByVal c4&)
Declare Sub code4timeoutSet Lib "cb6demo.dll" (ByVal c4&, ByVal Value&)
Declare Function code4tranStart% Lib "cb6demo.dll" (ByVal c4&)
Declare Function code4tranStatus% Lib "cb6demo.dll" Alias "code4tranStatusVB" (ByVal c4&)
Declare Function code4tranCommit% Lib "cb6demo.dll" (ByVal c4&)
Declare Function code4tranRollback% Lib "cb6demo.dll" (ByVal c4&)
Declare Function code4unlock% Lib "cb6demo.dll" (ByVal c4&)
Declare Function code4unlockAuto% Lib "cb6demo.dll" Alias "code4unlockAutoVB" (ByVal c4&)
Declare Sub code4unlockAutoSet Lib "cb6demo.dll" Alias "code4unlockAutoSetVB" (ByVal c4&, ByVal Value%)

'===============================================================================================
'
'                                 CodeControls function prototypes
'
'-----------------------------------------------------------------------------------------------

Declare Function ctrl4init Lib "cc2.vbx" Alias "ctrl4initVB" () As Long
Declare Function ctrl4initUndo Lib "cc2.vbx" Alias "ctrl4initUndoVB" (ByVal code As Long) As Integer

'===============================================================================================
'
'                                 Data File Functions' Prototypes
'
'-----------------------------------------------------------------------------------------------
Declare Function d4aliasCB& Lib "cb6demo.dll" Alias "d4alias" (ByVal d4&)
Declare Sub d4aliasSet Lib "cb6demo.dll" (ByVal d4&, ByVal AliasValue$)
Declare Function d4append% Lib "cb6demo.dll" (ByVal d4&)
Declare Function d4appendBlank% Lib "cb6demo.dll" (ByVal d4&)
Declare Function d4appendStart% Lib "cb6demo.dll" Alias "d4appendStartVB" (ByVal d4&, ByVal UseMemoEntries%)
Declare Sub d4blank Lib "cb6demo.dll" (ByVal d4&)
Declare Function d4bof% Lib "cb6demo.dll" (ByVal d4&)
Declare Function d4bottom% Lib "cb6demo.dll" (ByVal d4&)
Declare Function d4changed% Lib "cb6demo.dll" Alias "d4changedVB" (ByVal d4&, ByVal intFlag%)
Declare Function d4check% Lib "cb6demo.dll" (ByVal d4&)
Declare Function d4close% Lib "cb6demo.dll" (ByVal d4&)
Declare Function d4createCB& Lib "cb6demo.dll" Alias "d4create" (ByVal c4&, ByVal DbfName$, fieldinfo As Any, tagInfo As Any)
Declare Sub d4delete Lib "cb6demo.dll" (ByVal d4&)
Declare Function d4deleted% Lib "cb6demo.dll" (ByVal d4&)
Declare Function d4eof% Lib "cb6demo.dll" (ByVal d4&)
Declare Function d4field& Lib "cb6demo.dll" (ByVal d4&, ByVal FieldName$)
Declare Function d4fieldInfo& Lib "cb6demo.dll" (ByVal d4&)
Declare Function d4fieldJ& Lib "cb6demo.dll" Alias "d4fieldJVB" (ByVal d4&, ByVal JField%)
Declare Function d4fieldNumber% Lib "cb6demo.dll" (ByVal d4&, ByVal FieldName$)
Declare Function d4fileNameCB& Lib "cb6demo.dll" Alias "d4fileNameVB" (ByVal d4&)
Declare Function d4flush% Lib "cb6demo.dll" (ByVal d4&)
Declare Function d4freeBlocks% Lib "cb6demo.dll" (ByVal d4&)
Declare Function d4go% Lib "cb6demo.dll" (ByVal d4&, ByVal RecNum&)
Declare Function d4goBof% Lib "cb6demo.dll" (ByVal d4&)
Declare Function d4goEof% Lib "cb6demo.dll" (ByVal d4&)
Declare Function d4index& Lib "cb6demo.dll" (ByVal d4&, ByVal IndexName$)
Declare Function d4log% Lib "cb6demo.dll" Alias "d4logVB" (ByVal d4&, ByVal logging%)
Declare Function d4lock% Lib "cb6demo.dll" (ByVal d4&, ByVal RecordNum&)
Declare Function d4lockAdd% Lib "cb6demo.dll" (ByVal d4&, ByVal RecordNum&)
Declare Function d4lockAddAll% Lib "cb6demo.dll" (ByVal d4&)
Declare Function d4lockAddAppend% Lib "cb6demo.dll" (ByVal d4&)
Declare Function d4lockAddFile% Lib "cb6demo.dll" (ByVal d4&)
Declare Function d4lockAll% Lib "cb6demo.dll" (ByVal d4&)
Declare Function d4lockAppend% Lib "cb6demo.dll" (ByVal d4&)
Declare Function d4lockFile% Lib "cb6demo.dll" (ByVal d4&)
Declare Function d4memoCompress% Lib "cb6demo.dll" (ByVal d4&)
Declare Function d4numFields% Lib "cb6demo.dll" (ByVal d4&)
Declare Function d4open& Lib "cb6demo.dll" (ByVal c4&, ByVal DbfName$)
Declare Function d4openClone& Lib "cb6demo.dll" (ByVal d4&)
Declare Function d4optimize% Lib "cb6demo.dll" Alias "d4optimizeVB" (ByVal d4&, ByVal OptFlag%)
Declare Function d4optimizeWrite% Lib "cb6demo.dll" Alias "d4optimizeWriteVB" (ByVal d4&, ByVal OptFlag%)
Declare Function d4pack% Lib "cb6demo.dll" (ByVal d4&)
Declare Function d4packData% Lib "cb6demo.dll" (ByVal d4&)
Declare Function d4position# Lib "cb6demo.dll" (ByVal d4&)
Declare Function d4positionSet% Lib "cb6demo.dll" (ByVal d4&, ByVal Percentage#)
Declare Sub d4recall Lib "cb6demo.dll" (ByVal d4&)
Declare Function d4recCount& Lib "cb6demo.dll" Alias "d4recCountDo" (ByVal d4&)
Declare Function d4recNo& Lib "cb6demo.dll" (ByVal d4&)
Declare Function d4record& Lib "cb6demo.dll" (ByVal d4&)
Declare Function d4recWidth& Lib "cb6demo.dll" Alias "d4recWidth_v" (ByVal d4&)
Declare Function d4remove% Lib "cb6demo.dll" (ByVal d4&)
Declare Function d4refresh% Lib "cb6demo.dll" (ByVal d4&)
Declare Function d4refreshRecord% Lib "cb6demo.dll" (ByVal d4&)
Declare Function d4reindex% Lib "cb6demo.dll" (ByVal d4&)
Declare Function d4seek% Lib "cb6demo.dll" (ByVal d4&, ByVal seekValue$)
Declare Function d4seekDouble% Lib "cb6demo.dll" (ByVal d4&, ByVal Value#)
Declare Function d4seekN% Lib "cb6demo.dll" Alias "d4seekNVB" (ByVal d4&, ByVal seekValue$, ByVal seekLen%)
Declare Function d4seekNext% Lib "cb6demo.dll" (ByVal d4&, ByVal seekValue$)
Declare Function d4seekNextDouble% Lib "cb6demo.dll" (ByVal d4&, ByVal seekValue#)
Declare Function d4seekNextN% Lib "cb6demo.dll" Alias "d4seekNextNVB" (ByVal d4&, ByVal seekValue$, ByVal seekLen%)
Declare Function d4skip% Lib "cb6demo.dll" (ByVal d4&, ByVal NumberRecords&)
Declare Function d4tag& Lib "cb6demo.dll" (ByVal d4&, ByVal TagName$)
Declare Function d4tagDefault& Lib "cb6demo.dll" (ByVal d4&)
Declare Function d4tagNext& Lib "cb6demo.dll" (ByVal d4&, ByVal TagOn&)
Declare Function d4tagPrev& Lib "cb6demo.dll" (ByVal d4&, ByVal TagOn&)
Declare Sub d4tagSelect Lib "cb6demo.dll" (ByVal d4&, ByVal tPtr&)
Declare Function d4tagSelected& Lib "cb6demo.dll" (ByVal d4&)
Declare Function d4tagSync% Lib "cb6demo.dll" (ByVal d4&, ByVal tPtr&)
Declare Function d4top% Lib "cb6demo.dll" (ByVal d4&)
Declare Function d4unlock% Lib "cb6demo.dll" (ByVal d4&)
Declare Function d4unlockFiles% Lib "cb6demo.dll" Alias "code4unlock" (ByVal d4&)
Declare Function d4write% Lib "cb6demo.dll" Alias "d4writeVB" (ByVal d4&, ByVal RecNum&)
Declare Function d4zap% Lib "cb6demo.dll" (ByVal d4&, ByVal StartRecord&, ByVal EndRecord&)

'===============================================================================================
'
'                                   Date Functions' Prototypes
'
'-----------------------------------------------------------------------------------------------
Declare Sub date4assignCB Lib "cb6demo.dll" Alias "date4assign" (ByVal dateForm$, ByVal julianDay&)
Declare Function date4cdowCB& Lib "cb6demo.dll" Alias "date4cdow" (ByVal dateForm$)
Declare Function date4cmonthCB& Lib "cb6demo.dll" Alias "date4cmonth" (ByVal dateForm$)
Declare Function date4day% Lib "cb6demo.dll" Alias "date4day_v" (ByVal dateForm$)
Declare Function date4dow% Lib "cb6demo.dll" (ByVal dateForm$)
Declare Sub date4formatCB Lib "cb6demo.dll" Alias "date4format" (ByVal dateForm$, ByVal Result$, ByVal pic$)
Declare Sub date4initCB Lib "cb6demo.dll" Alias "date4init" (ByVal dateForm$, ByVal Value$, ByVal pic$)
Declare Function date4isLeap% Lib "cb6demo.dll" (ByVal dateForm$)
Declare Function date4long& Lib "cb6demo.dll" (ByVal dateForm$)
Declare Function date4month% Lib "cb6demo.dll" Alias "date4month_v" (ByVal dateForm$)
Declare Sub date4timeNow Lib "cb6demo.dll" (ByVal TimeForm$)
Declare Sub date4todayCB Lib "cb6demo.dll" Alias "date4today" (ByVal dateForm$)
Declare Function date4year% Lib "cb6demo.dll" Alias "date4year_v" (ByVal dateForm$)

'===============================================================================================
'
'                          Error  Functions' Prototypes
'
'-----------------------------------------------------------------------------------------------
Declare Function error4% Lib "cb6demo.dll" Alias "error4VB" (ByVal c4&, ByVal errCode%, ByVal extraInfo&)
Declare Sub error4exitTest Lib "cb6demo.dll" (ByVal c4&)
Declare Function error4describe% Lib "cb6demo.dll" Alias "error4describeVB" (ByVal c4&, ByVal errCode%, ByVal extraInfo&, ByVal desc1$, ByVal desc2$, ByVal desc3$)
Declare Function error4file% Lib "cb6demo.dll" (ByVal c4&, ByVal fileName$, ByVal overwrite%)
Declare Function error4set% Lib "cb6demo.dll" (ByVal c4&, ByVal errCode%)
Declare Function error4textCB& Lib "cb6demo.dll" Alias "error4text" (ByVal c4&, ByVal errCode&)
'===============================================================================================
'
'                          Expression Evaluation Functions' Prototypes
'
'-----------------------------------------------------------------------------------------------
Declare Function expr4data& Lib "cb6demo.dll" Alias "expr4dataVB" (ByVal exprPtr&)
Declare Function expr4double# Lib "cb6demo.dll" (ByVal exprPtr&)
Declare Sub expr4free Lib "cb6demo.dll" Alias "expr4freeVB" (ByVal exprPtr&)
Declare Function expr4len% Lib "cb6demo.dll" Alias "expr4lenVB" (ByVal exprPtr&)
Declare Function expr4parse& Lib "cb6demo.dll" Alias "expr4parseVB" (ByVal d4&, ByVal expression$)
Declare Function expr4sourceCB& Lib "cb6demo.dll" Alias "expr4source" (ByVal exprPtr&)
Declare Function expr4strCB& Lib "cb6demo.dll" Alias "expr4str" (ByVal exprPtr&)
Declare Function expr4true% Lib "cb6demo.dll" (ByVal exprPtr&)
Declare Function expr4typeVB% Lib "cb6demo.dll" (ByVal exprPtr&)

'===============================================================================================
'
'                            Field Functions' Prototypes
'
'-----------------------------------------------------------------------------------------------
Declare Sub f4assignChar Lib "cb6demo.dll" Alias "f4assignCharVB" (ByVal fPtr&, ByVal Char%)
Declare Sub f4assignDouble Lib "cb6demo.dll" (ByVal fPtr&, ByVal Value#)
Declare Sub f4assignField Lib "cb6demo.dll" (ByVal fPtrTo&, ByVal fPtrFrom&)
Declare Sub f4assignIntVB Lib "cb6demo.dll" (ByVal fPtr&, ByVal Value%)
Declare Sub f4assignLong Lib "cb6demo.dll" (ByVal fPtr&, ByVal Value&)
Declare Sub f4assignN Lib "cb6demo.dll" Alias "f4assignNVB" (ByVal fPtr&, ByVal Value$, ByVal Length%)
Declare Sub f4blank Lib "cb6demo.dll" (ByVal fPtr&)
Declare Function f4char% Lib "cb6demo.dll" (ByVal fPtr&)
Declare Function f4data& Lib "cb6demo.dll" (ByVal fPtr&)
Declare Function f4decimals% Lib "cb6demo.dll" (ByVal fPtr&)
Declare Function f4double# Lib "cb6demo.dll" (ByVal fPtr&)
Declare Function f4int% Lib "cb6demo.dll" (ByVal fPtr&)
Declare Function f4len% Lib "cb6demo.dll" Alias "f4len_v" (ByVal fPtr&)
Declare Function f4long& Lib "cb6demo.dll" (ByVal fPtr&)
Declare Function f4memoAssign% Lib "cb6demo.dll" (ByVal fPtr&, ByVal Value$)
Declare Function f4memoAssignN% Lib "cb6demo.dll" (ByVal fPtr&, ByVal Value$, ByVal Length%)
Declare Sub f4memoFree Lib "cb6demo.dll" (ByVal fPtr&)
Declare Function f4memoLen& Lib "cb6demo.dll" Alias "f4memoLenVB" (ByVal fPtr&)
Declare Function f4memoNcpy% Lib "cb6demo.dll" (ByVal fPtr&, ByVal memPtr$, ByVal memLen%)
Declare Function f4memoPtr& Lib "cb6demo.dll" (ByVal fPtr&)
Declare Function f4nameCB& Lib "cb6demo.dll" Alias "f4name" (ByVal fPtr&)
Declare Function f4ncpyCB% Lib "cb6demo.dll" Alias "f4ncpy" (ByVal fPtr&, ByVal memPtr$, ByVal memLength%)
Declare Function f4number% Lib "cb6demo.dll" (ByVal fPtr&)
Declare Function f4ptr& Lib "cb6demo.dll" (ByVal fPtr&)
Declare Function f4strCB& Lib "cb6demo.dll" Alias "f4str" (ByVal fPtr&)
Declare Function f4true% Lib "cb6demo.dll" (ByVal fPtr&)
Declare Function f4type% Lib "cb6demo.dll" (ByVal fPtr&)


'===============================================================================================
'
'                             Index Functions' Prototypes
'
'-----------------------------------------------------------------------------------------------
Declare Function i4close% Lib "cb6demo.dll" (ByVal i4&)
Declare Function i4createCB& Lib "cb6demo.dll" Alias "i4create" (ByVal d4&, ByVal fileName As Any, tagInfo As TAG4INFOCB)
Declare Function i4fileNameCB& Lib "cb6demo.dll" Alias "i4fileName" (ByVal i4&)
Declare Function i4openCB& Lib "cb6demo.dll" Alias "i4open" (ByVal d4&, ByVal fileName As Any)
Declare Function i4reindex% Lib "cb6demo.dll" (ByVal i4&)
Declare Function i4tag& Lib "cb6demo.dll" (ByVal i4&, ByVal fileName$)
Declare Function i4tagInfo& Lib "cb6demo.dll" (ByVal i4&)
Declare Function i4tagAddCB% Lib "cb6demo.dll" Alias "i4tagAdd" (ByVal i4&, tagInfo As TAG4INFOCB)

'===============================================================================================
'
'                            Relate Functions' Prototypes
'
'-----------------------------------------------------------------------------------------------

Declare Function relate4bottom% Lib "cb6demo.dll" (ByVal r4&)
Declare Sub relate4changed Lib "cb6demo.dll" (ByVal r4&)
Declare Function relate4createSlave& Lib "cb6demo.dll" (ByVal r4&, ByVal d4&, ByVal mExpr$, ByVal t4 As Any)
Declare Function relate4data& Lib "cb6demo.dll" Alias "relate4data_v" (ByVal r4&)
Declare Function relate4dataTag& Lib "cb6demo.dll" Alias "relate4dataTag_v" (ByVal r4&)
Declare Function relate4doAll% Lib "cb6demo.dll" (ByVal r4&)
Declare Function relate4doOne% Lib "cb6demo.dll" (ByVal r4&)
Declare Function relate4eof% Lib "cb6demo.dll" (ByVal r4&)
Declare Function relate4errorAction% Lib "cb6demo.dll" Alias "relate4errorActionVB" (ByVal r4&, ByVal ErrAction%)
Declare Function relate4free% Lib "cb6demo.dll" Alias "relate4freeVB" (ByVal r4&, ByVal CloseFlag%)
Declare Function relate4init& Lib "cb6demo.dll" (ByVal d4&)
Declare Function relate4lockAdd% Lib "cb6demo.dll" (ByVal r4&)
Declare Function relate4master& Lib "cb6demo.dll" Alias "relate4masterVB" (ByVal r4&)
Declare Function relate4masterExprVB& Lib "cb6demo.dll" (ByVal r4&)
Declare Function relate4matchLen% Lib "cb6demo.dll" Alias "relate4matchLenVB" (ByVal r4&, ByVal Length%)
Declare Function relate4next% Lib "cb6demo.dll" (r4&)
Declare Function relate4optimizeable% Lib "cb6demo.dll" (ByVal r4&)
Declare Function relate4querySet% Lib "cb6demo.dll" (ByVal r4&, ByVal expr As String)
Declare Function relate4retrieve& Lib "cb6demo.dll" (ByVal c4&, ByVal fileName$, ByVal openFiles%, ByVal dataPathName$)
Declare Function relate4save% Lib "cb6demo.dll" (ByVal rel4&, ByVal fileName$, ByVal savePathNames%)
Declare Function relate4skip% Lib "cb6demo.dll" (ByVal r4&, ByVal NumRecs&)
Declare Function relate4skipEnable% Lib "cb6demo.dll" Alias "relate4skipEnableVB" (ByVal r4&, ByVal DoEnable%)
Declare Function relate4sortSet% Lib "cb6demo.dll" (ByVal r4&, ByVal expr As String)
Declare Function relate4top% Lib "cb6demo.dll" (ByVal r4&)
Declare Function relate4topMaster& Lib "cb6demo.dll" (ByVal r4&)
Declare Function relate4type% Lib "cb6demo.dll" Alias "relate4typeVB" (ByVal r4&, ByVal rType%)

'===============================================================================================
'
'  Report function prototypes
'
'================================================================================================

Declare Function report4caption% Lib "cb6demo.dll" (ByVal r4&, ByVal caption$)
Declare Function report4currency% Lib "cb6demo.dll" (ByVal r4&, ByVal currncy$)
Declare Function report4dateFormat% Lib "cb6demo.dll" (ByVal r4&, ByVal dateFmt$)
Declare Function report4decimal% Lib "cb6demo.dll" Alias "report4decimal_v" (ByVal r4&, ByVal decChar$)
Declare Function report4do% Lib "cb6demo.dll" Alias "report4do_v" (ByVal r4&)
Declare Sub report4free Lib "cb6demo.dll" (ByVal r4&, ByVal freeRelate%, ByVal closeFiles%)
Declare Function report4margins% Lib "cb6demo.dll" (ByVal r4&, ByVal mLeft&, ByVal mRight&, ByVal mTop&, ByVal mBottom&, ByVal uType%)
Declare Function report4pageSize% Lib "cb6demo.dll" (ByVal r4&, ByVal pHeight&, ByVal pWidth&, ByVal uType%)
Declare Function report4parent% Lib "cb6demo.dll" (ByVal r4&, ByVal hWnd%)
Declare Sub report4printerSelect Lib "cb6demo.dll" (ByVal r4&)
Declare Function report4querySet% Lib "cb6demo.dll" (ByVal r4&, ByVal queryExpr$)
Declare Function report4relate& Lib "cb6demo.dll" (ByVal r4&)
Declare Function report4retrieve& Lib "cb6demo.dll" (ByVal c4&, ByVal fileName$, ByVal openFiles%, ByVal dataPath$)
Declare Function report4save% Lib "cb6demo.dll" (ByVal r4&, ByVal fileName$, ByVal savePaths%)
Declare Function report4screenBreaks% Lib "cb6demo.dll" (ByVal r4&, ByVal Value%)
Declare Function report4separator% Lib "cb6demo.dll" Alias "report4separator_v" (ByVal r4&, ByVal separator$)
Declare Function report4sortSet% Lib "cb6demo.dll" (ByVal r4&, ByVal sortExpr$)
Declare Function report4toScreen% Lib "cb6demo.dll" (ByVal r4&, ByVal toScreen%)


'===============================================================================================
'
'                            Tag Functions' Prototypes
'
'-----------------------------------------------------------------------------------------------

Declare Function t4aliasCB& Lib "cb6demo.dll" Alias "t4alias" (ByVal t4&)
Declare Function t4close% Lib "cb6demo.dll" (ByVal t4&)
Declare Function t4exprVB& Lib "cb6demo.dll" (ByVal t4&)
Declare Function t4filterVB& Lib "cb6demo.dll" (ByVal t4&)
Declare Function t4open& Lib "cb6demo.dll" Alias "t4open_v" (ByVal dbPtr&, ByVal IndexName$)
Declare Function t4unique% Lib "cb6demo.dll" (ByVal t4&)
Declare Function t4uniqueSet% Lib "cb6demo.dll" Alias "t4uniqueSetVB" (ByVal t4&, ByVal Value%)


'=======================================================================================
'
'                Utility function prototypes
'
'-----------------------------------------------------------------------------------------------

Declare Function u4alloc& Lib "cb6demo.dll" Alias "u4allocDefault" (ByVal amt&)
Declare Function u4allocFree& Lib "cb6demo.dll" Alias "u4allocFreeDefault" (ByVal c4&, ByVal amt&)
Declare Sub u4free Lib "cb6demo.dll" Alias "u4freeDefault" (ByVal memPtr&)

'16-Bit versions
Declare Function u4ncpy% Lib "cb6demo.dll" (ByVal MemPtr1$, ByVal memptr2&, ByVal memLength%)
Declare Function u4ncpy2% Lib "cb6demo.dll" Alias "u4ncpy" (ByVal MemPtr1&, ByVal memptr2$, ByVal memLength%)

'32-Bit versions
'Declare Function u4ncpy& Lib "cb6demo.dll" (ByVal MemPtr1$, ByVal memptr2&, ByVal memLength&)
'Declare Function u4ncpy2& Lib "cb6demo.dll" Alias "u4ncpy" (ByVal MemPtr1&, ByVal memptr2$, ByVal memLength&)

Declare Sub u4memCpy Lib "cb6demo.dll" (ByVal dest$, ByVal source&, ByVal numCopy&)
Declare Sub u4memCpy2 Lib "cb6demo.dll" Alias "u4memCpy" (ByVal dest&, source As Any, ByVal numCopy&)
Declare Function u4switch& Lib "cb6demo.dll" ()

'=======================================================================================
'
'                Misc. function prototypes
'
'========================================================================================

Declare Function v4Cstring& Lib "cb6demo.dll" (ByVal s$)
Declare Sub v4Cstringfree Lib "cb6demo.dll" (ByVal s&)

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
Global Const r4bin = "B"
Global Const r4str$ = "C"
Global Const r4float$ = "F"
Global Const r4num$ = "N"
Global Const r4date$ = "D"
Global Const r4gen$ = "G"
Global Const r4log$ = "L"
Global Const r4memo$ = "M"
Global Const r4dateDoub = "d"
Global Const r4numDoub = "n"

'Other CodeBasic Constants
Global Const cp0 = 0          'code4.codePage constant
Global Const cp437 = 1
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
Global Const sort4machine = 0 'code4.collatingSequence constant
Global Const sort4general = 1
Global Const WAIT4EVER = -1


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
    Dim rc As Integer      '16bit
    'Dim rc As Long        '32bit

    s$ = ""

    If Not p& = 0 Then
       rc = u4ncpy(s, p, 256)
    End If

    b4String$ = Left$(s, rc)

End Function

Function code4dateFormat$(c4Ptr&)

'This function returns the CODE4.dateFormat member

    Dim datePtr&, rc%

    'return present date format,
    datePtr = code4dateFormatVB(c4Ptr&)

    code4dateFormat = b4String(datePtr)

End Function

Function code4indexExtension$(c4Ptr&)
'This function returns the CodeBase DLL index format

    Dim strPtr&, rc%

    'return current index format
    strPtr = code4indexExtensionVB(c4Ptr&)

    code4indexExtension = b4String(strPtr)

End Function

Function code4lockFileName$(c4Ptr&)
'This function returns the locked file name

    Dim strPtr&

    'return currently locked file
    strPtr = code4lockFileNameVB(c4Ptr&)

    code4lockFileName = b4String(strPtr)

End Function

Function code4lockNetworkId$(c4Ptr&)
'This function returns the user's network id
'who has locked the current file

    Dim strPtr&, rc%

    'return user id
    strPtr = code4lockNetworkIdVB(c4Ptr&)

    code4lockNetworkId = b4String(strPtr)

End Function

Function code4lockUserId$(c4Ptr&)
'This function returns the user's name
'who has locked the current file

    Dim strPtr&, rc%

    'return user name
    strPtr = code4lockUserIdVB(c4Ptr&)

    code4lockUserId = b4String(strPtr)

End Function

Function code4logFileName$(c4Ptr&)
'This function returns the locked file name

    Dim strPtr&

    'return currently locked file
    strPtr = code4lockFileNameVB(c4Ptr&)

    code4logFileName = b4String(strPtr)

End Function


Function d4alias$(dbPtr&)

'This function returns the data file alias

   Dim AliasPtr&

   'Get pointer to alias string
   AliasPtr& = d4aliasCB(dbPtr)

   If AliasPtr& = 0 Then Exit Function

   d4alias = b4String(AliasPtr)

End Function

Function d4create&(ByVal cb&, dbname$, D() As FIELD4INFO, n() As TAG4INFO)

    ' d4create calls d4createCB() to create a new database.
    ' This function is the same as d4createData() except that
    ' it requires an additional parameter which it uses to
    ' create tag information for a database.
    '
    ' Variable n is an array of type TAG4INFO which corresponds
    ' to TAG4INFOCB, a structure that can be used by d4create.
    ' The difference once again is merely the difference in the
    ' representation of strings between C and Basic.
    ' d4create takes the contents from the TAG4INFO structure
    ' and builds a TAG4INFOCB structure which it passes to d4createCB().
    ' Note: the TAG4INFOCB array is one size larger than the TAG4INFO
    ' array.  The extra empty (zero filled) array element is the
    ' way that d4createCB() detects the end of the array.


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
    d4create = d4createCB(cb&, ByVal (dbname$), f(1), t(1))

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

    ' d4createData() calls d4createCB() to create a new database.
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

    d4createData = d4createCB(cb&, ByVal (dbname$), f(1), ByVal (0&))

    ' Since v4Cstring allocates memory for the storage of
    ' C strings, we must free the memory after it has been
    ' used.
    For i = 1 To s
      Call v4Cstringfree(f(i).fName)
    Next i

End Function

Function d4encodeHandle(temp As Long)
    Dim EncodedString As String
    EncodedString = "#" + Str$(temp)
    d4encodeHandle = EncodedString
End Function

Function d4fileName$(dbfPtr&)

   d4fileName$ = b4String(d4fileNameCB(dbfPtr))

End Function

Sub date4assign(dateString$, julianDay&)
'This functions converts the julian day into standard format puts the result in
'dateString

    'Size dateString$
    dateString$ = Space$(8 + 1)

    Call date4assignCB(dateString, julianDay)
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

'This functions formats Result$ using the date value
' in 'dateString$' and the format info. in 'Pic$'

    'Size Result$
    Result$ = Space$(Len(pic$) + 1)

    Call date4formatCB(dateString$, Result$, pic$)
    Result$ = Left$(Result$, Len(pic$))

End Sub

Sub date4init(Result$, dateString$, pic$)

'This functions copies the date, specified by dateString,
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

Function error4text$(c4&, errCode&)
'This function returns the error message string

   Dim msgPtr&

   'Get pointer to error message string
   msgPtr& = error4textCB(c4, errCode)

   If msgPtr& = 0 Then Exit Function

   error4text = b4String(msgPtr)

End Function

Function expr4source$(exPtr&)
'This function returns a copy of the original
'dBASE expression string

   Dim exprPtr&

   'Get pointer to alias string
   exprPtr& = expr4sourceCB(exPtr)

   If exprPtr& = 0 Then Exit Function

   expr4source = b4String(exprPtr)

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
   exprType = expr4typeVB(exPtr)

   If exprType = 0 Then Exit Function

   expr4type = Chr$(exprType)

End Function

Sub f4assign(fPtr As Long, fStr As String)

   Call f4assignN(fPtr, fStr, Len(fStr))

End Sub

Sub f4assignInt(fldPtr&, fldVal%)

   Call f4assignIntVB(fldPtr, fldVal)

End Sub

Function f4memoStr$(fPtr&)
'This function returns a string corresponding to the memo
'field pointer argument.

    Dim r4line$
    r4line = Chr$(10) + Chr$(13)

    Dim MemoLen&, MemoPtr&

    MemoLen& = f4memoLen(fPtr)          'Get memo length

    If MemoLen > r4maxVBStrFunction Then
        MsgBox "Error #: -910" + r4line + "Unexpected Information" + r4line + "Memo entry too long to return in String function. Try calling 'f4memoStrB'." + r4line + "Field Name:" + r4line + f4name(fPtr), 16, "CodeBasic Error"
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
   Dim fPtr&

    s = Space$(slen)                    'Make s$ one byte longer for null character that u4ncpy adds
   fPtr& = f4ptr(field)

   If fPtr& = 0 Then Exit Function

   u4memCpy s, fPtr, slen

   f4nCpy = Len(s)


End Function

Function f4str$(field&)

'This function returns the contents of a field

   Dim s$, fPtr&, flen%

   flen = f4len(field)                 'Get field length
   s = Space$(flen)                    'Make s$ one byte longer for null character that u4ncpy adds
   fPtr& = f4ptr(field)

   If fPtr& = 0 Then Exit Function

   u4memCpy s, fPtr, flen

   f4str = s

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

    Dim namePtr&

    'Get pointer to file name string
    namePtr = i4fileNameCB(iPtr)

    i4fileName = b4String(namePtr)

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
'This function returns the Relations expression
'string

    Dim strPtr&

    'return expression string
    strPtr = relate4masterExprVB(r4Ptr&)

    relate4masterExpr = b4String(strPtr)

End Function

Function t4Alias$(tPtr&)

'This function returns the alias of an index tag

    Dim AliasPtr&

    'Get pointer to alias string
    AliasPtr = t4aliasCB(tPtr)

    t4Alias = b4String(AliasPtr)

End Function

Function t4expr$(tPtr&)

'This function returns the original tag expression

    Dim exprPtr&

    'Get pointer to parsed dBASE expression
    exprPtr = t4exprVB(tPtr)

    t4expr = b4String(exprPtr)

End Function

Function t4filter$(tPtr&)

'This function returns the tag filter expression

    Dim FilterPtr&
    Dim filter As String * 255

    'Get pointer to parsed filter expression
    FilterPtr& = t4filterVB(tPtr&)

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

