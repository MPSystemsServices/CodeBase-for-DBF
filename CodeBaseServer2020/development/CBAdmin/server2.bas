Attribute VB_Name = "server"
Option Explicit

Global titleSuffix
Global secondsUntilNextBackup As Long
Global IntervalInfo As Long

Const dbg = True

' set to False to disable C/S-specific functionality
'#Const clientServer = True

'SystemParametersInfo and the RECT structure are needed
'to get the working screen size
Declare Function SystemParametersInfo Lib "user32" Alias "SystemParametersInfoA" (ByVal uAction As Long, ByVal uParam As Long, ByRef lpvParam As Any, ByVal fuWinIni As Long) As Long

Enum scheduleUnits
    schSeconds
    schMinutes
    schHours
    schDays
End Enum

Type RECT
    Left As Long
    Top As Long
    Right As Long
    Bottom As Long
End Type

'Extra functions/consts needed for next ver of codebase.bas
Declare Function code4errorCode2& Lib "c4dll.dll" (ByVal c4&, ByVal value&)
Declare Sub u4memCpy2 Lib "c4dll.dll" Alias "u4memCpy" (ByRef dest As Any, ByVal source&, ByVal numCopy&)
Declare Function u4ptrPtr2Long Lib "c4dll.dll" (ByVal ptr&, ByVal indx As Integer) As Long
Declare Function error4number2& Lib "c4dll.dll" (ByVal errnum&)
Declare Function u4dbfCode4& Lib "c4dll.dll" (ByVal dbfPtr&)

Declare Function code4connectAcceptNew% Lib "c4dll.dll" (ByVal c4&, ByVal setting As Byte)
Declare Function code4connectCut% Lib "c4dll.dll" (ByVal c4&, ByVal connId$)
Declare Function code4connectCutAll% Lib "c4dll.dll" (ByVal c4&)
Declare Function code4connectionStatus& Lib "c4dll.dll" (ByVal c4&)
Declare Function code4infoRetrieve& Lib "c4dll.dll" (ByVal c4&, ByRef memAlloc&, ByRef nClients%, ByRef runSec&, ByRef openFiles%)
Declare Function code4directory& Lib "c4dll.dll" (ByVal c4&, ByVal path$)
Declare Function code4serverCloseFiles% Lib "c4dll.dll" (ByVal c4&)
' LY Jul 28/04 : changed method of return string from code4serverCurrentDirectory and code4serverConfigName, due to memory leaks
Declare Function code4serverConfigNameVB% Lib "c4dll.dll" Alias "code4serverConfigName" (ByVal c4&, ByVal ptr&)
' LY Mar 23/05 : added third param, due to AS Mar 23/05 in c4util.c
Declare Sub code4serverCurrentDirectoryVB Lib "c4dll.dll" Alias "code4serverCurrentDirectory" (ByVal c4&, ByVal ptr&, ByVal ptrLen%)
Declare Function code4serverOS& Lib "c4dll.dll" (ByVal c4&)
Declare Function code4serverShutdown% Lib "c4dll.dll" (ByVal c4&)

Declare Function c4getAppVerify& Lib "c4dll.dll" (ByVal c4&)
Declare Sub getVerificationString Lib "u4stamp.dll" (ByVal verify&)
Declare Function hasStampValue% Lib "u4stamp.dll" ()

'backup recovery functions
Declare Function code4swapLogFile& Lib "c4dll.dll" (ByVal c4&, ByVal logName$, ByVal logNameLen%)
Declare Function log4recoverBackup% Lib "u4dll.dll" (ByVal prependLogFile$, ByVal backupLogFile$, ByVal pathToBackup$, ByVal SaveChanges%, ByVal silentMode%, ByVal hwnd As Long)

'Used to display help
Declare Function WinHelp Lib "user32" Alias "WinHelpA" (ByVal hwnd As Long, ByVal lpHelpFile As String, ByVal wCommand As Long, ByVal dwData As Long) As Long
Declare Function WinHelpKey Lib "user32" Alias "WinHelpA" (ByVal hwnd As Long, ByVal lpHelpFile As String, ByVal wCommand As Long, ByVal dwData As String) As Long
Public Const HELP_FINDER = &HB
Public Const HELP_QUIT = &H2
Public Const HELP_KEY = &H101

'Used for IP Address handler text boxes
Declare Function SendMessage Lib "user32" Alias "SendMessageA" (ByVal hwnd As Long, ByVal wMsg As Long, ByVal wParam As Long, lParam As Long) As Long
Public Const EM_GETSEL = &HB0

Global Const e4invalidUserId = -1440
Global Const e4invalidPassword = -1450

Global Const secInDay = 86400

'used to separate directories in a path (different for Win/UNIX)
Global SepChar As String

'used for turning codebase errors on/off
Dim oldErrCode%

'index format. used for specifying file extensions
Global ndxFmt$

'used to connect, and to pre-test various codecontrols functions for better error messages
Global gCode4&     'code4, gets initialized in InitErrValues
Global bLoggedIn As Boolean

'server login information
Global svrId As String
Global accountId As String
Global passwrd As String
Global portId As String

'backup log locations
Global backlog1 As String
Global backlog2 As String

'for the timer on the server information form
Global refreshTime As Long
'show full path in acct manager
Global bFullPath As Boolean
'file maintenance
Global userAbort As Boolean

Global configName As String  'server config file name (s4server.dbf)
Global SystemPath As String
Global defPath As String

Global encryptionKey As String
Global encryptionKeyFileName As String

'Global errorFlag As Boolean 'error flag for server information form
Global bErrLoadingForm As Boolean   'prevents form from loading if error occurs, such as opening forms data files

Global bFirstBrowseLoad As Boolean

'Used for specifying if IP address of server and local host are same, for Dir. browse functionality
Global bHostIsLocal As Boolean

'Error message strings
Global Const ECONNECT$ = "An error occured attempting to connect to the database server for the following reason:"
Global Const EDISCONNECT$ = "An error occured attempting to disconnect from the database server. The server may have been stopped, or is in a state of error."
Global Const ECREATE$ = "An error occurred attempting to create a required data, index or memo file file."
Global Const EFINISH$ = "If you are not able to correct this problem, please record the error details listed here and contact your system administrator for further assistance."
Global Const EOPEN$ = "An error occurred attempting to open a required data file."
Global Const EOPEN_NDX$ = "An error occurred attempting to open a required index file."
Global Const EGENERAL$ = "A general error occurred. The following error information is available:"
Global Const EGENCC3$ = "An error has occurred synchronizing one or more controls on this form with its associated data file. The following error information is available:"
Global Const ETAG$ = "An error occurred attempting to specify an index sort order (Tag) for the specified data file."

'Used for handling CbMaster ErrorCode events
Global Const EPRIV4$ = "This file is required for administering file privileges. "
Global Const EACCT4$ = "This file is required for administering account information."
Global Const SERVER4$ = "This file is required for configuring server behavior. There may be a problem with the database server."
Global Const ECONNECTION$ = "This file contains information on all current connections."
Global Const EUNSPECIFIED$ = "This file contains information required to perform the search operation."
Global Const EACCESS_MSG$ = "This error may be caused if you don't have appropriate access priviliges for this file."
Global Const EINDEX_MSG$ = "This file is required for displaying ordered table information and performing searches."
Global Const EINT_NDX_MSG$ = "There may be an internal problem with the index file."
Global Const ACCOUNT% = 1
Global Const PRIVILEGE% = 2
Global Const CONNECTION% = 3
Global Const TYPE_SERVER% = 4
Global Const UNSPECIFIED% = 4


Global Const caStandAlone% = 0
Global Const caCSConnected% = 1
Global Const caCSNotConnected% = 2

Global Const LOCK4OFF = 0
Global Const LOCK4ALL = 1
Global Const LOCK4DATA = 2
Global Const LOCK4APPEND = 10
Global Const LOCK4FILE = 20
Global Const LOCK4RECORD = 30
Global Const LOCK4INDEX = 40

Public OldWindowProc As Long  ' Original window proc

' Function to retrieve the address of the current Message-Handling routine
Declare Function GetWindowLong Lib "user32" Alias "GetWindowLongA" (ByVal hwnd As Long, ByVal nIndex As Long) As Long
' Function to define the address of the Message-Handling routine
Declare Function SetWindowLong Lib "user32" Alias "SetWindowLongA" (ByVal hwnd As Long, ByVal nIndex As Long, ByVal dwNewLong As Long) As Long
' Function to copy an object/variable/structure passed by reference onto a variable of your own
Declare Sub CopyMemory Lib "kernel32" Alias "RtlMoveMemory" (pDest As Any, pSource As Any, ByVal ByteLen As Long)
' Function to execute a function residing at a specific memory address
Declare Function CallWindowProc Lib "user32" Alias "CallWindowProcA" (ByVal lpPrevWndFunc As Long, ByVal hwnd As Long, ByVal msg As Long, ByVal wParam As Long, ByVal lParam As Long) As Long

' This is the message constant
Public Const WM_GETMINMAXINFO = &H24

' This is a structure referenced by the MINMAXINFO structure
Type POINTAPI
     x As Long
     y As Long
End Type

' This is the structure that is passed by reference (ie an address) to your message handler
' The key items in this structure are ptMinTrackSize and ptMaxTrackSize
Type MINMAXINFO
    ptReserved As POINTAPI
    ptMaxSize As POINTAPI
    ptMaxPosition As POINTAPI
    ptMinTrackSize As POINTAPI
    ptMaxTrackSize As POINTAPI
End Type
Global Const GWL_WNDPROC = (-4)

Global Const VK_LBUTTON = &H1
Public Declare Function GetKeyState Lib "user32" (ByVal nVirtKey As Long) As Integer

Public Declare Function PostMessage Lib "user32" Alias "PostMessageA" (ByVal hwnd As Long, ByVal wMsg As Long, ByVal wParam As Long, ByVal lParam As Long) As Long
Public Const WM_CLOSE = &H10

Function CloseDB%(dbf As Long)
    Dim rc%
    If dbf <> 0 Then
        rc = d4close(dbf)
        If rc = r4success Then
            dbf = 0
        End If
    End If
    CloseDB = rc
End Function

Public Function code4serverConfigName(c4&)
    Dim strPtr&
    Dim rc%

    'LY Jul 28/04 : changed method of returning string from code4serverConfigNameVB, due to memory leaks
    rc = -1
    strPtr = u4alloc(1024)
    If strPtr <> 0 Then
        rc = code4serverConfigNameVB(c4, strPtr)
    End If

    ' If strPtr = 0 Then
    If rc <> 0 Then
        code4serverConfigName = ""
    Else
        code4serverConfigName = b4String(strPtr)
    End If

    If strPtr <> 0 Then
        u4free strPtr
    End If
End Function

Public Function code4serverCurrentDirectory$(c4&)
    Dim strPtr&

    'LY Jul 28/04 : changed method of returning string from code4serverCurrentDirectoryVB, due to memory leaks
    strPtr = u4alloc(1024)
    If strPtr <> 0 Then
        Call code4serverCurrentDirectoryVB(c4, strPtr, 1024)
    End If

    If strPtr = 0 Then
        code4serverCurrentDirectory = ""
    Else
        code4serverCurrentDirectory = b4String(strPtr)
    End If

    If strPtr <> 0 Then
        u4free strPtr
    End If
End Function

Sub ErrOff()
    If gCode4 <> 0 Then
        Call code4errOff(gCode4, 1)
    End If
End Sub

Sub ErrOn()
    If gCode4 <> 0 Then
        Call code4errOff(gCode4, 0)
    End If
End Sub

Function ErrReset%()
    ErrReset = code4errorCode(gCode4, 0)
End Function

Public Function GetItems(lst As ListBox) As Collection
    ' return the selected list items in a ListBox control
    Dim items As New Collection
    Dim i%
    For i = 0 To (lst.ListCount - 1)
        items.Add lst.List(i)
    Next i
    Set GetItems = items
End Function

Public Function LMouseDown() As Boolean
    ' Returns True if the left mouse button is down
    LMouseDown = GetKeyState(VK_LBUTTON) And &H80
End Function

Private Function LockTypeToString$(DATA4&)
    Select Case f4long(d4field(DATA4, "LockType"))
        Case LOCK4OFF
            LockTypeToString = "none"
        Case LOCK4ALL
            LockTypeToString = "Table"
        Case LOCK4DATA
            LockTypeToString = "Data File"
        Case LOCK4APPEND
            LockTypeToString = "Append"
        Case LOCK4FILE
            LockTypeToString = "File"
        Case LOCK4RECORD
            LockTypeToString = "Record " & f4long(d4field(DATA4, "LockRecNo"))
        Case LOCK4INDEX
            LockTypeToString = "Index"
        Case Else
            LockTypeToString = "UNKNOWN"
    End Select
End Function

Sub InvalidIPMsg(txt As String)
    MsgBox txt & " is not a valid entry. Please specify a value between 0 and 255 for this field.", vbExclamation, "Invalid IP Address"
End Sub

Public Function isClientServer() As Boolean
    If u4switch() And &H8 Then
        isClientServer = True
    Else
        isClientServer = False
    End If
End Function

Function PadRight$(str$, padLen%)
    PadRight = str$ + Space$(padLen - Len(str))
End Function

Public Function ParseDelimitedString(ByVal source$) As Collection
    Dim temp$
    Dim a%
    Dim values As New Collection

    temp = source
    a = InStr(temp, Chr(0))
    Do
        If a = 0 Then
            values.Add temp
            a = -1
        Else
            values.Add Left(temp, a - 1)
            temp = Trim(Mid(temp, a + 1))
            a = InStr(temp, Chr(0))
        End If
    Loop While a >= 0

    Set ParseDelimitedString = values
End Function

Sub RestartApp()
    unloadForms
    DisconnectCode4
End Sub

Function DisconnectCode4%()
    Dim rc%
    If gCode4 <> 0 Then
        rc = code4initUndo(gCode4)
        gCode4 = 0
    End If
    bLoggedIn = False
    If VarType(titleSuffix) = vbEmpty Then
        frmMain2.caption = App.title
    Else
        frmMain2.caption = App.title & " " & titleSuffix
    End If
    frmMain2.setMenus caCSNotConnected

    DisconnectCode4 = rc
End Function

Function GetIpAddress(hostname$)

'Returns IP address in dotted notation

'CS 1999/05/06 Use Winsock control instead
    GetIpAddress = frmMain2.Winsock1.LocalIP
'First let's check if we've already been given data in dotted notation
'    If inet_addr(hostname) <> INADDR_NONE Then
'       GetIpAddress = hostname
'    Else
'       Dim hePtr As Long, he As hostent, tmp&
'       hePtr = gethostbyname(hostname)
'       Call u4memCpy2(he, hePtr, SZHOSTENT)
'       tmp = u4ptrPtr2Long(he.h_addr_list, 0)
'       GetIpAddress = Trim$(b4String(inet_ntoa(tmp)))
'    End If
End Function

Function GetLocalIpAddress()
   Dim x$, rc%
   x = Space$(255)

   rc = gethostname(x, 255)

   If rc = 0 Then    'success
      GetLocalIpAddress = GetIpAddress(TrimNull(x))
   Else
      GetLocalIpAddress = ""
   End If
End Function

Sub offErr(cb As Long)
    oldErrCode = code4errOff(cb, 1)
End Sub

Sub onErr(cb As Long)
    Call code4errOff(cb, oldErrCode)
End Sub

Sub ResetCode4()
'This function resets Code4 member settings
'to default values so that multiple forms can
'share the same code4, modifying any members
'as necessary. Only resets those members used
'somewhere in the program
    If gCode4 <> 0 Then
        Call code4accessMode(gCode4, OPEN4DENY_NONE)
        Call code4errOpen(gCode4, 1)
        Call code4errFieldName(gCode4, 1)
        Call code4errTagName(gCode4, 1)
        Call code4errOff(gCode4, 0)
        Call code4createTemp(gCode4, 0)
        Call code4singleOpen(gCode4, 1)
        Call code4safety(gCode4, 1)
    End If
End Sub

Public Sub showErrConnStatus()
    ShowError "Connection status information is not available", _
              "Ensure that your account has the necessary authorization to open this file"
End Sub

Public Sub showErrInit()
     ShowErrLow "An error occurred attempting to initialize a required CodeBase DLL:", , , _
                Space(5) & "C4DLL.DLL", "This DLL should be located in either this applications's directory, " _
              + "the Windows System directory, or in a directory in the system path."
End Sub

Function ShowErrLow(mainMsg$, Optional genErr, Optional specErr, Optional xtraInfo, Optional xtraInfo2, Optional msgOptions, Optional title) As Integer
    Dim msg$

    If IsMissing(msgOptions) Then
        msgOptions = vbOKOnly + vbExclamation
    End If

    If IsMissing(title) Then
        title = App.title & " - Error"
    End If

    If IsMissing(genErr) Then
        genErr = ""
    Else
        genErr = vbLf & vbLf & Space(5) & "General Error No." & genErr
    End If

    If IsMissing(specErr) Then
        specErr = ""
    Else
        specErr = vbLf & vbLf & Space(5) & "Specific Error No." & specErr
    End If

    If IsMissing(xtraInfo) Then
        xtraInfo = ""
    Else
        If Trim(xtraInfo) <> "" Then
            xtraInfo = vbLf & vbLf & Space(5) & xtraInfo
        End If
    End If

    If IsMissing(xtraInfo2) Then
        xtraInfo2 = ""
    Else
        If Trim(xtraInfo2) <> "" Then
            xtraInfo2 = vbLf & vbLf & xtraInfo2
        End If
    End If

    msg = mainMsg & xtraInfo & genErr & specErr & xtraInfo2 & vbLf & vbLf & EFINISH

    ShowErrLow = MsgBox(msg, msgOptions, title)
End Function

Function ShowError%(Optional xtraInfo, Optional xtraInfo2)
    Dim err1&, err2&, err2Index&, x$
    Dim errOne, errTwo
    Dim errClass$

    If IsMissing(xtraInfo) Then
        xtraInfo = ""
    End If

    If IsMissing(xtraInfo2) Then
        xtraInfo2 = ""
    End If

    err1 = code4errorCode(gCode4, 0)
    errOne = str(err1) & ": " & error4text(gCode4, err1)

    err2Index = code4errorCode2(gCode4, 0)
    err2 = error4number2(err2Index)
    errTwo = str(err2) & ": " & error4text(gCode4, err2Index)

    Select Case err1
    Case e4create
        errClass = ECREATE
    Case e4open, e4data
        errClass = EOPEN
    Case e4socket
        errClass = ECONNECT
        Select Case err2
        Case 89001
            If xtraInfo = "" Then xtraInfo = "Cannot locate server: " & svrId
        Case 88061
            If xtraInfo = "" Then xtraInfo = "Cannot locate server on host machine with process Id: " & portId
        End Select
    Case e4invalidUserId
        errClass = ECONNECT
        If xtraInfo = "" Then xtraInfo = "An invalid User Id was specified."
    Case e4invalidPassword
        errClass = ECONNECT
        If xtraInfo = "" Then xtraInfo = "An invalid password was specified for this account."
    Case Else
        errClass = EGENERAL
    End Select

    ShowErrLow errClass, errOne, errTwo, xtraInfo, xtraInfo2

    'Debug.Print err1, err2, err2Index
End Function

Public Sub showWarn()
    If Not bHostIsLocal Then
        If Form24.chkHostWarning.value = 0 Then
            If Not bFirstBrowseLoad Then
                bFirstBrowseLoad = True
                Form24.chkHostWarning.value = 1
            End If
            Form24.Show 1
        End If
    End If
End Sub

Function StrICmp(str1$, str2$)
    If UCase$(Trim(str1)) = UCase$(Trim(str2)) Then
        StrICmp = True
    Else
        StrICmp = False
    End If
End Function

Function TrimNull(x As String) As String
   Dim nullPos%

   'trim nulls if any from string
   nullPos = InStr(x, Chr$(0))
   If nullPos > 0 Then
      TrimNull = Left$(x, nullPos - 1)
   Else
      TrimNull = x
   End If
End Function

Public Sub TxtGotFocus(f As Form)
    If LMouseDown() = False Then
        With f.ActiveControl
            .SelStart = 0
            .SelLength = Len(.Text)
        End With
    End If
End Sub

Sub unloadForms()
'    Unload Form26
'    Unload Form25
    Unload Form24
'    Unload Form23
'    Unload Form22
    Unload Form21
    Unload frmLockTLC
    Unload frmLockTCL
    Unload frmLockCLT
    Unload frmLockCTL
    Unload frmLockLTC
    Unload frmLockLCT
    Unload Form14
    Unload Form13
    Unload Form12
'    Unload Form11
'    Unload Form11a
'    Unload Form10
    Unload Form9
    Unload frmProgress
    Unload Form7
    Unload Form6
'    Unload frmConfig2
'    Unload Form2
'    Unload Form1
'    Unload FmBrowse
'    Unload frmTagSelect
    Unload Splash
End Sub

Function UpperKey%(KeyAscii As Integer)
    'makes a keypress character into upper case
    UpperKey = Asc(UCase$(Chr$(KeyAscii)))
End Function

Public Function validateIP(ByVal ipAddr As String) As Boolean
    Dim addr As String
    addr = Trim$(ipAddr)

    If Len(Trim(addr)) = 0 Then
        validateIP = True
        Exit Function
    End If

    ' make sure the IP address is valid
    Dim start As Integer, pos As Integer
    ' value 1
    pos = InStr(1, addr, ".")
    If pos = 0 Then
        validateIP = False
        Exit Function
    End If
    If val(Left(addr, pos - 1)) > &HFF Then
        validateIP = False
        Exit Function
    End If

    ' value 2
    start = pos + 1
    pos = InStr(start, addr, ".")
    If pos = 0 Then
        validateIP = False
        Exit Function
    End If
    If val(Mid(addr, start, pos - start)) > &HFF Then
        validateIP = False
        Exit Function
    End If

    ' value 3
    start = pos + 1
    pos = InStr(start, addr, ".")
    If pos = 0 Then
        validateIP = False
        Exit Function
    End If
    If val(Mid(addr, start, pos - start)) > &HFF Then
        validateIP = False
        Exit Function
    End If

    ' value 4
    start = pos + 1
    pos = InStr(start, addr, ".")
    If pos <> 0 Then  ' there should not be another dot
        validateIP = False
        Exit Function
    End If
    If val(Mid(addr, start)) > &HFF Then
        validateIP = False
        Exit Function
    End If

    validateIP = True
End Function

Public Function ValidateIPNew(txt As String) As Boolean
    If val(txt) < 0 Or val(txt) > 255 Then
        ValidateIPNew = False
    Else
        ValidateIPNew = True
    End If
End Function

Private Function FieldNameFull(shortName$)
    If StrComp(shortName, "ACCOUNT_ID", vbTextCompare) = 0 Then
        FieldNameFull = "Account ID"
    ElseIf StrComp(shortName, "TCPADDRESS", vbTextCompare) = 0 Then
        FieldNameFull = "IP Address"
    ElseIf StrComp(shortName, "CONNECT_ID", vbTextCompare) = 0 Then
        FieldNameFull = "Account ID"
    ElseIf StrComp(shortName, "PATH", vbTextCompare) = 0 Then
        FieldNameFull = "Path"
    ElseIf StrComp(shortName, "TABLE", vbTextCompare) = 0 Then
        FieldNameFull = "Table"
    ElseIf StrComp(shortName, "FILETYPE", vbTextCompare) = 0 Then
        FieldNameFull = "File Type"
    ElseIf StrComp(shortName, "RECCOUNT", vbTextCompare) = 0 Then
        FieldNameFull = "Number of Records"
    ElseIf StrComp(shortName, "NUMFIELDS", vbTextCompare) = 0 Then
        FieldNameFull = "Number of Fields"
    ElseIf StrComp(shortName, "REC_WIDTH", vbTextCompare) = 0 Then
        FieldNameFull = "Record Width"
    ElseIf StrComp(shortName, "READ_ONLY", vbTextCompare) = 0 Then
        FieldNameFull = "Read Only"
    ElseIf StrComp(shortName, "ACCESSMODE", vbTextCompare) = 0 Then
        FieldNameFull = "Access Mode"
    ElseIf StrComp(shortName, "LOCKTYPE", vbTextCompare) = 0 Then
        FieldNameFull = "Lock Type"
    ElseIf StrComp(shortName, "LOCKRECNO", vbTextCompare) = 0 Then
        FieldNameFull = "Record" & vbCrLf & "Locked"
    End If
End Function

Public Function ReportFormLoad2(lv As ListView, TagName$, fieldNames As Collection) As Boolean
    lv.HideSelection = False
    lv.AllowColumnReorder = True
    lv.FullRowSelect = True
    lv.LabelEdit = lvwManual
    lv.View = lvwReport

    Dim col, rc%
    For Each col In fieldNames
        lv.ColumnHeaders.Add , col, FieldNameFull(CStr(col))
    Next col

Call MsgBox("rfl 1" & vbCrLf & code4errorCode(gCode4, r4check))
    Dim data&
    data = code4connectionStatus(gCode4)
Call MsgBox("rfl 2" & vbCrLf & code4errorCode(gCode4, r4check))
    If data <> 0 Then
        Dim li As ListItem, i%
        Dim t4 As Long
        If UCase(code4indexExtension(gCode4)) = "NTX" Then
            t4 = t4open(data, TagName)
            Call d4tagSelect(data, t4)
        Else
            Call d4tagSelect(data, d4tag(data, TagName))
        End If
Call MsgBox("rfl 3" & vbCrLf & code4errorCode(gCode4, r4check))
        rc = d4top(data)
        Do While rc = r4success
            If UCase(lv.ColumnHeaders(1).key) = "LOCKTYPE" Then
                Set li = lv.ListItems.Add(, , LockTypeToString(data))
            Else
                Set li = lv.ListItems.Add(, , Trim(f4str(d4field(data, lv.ColumnHeaders(1).key))))
            End If
            ' CS 2006/09/08 Switch from f4long to f4str because
            ' connection IDs for ODBC clients are longer than a long.
            li.Tag = Trim(f4str(d4field(data, "CONNECT_ID")))
            For i = 2 To lv.ColumnHeaders.count
                If UCase(lv.ColumnHeaders(i).key) = "LOCKTYPE" Then
                    li.ListSubItems.Add , , LockTypeToString(data)
                Else
                    li.ListSubItems.Add , , Trim(f4str(d4field(data, lv.ColumnHeaders(i).key)))
                End If
            Next i
            rc = d4skip(data, 1)
        Loop

Call MsgBox("rfl 4" & vbCrLf & code4errorCode(gCode4, r4check))
        If UCase(code4indexExtension(gCode4)) = "NTX" Then
            Call t4close(t4)
        End If
        Call d4close(data)
    End If
Call MsgBox("rfl 5" & vbCrLf & code4errorCode(gCode4, r4check))

    Dim errno%
    errno = code4errorCode(gCode4, 0)
    If errno <> r4success Then
        Select Case errno
            Case -1399 To -1300
                ' connection error; probably lost connection to server
                Call MsgBox("Connection to host lost.", vbExclamation, App.title)
                Call frmMain2.mnuDisconnect_Click
            Case Else
                Call MsgBox("Error getting status. CodeBase error" & errno & ".", vbExclamation, App.title)
        End Select
        Exit Function
    End If

    ReportFormLoad2 = True
End Function

Public Function AcctManagerResize(ByVal hwnd As Long, ByVal msg As Long, ByVal wp As Long, ByVal lp As Long) As Long
    ' Watch for the pertinent message to come in
    If msg = WM_GETMINMAXINFO Then

        Dim MinMax As MINMAXINFO

        ' This is necessary because the structure was passed by its address and there
        ' is currently no intrinsic way to use an address in Visual Basic
        CopyMemory MinMax, ByVal lp, Len(MinMax)

        ' This is where you set the values of the MinX,MinY,MaxX, and MaxY
        ' The values placed in the structure must be in pixels. The values
        ' normally used in Visual Basic are in twips. The conversion is as follows:
        ' pixels = twips\twipsperpixel
        MinMax.ptMinTrackSize.x = 7215 \ Screen.TwipsPerPixelX
        MinMax.ptMinTrackSize.y = 6405 \ Screen.TwipsPerPixelY

        ' Here we copy the datastructure back up to the address passed in the parameters
        ' because Windows will look there for the information.
        CopyMemory ByVal lp, MinMax, Len(MinMax)

        ' This message tells Windows that the message was handled successfully
        AcctManagerResize = 1
        Exit Function

    End If

    ' Here, we forward all irrelevant messages on to the default message handler.
    AcctManagerResize = CallWindowProc(OldWindowProc, hwnd, msg, wp, lp)
End Function

Public Function IntervalToSeconds(interval As Integer, units) As Long
    ' interval units: 0=seconds, 1=min, 2=hours, 3=days
    Select Case units
        Case schSeconds
            IntervalToSeconds = interval
        Case schMinutes
            IntervalToSeconds = interval * 60
        Case schHours
            IntervalToSeconds = interval * 3600
        Case schDays
            IntervalToSeconds = interval * 86400
    End Select
End Function

Public Function doBackup(preLog$, path$, Optional backlog, Optional silent As Boolean = False) As Integer
    Screen.MousePointer = vbHourglass

    Dim showFinalMsg As Boolean

    Dim trySwap As Boolean
    trySwap = bLoggedIn

    Dim SaveChanges%, rc&, logName As String * 256
    SaveChanges = 0
    If IsMissing(backlog) Then
        If Not bLoggedIn Then
            If Exist(backlog1) Then
                logName = backlog1
            ElseIf Exist(backlog2) Then
                logName = backlog2
            End If
        End If
    Else
        logName = backlog
    End If

    Do While trySwap
        Dim timeout&
        timeout = GetSetting(App.title, "General", "Timeout", 30) - 4
        If timeout < 1 Then timeout = 1
        Call code4errOff(gCode4, 1)
        rc = code4swapLogFile(gCode4, logName, 256)
        Call code4errOff(gCode4, 0)
        DoEvents

        Select Case rc
            Case r4success, e4len
                ' If code4swapLogFile returns e4len, it means that
                ' the server did not swap log files because the log
                ' file that is currently unused is not empty and the
                ' utility functions still need to process the log file.
                Call code4errorCode(gCode4, 0)
                trySwap = False
            Case e4config
            Case e4notSupported  ' AS Jan 12/06 - this is returned as well in some cases
                MsgBox "The CodeBase server is not set up for or does not support backup recovery.", vbExclamation, "Backup Recovery"
                Screen.MousePointer = vbDefault
                Exit Function
            Case e4timeOut, r4timeout, e4connection
                'Server is either busy or down. Try reconnect.
                Call code4errOff(gCode4, 1)
                Call code4timeoutSet(gCode4, 4) ' suspect server not available, so don't wait long
                DisconnectCode4
                DoEvents
                'gCode4 = code4init()
                'Call code4timeoutSet(gCode4, 3)
                'Call code4errOff(gCode4, 1)
                'rc = code4connect(gCode4, svrId, portId, accountId, passwrd, "")
                'DoEvents
                'If rc = r4success Then
                '    bLoggedIn = True
                '    Call code4timeoutSet(gCode4, timeout)
                'Else
                    ' reconnect failed. server must be down
                    ' determine which backup log file is available
                    If Exist(backlog1) Then
                        logName = backlog1
                    ElseIf Exist(backlog2) Then
                        logName = backlog2
                    Else
                        MsgBox "CodeBase Server is down and the backup log files are not available.", vbCritical, "Update Backup"
                        doBackup = e4open
                        Exit Function
                    End If
                    showFinalMsg = True
                    trySwap = False
                'End If
            Case Else
                ShowError
                Call code4errorCode(gCode4, 0)
                Screen.MousePointer = vbDefault
                doBackup = rc
                Exit Function
        End Select
        SaveChanges = 1
    Loop

    DoEvents
    vnumDataParams = 0
    UTILITYS.preLog = Trim(preLog)
    UTILITYS.logName = Trim(logName)
    UTILITYS.path = path
    UTILITYS.SaveChanges = SaveChanges
    UTILITYS.silent = silent
    currentUtility = LOG4RECOVER_BACKUP
    frm32Status.Show 1
    'doBackup = log4recoverBackup(Trim(preLog), Trim(logName), path, SaveChanges, CInt(silent))

    Call SaveSetting(App.title, "Backup", "BackLog", logName)
    Screen.MousePointer = vbDefault

    If showFinalMsg Then
        Dim msg$
        msg = "CodeBase server is down. CodeBase administrator has performed a final update of the backup data files."
        msg = msg & " You should start the backup CodeBase server."
        MsgBox msg, vbInformation, "Update Backup"
    End If
End Function

Public Function ServerIsRunning() As Boolean
    Dim cbTemp&
    cbTemp = code4init()
    If cbTemp <> 0 Then
        Select Case code4connect(cbTemp, svrId, portId, accountId, passwrd, "")
            Case r4success, e4max
                ServerIsRunning = True
        End Select
    End If
End Function

Public Function CBAConnect(Optional host$ = "", Optional Port$ = "", Optional User$ = "", Optional pw$ = "", Optional silent = False) As Integer
    Dim db As Long
    Dim field As Long
    Dim rc As Integer, rcl&
    Dim tempC4 As Long

    tempC4 = gCode4
    gCode4 = code4init()
    Call code4timeoutSet(gCode4, GetSetting(App.title, "General", "Timeout", 30))
    Call code4logOpenOff(gCode4)

    'get the config file name from server
    If gCode4 <> 0 Then
        Call ErrOff 'turn off codebase error messages
        Call code4errorCode(gCode4, 0)
        If hasStampValue() <> 0 Then
            Call code4verifySet(gCode4, "s")
            Call getVerificationString(c4getAppVerify(gCode4))
        End If
        rc = code4connect(gCode4, Trim(host), Trim(Port), Trim(User), Trim(pw), "C4SOCK.DLL")
' CS 2009/05/26 e4encryptRequired is not defined anywhere. What is its value?
'        If rc = e4encryptRequired Then
'            Call code4errorCode(gCode4, 0)
'            Call code4encryptConnection(gCode4, r4passwordEncryption, 256)
'            rc = code4connect(gCode4, Trim(host), Trim(Port), Trim(User), Trim(pw), "C4SOCK.DLL")
'
'            If rc <> r4success And rc <> r4connected And CInt(rc / 100) <> -14 Then
'                ' CS 2002/12/18  If rc = r4success or r4connected, the
'                ' connection is successful, so no need to go any further.
'                ' If rc is anything in the -1400 to -1499 range, the server
'                ' refused the connection, but not because of an encryption
'                ' problem, so don't need to try again with encryption.
'                Call code4errorCode(gCode4, 0)
'                Call code4encryptConnection(gCode4, r4publicEncryption, 256)
'                rc = code4connect(gCode4, Trim(host), Trim(Port), Trim(User), Trim(pw), "C4SOCK.DLL")
'            End If
'        End If
        If rc = r4success Or rc = r4connected Then
            frmMain2.setMenus caCSConnected
            unloadForms
            Call code4initUndo(tempC4)
            ' get server connection info and save it to global variables
            svrId = Trim(host)
            portId = Trim(Port)
            accountId = Trim(User)
            passwrd = Trim(pw)
            Dim junk&
            SepChar = "\"
            On Error GoTo endCheckServerOS
            If (code4serverOS(gCode4) And &H7) = OS4UNIX Then
                SepChar = "/"
            End If
endCheckServerOS:
            On Error GoTo 0

            'save login information to registry
            SaveSetting App.title, "Startup", "ServerId", svrId
            SaveSetting App.title, "Startup", "AccountId", accountId
            SaveSetting App.title, "Startup", "PortId", portId

            ndxFmt = "." & code4indexExtension(gCode4)
            configName = code4serverConfigName(gCode4)

            If configName = "" Then
                If Not silent Then
                    ShowError , "Unable to query the database server for the name of its configuration file. There may be a problem with the server, or the connection to the server may have failed."
                End If
                DisconnectCode4
                CBAConnect = e4info
                Exit Function
            End If

            ' don't do this if testing in stand-alone
            If UCase$(Trim$(svrId)) = "LOCALHOST" Then
                bHostIsLocal = True
            ElseIf GetIpAddress(svrId) = GetLocalIpAddress Then
                bHostIsLocal = True
            Else
                bHostIsLocal = False
            End If

            'get the system path from configuration file for
            'location of ACCOUNT4, PRIV4 and other files
            Call code4autoOpen(gCode4, 0)
            db = d4open(gCode4, configName)
            Call code4autoOpen(gCode4, 1)
            If db <> 0 Then
                Call d4top(db)
                Call code4errFieldName(gCode4, 0)

                field = d4field(db, "DEFPATH")
                If field <> 0 Then
                    defPath = Trim(f4str(field))
                    If Len(defPath) = 0 Then
                        defPath = code4serverCurrentDirectory(gCode4)
                    End If
                Else
                    defPath = code4serverCurrentDirectory(gCode4)
                End If
                If Right(defPath, 1) = SepChar Then
                    defPath = Left(defPath, Len(defPath) - 1)
                End If

                field = d4field(db, "SYSTEMPATH")
                If field <> 0 Then
                    SystemPath = Trim(f4str(field))
                    'CJ causes problems if defpath is not a fully quantified path.
                    'If Len(SystemPath) = 0 Then
                    '    SystemPath = defPath
                    'End If
                Else
                    SystemPath = code4serverCurrentDirectory(gCode4)
                End If
                SystemPath = Trim(SystemPath)
                If Right(SystemPath, 1) = SepChar Then
                    SystemPath = Left(SystemPath, Len(SystemPath) - 1)
                End If

                Call code4errFieldName(gCode4, 1)
            Else
                SystemPath = ""
                defPath = ""
            End If

            If IsAdministrator() Then
                If VarType(titleSuffix) = vbEmpty Then
                    frmMain2.caption = App.title & " - Connected to " & svrId
                Else
                    frmMain2.caption = App.title & " " & titleSuffix & " - Connected to " & svrId
                End If
                bLoggedIn = True
            Else
                Call frmMain2.mnuDisconnect_Click
                Call MsgBox("This user does not have permission to connect CodeBase Administrator to the server. Contact your system administrator.", vbExclamation, "Connection Denied")
            End If
        Else
            CBAConnect = code4errorCode(gCode4, r4check)
            If Not silent Then
                rc = ShowError
            End If
            Call code4initUndo(gCode4)
            gCode4 = tempC4
        End If

        If gCode4 <> 0 Then
            Call ErrOn   'turn back on until all testing is done
            Call code4close(gCode4)
        End If
    End If
End Function

Public Function IsAdministrator() As Boolean
    ' Check the account table to determine if the
    ' account has the right to run the Administrator
    If UCase(accountId) = "ADMIN" And (LCase(svrId) = "localhost" Or svrId = "127.0.0.1") Then
        IsAdministrator = True
    Else
        Dim accountdb&, IDField&, adminField&
        If Len(SystemPath) = 0 Then
            accountdb = d4open(gCode4, "ACCOUNT4")  ' CS 2003/04/24 uppercase for UNIX
        Else
            accountdb = d4open(gCode4, SystemPath & SepChar & "ACCOUNT4")
        End If

        If accountdb = 0 Then
            Call code4errorCode(gCode4, 0)
            Exit Function
        End If

        adminField = d4field(accountdb, "ADMIN")
        If adminField = 0 Then
            Call code4errorCode(gCode4, 0)
            Call d4close(accountdb)
            IsAdministrator = True
            Exit Function
        End If

        IDField = d4field(accountdb, "ACCOUNTID")
        If IDField = 0 Then
            Call code4errorCode(gCode4, 0)
            Call d4close(accountdb)
            Exit Function
        End If

        Call d4tagSelect(accountdb, d4tag(accountdb, "NAME"))
        Dim rc%
        rc = d4seek(accountdb, Left(UCase(accountId) & Space(50), f4len(IDField)))

        If rc = r4success Then
            If f4true(adminField) <> 0 Then
                IsAdministrator = True
            End If
        End If

        Call code4errorCode(gCode4, 0)
        Call d4close(accountdb)
    End If
End Function

Public Sub setSchedule(scheduleInterval As Integer, scheduleUnits)
    On Error GoTo badvalue
    secondsUntilNextBackup = IntervalToSeconds(scheduleInterval, scheduleUnits)
    On Error GoTo 0

    frmMain2.timerBackup.Enabled = True
    Call frmMain2.refreshStatus

    Exit Sub

badvalue:
    MsgBox "Interval value " & scheduleInterval & " is invalid.", vbCritical, "Backup Scheduler"
End Sub

Public Sub CloseWindow(hwnd As Long)
    Call PostMessage(hwnd, WM_CLOSE, 0, 0)
End Sub
