VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "COMDLG32.OCX"
Object = "{248DD890-BB45-11CF-9ABC-0080C7E7B78D}#1.0#0"; "MSWINSCK.OCX"
Object = "{831FDD16-0C5C-11D2-A9FC-0000F8754DA1}#2.0#0"; "MSCOMCTL.OCX"
Begin VB.MDIForm frmMain2
   BackColor       =   &H8000000C&
   Caption         =   "CodeBase Administrator"
   ClientHeight    =   5430
   ClientLeft      =   165
   ClientTop       =   450
   ClientWidth     =   8880
   HelpContextID   =   33
   Icon            =   "suMain2.frx":0000
   LinkTopic       =   "MDIForm1"
   StartUpPosition =   2  'CenterScreen
   Begin MSComDlg.CommonDialog CommonDialog1
      Left            =   5520
      Top             =   3120
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
   End
   Begin VB.Timer timerBackup
      Enabled         =   0   'False
      Interval        =   1000
      Left            =   2640
      Top             =   1500
   End
   Begin MSComctlLib.StatusBar StatusBar1
      Align           =   2  'Align Bottom
      Height          =   315
      Left            =   0
      TabIndex        =   0
      Top             =   5115
      Width           =   8880
      _ExtentX        =   15663
      _ExtentY        =   556
      Style           =   1
      _Version        =   393216
      BeginProperty Panels {8E3867A5-8586-11D1-B16A-00C0F0283628}
         NumPanels       =   1
         BeginProperty Panel1 {8E3867AB-8586-11D1-B16A-00C0F0283628}
         EndProperty
      EndProperty
   End
   Begin MSWinsockLib.Winsock Winsock1
      Left            =   840
      Top             =   2460
      _ExtentX        =   741
      _ExtentY        =   741
      _Version        =   393216
   End
   Begin VB.Menu mnuFile
      Caption         =   "&File"
      Begin VB.Menu mnuConnect
         Caption         =   "&Connect..."
      End
      Begin VB.Menu mnuDisconnect
         Caption         =   "&Disconnect"
      End
      Begin VB.Menu mnuSep4
         Caption         =   "-"
      End
      Begin VB.Menu mnuEncKey
         Caption         =   "Use &Encryption Key..."
      End
      Begin VB.Menu mnuMaint
         Caption         =   "File Maintenance..."
         Shortcut        =   ^M
      End
      Begin VB.Menu mnuSep5
         Caption         =   "-"
      End
      Begin VB.Menu mnuExit
         Caption         =   "E&xit"
      End
   End
   Begin VB.Menu mnuServer
      Caption         =   "&Server"
      Begin VB.Menu mnuServerInfo
         Caption         =   "&Information..."
         Shortcut        =   ^I
      End
      Begin VB.Menu mnuConfig
         Caption         =   "&Settings..."
         Shortcut        =   ^E
      End
      Begin VB.Menu mnuSep1
         Caption         =   "-"
      End
      Begin VB.Menu mnuAccounts
         Caption         =   "&Account Manager..."
      End
      Begin VB.Menu mnuSep2
         Caption         =   "-"
      End
      Begin VB.Menu mnuCloseFiles
         Caption         =   "Close &Unused Cached Files"
      End
      Begin VB.Menu mnuSep3
         Caption         =   "-"
      End
      Begin VB.Menu mnuShutDown
         Caption         =   "Shut &Down"
      End
      Begin VB.Menu mnuAcceptNew
         Caption         =   "Accept &New Connections"
         Checked         =   -1  'True
      End
      Begin VB.Menu mnuDisconnectClient
         Caption         =   "Dis&connect Client(s)..."
      End
      Begin VB.Menu mnuDisconnectAll
         Caption         =   "Disconnect All Clients"
      End
   End
   Begin VB.Menu mnuReports
      Caption         =   "&Reports"
      Begin VB.Menu mnuCon
         Caption         =   "&Connections"
      End
      Begin VB.Menu mnuConTable
         Caption         =   "Connections with &Tables"
      End
      Begin VB.Menu mnuTables
         Caption         =   "&Open Tables"
      End
      Begin VB.Menu mnuTableCon
         Caption         =   "O&pen Tables with Connections"
      End
      Begin VB.Menu mnuLock
         Caption         =   "&Locks"
         Begin VB.Menu mnuLockTCL
            Caption         =   "&1 Table, Connection, Lock Type"
         End
         Begin VB.Menu mnuLockTLC
            Caption         =   "&2 Table, Lock Type, Connection"
         End
         Begin VB.Menu mnuLockCTL
            Caption         =   "&3 Connection, Table, Lock Type"
         End
         Begin VB.Menu mnuLockCLT
            Caption         =   "&4 Connection, Lock Type, Table"
         End
         Begin VB.Menu mnuLockLTC
            Caption         =   "&5 Lock Type, Table, Connection"
         End
         Begin VB.Menu mnuLockLCT
            Caption         =   "&6 Lock Type, Connection, Table"
         End
      End
   End
   Begin VB.Menu mnuTools
      Caption         =   "&Tools"
      Begin VB.Menu mnuBackupSettings
         Caption         =   "Backup &Settings..."
      End
      Begin VB.Menu mnuBackup
         Caption         =   "Update &Backup..."
         Shortcut        =   ^B
      End
      Begin VB.Menu mnuSepToolBackup
         Caption         =   "-"
      End
      Begin VB.Menu mnuLogAnalyze
         Caption         =   "&Analyze Log File..."
      End
      Begin VB.Menu mnuLogRecover
         Caption         =   "&Recover Log File..."
      End
      Begin VB.Menu mnuLogBackup
         Caption         =   "Back&up Log File..."
      End
      Begin VB.Menu mnuLogRestore
         Caption         =   "Res&tore From Log File..."
      End
      Begin VB.Menu mnuSepTool2
         Caption         =   "-"
      End
      Begin VB.Menu mnuOptions
         Caption         =   "&Options..."
      End
   End
   Begin VB.Menu mnuHelp
      Caption         =   "&Help"
      Begin VB.Menu mnuContents
         Caption         =   "&Contents..."
         Shortcut        =   {F1}
      End
      Begin VB.Menu mnuSepH1
         Caption         =   "-"
      End
      Begin VB.Menu mnuAbout
         Caption         =   "&About CodeBase Administrator..."
      End
   End
   Begin VB.Menu mnuStatusPopup
      Caption         =   "Status Popup"
      Visible         =   0   'False
      Begin VB.Menu mnuBackupStop
         Caption         =   "Stop"
      End
   End
End
Attribute VB_Name = "frmMain2"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private IconData As NOTIFYICONDATA  ' used to put icon in the task bar

Private Sub MDIForm_Load()
    ' if TitleAppend.txt exists, store its contents
    On Error GoTo aftertitle
    Dim fs As Variant
    Set fs = CreateObject("Scripting.FileSystemObject")
    If fs.FileExists(App.path & "\" & "TitleAppend.txt") Then
        Dim f
        Set f = fs.OpenTextFile(App.path & "\" & "TitleAppend.txt", 1)
        titleSuffix = f.readLine
        f.Close
    End If
aftertitle:
    On Error GoTo 0

    Call disableBackup

    Dim showLogin As Boolean

    If Len(Command()) = 0 Then
        Splash.Show
    End If
    Call SetHelp
    SepChar = "\"

    With IconData  ' this struct is used to put an icon in the task bar
        .cbSize = Len(IconData)
        .hWnd = Me.hWnd
        .uID = vbNull
        .uFlags = NIF_ICON Or NIF_TIP Or NIF_MESSAGE  ' icon, tooltip, message handling
        .uCallbackMessage = WM_LBUTTONUP  ' send messages to the MouseUp event
        .hIcon = Me.Icon
        .szTip = App.title & Chr(0)
    End With

    ' set defaults
    IntervalInfo = CStr(GetSetting(App.title, "ServerInfo", "RefreshInterval", 2))
    'HelpContextID = CBAdmin_Introduction

    gCode4 = code4init()
    If gCode4 = 0 Then
        showErrInit
        End
    End If
    Call code4logOpenOff(gCode4)
    Call code4timeoutSet(gCode4, GetSetting(App.title, "General", "Timeout", 30))

    If isClientServer Then
        setMenus caCSNotConnected
    Else
        setMenus caStandAlone
    End If

    If Len(Command()) <> 0 Then
        Call Me.Show
        DoEvents
        If ProcessCommand = True Then
            Call CloseWindow(Me.hWnd)
        End If
    Else
        If isClientServer Then
            showLogin = GetSetting(App.title, "Startup", "DisplayLogin", vbChecked)
            If showLogin Then
                Load frmLogin
            End If
        End If
        If VarType(titleSuffix) = vbEmpty Then
            Me.caption = App.title
        Else
            Me.caption = App.title & " " & titleSuffix
        End If
        Me.Show
        Splash.SetFocus
        Do While Splash.timerDone = False
            DoEvents
        Loop
        Unload Splash
        If isClientServer And showLogin Then
            frmLogin.Show 1
        End If
    End If
End Sub

Private Sub MDIForm_MouseUp(Button As Integer, Shift As Integer, x As Single, y As Single)
    Dim msg&
    msg = x / Screen.TwipsPerPixelX
    If Not Me.Visible And Me.WindowState = vbMinimized And msg = WM_LBUTTONUP Then
        ' The window is minimized to the task bar.
        ' The user clicked the icon.
        ' Restore the window.
        Me.WindowState = vbNormal
        Shell_NotifyIcon NIM_DELETE, IconData
        Me.Show
    End If
End Sub

Private Sub MDIForm_QueryUnload(Cancel As Integer, UnloadMode As Integer)
    Dim OldMP%
    OldMP = Screen.MousePointer
    Screen.MousePointer = vbHourglass
    DisconnectCode4
    Screen.MousePointer = OldMP
End Sub

Private Sub MDIForm_Resize()
    If Me.WindowState = vbMinimized Then
        ' window being minimized.
        If GetSetting(App.title, "General", "MinToTray", vbUnchecked) = vbChecked Then
            Call Shell_NotifyIcon(NIM_ADD, IconData)
            Me.Hide
        End If
    End If
End Sub

Private Sub MDIForm_Unload(Cancel As Integer)
    Dim OldMP%
    OldMP = Screen.MousePointer
    Screen.MousePointer = vbHourglass

    Call WinHelp(Me.hWnd, "cbadmin.hlp", HELP_QUIT, 0)

    ErrOff
    Dim rc&
    rc = DisconnectCode4
    If rc <> 0 Then
        MsgBox EDISCONNECT & vbCrLf & "CodeBase error " & rc & ": " & error4text(gCode4, rc), vbCritical, "Disconnecting"
    End If

    Shell_NotifyIcon NIM_DELETE, IconData
    unloadForms

    Screen.MousePointer = OldMP
End Sub

Private Sub mnuAbout_Click()
    frmAbout.Show 1, Me
End Sub

Private Sub mnuAcceptNew_Click()
    Dim rc As Integer
    Dim flag As Boolean

    Call ErrOff
    rc = code4connectAcceptNew(gCode4, Not mnuAcceptNew.Checked)
    Call ErrOn
    If rc = r4success Then
        mnuAcceptNew.Checked = Not mnuAcceptNew.Checked

        If mnuAcceptNew.Checked = False Then
            rc = MsgBox("No new clients may connect to the server.", vbInformation + vbOKOnly, "Accept New Connections")
        Else
            rc = MsgBox("New clients may connect to the server.", vbInformation + vbOKOnly, "Accept New Connections")
        End If
    Else
        Dim errno%
        errno = code4errorCode(gCode4, 0)
        Select Case errno
            Case -1399 To -1300
                ' connection error; probably lost connection to server
                Call MsgBox("Connection to host lost.", vbExclamation, App.title)
                Call frmMain2.mnuDisconnect_Click
            Case e4authorize
                Call code4errorCode(gCode4, 0)
                Call MsgBox("You do not have permission to change this setting.", vbInformation + vbOKOnly, "Immediate Shutdown")
            Case Else
                Call MsgBox("Unable to change this setting. CodeBase error" & errno & ".", vbExclamation, App.title)
        End Select
    End If
End Sub

Private Sub mnuAccounts_Click()
    frmAcctManager.Show 1, Me
End Sub

Private Sub mnuBackup_Click()
    Me.MousePointer = vbHourglass
    frmRecover.Show 1, Me
    Me.MousePointer = vbDefault
End Sub

Private Sub mnuBackupSettings_Click()
    Load frmConfig
    frmConfig.lstCategory.ListIndex = 5
    frmConfig.Show 1, Me
End Sub

Private Sub mnuBackupStop_Click()
    Call disableBackup
End Sub

Private Sub mnuCloseFiles_Click()
    Dim rc As Integer

    rc = MsgBox("Close all unused files being cached by the server?", vbExclamation + vbYesNo, "Close Unused Cached Files")
    If rc = vbYes Then
        Call code4errOff(gCode4, 1)
        rc = code4serverCloseFiles(gCode4)
        If rc = r4success Then
            rc = MsgBox("The unused cached files were successfully closed.", vbInformation + vbOKOnly, "Close Unused Cached Files")
        Else
            Dim errno%
            errno = code4errorCode(gCode4, 0)
            Select Case errno
                Case -1399 To -1300
                    ' connection error; probably lost connection to server
                    Call MsgBox("Connection to host lost.", vbExclamation, App.title)
                    Call frmMain2.mnuDisconnect_Click
                Case Else
                    Call MsgBox("Unable to unused cached files. CodeBase error" & errno & ".", vbExclamation, App.title)
            End Select
        End If
    End If
End Sub

Private Sub mnuCon_Click()
    Me.MousePointer = 11
    Form6.Show
    Me.MousePointer = 0
End Sub

Private Sub mnuConfig_Click()
    frmConfig.Show 1, Me
End Sub

Private Sub mnuConnect_Click()
    frmLogin.Show 1, Me
    Me.MousePointer = 0
End Sub

Private Sub mnuConTable_Click()
    Form12.Show
End Sub

Private Sub mnuContents_Click()
    Call HtmlHelp(Me.hWnd, App.path + "\" + App.HelpFile, HH_DISPLAY_TOC, 0)
End Sub

Sub mnuDisconnect_Click()
    Call disableBackup
    Call DisconnectCode4
    Call setMenus(caCSNotConnected)
End Sub

Private Sub mnuDisconnectClient_Click()
    Form21.Show 1, Me
End Sub

Private Sub mnuDisconnectAll_Click()
    Dim rc As Integer

    rc = MsgBox("WARNING: Are you sure you want to disconnect all other clients except this application?", vbExclamation + vbYesNo, "Disconnect All Other Clients")
    If rc = vbYes Then
        Call ErrOff
        rc = code4connectCutAll(gCode4)
        If rc = r4success Then
            Call MsgBox("All other clients successfully disconnected.", vbInformation + vbOKOnly, "Disconnect Other Clients")
        Else
            Dim errno%
            errno = code4errorCode(gCode4, 0)
            Select Case errno
                Case -1399 To -1300
                    ' connection error; probably lost connection to server
                    Call MsgBox("Connection to host lost.", vbExclamation, App.title)
                    Call frmMain2.mnuDisconnect_Click
                Case e4authorize
                    Call code4errorCode(gCode4, 0)
                    Call MsgBox("You do not have permission to disconnect other clients.", vbInformation + vbOKOnly, "Immediate Shutdown")
                Case Else
                    Call MsgBox("Unable to disconnect clients. CodeBase error" & errno & ".", vbExclamation, App.title)
            End Select
        End If
    End If
End Sub

Private Sub mnuEncKey_Click()
    CommonDialog1.DialogTitle = "Select Encryption Key File"
    CommonDialog1.filter = "All files (*.*)|*.*"
    CommonDialog1.Flags = cdlOFNFileMustExist Or cdlOFNHideReadOnly Or cdlOFNPathMustExist
    CommonDialog1.ShowOpen

    If Len(CommonDialog1.fileName) > 0 Then
        On Error GoTo encerr
        Dim fLen As Integer
        fLen = fileLen(CommonDialog1.fileName)
        Select Case fLen
            Case Is > 70
                Call MsgBox("Invalid encryption key size. Must be 1, 2, 4, 8, 16 or 32 bytes.", vbExclamation, App.ProductName)
                Exit Sub
            Case Is >= 64
                fLen = 64
            Case Is >= 32
                fLen = 32
            Case Is >= 16
                fLen = 16
            Case Is >= 8
                fLen = 8
            Case Is >= 4
                fLen = 4
            Case Is >= 2
                fLen = 2
            Case Else
                fLen = 1
        End Select

        Dim fileNum As Integer
        fileNum = FreeFile

        Open CommonDialog1.fileName For Binary Access Read Shared As #fileNum
        encryptionKey = Space(fLen)
        Get #fileNum, , encryptionKey
        Close #fileNum

        Dim rc As Integer
        rc = code4encryptInit(gCode4, encryptionKey, fLen)
        If rc = r4success Then
            encryptionKeyFileName = CommonDialog1.fileName
            Call MsgBox(CStr(fLen * 8) & "-bit encryption key set.", vbInformation, App.ProductName)
        Else
            Call code4errorCode(gCode4, 0)
        End If
    End If

    Exit Sub
encerr:
    Call MsgBox("Unable to read encryption key from file.", vbExclamation, App.ProductName)
End Sub

Private Sub mnuExit_Click()
    Unload Me
End Sub

Public Sub setMenus(status%)
    Select Case status
        Case caStandAlone
                ' file menu
                mnuConnect.Visible = False
                mnuDisconnect.Visible = False
                mnuSep4.Visible = False
            mnuServer.Visible = False
            mnuReports.Visible = False
                ' tools menu (CS 2003/08/29 Make backup menu items invisible as per LY/AS.)
                mnuBackupSettings.Visible = False
                mnuBackup.Visible = False
                mnuSepToolBackup.Visible = False
        Case caCSConnected, caCSNotConnected
                ' file menu
                mnuConnect.Visible = True
                mnuDisconnect.Visible = True
                mnuSep4.Visible = True
            mnuServer.Visible = True
            mnuReports.Visible = True

            Select Case status
                Case caCSConnected
                        ' file menu
                        mnuDisconnect.Enabled = True
                        mnuMaint.Enabled = True
                    mnuServer.Enabled = True
                    mnuReports.Enabled = True
                        'Tools menu
                        mnuBackupSettings.Enabled = True
                Case caCSNotConnected
                        ' file menu
                        mnuDisconnect.Enabled = False
                        mnuMaint.Enabled = False
                    mnuServer.Enabled = False
                    mnuReports.Enabled = False
                        'Tools menu
                        mnuBackupSettings.Enabled = False
            End Select
    End Select
End Sub

Private Sub mnuLockCLT_Click()
    frmLockCLT.Show
End Sub

Private Sub mnuLockCTL_Click()
    frmLockCTL.Show
End Sub

Private Sub mnuLockLCT_Click()
    frmLockLCT.Show
End Sub

Private Sub mnuLockLTC_Click()
    frmLockLTC.Show
End Sub

Private Sub mnuLockTCL_Click()
    frmLockTCL.Show
End Sub

Private Sub mnuLockTLC_Click()
    frmLockTLC.Show
End Sub

Private Sub mnuLogAnalyze_Click()
    frm32Dbf.Show 1, Me
End Sub

Private Sub mnuLogBackup_Click()
    frm32Back.Show 1, Me
End Sub

Private Sub mnuLogRecover_Click()
    frm32Fix.Show 1, Me
End Sub

Private Sub mnuLogRestore_Click()
    frm32Res.Show 1, Me
End Sub

Private Sub mnuMaint_Click()
    'Show Generic file handling form
    Form7.Show
End Sub

Private Sub mnuOptions_Click()
    frmOptions.Show 1, Me
End Sub

Private Sub mnuServerInfo_Click()
    Me.MousePointer = 11
    Form9.Show
    Me.MousePointer = 0
End Sub

Private Sub mnuShutDown_Click()
    Dim rc As VbMsgBoxResult

    rc = MsgBox("WARNING: Shutting down the server will disconnect all users, " _
            & "possibly interrupting work in progress." & vbCrLf & _
            "Are you sure you want to shutdown the server now?", vbExclamation + vbYesNo + vbDefaultButton2, "Immediate Shutdown")
    If rc = vbYes Then
        Call ErrOff
        Call code4errorCode(gCode4, 0)
        rc = code4serverShutdown(gCode4)
        If rc = r4success Then
            Call MsgBox("Server was successfully shut down.", vbInformation + vbOKOnly, "Immediate Shutdown")
        Else
            Dim errno%
            errno = code4errorCode(gCode4, 0)
            Select Case errno
                Case -1399 To -1300
                    ' connection error; probably lost connection to server
                    Call MsgBox("Connection to host lost.", vbExclamation, App.title)
                    Call frmMain2.mnuDisconnect_Click
                Case e4authorize
                    Call code4errorCode(gCode4, 0)
                    Call MsgBox("You do not have permission to shut down the server.", vbInformation + vbOKOnly, "Immediate Shutdown")
                Case Else
                    Call MsgBox("Unable to shut down the server. CodeBase error " & errno & ".", vbExclamation, App.title)
            End Select
        End If
    End If
End Sub

Private Sub mnuTableCon_Click()
    Form14.Show
End Sub

Private Sub mnuTables_Click()
    Form13.Show
End Sub

Private Sub SetHelp()
    'set the HelpContextID for each menu item
    HelpContextID = CBAdmin_Introduction

    'file menu
    mnuConnect.HelpContextID = Client_Server_Startup
    mnuDisconnect.HelpContextID = Disconnect_menu
    mnuFile.HelpContextID = File_Maintenance

    'server menu
    mnuServerInfo.HelpContextID = Server_Information
    mnuConfig.HelpContextID = Server_Settings
    mnuAccounts.HelpContextID = Account_Manager
    mnuCloseFiles.HelpContextID = Close_Unused_Cached_Files
    mnuShutDown.HelpContextID = help.Shutdown
    mnuAcceptNew.HelpContextID = Accept_New_Connections
    mnuDisconnectClient.HelpContextID = Disconnect_menu
    mnuDisconnectAll.HelpContextID = Disconnect_All_menu

    'reports menu
    mnuCon.HelpContextID = Connections_Report
    mnuConTable.HelpContextID = Connections_Tables_Report
    mnuTables.HelpContextID = Open_Tables_Report
    mnuTableCon.HelpContextID = Open_Tables_con_Report
    mnuLock.HelpContextID = Lock_Reports
    mnuLockCTL.HelpContextID = Lock_Reports
    mnuLockCLT.HelpContextID = Lock_Reports
    mnuLockTLC.HelpContextID = Lock_Reports
    mnuLockTCL.HelpContextID = Lock_Reports
    mnuLockLTC.HelpContextID = Lock_Reports
    mnuLockLCT.HelpContextID = Lock_Reports

    'tools menu
    mnuBackupSettings.HelpContextID = Backup_Settings
    mnuBackup.HelpContextID = Update_Backup
    mnuLogAnalyze.HelpContextID = Log_Analyze
    mnuLogRecover.HelpContextID = Log_Recover
    mnuLogBackup.HelpContextID = Log_Backup
    mnuLogRestore.HelpContextID = Log_Restore
    mnuOptions.HelpContextID = Options
End Sub

Private Sub StatusBar1_DblClick()
    If bLoggedIn Then frmRecover.Show 1, Me
End Sub

Private Sub StatusBar1_MouseUp(Button As Integer, Shift As Integer, x As Single, y As Single)
    If timerBackup.Enabled Then
        PopupMenu mnuStatusPopup, , , , mnuBackupStop
    End If
End Sub

Private Sub timerBackup_Timer()
    secondsUntilNextBackup = secondsUntilNextBackup - 1

    If secondsUntilNextBackup <= 0 Then
        ' The time to the next backup has elapsed, so do it.
        timerBackup.Enabled = False  ' disable the timer until the backup is done
        StatusBar1.SimpleText = "Running backup"

        Dim prepend$, path$
        prepend = GetSetting(App.title, "Backup", "PreLog")
        If GetSetting(App.title, "Backup", "UseLogPath") = vbUnchecked Then
            path = GetSetting(App.title, "Backup", "Path")
        End If

        Dim rc%
        rc = doBackup(prepend, path, , True)
        If rc <> r4success Then
            Call disableBackup
            MsgBox "Automatic backup failed. Error " & rc & ". Backup schedule stopped.", vbCritical, "Backup Error"
            Exit Sub
        End If

        ' reset the time to the next backup
        secondsUntilNextBackup = IntervalToSeconds( _
            CLng(GetSetting(App.title, "Backup", "Interval")), _
            CLng(GetSetting(App.title, "Backup", "IntervalUnits")))
        If bLoggedIn Then
            timerBackup.Enabled = True  ' reenable the timer
        Else
            Call disableBackup
        End If
    End If

    Call refreshStatus
End Sub

Public Sub refreshStatus()
    ' display the time to the next backup in the status bar
    If timerBackup.Enabled Then
        Dim unitStr$, amtLeft%
        Select Case secondsUntilNextBackup
            Case 1
                amtLeft = secondsUntilNextBackup
                unitStr = "second"
            Case 2 To 119  ' under 2 minutes
                amtLeft = secondsUntilNextBackup
                unitStr = "seconds"
            Case 120 To 6399  ' 2 min to 2 hours
                amtLeft = secondsUntilNextBackup / 60
                unitStr = "minutes"
            Case 6400 To 172799  ' 2 hours to 2 days
                amtLeft = secondsUntilNextBackup / 3600
                unitStr = "hours"
            Case Else  ' more than 2 days
                amtLeft = secondsUntilNextBackup / 86400
                unitStr = "days"
        End Select

        StatusBar1.SimpleText = "Next backup in " & amtLeft & " " & unitStr & "."
    End If
End Sub

Public Sub disableBackup()
    timerBackup.Enabled = False
    StatusBar1.SimpleText = "Backup not scheduled."
End Sub
