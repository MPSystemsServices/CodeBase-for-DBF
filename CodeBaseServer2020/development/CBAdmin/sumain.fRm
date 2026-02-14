VERSION 5.00
Object = "{248DD890-BB45-11CF-9ABC-0080C7E7B78D}#1.0#0"; "MSWINSCK.OCX"
Begin VB.Form frmMain
   BorderStyle     =   1  'Fixed Single
   Caption         =   "CodeBase Administrator"
   ClientHeight    =   2715
   ClientLeft      =   570
   ClientTop       =   1665
   ClientWidth     =   8775
   Icon            =   "sumain.frx":0000
   LinkTopic       =   "Form2"
   MaxButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   2715
   ScaleWidth      =   8775
   Begin MSWinsockLib.Winsock Winsock1
      Left            =   6480
      Top             =   0
      _ExtentX        =   741
      _ExtentY        =   741
      _Version        =   393216
   End
   Begin VB.Menu MenuGeneral
      Caption         =   "&General"
      Begin VB.Menu MenuLogin
         Caption         =   "&Login As Different User"
         Shortcut        =   ^L
      End
      Begin VB.Menu MenuServerInfo
         Caption         =   "Server &Information"
         Shortcut        =   ^I
      End
      Begin VB.Menu MenuGenConfig
         Caption         =   "Server &Configuration"
         Shortcut        =   ^E
      End
      Begin VB.Menu MenuShutdown
         Caption         =   "&Shutdown"
         Begin VB.Menu MenuImmedShut
            Caption         =   "&Immediate Shutdown"
         End
         Begin VB.Menu MenuNewConn
            Caption         =   "&Accept New Connections"
            Checked         =   -1  'True
         End
         Begin VB.Menu MenuDisconnOthers
            Caption         =   "&Disconnect All Others"
         End
         Begin VB.Menu MenuDisconnSpecific
            Caption         =   "Disconnect &Specific"
         End
         Begin VB.Menu MenuCloseFiles
            Caption         =   "&Close Unused Cached Files"
         End
      End
      Begin VB.Menu MenuGeneric
         Caption         =   "File &Maintenance"
         Shortcut        =   ^M
      End
      Begin VB.Menu Separator
         Caption         =   "-"
      End
      Begin VB.Menu mnuQuit
         Caption         =   "E&xit"
      End
   End
   Begin VB.Menu MenuAcct
      Caption         =   "&Accounts"
      Begin VB.Menu MenuEditAcct
         Caption         =   "&Edit"
      End
      Begin VB.Menu MenuRepAcct
         Caption         =   "&Report"
      End
   End
   Begin VB.Menu MenuPriv
      Caption         =   "&File Privileges"
      Begin VB.Menu MenuEditPriv
         Caption         =   "&Edit"
      End
      Begin VB.Menu MenuRepPriv
         Caption         =   "&Report"
         Begin VB.Menu MenuRepPrivAcct
            Caption         =   "&AccountId with Tables"
         End
         Begin VB.Menu MenuRepPrivTable
            Caption         =   "&Tables with  AccountId"
         End
      End
   End
   Begin VB.Menu MenuConn
      Caption         =   "&Connection Reports"
      Begin VB.Menu MenuRepConn
         Caption         =   "&Connections"
      End
      Begin VB.Menu MenuRepConnTable
         Caption         =   "Connections with &Tables"
      End
      Begin VB.Menu MenuRepTable
         Caption         =   "&Open Tables"
      End
      Begin VB.Menu MenuRepTableConn
         Caption         =   "O&pen Tables with Connections"
      End
      Begin VB.Menu MenuRepLock
         Caption         =   "&Locks"
         Begin VB.Menu MenuRepTableConnLock
            Caption         =   "&1 Table, Connection, Lock Type"
         End
         Begin VB.Menu MenuRepTableLockConn
            Caption         =   "&2 Table, Lock Type, Connection"
         End
         Begin VB.Menu MenuRepConnTableLock
            Caption         =   "&3 Connection, Table, Lock Type"
         End
         Begin VB.Menu MenuRepConnLockTable
            Caption         =   "&4 Connection, Lock Type, Table"
         End
         Begin VB.Menu MenuRepLockTableConn
            Caption         =   "&5 Lock Type, Table, Connection"
         End
         Begin VB.Menu MenuRepLockConnTable
            Caption         =   "&6 Lock Type, Connection, Table"
         End
      End
   End
   Begin VB.Menu MenuHelp
      Caption         =   "&Help"
      Begin VB.Menu MenuContents
         Caption         =   "&Contents"
      End
      Begin VB.Menu MenuAbout
         Caption         =   "&About..."
      End
   End
End
Attribute VB_Name = "frmMain"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Public Sub setMenus(status%)
    Select Case status
        Case caStandAlone
            MenuGeneral.Visible = True
                MenuLogin.Visible = False
                MenuServerInfo.Visible = False
                MenuGenConfig.Visible = False
                MenuShutdown.Visible = False
                MenuGeneric.Visible = True
            MenuAcct.Visible = False
            MenuPriv.Visible = False
            MenuConn.Visible = False
            MenuGeneric.Visible = True
            MenuHelp.Visible = True
        Case caCSConnected, caCSNotConnected
            MenuGeneral.Visible = True
                MenuLogin.Visible = True
                MenuServerInfo.Visible = True
                MenuGenConfig.Visible = True
                MenuShutdown.Visible = True
                MenuGeneric.Visible = True
            MenuAcct.Visible = True
            MenuPriv.Visible = True
            MenuConn.Visible = True
            MenuGeneric.Visible = True
            MenuHelp.Visible = True

            MenuGeneral.Enabled = True
            Select Case status
                Case caCSConnected
                    MenuGeneral.Enabled = True
                        MenuLogin.Enabled = True
                        MenuServerInfo.Enabled = True
                        MenuGenConfig.Enabled = True
                        MenuShutdown.Enabled = True
                        MenuGeneric.Enabled = True
                    MenuAcct.Enabled = True
                    MenuPriv.Enabled = True
                    MenuConn.Enabled = True
                Case caCSNotConnected
                    MenuGeneral.Enabled = True
                        MenuLogin.Enabled = True
                        MenuServerInfo.Enabled = False
                        MenuGenConfig.Enabled = False
                        MenuShutdown.Enabled = False
                        MenuGeneric.Enabled = False
                    MenuAcct.Enabled = False
                    MenuPriv.Enabled = False
                    MenuConn.Enabled = False
            End Select
    End Select
End Sub

Private Sub Form_Load()
    Splash.Show
    HelpContextID = Introduction_to_CodeBase_Server_Administrator
    SepChar = "\"

    gCode4 = code4init()
    If gCode4 = 0 Then
        showErrInit
        End
    End If
    Call code4logOpenOff(gCode4)

    If isClientServer Then
        Load Form1
        setMenus caCSNotConnected
    Else
        setMenus caStandAlone
    End If
    Me.caption = App.title
    Me.Show
    Do While Splash.timerDone = False
        DoEvents
    Loop
    Unload Splash
    If isClientServer Then
        Form1.Show 1
    End If
End Sub

Private Sub Form_Paint()
    'set the dimentions of the window to the dimensions of the screen
    Dim dimensions As RECT
    Call SystemParametersInfo(48, 0, dimensions, 0)
    Left = dimensions.Left * 15
    Top = 0
    Width = (dimensions.Right - dimensions.Left) * 15
    Height = 800
End Sub

Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
    DisconnectCode4
End Sub

Private Sub Form_Unload(Cancel As Integer)
    Dim rc%, errMsg$
    Call WinHelp(frmMain.hWnd, "cbadmin.hlp", HELP_QUIT, 0)

    ErrOff
    rc = DisconnectCode4
    If rc <> 0 Then
        Select Case rc
        Case -1310
            errMsg = " : Connection Failure"
        Case -1320
            errMsg = " : Socket Failure"
        Case -1390
            errMsg = " : System-level Communications Failure"
        End Select
        ShowErrLow EDISCONNECT, str$(rc) & errMsg
    End If
    End
End Sub
Private Sub MenuAbout_Click()
    frmAbout.Show 1
End Sub
Private Sub MenuCloseFiles_Click()
    Dim rc As Integer

    rc = MsgBox("WARNING: Are you sure you want close all unused files being cached by the server?", vbExclamation + vbYesNo, "Close Unused Cached Files")
    If rc = vbYes Then
        If gCode4 = 0 Then
            showErrInit
        Else
            Call ErrOff
            rc = code4serverCloseFiles(gCode4)
            If rc = r4success Then
                rc = MsgBox("The unused cached files were successfully closed", vbInformation + vbOKOnly, "Close Unused Cached Files")
            Else
                Call code4errorCode(gCode4, 0)
                rc = MsgBox("You do not have permission to close the unused cached files", vbInformation + vbOKOnly, "Close Unused Cached Files")
            End If
        End If
    End If
End Sub

Private Sub MenuContents_Click()
    Call WinHelp(frmMain.hWnd, "cbadmin.hlp", HELP_FINDER, 0)
End Sub
Private Sub MenuDisconnOthers_Click()
    Dim rc As Integer

    rc = MsgBox("WARNING: Are you sure you want to disconnect all other clients except this application?", vbExclamation + vbYesNo, "Disconnect All Other Clients")
    If rc = vbYes Then
        Call ErrOff
        rc = code4connectCutAll(gCode4)
        If rc = r4success Then
            Call MsgBox("All other clients successfully disconnected", vbInformation + vbOKOnly, "Disconnect Other Clients")
        Else
            Call MsgBox("You do not have permission to disconnect other clients", vbInformation + vbOKOnly, "Disconnect Other Clients")
            Call code4errorCode(gCode4, 0)
        End If
    End If
End Sub
Private Sub MenuDisconnSpecific_Click()
    Me.MousePointer = 11
    'Form21.Show
    '
    If Form21.RegenerateStatus Then Form21.Show
    Me.MousePointer = 0
End Sub
Private Sub MenuEditAcct_Click()
'Edit the Account data file information
'check if account has permission to edit the
'password, if not lock the password edit box
    Me.MousePointer = 11
    'Form4.Show
    Me.MousePointer = 0
End Sub
Private Sub MenuEditPriv_Click()
'edit the privileges file
    Me.MousePointer = 11
    'Form5.Show
    Me.MousePointer = 0
End Sub
Private Sub MenuGenConfig_Click()
'edit the server configuration file
    Me.MousePointer = 11
    'Form3.Show
    frmConfig2.Show
    Me.MousePointer = 0
End Sub
Private Sub MenuGeneric_Click()
'Show Generic file handling form
    Screen.MousePointer = 11
    Form7.Show
    Screen.MousePointer = 0
End Sub
Private Sub MenuImmedShut_Click()
    Dim rc As Integer

    rc = MsgBox("WARNING: Shutting down the server will disconnect all users, possibly interrupting work in progress, in addition to ending this application." & Chr(13) & Chr(10) & Chr(13) & Chr(10) & _
    "Are you sure you want to shutdown the server now?", vbExclamation + vbYesNo, "Immediate Shutdown")
    If rc = vbYes Then
        Call ErrOff
        Call code4errorCode(gCode4, 0)
        rc = code4serverShutdown(gCode4)
        Select Case rc
            Case r4success
                'Me.Hide
                'unloadForms
                Call MsgBox("Server was successfully shutdown", vbInformation + vbOKOnly, "Immediate Shutdown")
                'Form1.Show
                End
            Case e4authorize
                Call code4errorCode(gCode4, 0)
                Call MsgBox("You do not have permission to shutdown the server", vbInformation + vbOKOnly, "Immediate Shutdown")
            Case Else
                ShowError
        End Select
    End If
End Sub
Private Sub MenuLogin_Click()
'df moved UnloadForms to Form1.Connect_Click() event
'UnloadForms
    Form1.Show 1
    Me.MousePointer = 0
End Sub
Private Sub MenuNewConn_Click()
    Dim rc As Integer
    Dim flag As Boolean

    Call ErrOff
    rc = code4connectAcceptNew(gCode4, Not MenuNewConn.Checked)
    Call ErrOn
    If rc = r4success Then
        MenuNewConn.Checked = Not MenuNewConn.Checked

        If MenuNewConn.Checked = False Then
            rc = MsgBox("No new clients may connect to the server", vbInformation + vbOKOnly, "Accept New Connections")
        Else
            rc = MsgBox("New clients may connect to the server", vbInformation + vbOKOnly, "Accept New Connections")
        End If
    Else
        Call code4errorCode(gCode4, 0)
        rc = MsgBox("You do not have permission to change this setting", vbInformation + vbOKOnly, "Accept New Connections")
    End If
End Sub
Private Sub MenuRepAcct_Click()
'Show the Account data file information

'if previous db error on form, reload it in case
'it was only hidden previously, rather than
'being unloaded
    Me.MousePointer = 11
    Form10.Show
    Me.MousePointer = 0
End Sub
'Show the Connections form
Private Sub MenuRepConn_Click()
    Me.MousePointer = 11
    Form6.Show
    Me.MousePointer = 0
End Sub
Private Sub MenuRepConnLockTable_Click()
'Show Locks -Connections, Lock Type and Tables form
    Me.MousePointer = 11
    Form17.Show
    Me.MousePointer = 0
End Sub
'show the connections and tables form
Private Sub MenuRepConnTable_Click()
    Me.MousePointer = 11
    Form12.Show
    Me.MousePointer = 0
End Sub
Private Sub MenuRepConnTableLock_Click()
'Show Locks -Connections, Table and LockType
    Me.MousePointer = 11
    Form18.Show
    Me.MousePointer = 0
End Sub
Private Sub MenuRepLockConnTable_Click()
'show Locks -Lock Type, Connection, table
    Me.MousePointer = 11
    Form20.Show
    Me.MousePointer = 0
End Sub
Private Sub MenuRepLockTableConn_Click()
'Show Locks -Lock Type, Table, Connection
    Me.MousePointer = 11
    Form19.Show
    Me.MousePointer = 0
End Sub
Private Sub MenuRepPrivAcct_Click()
'Show Privileges Report -AccountId with Tables
    Me.MousePointer = 11
    Form11a.Show
    Me.MousePointer = 0
End Sub
Private Sub MenuRepPrivTable_Click()
'Show Privileges Report- Tables with AccountId
    Me.MousePointer = 11
    Form11.Show
    Me.MousePointer = 0
End Sub
'show open tables form
Private Sub MenuRepTable_Click()
    Me.MousePointer = 11
    Form13.Show
    Me.MousePointer = 0
End Sub
Private Sub MenuRepTableConn_Click()
'show tables with connection information
    Me.MousePointer = 11
    Form14.Show
    Me.MousePointer = 0
End Sub
Private Sub MenuRepTableConnLock_Click()
'Show Locks: Path+Table+Connection+locktype
    Me.MousePointer = 11
    Form15.Show
    Me.MousePointer = 0
End Sub
Private Sub MenuRepTableLockConn_Click()
'show the Locks -Table, LockType and Connections
    Me.MousePointer = 11
    Form16.Show
    Me.MousePointer = 0
End Sub
Private Sub MenuServerInfo_Click()
    Me.MousePointer = 11
    Form9.Show
    Me.MousePointer = 0
End Sub
Private Sub mnuQuit_Click()
    Unload Me
End Sub


