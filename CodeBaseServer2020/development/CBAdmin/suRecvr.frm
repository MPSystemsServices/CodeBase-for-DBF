VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "comdlg32.ocx"
Begin VB.Form frmRecover
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Update Backup"
   ClientHeight    =   2715
   ClientLeft      =   2415
   ClientTop       =   1515
   ClientWidth     =   5940
   HasDC           =   0   'False
   Icon            =   "suRecvr.frx":0000
   LinkTopic       =   "Form2"
   LockControls    =   -1  'True
   MaxButton       =   0   'False
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   2715
   ScaleWidth      =   5940
   ShowInTaskbar   =   0   'False
   StartUpPosition =   2  'CenterScreen
   Begin VB.ComboBox cmbIntervalUnits
      Height          =   315
      ItemData        =   "suRecvr.frx":000C
      Left            =   1920
      List            =   "suRecvr.frx":001C
      Style           =   2  'Dropdown List
      TabIndex        =   14
      Top             =   2280
      Width           =   1215
   End
   Begin VB.TextBox txtInterval
      Height          =   315
      Left            =   1260
      TabIndex        =   12
      Text            =   "Text1"
      Top             =   2280
      Width           =   555
   End
   Begin VB.CheckBox chkSchedule
      Caption         =   "Run every"
      Height          =   195
      Left            =   120
      TabIndex        =   13
      Top             =   2325
      Width           =   1155
   End
   Begin VB.CheckBox chkUseLogPath
      Caption         =   "&Use path from log file"
      Height          =   195
      Left            =   2040
      TabIndex        =   6
      Top             =   1560
      Width           =   2055
   End
   Begin MSComDlg.CommonDialog CommonDialog1
      Left            =   4920
      Top             =   1440
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
      DefaultExt      =   "log"
      Filter          =   "Log Files (*.log)|*.log|All Files (*.*)|*.*"
   End
   Begin VB.CommandButton cmdDirBrowse
      Caption         =   "..."
      Height          =   315
      Left            =   4200
      TabIndex        =   9
      Top             =   1800
      Width           =   315
   End
   Begin VB.CommandButton cmdBrowse
      Caption         =   "..."
      BeginProperty Font
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   315
      Index           =   1
      Left            =   4200
      TabIndex        =   5
      Top             =   1080
      Width           =   315
   End
   Begin VB.CommandButton cmdBrowse
      Caption         =   "..."
      BeginProperty Font
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   315
      Index           =   0
      Left            =   4200
      TabIndex        =   2
      Top             =   360
      Width           =   315
   End
   Begin VB.CommandButton cmdCancel
      Cancel          =   -1  'True
      Caption         =   "Cancel"
      Height          =   375
      Left            =   4680
      TabIndex        =   11
      Top             =   600
      Width           =   1130
   End
   Begin VB.CommandButton cmdOK
      Caption         =   "Update Now"
      Default         =   -1  'True
      Height          =   350
      Left            =   4680
      TabIndex        =   10
      Top             =   120
      Width           =   1130
   End
   Begin VB.TextBox txtPreLog
      Height          =   315
      Left            =   120
      TabIndex        =   4
      Top             =   1080
      Width           =   4005
   End
   Begin VB.TextBox txtPath
      Height          =   315
      Left            =   120
      TabIndex        =   8
      Top             =   1800
      Width           =   4005
   End
   Begin VB.TextBox txtBackLog
      Height          =   315
      Left            =   120
      TabIndex        =   1
      Top             =   360
      Width           =   4005
   End
   Begin VB.Label Label5
      AutoSize        =   -1  'True
      Caption         =   "P&ending Transaction File:"
      Height          =   195
      Left            =   120
      TabIndex        =   3
      Top             =   840
      Width           =   1800
   End
   Begin VB.Label Label4
      AutoSize        =   -1  'True
      Caption         =   "&Path To Data Files:"
      Height          =   195
      Left            =   120
      TabIndex        =   7
      Top             =   1560
      Width           =   1365
   End
   Begin VB.Label lblBackLog
      AutoSize        =   -1  'True
      Caption         =   "&Backup Log File:"
      Height          =   195
      Left            =   120
      TabIndex        =   0
      Top             =   120
      Width           =   1200
   End
End
Attribute VB_Name = "frmRecover"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub chkSchedule_Click()
    Call checkFields
End Sub

Private Sub chkUseLogPath_Click()
    If chkUseLogPath.value = vbChecked Then
        cmdDirBrowse.Enabled = False
        txtPath.Enabled = False
        txtPath.BackColor = vbButtonFace
    Else
        cmdDirBrowse.Enabled = True
        txtPath.Enabled = True
        txtPath.BackColor = vbWindowBackground
    End If
    Call checkFields
End Sub

Private Sub cmdBrowse_Click(Index As Integer)
    CommonDialog1.Flags = cdlOFNHideReadOnly + cdlOFNPathMustExist
    Select Case Index
        Case 0
            ' backup log file
            CommonDialog1.DialogTitle = "Locate Backup Log File"
            CommonDialog1.Flags = CommonDialog1.Flags + cdlOFNFileMustExist
        Case 1
            ' pending transaction (prepend) file
            CommonDialog1.DialogTitle = "Locate Pending Transaction File"
    End Select

    CommonDialog1.ShowOpen
    If Len(CommonDialog1.fileName) = 0 Then
        ' user pressed cancel
        Exit Sub
    End If

    Select Case Index
        Case 0
            txtBackLog.Text = CommonDialog1.fileName
        Case 1
            txtPreLog.Text = CommonDialog1.fileName
    End Select
End Sub

Private Sub cmdCancel_Click()
    Unload Me
End Sub

Private Sub cmdDirBrowse_Click()
    BrowseForFolder.SetDefaults
    Call BrowseForFolder.Display(Me.hWnd, "Select location of backup data files.")
    If BrowseForFolder.successful Then
        txtPath = BrowseForFolder.folderName
    End If
End Sub

Private Sub cmdOK_Click()
    Dim path$
    If chkUseLogPath.value = vbChecked Then
        path = ""
    Else
        path = txtPath.Text
    End If

    Call SaveSetting(App.title, "Backup", "PreLog", txtPreLog.Text)
    Call SaveSetting(App.title, "Backup", "Path", txtPath.Text)
    Call SaveSetting(App.title, "Backup", "UseLogPath", chkUseLogPath.value)
    Call SaveSetting(App.title, "Backup", "Schedule", chkSchedule.value)
    Call SaveSetting(App.title, "Backup", "Interval", txtInterval.Text)
    Call SaveSetting(App.title, "Backup", "IntervalUnits", cmbIntervalUnits.ListIndex)



    If chkSchedule.Enabled And chkSchedule.value = vbChecked Then
        Call setSchedule(CInt(txtInterval.Text), cmbIntervalUnits.ListIndex)
    Else
        Call doBackup(txtPreLog.Text, path, txtBackLog.Text)
    End If

    Unload Me
End Sub

Private Sub Form_Load()
    Dim rc%

    chkSchedule.value = CInt(GetSetting(App.title, "Backup", "Schedule", vbUnchecked))
    txtInterval.Text = GetSetting(App.title, "Backup", "Interval", 5)
    cmbIntervalUnits.ListIndex = CInt(GetSetting(App.title, "Backup", "IntervalUnits", 1))

    If bLoggedIn Then
        Dim s4server&
        Dim fBacklog1&, fBacklog2&, fPrebacklog&
        rc = code4errOff(gCode4, 1)
        s4server = d4open(gCode4, configName)
        Call code4errOff(gCode4, rc)
        If s4server = 0 Then
            If code4errorCode(gCode4, 0) = e4timeOut Then
                DisconnectCode4
            Else
                MsgBox "Error " & code4errorCode(gCode4, 0) & " opening " & configName & ".", vbCritical, configName
                CloseWindow Me.hWnd
                Exit Sub
            End If
        End If
    End If

    If bLoggedIn Then
        Call d4top(s4server)

        Call code4errFieldName(gCode4, 0)
        fPrebacklog = d4field(s4server, "PREBACKLOG")
        Call code4errFieldName(gCode4, 1)
        If fPrebacklog = 0 Then
            MsgBox "Automatic Recovery fields not found in " & configName & ".", vbCritical, configName
            Call d4close(s4server)
            CloseWindow Me.hWnd
            Exit Sub
        End If

        backlog1 = Trim(f4str(d4field(s4server, "BACKLOG1")))
        backlog2 = Trim(f4str(d4field(s4server, "BACKLOG2")))

        txtPreLog.Text = Trim(f4str(fPrebacklog))

        Call d4close(s4server)
        s4server = 0

        ' we are connected to the server
        With txtBackLog
            .Text = "(Will Receive From Server)"
            .BackColor = vbButtonFace
            .Enabled = False
        End With
        cmdBrowse(0).Enabled = False
    Else
        ' not connected to the server
        With txtBackLog
            .Text = GetSetting(App.title, "Backup", "BackLog")
            .BackColor = vbWindowBackground
            .Enabled = True
        End With
        cmdBrowse(0).Enabled = True

        txtPreLog.Text = GetSetting(App.title, "Backup", "PreLog")

        chkSchedule.Enabled = False
    End If

    txtPath.Text = GetSetting(App.title, "Backup", "Path")
    chkUseLogPath.value = GetSetting(App.title, "Backup", "UseLogPath", vbChecked)
End Sub

Private Sub checkFields()
    If chkSchedule.Enabled And chkSchedule.value = vbChecked Then
        cmdOK.caption = "OK"

        txtInterval.Enabled = True
        txtInterval.BackColor = vbWindowBackground
        cmbIntervalUnits.Enabled = True
        cmbIntervalUnits.BackColor = vbWindowBackground
    Else
        cmdOK.caption = "Update Now"

        txtInterval.Enabled = False
        txtInterval.BackColor = vbButtonFace
        cmbIntervalUnits.Enabled = False
        cmbIntervalUnits.BackColor = vbButtonFace
    End If

    If Len(Trim(txtBackLog.Text)) = 0 Or _
            Len(Trim(txtPreLog.Text)) = 0 Or _
            chkUseLogPath.value = vbUnchecked And Len(Trim(txtPath.Text)) = 0 Or _
            chkSchedule.Enabled And chkSchedule.value = vbChecked And Len(Trim(txtInterval.Text)) = 0 Then
        cmdOK.Enabled = False
    Else
        cmdOK.Enabled = True
    End If
End Sub

Private Sub txtBackLog_Change()
    Call checkFields
End Sub

Private Sub txtBackLog_GotFocus()
    TxtGotFocus Me
End Sub

Private Sub txtInterval_Change()
    checkFields
End Sub

Private Sub txtInterval_KeyPress(KeyAscii As Integer)
    If Not IsNumeric(KeyAscii) Then
        KeyAscii = 0
    End If
End Sub

Private Sub txtPath_Change()
    Call checkFields
End Sub

Private Sub txtPath_GotFocus()
    TxtGotFocus Me
End Sub

Private Sub txtPreLog_Change()
    Call checkFields
End Sub

Private Sub txtPreLog_GotFocus()
    TxtGotFocus Me
End Sub
