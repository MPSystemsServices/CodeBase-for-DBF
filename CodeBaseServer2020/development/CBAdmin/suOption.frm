VERSION 5.00
Begin VB.Form frmOptions
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Options"
   ClientHeight    =   3165
   ClientLeft      =   2565
   ClientTop       =   1500
   ClientWidth     =   4410
   Icon            =   "suOption.frx":0000
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form1"
   LockControls    =   -1  'True
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   3165
   ScaleWidth      =   4410
   ShowInTaskbar   =   0   'False
   StartUpPosition =   2  'CenterScreen
   Begin VB.TextBox txtTimeout
      Height          =   300
      Left            =   3270
      MaxLength       =   3
      TabIndex        =   3
      Top             =   480
      Width           =   645
   End
   Begin VB.CheckBox chkMinToTray
      Caption         =   "Mi&nimize to the taskbar notification area "
      Height          =   195
      Left            =   105
      TabIndex        =   7
      Top             =   2160
      Width           =   3375
   End
   Begin VB.CheckBox chkShowConnect
      Caption         =   "Display &login dialog at startup (client/server only)"
      Height          =   225
      Left            =   105
      TabIndex        =   6
      Top             =   1890
      Width           =   3900
   End
   Begin VB.TextBox txtRefreshInfo
      Height          =   300
      Left            =   3270
      MaxLength       =   3
      TabIndex        =   1
      Top             =   90
      Width           =   645
   End
   Begin VB.Frame Frame2
      Caption         =   "Account Manager"
      Height          =   750
      Left            =   105
      TabIndex        =   4
      Top             =   945
      Width           =   4215
      Begin VB.CheckBox chkFullPath
         Caption         =   "Show Full &Path"
         Height          =   255
         Left            =   210
         TabIndex        =   5
         Top             =   315
         Width           =   2295
      End
   End
   Begin VB.PictureBox picOptions
      BorderStyle     =   0  'None
      Height          =   3780
      Index           =   3
      Left            =   -20000
      ScaleHeight     =   3780
      ScaleWidth      =   5685
      TabIndex        =   12
      TabStop         =   0   'False
      Top             =   480
      Width           =   5685
      Begin VB.Frame fraSample4
         Caption         =   "Sample 4"
         Height          =   1785
         Left            =   2100
         TabIndex        =   15
         Top             =   840
         Width           =   2055
      End
   End
   Begin VB.PictureBox picOptions
      BorderStyle     =   0  'None
      Height          =   3780
      Index           =   2
      Left            =   -20000
      ScaleHeight     =   3780
      ScaleWidth      =   5685
      TabIndex        =   11
      TabStop         =   0   'False
      Top             =   480
      Width           =   5685
      Begin VB.Frame fraSample3
         Caption         =   "Sample 3"
         Height          =   1785
         Left            =   1545
         TabIndex        =   14
         Top             =   675
         Width           =   2055
      End
   End
   Begin VB.PictureBox picOptions
      BorderStyle     =   0  'None
      Height          =   3780
      Index           =   1
      Left            =   -20000
      ScaleHeight     =   3780
      ScaleWidth      =   5685
      TabIndex        =   10
      TabStop         =   0   'False
      Top             =   480
      Width           =   5685
      Begin VB.Frame fraSample2
         Caption         =   "Sample 2"
         Height          =   1785
         Left            =   645
         TabIndex        =   13
         Top             =   300
         Width           =   2055
      End
   End
   Begin VB.CommandButton cmdCancel
      Cancel          =   -1  'True
      Caption         =   "Cancel"
      Height          =   375
      Left            =   3225
      TabIndex        =   9
      Top             =   2685
      Width           =   1095
   End
   Begin VB.CommandButton cmdOK
      Caption         =   "OK"
      Height          =   375
      Left            =   1995
      TabIndex        =   8
      Top             =   2685
      Width           =   1095
   End
   Begin VB.Label Label2
      AutoSize        =   -1  'True
      Caption         =   "&Timeout (seconds):"
      Height          =   195
      Left            =   120
      TabIndex        =   2
      Top             =   525
      Width           =   1350
   End
   Begin VB.Label Label1
      AutoSize        =   -1  'True
      Caption         =   "Server &information refresh rate (seconds):"
      Height          =   195
      Left            =   105
      TabIndex        =   0
      Top             =   135
      Width           =   2895
   End
End
Attribute VB_Name = "frmOptions"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub saveSettings()
    IntervalInfo = val(txtRefreshInfo.Text)
    Call SaveSetting(App.title, "ServerInfo", "RefreshInterval", txtRefreshInfo.Text)
    Call SaveSetting(App.title, "General", "Timeout", txtTimeout.Text)
    Call SaveSetting(App.title, "AccountManager", "FullPath", chkFullPath.value)
    Call SaveSetting(App.title, "Startup", "DisplayLogin", chkShowConnect.value)
    Call SaveSetting(App.title, "General", "MinToTray", chkMinToTray.value)

    If gCode4 <> 0 Then
        Call code4timeoutSet(gCode4, txtTimeout.Text)
    End If
End Sub

Private Sub cmdApply_Click()
    Call saveSettings
    'cmdApply.Enabled = False
End Sub

Private Sub cmdCancel_Click()
    Unload Me
End Sub

Private Sub cmdOK_Click()
    Call saveSettings
    Unload Me
End Sub

Private Sub Form_Load()
    Me.HelpContextID = Options

    txtRefreshInfo.Text = CStr(IntervalInfo)
    txtTimeout.Text = GetSetting(App.title, "General", "Timeout", 30)
    chkFullPath.value = GetSetting(App.title, "AccountManager", "FullPath", vbUnchecked)
    chkShowConnect.value = GetSetting(App.title, "Startup", "DisplayLogin", vbChecked)
    chkMinToTray.value = GetSetting(App.title, "General", "MinToTray", vbUnchecked)
End Sub

Private Sub txtRefreshInfo_GotFocus()
    TxtGotFocus Me
End Sub

Private Sub txtRefreshInfo_KeyPress(KeyAscii As Integer)
    If KeyAscii <> &H8 And Not IsNumeric(Chr(KeyAscii)) Then KeyAscii = 0
End Sub

Private Sub txtTimeout_GotFocus()
    TxtGotFocus Me
End Sub

Private Sub txtTimeout_KeyPress(KeyAscii As Integer)
    If KeyAscii <> &H8 And Not IsNumeric(Chr(KeyAscii)) Then KeyAscii = 0
End Sub
