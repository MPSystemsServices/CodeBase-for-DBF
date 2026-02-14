VERSION 5.00
Begin VB.Form frmLogin
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Login to CodeBase Server"
   ClientHeight    =   2025
   ClientLeft      =   1230
   ClientTop       =   3870
   ClientWidth     =   5085
   Icon            =   "sulogin.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   2025
   ScaleWidth      =   5085
   ShowInTaskbar   =   0   'False
   Begin VB.TextBox txtPort
      Height          =   330
      Left            =   1365
      TabIndex        =   7
      Text            =   "Text4"
      Top             =   1560
      Width           =   2325
   End
   Begin VB.TextBox txtPW
      Height          =   330
      IMEMode         =   3  'DISABLE
      Left            =   1365
      PasswordChar    =   "*"
      TabIndex        =   5
      Text            =   "Text3"
      Top             =   1080
      Width           =   2325
   End
   Begin VB.TextBox txtAccount
      Height          =   330
      Left            =   1365
      TabIndex        =   3
      Text            =   "Text2"
      Top             =   600
      Width           =   2325
   End
   Begin VB.TextBox txtHostName
      Height          =   330
      Left            =   1365
      TabIndex        =   1
      Text            =   "Text1"
      Top             =   120
      Width           =   2325
   End
   Begin VB.CommandButton Cancel
      Cancel          =   -1  'True
      Caption         =   "Ca&ncel"
      Height          =   345
      Left            =   3840
      TabIndex        =   9
      Top             =   600
      Width           =   1125
   End
   Begin VB.CommandButton Ok
      Caption         =   "&Connect"
      Default         =   -1  'True
      Height          =   345
      Left            =   3840
      TabIndex        =   8
      Top             =   120
      Width           =   1125
   End
   Begin VB.Label lblPortId
      AutoSize        =   -1  'True
      Caption         =   "Port &Id:"
      Height          =   195
      Left            =   165
      TabIndex        =   6
      Top             =   1605
      Width           =   510
   End
   Begin VB.Label lblPassword
      AutoSize        =   -1  'True
      Caption         =   "&Password:"
      Height          =   195
      Left            =   165
      TabIndex        =   4
      Top             =   1125
      Width           =   735
   End
   Begin VB.Label lblAccountId
      AutoSize        =   -1  'True
      Caption         =   "&Account Id:"
      Height          =   195
      Left            =   165
      TabIndex        =   2
      Top             =   645
      Width           =   825
   End
   Begin VB.Label lblServerName
      AutoSize        =   -1  'True
      Caption         =   "&Server Name:"
      Height          =   195
      Left            =   165
      TabIndex        =   0
      Top             =   165
      Width           =   975
   End
End
Attribute VB_Name = "frmLogin"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub Cancel_Click()
    Unload Me
End Sub

Private Sub Command1_Click()
    Dim rcl&
    Dim keyString$
    keyString = "Logging into the server"
    rcl = WinHelpKey(hWnd, "cbadmin.hlp", HELP_KEY, keyString)
End Sub

Private Sub Form_Load()
    HelpContextID = Client_Server_Startup

    Left = (Screen.Width - Width) / 2   ' Center form horizontally.
    Top = (Screen.Height - Height) / 2  ' Center form vertically.

    'use the last login information from the registry when
    'program is first started
    txtHostName.Text = GetSetting(App.title, "Startup", "ServerId", "LOCALHOST")
    txtAccount.Text = GetSetting(App.title, "Startup", "AccountId", "PUBLIC")
    txtPW.Text = ""
    txtPort.Text = GetSetting(App.title, "Startup", "PortId", "23165")
    txtHostName.SelLength = Len(txtHostName.Text)
End Sub

Private Sub Ok_Click()
    Dim rc%

    MousePointer = 11
    rc = CBAConnect(txtHostName.Text, txtPort.Text, txtAccount.Text, txtPW.Text)
    MousePointer = 0

    If rc = r4success Then
        Unload Me
    End If
End Sub

Private Sub txtAccount_GotFocus()
    TxtGotFocus Me
End Sub

Private Sub txtHostName_GotFocus()
    TxtGotFocus Me
End Sub

Private Sub txtPort_GotFocus()
    TxtGotFocus Me
End Sub

Private Sub txtPW_GotFocus()
    TxtGotFocus Me
End Sub
