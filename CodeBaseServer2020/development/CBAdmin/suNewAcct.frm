VERSION 5.00
Begin VB.Form frmNewAccount
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "New Account"
   ClientHeight    =   2205
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   3585
   ControlBox      =   0   'False
   LinkTopic       =   "Form2"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   2205
   ScaleWidth      =   3585
   ShowInTaskbar   =   0   'False
   StartUpPosition =   1  'CenterOwner
   Begin VB.CheckBox chkFiles
      Caption         =   "Include &Files"
      Height          =   225
      Left            =   1050
      TabIndex        =   5
      Top             =   1260
      Value           =   1  'Checked
      Width           =   1905
   End
   Begin VB.ComboBox cmbCopyFrom
      Height          =   315
      Left            =   1050
      Style           =   2  'Dropdown List
      TabIndex        =   4
      Top             =   840
      Width           =   2430
   End
   Begin VB.CheckBox chkPassword
      Caption         =   "Set &Password"
      Height          =   225
      Left            =   1050
      TabIndex        =   2
      Top             =   525
      Value           =   1  'Checked
      Width           =   1380
   End
   Begin VB.CommandButton cmdCancel
      Cancel          =   -1  'True
      Caption         =   "Cancel"
      Height          =   390
      Left            =   2310
      TabIndex        =   7
      Top             =   1680
      Width           =   1170
   End
   Begin VB.CommandButton cmdOK
      Caption         =   "OK"
      Default         =   -1  'True
      Enabled         =   0   'False
      Height          =   390
      Left            =   1050
      TabIndex        =   6
      Top             =   1680
      Width           =   1170
   End
   Begin VB.TextBox txtAcctName
      Height          =   300
      IMEMode         =   3  'DISABLE
      Left            =   1050
      MaxLength       =   20
      TabIndex        =   1
      Top             =   105
      Width           =   2430
   End
   Begin VB.Label Label1
      AutoSize        =   -1  'True
      Caption         =   "&Copy From:"
      Height          =   195
      Left            =   105
      TabIndex        =   3
      Top             =   880
      Width           =   795
   End
   Begin VB.Label lblLabels
      AutoSize        =   -1  'True
      Caption         =   "&Account Id:"
      Height          =   195
      Index           =   0
      Left            =   105
      TabIndex        =   0
      Top             =   145
      Width           =   825
   End
End
Attribute VB_Name = "frmNewAccount"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Public success As Boolean

Private Sub cmbCopyFrom_Click()
    If cmbCopyFrom.ListIndex = 0 Then
        chkFiles.Enabled = False
    Else
        chkFiles.Enabled = True
    End If
End Sub

Private Sub cmdCancel_Click()
    success = False
    Me.Hide
End Sub

Private Sub cmdOK_Click()
    success = True
    Me.Hide
End Sub

Private Sub txtAcctName_Change()
    If Len(Trim(txtAcctName.Text)) = 0 Then
        cmdOK.Enabled = False
    Else
        cmdOK.Enabled = True
    End If
End Sub

Private Sub txtAcctName_GotFocus()
    TxtGotFocus Me
End Sub
