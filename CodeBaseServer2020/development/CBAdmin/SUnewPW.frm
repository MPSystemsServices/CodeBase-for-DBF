VERSION 5.00
Begin VB.Form frmPassword
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "New Password"
   ClientHeight    =   1545
   ClientLeft      =   2835
   ClientTop       =   3480
   ClientWidth     =   4500
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   1545
   ScaleWidth      =   4500
   ShowInTaskbar   =   0   'False
   StartUpPosition =   2  'CenterScreen
   Begin VB.TextBox txtPW1
      Height          =   300
      IMEMode         =   3  'DISABLE
      Left            =   2025
      MaxLength       =   20
      PasswordChar    =   "*"
      TabIndex        =   1
      Top             =   105
      Width           =   2325
   End
   Begin VB.CommandButton cmdOK
      Caption         =   "OK"
      Default         =   -1  'True
      Height          =   390
      Left            =   1260
      TabIndex        =   4
      Top             =   1019
      Width           =   1140
   End
   Begin VB.CommandButton cmdCancel
      Cancel          =   -1  'True
      Caption         =   "Cancel"
      Height          =   390
      Left            =   2835
      TabIndex        =   5
      Top             =   1019
      Width           =   1140
   End
   Begin VB.TextBox txtPW2
      Height          =   300
      IMEMode         =   3  'DISABLE
      Left            =   2025
      MaxLength       =   20
      PasswordChar    =   "*"
      TabIndex        =   3
      Top             =   525
      Width           =   2325
   End
   Begin VB.Label lblLabels
      AutoSize        =   -1  'True
      Caption         =   "New &Password:"
      Height          =   195
      Index           =   0
      Left            =   105
      TabIndex        =   0
      Top             =   145
      Width           =   1110
   End
   Begin VB.Label lblLabels
      AutoSize        =   -1  'True
      Caption         =   "&Confirm New Password:"
      Height          =   195
      Index           =   1
      Left            =   105
      TabIndex        =   2
      Top             =   565
      Width           =   1680
   End
End
Attribute VB_Name = "frmPassword"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Public newPasswordOK As Boolean

Private Sub cmdCancel_Click()
    newPasswordOK = False
    Me.Hide
End Sub

Private Sub cmdOK_Click()
    If txtPW1.Text <> txtPW2.Text Then
        MsgBox "The new and confirmed passwords do not match.", vbExclamation, "New Password"
        Exit Sub
    End If
    newPasswordOK = True
    Me.Hide
End Sub

Private Sub Form_Load()
    Me.caption = "New Password for account <" & frmAcctManager.TreeView1.SelectedItem.Text & ">"
End Sub

Private Sub txtPW1_GotFocus()
    TxtGotFocus Me
End Sub

Private Sub txtPW2_GotFocus()
    TxtGotFocus Me
End Sub
