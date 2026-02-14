VERSION 5.00
Begin VB.Form frm32SelDir
   Caption         =   "Select Output Directory"
   ClientHeight    =   3228
   ClientLeft      =   48
   ClientTop       =   336
   ClientWidth     =   7020
   Icon            =   "frm32SelDir.frx":0000
   LinkTopic       =   "Form1"
   ScaleHeight     =   3228
   ScaleWidth      =   7020
   StartUpPosition =   3  'Windows Default
   Begin VB.DriveListBox Drive1
      Height          =   288
      Left            =   3000
      TabIndex        =   4
      Top             =   1200
      Width           =   2445
   End
   Begin VB.DirListBox Dir1
      Height          =   1800
      Left            =   240
      TabIndex        =   3
      Top             =   1200
      Width           =   2445
   End
   Begin VB.TextBox txtDirName
      Height          =   288
      Left            =   240
      TabIndex        =   2
      Top             =   450
      Width           =   5235
   End
   Begin VB.CommandButton cmdOkCancel
      Caption         =   "Cancel"
      Height          =   345
      Index           =   1
      Left            =   5730
      TabIndex        =   1
      Top             =   780
      Width           =   1095
   End
   Begin VB.CommandButton cmdOkCancel
      Caption         =   "OK"
      Height          =   345
      Index           =   0
      Left            =   5730
      TabIndex        =   0
      Top             =   300
      Width           =   1095
   End
   Begin VB.Label Label3
      Caption         =   "Drives:"
      Height          =   255
      Left            =   3000
      TabIndex        =   7
      Top             =   930
      Width           =   2205
   End
   Begin VB.Label Label2
      Caption         =   "Folders:"
      Height          =   252
      Left            =   240
      TabIndex        =   6
      Top             =   930
      Width           =   2205
   End
   Begin VB.Label Label1
      Caption         =   "Directory Name:"
      Height          =   255
      Left            =   240
      TabIndex        =   5
      Top             =   180
      Width           =   2205
   End
End
Attribute VB_Name = "frm32SelDir"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub cmdOkCancel_Click(Index As Integer)
   If Index = cCmdOk Then
      vOutDir = txtDirName
   End If

   Unload Me

End Sub

Private Sub Dir1_Change()
   ChDir Dir1.Path
   txtDirName = Dir1.Path

End Sub

Private Sub Dir1_KeyPress(KeyAscii As Integer)
   Dim rcl  As Long

   'Simulate selection using mouse
   If KeyAscii = 13 Then
      rcl = SendMessage(Dir1.hWnd, WM_LBUTTONDBLCLK, 0, 0)
   End If

End Sub

Private Sub Drive1_Change()
    Dir1.Path = CurDir$(Drive1.Drive)

End Sub

Private Sub Form_Load()
   Call SetDeviceIndependentWindow(Me)

   txtDirName = Dir1.Path

End Sub
