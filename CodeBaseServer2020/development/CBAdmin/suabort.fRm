VERSION 5.00
Begin VB.Form frmProgress
   Caption         =   "Form8"
   ClientHeight    =   2895
   ClientLeft      =   1365
   ClientTop       =   3915
   ClientWidth     =   6690
   ControlBox      =   0   'False
   LinkTopic       =   "Form8"
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   2895
   ScaleWidth      =   6690
   ShowInTaskbar   =   0   'False
   Begin VB.CommandButton Command1
      Caption         =   "Cancel"
      Default         =   -1  'True
      Height          =   375
      Left            =   2520
      TabIndex        =   1
      Top             =   2400
      Width           =   1095
   End
   Begin VB.ListBox lstReport
      Height          =   2205
      Left            =   0
      TabIndex        =   0
      Top             =   75
      Width           =   6615
   End
End
Attribute VB_Name = "frmProgress"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub Command1_Click()
    userAbort = True
    If frmProgress.Command1.caption = "OK" Then
        Form7.Enabled = True
        Unload Me
    End If
End Sub

Private Sub Form_Load()
    Left = (frmMain2.ScaleWidth - Width) / 2   ' Center form horizontally.
    Top = (frmMain2.ScaleHeight - Height) / 2  ' Center form vertically.
End Sub

Private Sub Form_Resize()
    Command1.Top = Me.ScaleHeight - Command1.Height - 50
    Command1.Left = (Me.Width - Command1.Width) / 2
    lstReport.Height = Command1.Top - 100
    lstReport.Width = Me.ScaleWidth
End Sub
