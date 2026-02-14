VERSION 5.00
Begin VB.Form Form24
   Caption         =   "Warning"
   ClientHeight    =   2745
   ClientLeft      =   1620
   ClientTop       =   2595
   ClientWidth     =   6585
   Icon            =   "suwarn.frx":0000
   LinkTopic       =   "Form24"
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   2745
   ScaleWidth      =   6585
   Begin VB.CheckBox chkHostWarning
      Caption         =   "&Don't show me this warning again for this session"
      Height          =   285
      Left            =   210
      TabIndex        =   3
      Top             =   2325
      Width           =   3885
   End
   Begin VB.CommandButton Command1
      Caption         =   "OK"
      Default         =   -1  'True
      Height          =   390
      Left            =   2940
      TabIndex        =   2
      Top             =   1695
      Width           =   1125
   End
   Begin VB.PictureBox Picture1
      BorderStyle     =   0  'None
      Height          =   510
      Left            =   180
      Picture         =   "suwarn.frx":030A
      ScaleHeight     =   510
      ScaleWidth      =   510
      TabIndex        =   1
      Top             =   360
      Width           =   510
   End
   Begin VB.Label Label2
      Caption         =   $"suwarn.frx":077C
      Height          =   675
      Left            =   1020
      TabIndex        =   4
      Top             =   780
      Width           =   5280
   End
   Begin VB.Label Label1
      Caption         =   "This program has detected that you are accessing the database server on a remote machine."
      Height          =   435
      Left            =   1020
      TabIndex        =   0
      Top             =   240
      Width           =   5325
   End
End
Attribute VB_Name = "Form24"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub Command1_Click()
Me.Hide
End Sub


Private Sub Form_Load()
Left = (Screen.Width - Width) / 2   ' Center form horizontally.
Top = (Screen.Height - Height) / 2  ' Center form vertically.

End Sub


