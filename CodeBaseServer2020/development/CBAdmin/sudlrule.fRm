VERSION 5.00
Begin VB.Form Form27
   Caption         =   "Warning"
   ClientHeight    =   3015
   ClientLeft      =   585
   ClientTop       =   3315
   ClientWidth     =   9330
   Icon            =   "sudlrule.frx":0000
   LinkTopic       =   "Form26"
   LockControls    =   -1  'True
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   3015
   ScaleWidth      =   9330
   Begin VB.PictureBox Picture1
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      BorderStyle     =   0  'None
      ForeColor       =   &H80000008&
      Height          =   570
      Left            =   180
      Picture         =   "sudlrule.frx":030A
      ScaleHeight     =   570
      ScaleWidth      =   570
      TabIndex        =   3
      Top             =   210
      Width           =   570
   End
   Begin VB.CommandButton Command2
      Caption         =   "Cancel"
      Height          =   345
      Left            =   4905
      TabIndex        =   2
      Top             =   2235
      Width           =   1125
   End
   Begin VB.CommandButton Command1
      Cancel          =   -1  'True
      Caption         =   "OK"
      Height          =   345
      Left            =   3615
      TabIndex        =   1
      Top             =   2235
      Width           =   1125
   End
   Begin VB.Label Label2
      Caption         =   $"sudlrule.frx":077C
      Height          =   870
      Left            =   960
      TabIndex        =   4
      Top             =   1005
      Width           =   8265
   End
   Begin VB.Label Label1
      Caption         =   $"sudlrule.frx":0911
      Height          =   870
      Left            =   945
      TabIndex        =   0
      Top             =   195
      Width           =   8175
   End
End
Attribute VB_Name = "Form27"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Dim bOkClick As Boolean

Private Sub Command1_Click()

    bOkClick = True
    gOptDelPrivRecs = True

    Unload Me

End Sub


Private Sub Command2_Click()

    gOptDelPrivRecs = False

    Unload Me

End Sub


Private Sub Form_Load()
    Left = (Screen.Width - Width) / 2   ' Center form horizontally.
    Top = (Screen.Height - Height) / 2  ' Center form vertically.
End Sub

Private Sub Form_Unload(Cancel As Integer)

    If Not bOkClick Then gOptDelPrivRecs = False


End Sub


