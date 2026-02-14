VERSION 5.00
Begin VB.Form Form0 
   Caption         =   "CodeBase Demo Program"
   ClientHeight    =   5745
   ClientLeft      =   1200
   ClientTop       =   1290
   ClientWidth     =   8115
   Icon            =   "Acme0.frx":0000
   LinkTopic       =   "Form4"
   LockControls    =   -1  'True
   ScaleHeight     =   5745
   ScaleWidth      =   8115
   Begin VB.PictureBox Picture2 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BorderStyle     =   0  'None
      ForeColor       =   &H80000008&
      Height          =   375
      Left            =   1800
      Picture         =   "Acme0.frx":030A
      ScaleHeight     =   375
      ScaleWidth      =   6015
      TabIndex        =   5
      Top             =   90
      Width           =   6015
   End
   Begin VB.PictureBox Picture1 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      ForeColor       =   &H80000008&
      Height          =   1155
      Left            =   240
      Picture         =   "Acme0.frx":7D2E
      ScaleHeight     =   1155
      ScaleWidth      =   1080
      TabIndex        =   4
      Top             =   75
      Width           =   1080
   End
   Begin VB.CommandButton Command2 
      Caption         =   "Exit"
      Height          =   375
      Left            =   6930
      TabIndex        =   3
      Top             =   5280
      Width           =   1080
   End
   Begin VB.CommandButton Command1 
      Caption         =   "Continue..."
      Default         =   -1  'True
      Height          =   375
      Left            =   5640
      TabIndex        =   2
      Top             =   5280
      Width           =   1095
   End
   Begin VB.TextBox Text1 
      Height          =   3735
      Left            =   105
      Locked          =   -1  'True
      MultiLine       =   -1  'True
      ScrollBars      =   2  'Vertical
      TabIndex        =   1
      Text            =   "Acme0.frx":8B19
      Top             =   1350
      Width           =   7890
   End
   Begin VB.Label Label2 
      Caption         =   $"Acme0.frx":90F2
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   750
      Left            =   1545
      TabIndex        =   0
      Top             =   570
      Width           =   6525
   End
End
Attribute VB_Name = "Form0"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub Command1_Click()
Form1.Show
Unload Me
End Sub

Private Sub Command2_Click()
End
End Sub

Private Sub Form_Load()
Me.Left = (Screen.Width - Me.ScaleWidth) / 2
Me.Top = (Screen.Height - ScaleHeight) / 2
End Sub
