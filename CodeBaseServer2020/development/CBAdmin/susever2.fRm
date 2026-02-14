VERSION 5.00
Object = "{564BFFC3-B5F3-101C-A2C8-A27C829646CD}#1.0#0"; "EDT4.OCX"
Begin VB.Form Form22
   Caption         =   "Disconnect Specific Clients"
   ClientHeight    =   2910
   ClientLeft      =   2520
   ClientTop       =   1890
   ClientWidth     =   6690
   LinkTopic       =   "Form22"
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   2910
   ScaleWidth      =   6690
   ShowInTaskbar   =   0   'False
   Begin VB.CommandButton Command1
      Caption         =   "OK"
      Default         =   -1  'True
      Height          =   345
      Left            =   2835
      TabIndex        =   1
      Top             =   2385
      Width           =   1145
   End
   Begin Edt4Lib.CbEdit CbEdit1
      Height          =   2115
      Left            =   105
      TabIndex        =   0
      TabStop         =   0   'False
      Top             =   60
      Width           =   6435
      _Version        =   65536
      _ExtentX        =   11351
      _ExtentY        =   3731
      _StockProps     =   125
      Text            =   "CbEdit1"
      BackColor       =   -2147483643
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851}
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Locked          =   -1  'True
      MultiLine       =   -1  'True
      ScrollBars      =   2
      ToolTipEnabled  =   0   'False
      ContainerID     =   24584264
   End
End
Attribute VB_Name = "Form22"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub CbButton1_Click()
Form22.Hide
End Sub


Private Sub Command1_Click()
Form22.Hide
End Sub

