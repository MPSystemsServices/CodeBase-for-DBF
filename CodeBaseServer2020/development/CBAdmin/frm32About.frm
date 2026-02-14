VERSION 5.00
Begin VB.Form frm32About
   Caption         =   "About CodeUtil Utilities"
   ClientHeight    =   2112
   ClientLeft      =   60
   ClientTop       =   348
   ClientWidth     =   3252
   Icon            =   "frm32About.frx":0000
   LinkTopic       =   "Form1"
   ScaleHeight     =   2112
   ScaleWidth      =   3252
   StartUpPosition =   3  'Windows Default
   Begin VB.CommandButton Command1
      Caption         =   "OK"
      Height          =   375
      Left            =   2280
      TabIndex        =   0
      Top             =   1560
      Width           =   855
   End
   Begin VB.Label Label1
      Caption         =   "Sequiter Software Inc."
      Height          =   255
      Index           =   2
      Left            =   120
      TabIndex        =   3
      Top             =   840
      Width           =   2895
   End
   Begin VB.Label Label1
      Caption         =   "Copyright (c) 1990-2000"
      Height          =   255
      Index           =   1
      Left            =   120
      TabIndex        =   2
      Top             =   600
      Width           =   2895
   End
   Begin VB.Label Label1
      Caption         =   "CodeBase Utilities Version 6"
      Height          =   255
      Index           =   0
      Left            =   120
      TabIndex        =   1
      Top             =   240
      Width           =   2895
   End
End
Attribute VB_Name = "frm32About"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub Command1_Click()
Unload frm32About
End Sub

Private Sub Form_Load()
Call SetDeviceIndependentWindow(frm32About)
End Sub
