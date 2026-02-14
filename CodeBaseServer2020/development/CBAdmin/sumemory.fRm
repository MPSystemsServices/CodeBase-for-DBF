VERSION 4.00
Begin VB.Form frmMemory
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Server Memory Settings"
   ClientHeight    =   3330
   ClientLeft      =   2940
   ClientTop       =   1740
   ClientWidth     =   5415
   Height          =   3735
   Left            =   2880
   LinkTopic       =   "Form2"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   3330
   ScaleWidth      =   5415
   Top             =   1395
   Width           =   5535
   Begin VB.CommandButton btnCancel
      Cancel          =   -1  'True
      Caption         =   "&Cancel"
      Height          =   350
      Left            =   4080
      TabIndex        =   14
      Top             =   720
      Width           =   1140
   End
   Begin VB.CommandButton btnOK
      Caption         =   "OK"
      Default         =   -1  'True
      Height          =   350
      Left            =   4080
      TabIndex        =   13
      Top             =   240
      Width           =   1140
   End
   Begin VB.CommandButton btnDefault
      Caption         =   "&Defaults"
      Height          =   350
      Left            =   4080
      TabIndex        =   12
      Top             =   1200
      Width           =   1140
   End
   Begin VB.TextBox MemSzSb
      Alignment       =   1  'Right Justify
      Height          =   300
      Left            =   2760
      TabIndex        =   11
      Top             =   2160
      Width           =   1000
   End
   Begin VB.TextBox MemSzSp
      Alignment       =   1  'Right Justify
      Height          =   300
      Left            =   2760
      TabIndex        =   10
      Top             =   2640
      Width           =   1000
   End
   Begin VB.TextBox MemSzMemo
      Alignment       =   1  'Right Justify
      Height          =   300
      Left            =   2760
      TabIndex        =   9
      Top             =   1680
      Width           =   1000
   End
   Begin VB.TextBox MemSzBuf
      Alignment       =   1  'Right Justify
      Height          =   300
      Left            =   2760
      TabIndex        =   8
      Top             =   1200
      Width           =   1000
   End
   Begin VB.TextBox MemSzBlock
      Alignment       =   1  'Right Justify
      Height          =   300
      Left            =   2760
      TabIndex        =   7
      Top             =   720
      Width           =   1000
   End
   Begin VB.TextBox MemStMax
      Alignment       =   1  'Right Justify
      Height          =   300
      Left            =   2760
      TabIndex        =   6
      Top             =   240
      Width           =   1000
   End
   Begin VB.Label Label31
      Caption         =   "Amount of Memory for Sorting:"
      Height          =   255
      Left            =   240
      TabIndex        =   5
      Top             =   2745
      Width           =   2400
   End
   Begin VB.Label Label30
      Caption         =   "Size of Sort Buffers:"
      Height          =   255
      Left            =   240
      TabIndex        =   4
      Top             =   2235
      Width           =   2400
   End
   Begin VB.Label Label29
      Caption         =   "Size of Memory Block for Memos:"
      Height          =   255
      Left            =   240
      TabIndex        =   3
      Top             =   1725
      Width           =   2400
   End
   Begin VB.Label Label28
      Caption         =   "Size of Memory Buffers:"
      Height          =   255
      Left            =   240
      TabIndex        =   2
      Top             =   1245
      Width           =   2400
   End
   Begin VB.Label Label27
      Caption         =   "Size of Memory Block:"
      Height          =   255
      Left            =   240
      TabIndex        =   1
      Top             =   735
      Width           =   2400
   End
   Begin VB.Label Label25
      Caption         =   "Memory for Optimization:"
      Height          =   255
      Left            =   240
      TabIndex        =   0
      Top             =   240
      Width           =   2400
   End
End
Attribute VB_Name = "frmMemory"
Attribute VB_Creatable = False
Attribute VB_Exposed = False
Option Explicit


Private Sub btnDefault_Click()
    'assign the default values to the configuration file
    'for the memory settings
    MemStMax.Text = "983040"
    MemSzBlock.Text = "1024"
    MemSzBuf.Text = "32768"
    MemSzMemo.Text = "512"
    MemSzSb.Text = "32768"
    MemSzSp.Text = "61440"
End Sub

Private Sub btnOK_Click()
    Form3.CbMaster1.SetField "MemStMax", MemStMax.Text
    Form3.CbMaster1.SetField "MemSzBlock", MemSzBlock.Text
    Form3.CbMaster1.SetField "MemSzBuf", MemSzBuf.Text
    Form3.CbMaster1.SetField "MemSzMemo", MemSzMemo.Text
    Form3.CbMaster1.SetField "MemSzSb", MemSzSb.Text
    Form3.CbMaster1.SetField "MemSzSp", MemSzSp.Text
    Unload Me
End Sub


Private Sub btnCancel_Click()
    Unload Me
End Sub


Private Sub Form_Load()
    MemStMax.Text = Trim(f4str(d4field(Form3.CbMaster1.DATA4, "MemStMax")))
    MemSzBlock.Text = Trim(f4str(d4field(Form3.CbMaster1.DATA4, "MemSzBlock")))
    MemSzBuf.Text = Trim(f4str(d4field(Form3.CbMaster1.DATA4, "MemSzBuf")))
    MemSzMemo.Text = Trim(f4str(d4field(Form3.CbMaster1.DATA4, "MemSzMemo")))
    MemSzSb.Text = Trim(f4str(d4field(Form3.CbMaster1.DATA4, "MemSzSb")))
    MemSzSp.Text = Trim(f4str(d4field(Form3.CbMaster1.DATA4, "MemSzSp")))
End Sub




