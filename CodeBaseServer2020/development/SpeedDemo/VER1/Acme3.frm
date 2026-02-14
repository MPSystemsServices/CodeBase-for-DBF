VERSION 5.00
Begin VB.Form Form3 
   Caption         =   "CodeBase Query Results"
   ClientHeight    =   3552
   ClientLeft      =   1212
   ClientTop       =   1836
   ClientWidth     =   6804
   ControlBox      =   0   'False
   Icon            =   "Acme3.frx":0000
   LinkTopic       =   "Form3"
   ScaleHeight     =   3552
   ScaleWidth      =   6804
   Begin VB.Frame Frame1 
      Height          =   1812
      Left            =   3720
      TabIndex        =   27
      Top             =   1680
      Width           =   2655
      Begin VB.Label Label1 
         Caption         =   "For More Information Contact:"
         Height          =   255
         Left            =   240
         TabIndex        =   33
         Top             =   240
         Width           =   2295
      End
      Begin VB.Label Label2 
         Caption         =   "Sequiter Software Inc."
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   7.8
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   252
         Left            =   240
         TabIndex        =   32
         Top             =   480
         Width           =   2052
      End
      Begin VB.Label Label6 
         Caption         =   "Ph: 403 437-2410"
         Height          =   252
         Left            =   240
         TabIndex        =   31
         Top             =   720
         Width           =   2172
      End
      Begin VB.Label Label8 
         Caption         =   "Fax: 403 436-2999"
         Height          =   252
         Left            =   240
         TabIndex        =   30
         Top             =   960
         Width           =   2292
      End
      Begin VB.Label Label9 
         Caption         =   "Web: www.sequiter.com"
         Height          =   252
         Left            =   240
         TabIndex        =   29
         Top             =   1200
         Width           =   2172
      End
      Begin VB.Label Label10 
         Caption         =   "Email: info@sequiter.com"
         Height          =   252
         Left            =   240
         TabIndex        =   28
         Top             =   1440
         Width           =   1932
      End
   End
   Begin VB.CommandButton Command3 
      Caption         =   "End Demo"
      Height          =   492
      Left            =   2400
      TabIndex        =   26
      Top             =   3000
      Width           =   972
   End
   Begin VB.CommandButton Command2 
      Caption         =   "New Query"
      Height          =   492
      Left            =   1320
      TabIndex        =   25
      Top             =   3000
      Width           =   972
   End
   Begin VB.CommandButton Command1 
      Height          =   852
      Left            =   3720
      TabIndex        =   24
      Top             =   840
      Width           =   2652
   End
   Begin VB.CommandButton PgDnBttn 
      Caption         =   "Skip Forward 10 Records"
      Height          =   375
      Left            =   1320
      TabIndex        =   19
      Top             =   2040
      Width           =   2055
   End
   Begin VB.CommandButton SkpDnBttn 
      Caption         =   "Skip Forward 1 Record"
      Height          =   375
      Left            =   1320
      TabIndex        =   18
      Top             =   1560
      Width           =   2055
   End
   Begin VB.CommandButton BottomBttn 
      Caption         =   "Bottom of Query"
      Height          =   375
      Left            =   1320
      TabIndex        =   17
      Top             =   2520
      Width           =   2055
   End
   Begin VB.CommandButton TopBttn 
      Caption         =   "Top of Query"
      Height          =   375
      Left            =   1320
      TabIndex        =   16
      Top             =   120
      Width           =   2055
   End
   Begin VB.CommandButton PgUpBttn 
      Caption         =   "Skip Back 10 Records"
      Height          =   375
      Left            =   1320
      TabIndex        =   15
      Top             =   600
      Width           =   2055
   End
   Begin VB.CommandButton SkpUpBttn 
      Caption         =   "Skip Back 1 Record"
      Height          =   375
      Left            =   1320
      TabIndex        =   14
      Top             =   1080
      Width           =   2055
   End
   Begin VB.Label Label7 
      Alignment       =   1  'Right Justify
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   1  'Fixed Single
      Height          =   252
      Left            =   5280
      TabIndex        =   23
      Top             =   120
      Width           =   1332
   End
   Begin VB.Label Label5 
      Caption         =   "Time to Finish Query:"
      Height          =   252
      Left            =   3600
      TabIndex        =   22
      Top             =   120
      Width           =   1572
   End
   Begin VB.Label Label4 
      Alignment       =   1  'Right Justify
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   1  'Fixed Single
      Height          =   252
      Left            =   5280
      TabIndex        =   21
      Top             =   480
      Width           =   1332
   End
   Begin VB.Label Label3 
      Caption         =   "Record Number        ="
      Height          =   252
      Left            =   3600
      TabIndex        =   20
      Top             =   480
      Width           =   1572
   End
   Begin VB.Label ClrLbl 
      Alignment       =   2  'Center
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   1  'Fixed Single
      Height          =   255
      Left            =   0
      TabIndex        =   13
      Top             =   3240
      Width           =   1095
   End
   Begin VB.Label QtyLbl 
      Alignment       =   1  'Right Justify
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   1  'Fixed Single
      Height          =   255
      Left            =   0
      TabIndex        =   12
      Top             =   2760
      Width           =   1095
   End
   Begin VB.Label WgtLbl 
      Alignment       =   1  'Right Justify
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   1  'Fixed Single
      Height          =   255
      Left            =   0
      TabIndex        =   11
      Top             =   2280
      Width           =   1095
   End
   Begin VB.Label HgtLbl 
      Alignment       =   1  'Right Justify
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   1  'Fixed Single
      Height          =   255
      Left            =   0
      TabIndex        =   10
      Top             =   1800
      Width           =   1095
   End
   Begin VB.Label WdtLbl 
      Alignment       =   1  'Right Justify
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   1  'Fixed Single
      Height          =   255
      Left            =   0
      TabIndex        =   9
      Top             =   1320
      Width           =   1095
   End
   Begin VB.Label LgtLbl 
      Alignment       =   1  'Right Justify
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   1  'Fixed Single
      Height          =   255
      Left            =   0
      TabIndex        =   8
      Top             =   840
      Width           =   1095
   End
   Begin VB.Label LocLbl 
      Alignment       =   2  'Center
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   1  'Fixed Single
      Height          =   255
      Left            =   0
      TabIndex        =   7
      Top             =   360
      Width           =   1095
   End
   Begin VB.Label Colour 
      Caption         =   "Colour"
      Height          =   255
      Left            =   0
      TabIndex        =   0
      Top             =   3000
      Width           =   735
   End
   Begin VB.Label Quantity 
      Caption         =   "Quantity"
      Height          =   255
      Left            =   0
      TabIndex        =   6
      Top             =   2520
      Width           =   735
   End
   Begin VB.Label Weight 
      Caption         =   "Weight"
      Height          =   255
      Left            =   0
      TabIndex        =   5
      Top             =   2040
      Width           =   735
   End
   Begin VB.Label Height 
      Caption         =   "Height"
      Height          =   255
      Left            =   0
      TabIndex        =   4
      Top             =   1560
      Width           =   735
   End
   Begin VB.Label Width 
      Caption         =   "Width"
      Height          =   255
      Left            =   0
      TabIndex        =   3
      Top             =   1080
      Width           =   735
   End
   Begin VB.Label Length 
      Caption         =   "Length"
      Height          =   255
      Left            =   0
      TabIndex        =   2
      Top             =   600
      Width           =   735
   End
   Begin VB.Label Location 
      Caption         =   "Location"
      Height          =   255
      Left            =   0
      TabIndex        =   1
      Top             =   120
      Width           =   735
   End
End
Attribute VB_Name = "Form3"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False

Private Sub BottomBttn_Click()

rc = relate4bottom(relation)
Call refreshlabels

End Sub

Private Sub refreshlabels()

If rc = r4bof Then
  MsgBox ("Top of file")
  rc = relate4top(relation)
ElseIf rc = r4eof Then
  MsgBox ("Bottom of file")
  rc = relate4bottom(relation)
End If
LocLbl.caption = f4str(loc)
LgtLbl.caption = f4int(lng)
WdtLbl.caption = f4int(wdt)
HgtLbl.caption = f4int(hgt)
WgtLbl.caption = f4int(wgt)
QtyLbl.caption = f4int(qty)
ClrLbl.caption = f4str(clr)
Label4.caption = d4recNo(data)

End Sub

Private Sub Command1_Click()
Dim counter As Long

Screen.MousePointer = 11
Command1.caption = "Working"
counter = 0
rc = relate4top(relation)
time1 = GetTickCount
Do While rc = r4success
  counter = counter + 1
  rc = relate4skip(relation, 1)
Loop
time2 = GetTickCount
Command1.caption = "There were " & counter & " records matching your query.  It took " & (time2 - time1) / 1000 & " seconds to skip through them all."
rc = relate4top(relation)
Call refreshlabels
Screen.MousePointer = 0

End Sub

Private Sub Command2_Click()

rc = relate4free(relation, 0)
Form2.Show
Form3.Hide

End Sub

Private Sub Command3_Click()

rc = relate4free(relation, 0)
rc = code4close(cb)
rc = code4initUndo(cb)
End

End Sub

Private Sub Form_Activate()

Call refreshlabels
Label7.caption = (time2 - time1) / 1000 & " sec"
Command1.caption = "Press this button to find out how many records matched your query."

End Sub

Private Sub PgDnBttn_Click()

rc = relate4skip(relation, 10)
Call refreshlabels

End Sub

Private Sub PgUpBttn_Click()

rc = relate4skip(relation, -10)
Call refreshlabels

End Sub

Private Sub SkpDnBttn_Click()

rc = relate4skip(relation, 1)
Call refreshlabels

End Sub

Private Sub SkpUpBttn_Click()

rc = relate4skip(relation, -1)
Call refreshlabels

End Sub

Private Sub TopBttn_Click()

rc = relate4top(relation)
Call refreshlabels

End Sub
