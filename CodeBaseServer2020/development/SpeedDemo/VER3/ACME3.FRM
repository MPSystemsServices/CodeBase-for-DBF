VERSION 5.00
Begin VB.Form Form3 
   Caption         =   "CodeBase Speed Demo - Query Results"
   ClientHeight    =   3960
   ClientLeft      =   1215
   ClientTop       =   1830
   ClientWidth     =   7290
   Icon            =   "Acme3.frx":0000
   LinkTopic       =   "Form3"
   LockControls    =   -1  'True
   MaxButton       =   0   'False
   ScaleHeight     =   3960
   ScaleWidth      =   7290
   Begin VB.Frame Frame3 
      Caption         =   "Performance"
      Height          =   1830
      Left            =   3210
      TabIndex        =   32
      Top             =   90
      Width           =   3915
      Begin VB.CommandButton Command1 
         Caption         =   "Additional Info."
         Height          =   390
         Left            =   150
         TabIndex        =   33
         Top             =   690
         Width           =   1515
      End
      Begin VB.Label lblAddInfo 
         BorderStyle     =   1  'Fixed Single
         Height          =   1050
         Left            =   1860
         TabIndex        =   36
         Top             =   675
         Width           =   1875
      End
      Begin VB.Label Label5 
         Caption         =   "Time to Finish Query:"
         Height          =   255
         Left            =   180
         TabIndex        =   35
         Top             =   300
         Width           =   1575
      End
      Begin VB.Label Label7 
         BorderStyle     =   1  'Fixed Single
         Caption         =   "Label7"
         Height          =   285
         Left            =   1860
         TabIndex        =   34
         Top             =   285
         Width           =   1860
      End
   End
   Begin VB.Frame Frame2 
      Caption         =   "Result Set"
      Height          =   3720
      Left            =   90
      TabIndex        =   9
      Top             =   90
      Width           =   2895
      Begin VB.CommandButton TopBttn 
         Height          =   345
         Left            =   165
         Picture         =   "Acme3.frx":030A
         Style           =   1  'Graphical
         TabIndex        =   15
         Top             =   2640
         Width           =   405
      End
      Begin VB.CommandButton PgDnBttn 
         Height          =   345
         Left            =   1785
         Picture         =   "Acme3.frx":050C
         Style           =   1  'Graphical
         TabIndex        =   14
         Top             =   2640
         Width           =   405
      End
      Begin VB.CommandButton SkpDnBttn 
         Height          =   345
         Left            =   1380
         Picture         =   "Acme3.frx":070E
         Style           =   1  'Graphical
         TabIndex        =   13
         Top             =   2640
         Width           =   405
      End
      Begin VB.CommandButton BottomBttn 
         Height          =   345
         Left            =   2190
         Picture         =   "Acme3.frx":0910
         Style           =   1  'Graphical
         TabIndex        =   12
         Top             =   2640
         Width           =   405
      End
      Begin VB.CommandButton PgUpBttn 
         Height          =   345
         Left            =   570
         Picture         =   "Acme3.frx":0B12
         Style           =   1  'Graphical
         TabIndex        =   11
         Top             =   2640
         Width           =   405
      End
      Begin VB.CommandButton SkpUpBttn 
         Height          =   345
         Left            =   975
         Picture         =   "Acme3.frx":0D14
         Style           =   1  'Graphical
         TabIndex        =   10
         Top             =   2640
         Width           =   405
      End
      Begin VB.Label Location 
         Caption         =   "Location"
         Height          =   240
         Left            =   135
         TabIndex        =   31
         Top             =   315
         Width           =   735
      End
      Begin VB.Label Length 
         Caption         =   "Length"
         Height          =   255
         Left            =   135
         TabIndex        =   30
         Top             =   1050
         Width           =   645
      End
      Begin VB.Label Width 
         Caption         =   "Width"
         Height          =   255
         Left            =   1125
         TabIndex        =   29
         Top             =   1050
         Width           =   735
      End
      Begin VB.Label Height 
         Caption         =   "Height"
         Height          =   255
         Left            =   2145
         TabIndex        =   28
         Top             =   1035
         Width           =   555
      End
      Begin VB.Label Weight 
         Caption         =   "Weight"
         Height          =   225
         Left            =   135
         TabIndex        =   27
         Top             =   1755
         Width           =   735
      End
      Begin VB.Label Quantity 
         Caption         =   "Quantity"
         Height          =   240
         Left            =   1545
         TabIndex        =   26
         Top             =   315
         Width           =   735
      End
      Begin VB.Label Colour 
         Caption         =   "Color"
         Height          =   195
         Left            =   1530
         TabIndex        =   25
         Top             =   1770
         Width           =   735
      End
      Begin VB.Label LocLbl 
         Alignment       =   2  'Center
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   1  'Fixed Single
         Height          =   270
         Left            =   135
         TabIndex        =   24
         Top             =   555
         Width           =   1095
      End
      Begin VB.Label LgtLbl 
         Alignment       =   1  'Right Justify
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   1  'Fixed Single
         Height          =   270
         Left            =   120
         TabIndex        =   23
         Top             =   1290
         Width           =   525
      End
      Begin VB.Label WdtLbl 
         Alignment       =   1  'Right Justify
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   1  'Fixed Single
         Height          =   270
         Left            =   1125
         TabIndex        =   22
         Top             =   1290
         Width           =   525
      End
      Begin VB.Label HgtLbl 
         Alignment       =   1  'Right Justify
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   1  'Fixed Single
         Height          =   270
         Left            =   2130
         TabIndex        =   21
         Top             =   1275
         Width           =   525
      End
      Begin VB.Label WgtLbl 
         Alignment       =   1  'Right Justify
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   1  'Fixed Single
         Height          =   270
         Left            =   135
         TabIndex        =   20
         Top             =   1995
         Width           =   1095
      End
      Begin VB.Label QtyLbl 
         Alignment       =   1  'Right Justify
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   1  'Fixed Single
         Height          =   270
         Left            =   1545
         TabIndex        =   19
         Top             =   555
         Width           =   1095
      End
      Begin VB.Label ClrLbl 
         Alignment       =   2  'Center
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   1  'Fixed Single
         Height          =   270
         Left            =   1530
         TabIndex        =   18
         Top             =   1995
         Width           =   1095
      End
      Begin VB.Label Label3 
         Caption         =   "Record Number"
         Height          =   255
         Left            =   165
         TabIndex        =   17
         Top             =   3120
         Width           =   1200
      End
      Begin VB.Label Label4 
         BorderStyle     =   1  'Fixed Single
         Height          =   285
         Left            =   1425
         TabIndex        =   16
         Top             =   3105
         Width           =   1140
      End
   End
   Begin VB.Frame Frame1 
      Height          =   1812
      Left            =   3210
      TabIndex        =   2
      Top             =   2010
      Width           =   2655
      Begin VB.Label Label1 
         Caption         =   "For More Information Contact:"
         Height          =   255
         Left            =   240
         TabIndex        =   8
         Top             =   240
         Width           =   2295
      End
      Begin VB.Label Label2 
         Caption         =   "Sequiter Software Inc."
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   252
         Left            =   240
         TabIndex        =   7
         Top             =   480
         Width           =   2052
      End
      Begin VB.Label Label6 
         Caption         =   "Ph: 403 437-2410"
         Height          =   225
         Left            =   240
         TabIndex        =   6
         Top             =   750
         Width           =   2190
      End
      Begin VB.Label Label8 
         Caption         =   "Fax: 403 436-2999"
         Height          =   255
         Left            =   240
         TabIndex        =   5
         Top             =   975
         Width           =   2325
      End
      Begin VB.Label Label9 
         Caption         =   "Web: www.sequiter.com"
         Height          =   255
         Left            =   240
         TabIndex        =   4
         Top             =   1200
         Width           =   2190
      End
      Begin VB.Label Label10 
         Caption         =   "Email: info@sequiter.com"
         Height          =   255
         Left            =   240
         TabIndex        =   3
         Top             =   1440
         Width           =   1935
      End
   End
   Begin VB.CommandButton Command3 
      Caption         =   "Exit"
      Height          =   405
      Left            =   6000
      TabIndex        =   1
      Top             =   3405
      Width           =   1155
   End
   Begin VB.CommandButton Command2 
      Caption         =   "New Query"
      Height          =   420
      Left            =   6015
      TabIndex        =   0
      Top             =   2835
      Width           =   1125
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
  MsgBox ("End of file")
  rc = relate4bottom(relation)
End If
LocLbl.caption = f4str(loc)
LgtLbl.caption = f4int(lng)
WdtLbl.caption = f4int(wdt)
HgtLbl.caption = f4int(hgt)
WgtLbl.caption = f4int(wgt)
QtyLbl.caption = f4int(qty)
ClrLbl.caption = f4str(clr)
Label4.caption = " " & d4recNo(data)

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
lblAddInfo.caption = counter & " records in result set. The time to traverse entire set was " & (time2 - time1) / 1000 & " seconds."
rc = relate4top(relation)
Command1.caption = "Additional Info."
Call refreshlabels
Screen.MousePointer = 0

End Sub

Private Sub Command2_Click()
lblAddInfo.caption = ""
rc = relate4free(relation, 0)
Form2.Show
Form3.Hide

End Sub

Private Sub Command3_Click()
    Unload Me
End

End Sub

Private Sub Form_Activate()

Call refreshlabels
Label7.caption = (time2 - time1) / 1000 & " sec"
'Command1.caption = "Press this button to find out how many records matched your query."

End Sub

Private Sub Form_Load()
Me.Left = (Screen.Width - Me.ScaleWidth) / 2
Me.Top = (Screen.Height - ScaleHeight) / 2
End Sub

Private Sub Form_Unload(Cancel As Integer)
    rc = relate4free(relation, 0)
    rc = code4close(cb)
    rc = code4initUndo(cb)
    End
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
