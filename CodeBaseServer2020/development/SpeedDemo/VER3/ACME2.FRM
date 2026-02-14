VERSION 5.00
Begin VB.Form Form2 
   Caption         =   "CodeBase Speed Demo - Build Query"
   ClientHeight    =   5730
   ClientLeft      =   0
   ClientTop       =   765
   ClientWidth     =   9585
   Icon            =   "Acme2.frx":0000
   LinkTopic       =   "Form2"
   LockControls    =   -1  'True
   MaxButton       =   0   'False
   ScaleHeight     =   5730
   ScaleWidth      =   9585
   Begin VB.CommandButton ClearBttn 
      Caption         =   "Clear Query"
      Height          =   492
      Left            =   8400
      TabIndex        =   58
      Top             =   1560
      Width           =   1092
   End
   Begin VB.Frame ColoursBox 
      Caption         =   "Colors"
      Enabled         =   0   'False
      Height          =   972
      Left            =   5160
      TabIndex        =   37
      Top             =   3960
      Width           =   4452
      Begin VB.CommandButton VioletBttn 
         Enabled         =   0   'False
         Height          =   252
         Left            =   3360
         Picture         =   "Acme2.frx":030A
         Style           =   1  'Graphical
         TabIndex        =   45
         Top             =   600
         Width           =   972
      End
      Begin VB.CommandButton BlueBttn 
         Enabled         =   0   'False
         Height          =   252
         Left            =   2280
         Picture         =   "Acme2.frx":0355
         Style           =   1  'Graphical
         TabIndex        =   44
         Top             =   600
         Width           =   972
      End
      Begin VB.CommandButton GreenBttn 
         Enabled         =   0   'False
         Height          =   252
         Left            =   1200
         Picture         =   "Acme2.frx":03A0
         Style           =   1  'Graphical
         TabIndex        =   43
         Top             =   600
         Width           =   972
      End
      Begin VB.CommandButton YellowBttn 
         Enabled         =   0   'False
         Height          =   252
         Left            =   3360
         Picture         =   "Acme2.frx":03EB
         Style           =   1  'Graphical
         TabIndex        =   42
         Top             =   240
         Width           =   972
      End
      Begin VB.CommandButton OrangeBttn 
         Enabled         =   0   'False
         Height          =   252
         Left            =   2280
         Picture         =   "Acme2.frx":0436
         Style           =   1  'Graphical
         TabIndex        =   41
         Top             =   240
         Width           =   972
      End
      Begin VB.CommandButton RedBttn 
         Enabled         =   0   'False
         Height          =   252
         Left            =   1200
         Picture         =   "Acme2.frx":0481
         Style           =   1  'Graphical
         TabIndex        =   40
         Top             =   240
         Width           =   972
      End
      Begin VB.CommandButton WhiteBttn 
         Enabled         =   0   'False
         Height          =   252
         Left            =   120
         Picture         =   "Acme2.frx":04CC
         Style           =   1  'Graphical
         TabIndex        =   39
         Top             =   600
         Width           =   972
      End
      Begin VB.CommandButton BlackBttn 
         Enabled         =   0   'False
         Height          =   252
         Left            =   120
         Picture         =   "Acme2.frx":0517
         Style           =   1  'Graphical
         TabIndex        =   38
         Top             =   240
         Width           =   972
      End
   End
   Begin VB.Frame LocalsBox 
      Caption         =   "Locations"
      Enabled         =   0   'False
      Height          =   1692
      Left            =   5160
      TabIndex        =   36
      Top             =   2160
      Width           =   4452
      Begin VB.CommandButton MontrealBttn 
         Caption         =   "Montreal"
         Enabled         =   0   'False
         Height          =   372
         Left            =   3360
         TabIndex        =   57
         Top             =   1200
         Width           =   972
      End
      Begin VB.CommandButton TorontoBttn 
         Caption         =   "Toronto"
         Enabled         =   0   'False
         Height          =   372
         Left            =   2280
         TabIndex        =   56
         Top             =   1200
         Width           =   972
      End
      Begin VB.CommandButton SydneyBttn 
         Caption         =   "Sydney"
         Enabled         =   0   'False
         Height          =   372
         Left            =   1200
         TabIndex        =   55
         Top             =   1200
         Width           =   972
      End
      Begin VB.CommandButton HKBttn 
         Caption         =   "Hong Kong"
         Enabled         =   0   'False
         Height          =   372
         Left            =   120
         TabIndex        =   54
         Top             =   1200
         Width           =   972
      End
      Begin VB.CommandButton TokyoBttn 
         Caption         =   "Tokyo"
         Enabled         =   0   'False
         Height          =   372
         Left            =   3360
         TabIndex        =   53
         Top             =   720
         Width           =   972
      End
      Begin VB.CommandButton MoscowBttn 
         Caption         =   "Moscow"
         Enabled         =   0   'False
         Height          =   372
         Left            =   2280
         TabIndex        =   52
         Top             =   720
         Width           =   972
      End
      Begin VB.CommandButton BerlinBttn 
         Caption         =   "Berlin"
         Enabled         =   0   'False
         Height          =   372
         Left            =   1200
         TabIndex        =   51
         Top             =   750
         Width           =   972
      End
      Begin VB.CommandButton ParisBttn 
         Caption         =   "Paris"
         Enabled         =   0   'False
         Height          =   372
         Left            =   120
         TabIndex        =   50
         Top             =   720
         Width           =   972
      End
      Begin VB.CommandButton LondonBttn 
         Caption         =   "London"
         Enabled         =   0   'False
         Height          =   372
         Left            =   3360
         TabIndex        =   49
         Top             =   240
         Width           =   972
      End
      Begin VB.CommandButton LABttn 
         Caption         =   "Los Angeles"
         Enabled         =   0   'False
         Height          =   372
         Left            =   2280
         TabIndex        =   48
         Top             =   240
         Width           =   972
      End
      Begin VB.CommandButton NYBttn 
         Caption         =   "New York"
         Enabled         =   0   'False
         Height          =   372
         Left            =   1200
         TabIndex        =   47
         Top             =   240
         Width           =   972
      End
      Begin VB.CommandButton VanBttn 
         Caption         =   "Vancouver"
         Enabled         =   0   'False
         Height          =   372
         Left            =   120
         TabIndex        =   46
         Top             =   240
         Width           =   972
      End
   End
   Begin VB.CommandButton EndBttn 
      Caption         =   "Exit"
      Height          =   372
      Left            =   8400
      TabIndex        =   31
      Top             =   5280
      Width           =   1095
   End
   Begin VB.Frame Choices 
      BorderStyle     =   0  'None
      Caption         =   "Choices"
      Enabled         =   0   'False
      Height          =   732
      Left            =   2280
      TabIndex        =   30
      Top             =   5040
      Width           =   3252
      Begin VB.CommandButton SubmitBttn 
         Caption         =   "Submit Query"
         Enabled         =   0   'False
         Height          =   372
         Left            =   1800
         TabIndex        =   59
         Top             =   240
         Width           =   1332
      End
      Begin VB.CommandButton OrBttn 
         Caption         =   ".OR."
         Enabled         =   0   'False
         Height          =   372
         Left            =   960
         TabIndex        =   33
         Top             =   240
         Width           =   612
      End
      Begin VB.CommandButton AndBttn 
         Caption         =   ".AND."
         Enabled         =   0   'False
         Height          =   372
         Left            =   120
         TabIndex        =   32
         Top             =   240
         Width           =   612
      End
   End
   Begin VB.Frame Numbers 
      Caption         =   "Values"
      Enabled         =   0   'False
      Height          =   2772
      Left            =   2760
      TabIndex        =   4
      Top             =   2160
      Width           =   2295
      Begin VB.CommandButton EnterBttn 
         Caption         =   "ENTER"
         Enabled         =   0   'False
         Height          =   375
         Left            =   840
         TabIndex        =   29
         Top             =   1680
         Width           =   1335
      End
      Begin VB.CommandButton ZeroBttn 
         Caption         =   "0"
         Enabled         =   0   'False
         Height          =   372
         Left            =   120
         TabIndex        =   28
         Top             =   1680
         Width           =   615
      End
      Begin VB.CommandButton ClrAmntBttn 
         Caption         =   "Clear"
         Enabled         =   0   'False
         Height          =   495
         Left            =   120
         TabIndex        =   27
         Top             =   2160
         Width           =   615
      End
      Begin VB.CommandButton NineBttn 
         Caption         =   "9"
         Enabled         =   0   'False
         Height          =   372
         Left            =   1560
         TabIndex        =   26
         Top             =   1200
         Width           =   615
      End
      Begin VB.CommandButton EightBttn 
         Caption         =   "8"
         Enabled         =   0   'False
         Height          =   372
         Left            =   840
         TabIndex        =   25
         Top             =   1200
         Width           =   615
      End
      Begin VB.CommandButton SevenBttn 
         Caption         =   "7"
         Enabled         =   0   'False
         Height          =   372
         Left            =   120
         TabIndex        =   24
         Top             =   1200
         Width           =   615
      End
      Begin VB.CommandButton SixBttn 
         Caption         =   "6"
         Enabled         =   0   'False
         Height          =   372
         Left            =   1560
         TabIndex        =   23
         Top             =   720
         Width           =   615
      End
      Begin VB.CommandButton FiveBttn 
         Caption         =   "5"
         Enabled         =   0   'False
         Height          =   372
         Left            =   840
         TabIndex        =   22
         Top             =   720
         Width           =   615
      End
      Begin VB.CommandButton FourBttn 
         Caption         =   "4"
         Enabled         =   0   'False
         Height          =   372
         Left            =   120
         TabIndex        =   21
         Top             =   720
         Width           =   615
      End
      Begin VB.CommandButton ThreeBttn 
         Caption         =   "3"
         Enabled         =   0   'False
         Height          =   372
         Left            =   1560
         TabIndex        =   20
         Top             =   240
         Width           =   615
      End
      Begin VB.CommandButton TwoBttn 
         Caption         =   "2"
         Enabled         =   0   'False
         Height          =   372
         Left            =   840
         TabIndex        =   19
         Top             =   240
         Width           =   615
      End
      Begin VB.CommandButton OneBttn 
         Caption         =   "1"
         Enabled         =   0   'False
         Height          =   372
         Left            =   120
         TabIndex        =   18
         Top             =   240
         Width           =   615
      End
      Begin VB.Label MinLabel 
         Height          =   255
         Left            =   1440
         TabIndex        =   61
         Top             =   2160
         Width           =   735
      End
      Begin VB.Label MaxLabel 
         Height          =   255
         Left            =   1440
         TabIndex        =   60
         Top             =   2400
         Width           =   735
      End
      Begin VB.Label AmntBox 
         Alignment       =   1  'Right Justify
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   1  'Fixed Single
         Enabled         =   0   'False
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   18
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   495
         Left            =   840
         TabIndex        =   35
         Top             =   2160
         Width           =   615
         WordWrap        =   -1  'True
      End
   End
   Begin VB.Frame Operators 
      Caption         =   "Operators"
      Enabled         =   0   'False
      Height          =   1692
      Left            =   1560
      TabIndex        =   3
      Top             =   2160
      Width           =   1092
      Begin VB.CommandButton LeqBttn 
         Caption         =   "<="
         Enabled         =   0   'False
         Height          =   372
         Left            =   600
         TabIndex        =   17
         Top             =   1200
         Width           =   372
      End
      Begin VB.CommandButton LtnBttn 
         Caption         =   "<"
         Enabled         =   0   'False
         Height          =   372
         Left            =   120
         TabIndex        =   16
         Top             =   1200
         Width           =   372
      End
      Begin VB.CommandButton GeqBttn 
         Caption         =   ">="
         Enabled         =   0   'False
         Height          =   372
         Left            =   600
         TabIndex        =   15
         Top             =   720
         Width           =   372
      End
      Begin VB.CommandButton GtnBttn 
         Caption         =   ">"
         Enabled         =   0   'False
         Height          =   372
         Left            =   120
         TabIndex        =   14
         Top             =   720
         Width           =   372
      End
      Begin VB.CommandButton NeqBttn 
         Caption         =   "<>"
         Enabled         =   0   'False
         Height          =   372
         Left            =   600
         TabIndex        =   13
         Top             =   240
         Width           =   372
      End
      Begin VB.CommandButton EqlBttn 
         Caption         =   "="
         Enabled         =   0   'False
         Height          =   372
         Left            =   120
         TabIndex        =   12
         Top             =   240
         Width           =   372
      End
   End
   Begin VB.Frame Fields 
      Caption         =   "Attributes"
      Height          =   2775
      Left            =   120
      TabIndex        =   2
      Top             =   2160
      Width           =   1335
      Begin VB.CommandButton ColourBttn 
         Caption         =   "Color"
         Height          =   275
         Left            =   120
         TabIndex        =   11
         Top             =   2400
         Width           =   1095
      End
      Begin VB.CommandButton QtyBttn 
         Caption         =   "Quantity"
         Height          =   275
         Left            =   120
         TabIndex        =   10
         Top             =   2040
         Width           =   1095
      End
      Begin VB.CommandButton WgtBttn 
         Caption         =   "Weight"
         Height          =   275
         Left            =   120
         TabIndex        =   9
         Top             =   1680
         Width           =   1095
      End
      Begin VB.CommandButton HgtBttn 
         Caption         =   "Height"
         Height          =   275
         Left            =   120
         TabIndex        =   8
         Top             =   1320
         Width           =   1095
      End
      Begin VB.CommandButton WidBttn 
         Caption         =   "Width"
         Height          =   275
         Left            =   120
         TabIndex        =   7
         Top             =   960
         Width           =   1095
      End
      Begin VB.CommandButton LenBttn 
         Caption         =   "Length"
         Height          =   275
         Left            =   120
         TabIndex        =   6
         Top             =   600
         Width           =   1095
      End
      Begin VB.CommandButton LocBttn 
         Caption         =   "Location"
         Height          =   275
         Left            =   120
         TabIndex        =   5
         Top             =   240
         Width           =   1095
      End
   End
   Begin VB.Label QueryBox 
      BackColor       =   &H0000FFFF&
      BorderStyle     =   1  'Fixed Single
      Height          =   495
      Left            =   120
      TabIndex        =   34
      Top             =   1560
      Width           =   8175
      WordWrap        =   -1  'True
   End
   Begin VB.Label Label1 
      Caption         =   $"Acme2.frx":0562
      Height          =   465
      Left            =   120
      TabIndex        =   0
      Top             =   30
      Width           =   9375
      WordWrap        =   -1  'True
   End
   Begin VB.Label Label2 
      Caption         =   $"Acme2.frx":0643
      Height          =   855
      Left            =   120
      TabIndex        =   1
      Top             =   540
      Width           =   9375
      WordWrap        =   -1  'True
   End
End
Attribute VB_Name = "Form2"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Dim numstring As String
Dim max, min As Integer
Dim strflag As Boolean
Dim locflag As Boolean
Dim curquery As String

Public Sub able(frm As Frame, enable As Boolean)

Select Case frm.name
  Case "Fields"
    LocBttn.Enabled = enable
    LenBttn.Enabled = enable
    WidBttn.Enabled = enable
    HgtBttn.Enabled = enable
    WgtBttn.Enabled = enable
    QtyBttn.Enabled = enable
    ColourBttn.Enabled = enable
  Case "Operators"
    EqlBttn.Enabled = enable
    NeqBttn.Enabled = enable
    GtnBttn.Enabled = enable
    GeqBttn.Enabled = enable
    LtnBttn.Enabled = enable
    LeqBttn.Enabled = enable
  Case "Numbers"
    OneBttn.Enabled = enable
    TwoBttn.Enabled = enable
    ThreeBttn.Enabled = enable
    FourBttn.Enabled = enable
    FiveBttn.Enabled = enable
    SixBttn.Enabled = enable
    SevenBttn.Enabled = enable
    EightBttn.Enabled = enable
    NineBttn.Enabled = enable
    ZeroBttn.Enabled = enable
    AmntBox.Enabled = enable
    ClrAmntBttn.Enabled = enable
    EnterBttn.Enabled = enable
    numstring = ""
    AmntBox.caption = numstring
    MaxLabel.caption = ""
    MinLabel.caption = ""
    If enable = True Then
      MaxLabel.caption = "Max=" + Str(max)
      MinLabel.caption = "Min=" + Str(min)
    End If
  Case "LocalsBox"
    VanBttn.Enabled = enable
    NYBttn.Enabled = enable
    LABttn.Enabled = enable
    LondonBttn.Enabled = enable
    ParisBttn.Enabled = enable
    BerlinBttn.Enabled = enable
    MoscowBttn.Enabled = enable
    TokyoBttn.Enabled = enable
    HKBttn.Enabled = enable
    SydneyBttn.Enabled = enable
    TorontoBttn.Enabled = enable
    MontrealBttn.Enabled = enable
  Case "ColoursBox"
    BlackBttn.Enabled = enable
    WhiteBttn.Enabled = enable
    RedBttn.Enabled = enable
    OrangeBttn.Enabled = enable
    YellowBttn.Enabled = enable
    GreenBttn.Enabled = enable
    BlueBttn.Enabled = enable
    VioletBttn.Enabled = enable
  Case "Choices"
    AndBttn.Enabled = enable
    OrBttn.Enabled = enable
    SubmitBttn.Enabled = enable
End Select
frm.Enabled = enable

End Sub


Private Sub strbttns()

GtnBttn.Enabled = False
GeqBttn.Enabled = False
LtnBttn.Enabled = False
LeqBttn.Enabled = False
strflag = True

End Sub

Private Sub AndBttn_Click()

curquery = curquery + " .AND. "
QueryBox.caption = curquery
Call able(Choices, False)
Call able(Fields, True)
strflag = False

End Sub

Private Sub BerlinBttn_Click()

curquery = curquery + "'Berlin'"
QueryBox.caption = curquery
Call able(LocalsBox, False)
Call able(Choices, True)

End Sub

Private Sub BlackBttn_Click()

curquery = curquery + "'Black'"
QueryBox.caption = curquery
Call able(ColoursBox, False)
Call able(Choices, True)

End Sub

Private Sub BlueBttn_Click()

curquery = curquery + "'Blue'"
QueryBox.caption = curquery
Call able(ColoursBox, False)
Call able(Choices, True)

End Sub

Private Sub ClearBttn_Click()

Call able(ColoursBox, False)
Call able(LocalsBox, False)
Call able(Numbers, False)
Call able(Choices, False)
Call able(Operators, False)
Call able(Fields, True)
strflag = False
locflag = False
curquery = ""
QueryBox.caption = curquery

End Sub

Private Sub ClrAmntBttn_Click()

numstring = ""
AmntBox.caption = numstring

End Sub

Private Sub ColourBttn_Click()

curquery = curquery + "COLOUR"
QueryBox.caption = curquery
Call able(Fields, False)
Call able(Operators, True)
Call strbttns
locflag = False

End Sub

Private Sub EightBttn_Click()

numstring = numstring + "8"
AmntBox.caption = numstring

End Sub

Private Sub EnterBttn_Click()

If Val(AmntBox.caption) > max Or Val(AmntBox.caption) < min Then
  MsgBox ("This value does not fall into the Min/Max range.  Please enter another value.")
  AmntBox.caption = ""
  numstring = ""
Else
  curquery = curquery + AmntBox.caption
  QueryBox.caption = curquery
  Call able(Numbers, False)
  Call able(Choices, True)
End If

End Sub

Private Sub EqlBttn_Click()

curquery = curquery + "="
QueryBox.caption = curquery
Call able(Operators, False)
If strflag = False Then
  Call able(Numbers, True)
ElseIf locflag = True Then
  Call able(LocalsBox, True)
Else
  Call able(ColoursBox, True)
End If

End Sub

Private Sub FiveBttn_Click()

numstring = numstring + "5"
AmntBox.caption = numstring

End Sub

Private Sub Form_Load()
Me.Left = (Screen.Width - Me.ScaleWidth) / 2
Me.Top = (Screen.Height - ScaleHeight) / 2
End Sub

Private Sub Form_Unload(Cancel As Integer)
    rc = code4close(cb)
    rc = code4initUndo(cb)
    End
End Sub

Private Sub FourBttn_Click()

numstring = numstring + "4"
AmntBox.caption = numstring

End Sub

Private Sub GeqBttn_Click()

curquery = curquery + ">="
QueryBox.caption = curquery
Call able(Operators, False)
Call able(Numbers, True)

End Sub

Private Sub GreenBttn_Click()

curquery = curquery + "'Green'"
QueryBox.caption = curquery
Call able(ColoursBox, False)
Call able(Choices, True)

End Sub

Private Sub GtnBttn_Click()

curquery = curquery + ">"
QueryBox.caption = curquery
Call able(Operators, False)
Call able(Numbers, True)

End Sub

Private Sub HgtBttn_Click()

curquery = curquery + "HEIGHT"
QueryBox.caption = curquery
Call able(Fields, False)
Call able(Operators, True)
max = Int(maxval * 0.8)
min = Int(minval * 0.8)

End Sub

Private Sub HKBttn_Click()

curquery = curquery + "'HongKong'"
QueryBox.caption = curquery
Call able(LocalsBox, False)
Call able(Choices, True)

End Sub

Private Sub LABttn_Click()

curquery = curquery + "'LosAngeles'"
QueryBox.caption = curquery
Call able(LocalsBox, False)
Call able(Choices, True)

End Sub

Private Sub LenBttn_Click()

curquery = curquery + "LENGTH"
QueryBox.caption = curquery
Call able(Fields, False)
Call able(Operators, True)
max = maxval
min = minval

End Sub

Private Sub LeqBttn_Click()

curquery = curquery + "<="
QueryBox.caption = curquery
Call able(Operators, False)
Call able(Numbers, True)

End Sub

Private Sub LocBttn_Click()

curquery = curquery + "LOCATION"
QueryBox.caption = curquery
Call able(Fields, False)
Call able(Operators, True)
Call strbttns
locflag = True

End Sub

Private Sub LondonBttn_Click()

curquery = curquery + "'London'"
QueryBox.caption = curquery
Call able(LocalsBox, False)
Call able(Choices, True)

End Sub

Private Sub LtnBttn_Click()

curquery = curquery + "<"
QueryBox.caption = curquery
Call able(Operators, False)
Call able(Numbers, True)

End Sub

Private Sub MontrealBttn_Click()

curquery = curquery + "'Montreal'"
QueryBox.caption = curquery
Call able(LocalsBox, False)
Call able(Choices, True)

End Sub

Private Sub MoscowBttn_Click()

curquery = curquery + "'Moscow'"
QueryBox.caption = curquery
Call able(LocalsBox, False)
Call able(Choices, True)

End Sub

Private Sub NeqBttn_Click()

curquery = curquery + "<>"
QueryBox.caption = curquery
Call able(Operators, False)
If strflag = False Then
  Call able(Numbers, True)
ElseIf locflag = True Then
  Call able(LocalsBox, True)
Else
  Call able(ColoursBox, True)
End If

End Sub

Private Sub NineBttn_Click()

numstring = numstring + "9"
AmntBox.caption = numstring

End Sub

Private Sub NYBttn_Click()

curquery = curquery + "'NewYork'"
QueryBox.caption = curquery
Call able(LocalsBox, False)
Call able(Choices, True)

End Sub

Private Sub OneBttn_Click()

numstring = numstring + "1"
AmntBox.caption = numstring

End Sub

Private Sub OrangeBttn_Click()

curquery = curquery + "'Orange'"
QueryBox.caption = curquery
Call able(ColoursBox, False)
Call able(Choices, True)

End Sub

Private Sub OrBttn_Click()

curquery = curquery + " .OR. "
QueryBox.caption = curquery
Call able(Choices, False)
Call able(Fields, True)
strflag = False

End Sub

Private Sub ParisBttn_Click()

curquery = curquery + "'Paris'"
QueryBox.caption = curquery
Call able(LocalsBox, False)
Call able(Choices, True)

End Sub

Private Sub QtyBttn_Click()

curquery = curquery + "QUANTITY"
QueryBox.caption = curquery
Call able(Fields, False)
Call able(Operators, True)
max = 20
min = 1

End Sub

Private Sub RedBttn_Click()

curquery = curquery + "'Red'"
QueryBox.caption = curquery
Call able(ColoursBox, False)
Call able(Choices, True)

End Sub

Private Sub SevenBttn_Click()

numstring = numstring + "7"
AmntBox.caption = numstring

End Sub

Private Sub SixBttn_Click()

numstring = numstring + "6"
AmntBox.caption = numstring

End Sub

Private Sub SubmitBttn_Click()

If curquery = "" Then
  MsgBox ("Not a valid query.  Please try again.")
Else
  Screen.MousePointer = 11
  time1 = GetTickCount
  relation = relate4init(data)
  rc = relate4querySet(relation, curquery)
  rc = relate4skipEnable(relation, True)
  rc = relate4top(relation)
  time2 = GetTickCount
  If relate4eof(relation) <> 0 Then
    MsgBox ("No entries matched your query.  Please try again.")
    Screen.MousePointer = 0
  Else
    Form3.Show
    Form2.Hide
    Screen.MousePointer = 0
  End If
End If

End Sub

Private Sub EndBttn_Click()
    Unload Me
End

End Sub

Private Sub SydneyBttn_Click()

curquery = curquery + "'Sydney'"
QueryBox.caption = curquery
Call able(LocalsBox, False)
Call able(Choices, True)

End Sub

Private Sub ThreeBttn_Click()

numstring = numstring + "3"
AmntBox.caption = numstring

End Sub

Private Sub TokyoBttn_Click()

curquery = curquery + "'Tokyo'"
QueryBox.caption = curquery
Call able(LocalsBox, False)
Call able(Choices, True)

End Sub

Private Sub TorontoBttn_Click()

curquery = curquery + "'Toronto'"
QueryBox.caption = curquery
Call able(LocalsBox, False)
Call able(Choices, True)

End Sub

Private Sub TwoBttn_Click()

numstring = numstring + "2"
AmntBox.caption = numstring

End Sub

Private Sub VanBttn_Click()

curquery = curquery + "'Vancouver'"
QueryBox.caption = curquery
Call able(LocalsBox, False)
Call able(Choices, True)

End Sub

Private Sub VioletBttn_Click()

curquery = curquery + "'Violet'"
QueryBox.caption = curquery
Call able(ColoursBox, False)
Call able(Choices, True)

End Sub

Private Sub WgtBttn_Click()

curquery = curquery + "WEIGHT"
QueryBox.caption = curquery
Call able(Fields, False)
Call able(Operators, True)
max = 135
min = 40

End Sub

Private Sub WhiteBttn_Click()

curquery = curquery + "'White'"
QueryBox.caption = curquery
Call able(ColoursBox, False)
Call able(Choices, True)

End Sub

Private Sub WidBttn_Click()

curquery = curquery + "WIDTH"
QueryBox.caption = curquery
Call able(Fields, False)
Call able(Operators, True)
max = Int(maxval * 0.64)
min = Int(minval * 0.64)

End Sub

Private Sub YellowBttn_Click()

curquery = curquery + "'Yellow'"
QueryBox.caption = curquery
Call able(ColoursBox, False)
Call able(Choices, True)

End Sub

Private Sub ZeroBttn_Click()

numstring = numstring + "0"
AmntBox.caption = numstring

End Sub
