VERSION 5.00
Begin VB.Form Form1 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "CodeBase Demo"
   ClientHeight    =   4875
   ClientLeft      =   2025
   ClientTop       =   1170
   ClientWidth     =   5190
   Icon            =   "Acme1.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MouseIcon       =   "Acme1.frx":030A
   ScaleHeight     =   4872
   ScaleMode       =   0  'User
   ScaleWidth      =   5196
   WhatsThisButton =   -1  'True
   WhatsThisHelp   =   -1  'True
   Begin VB.HScrollBar HScroll1 
      Height          =   252
      LargeChange     =   10
      Left            =   240
      Max             =   100
      Min             =   1
      TabIndex        =   12
      Top             =   2160
      Value           =   1
      Width           =   3612
   End
   Begin VB.CommandButton Command3 
      Caption         =   "Increase Range x10"
      Height          =   612
      Left            =   3960
      TabIndex        =   10
      Top             =   2160
      Width           =   972
   End
   Begin VB.CommandButton Command2 
      Caption         =   "Go to Query Section"
      Enabled         =   0   'False
      Height          =   372
      Left            =   2640
      TabIndex        =   2
      Top             =   2880
      Width           =   2292
   End
   Begin VB.CommandButton Command1 
      Caption         =   "Build Database"
      Height          =   372
      Left            =   240
      TabIndex        =   1
      Top             =   2880
      Width           =   2292
   End
   Begin VB.Label Label10 
      Alignment       =   1  'Right Justify
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   1  'Fixed Single
      ForeColor       =   &H00000000&
      Height          =   252
      Left            =   3000
      TabIndex        =   13
      Top             =   4560
      Width           =   1092
   End
   Begin VB.Label Label9 
      Height          =   372
      Left            =   2880
      TabIndex        =   11
      Top             =   3840
      Width           =   1812
   End
   Begin VB.Label Label8 
      Height          =   372
      Left            =   2880
      TabIndex        =   9
      Top             =   3360
      Width           =   1812
      WordWrap        =   -1  'True
   End
   Begin VB.Label Label7 
      Height          =   372
      Left            =   480
      TabIndex        =   8
      Top             =   3360
      Width           =   1812
      WordWrap        =   -1  'True
   End
   Begin VB.Label Label6 
      Caption         =   $"Acme1.frx":074C
      Height          =   852
      Left            =   240
      TabIndex        =   7
      Top             =   1200
      Width           =   4692
      WordWrap        =   -1  'True
   End
   Begin VB.Label Label5 
      Caption         =   "This application demonstrates some of the features of CodeBase through the fictional Acme Anvil Company."
      Height          =   492
      Left            =   240
      TabIndex        =   6
      Top             =   600
      Width           =   4692
      WordWrap        =   -1  'True
   End
   Begin VB.Label Label4 
      BackColor       =   &H00FFFF00&
      BorderStyle     =   1  'Fixed Single
      Caption         =   "WELCOME TO THE CODEBASE 6 DEMO"
      BeginProperty Font 
         Name            =   "Times New Roman"
         Size            =   12
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   324
      Left            =   240
      TabIndex        =   5
      Top             =   120
      Width           =   4668
   End
   Begin VB.Label Label3 
      Alignment       =   1  'Right Justify
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   1  'Fixed Single
      Caption         =   "1000"
      ForeColor       =   &H00000000&
      Height          =   252
      Left            =   3120
      TabIndex        =   4
      Top             =   2520
      Width           =   732
      WordWrap        =   -1  'True
   End
   Begin VB.Label Label2 
      Caption         =   "Approximate disk space required:"
      Height          =   252
      Left            =   360
      TabIndex        =   3
      Top             =   4560
      Width           =   2412
   End
   Begin VB.Label Label1 
      Caption         =   "Number of Records (Max. 100 000)"
      Height          =   252
      Left            =   240
      TabIndex        =   0
      Top             =   2520
      Width           =   2772
   End
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub Command1_Click()
Dim ind As Long
Dim x As Long
Dim Result As Variant
Dim a As Integer
Dim b As Integer
Dim c As Integer
Dim locals As Variant
Dim colours As Variant
Dim buff As String

locals = Array("Vancouver ", "NewYork    ", "LosAngeles", "London    ", "Paris     ", "Berlin    ", "Moscow    ", "Tokyo     ", "HongKong  ", "Sydney    ", "Toronto   ", "Montreal  ", "Victoria  ", "Edmonton  ", "Calgary   ", "Regina    ", "Saskatoon ", "Winnipeg  ", "Whitehorse", "Halifax   ")
colours = Array("Black ", "White ", "Red   ", "Orange", "Yellow", "Green ", "Blue  ", "Violet")

Screen.MousePointer = 11
Label7.caption = ""
Label7.Refresh
Label8.caption = ""
Label8.Refresh
Label9.caption = ""
Label9.Refresh

Call CreateDbf
rc = d4optimize(data, OPT4ALL)

Command1.caption = "Working"
'num = Val(Label3.caption)
num = 2000000
Label7.caption = "Building Database:"
Label7.Refresh

rc = d4appendStart(data, 0)
buff = locals(Int(20 * Rnd))
Call f4assign(loc, buff)
a = Int(maxval * Rnd + minval)
b = Int(a * 0.8)
c = Int(b * 0.8)
Call f4assignInt(lng, a)
Call f4assignInt(wdt, c)
Call f4assignInt(hgt, b)
Call f4assignInt(wgt, Int((a * b * c) / (a + b + c)))
Call f4assignInt(qty, Int(20 * Rnd + 1))
buff = colours(Int(8 * Rnd))
Call f4assign(clr, buff)
rc = d4append(data)

Randomize
time1 = GetTickCount
For x = 0 To num - 2
  rc = d4appendStart(data, 0)
  Select Case (x Mod 3)
    Case 0
      buff = locals(Int(20 * Rnd))
      Call f4assign(loc, buff)
    Case 1
      a = Int(maxval * Rnd + minval)
      b = Int(a * 0.8)
      c = Int(b * 0.8)
      Call f4assignInt(lng, a)
      Call f4assignInt(wdt, c)
      Call f4assignInt(hgt, b)
      Call f4assignInt(wgt, Int((a * b * c) / (a + b + c)))
    Case 2
      buff = colours(Int(8 * Rnd))
      Call f4assign(clr, buff)
  End Select
  Call f4assignInt(qty, Int(20 * Rnd + 1))
  rc = d4append(data)
Next x

time2 = GetTickCount
Result = (time2 - time1) / 1000
If Result < 60 Then

  Label7.caption = "Time to build Database = " & Result & " sec."
Else
  Label7.caption = "Time to build Database = " & (CLng(1000 * Result / 60)) / 1000 & " min."
End If
Label7.Refresh

Label8.caption = "Building Seven Tags:"
Label8.Refresh
time1 = GetTickCount
ind = i4create(data, "", tagInfo())
time2 = GetTickCount
If ind = 0 Then
    MsgBox ("Cannot create index file.  Program terminated.")
    code4close (cb)
    code4initUndo (cb)
    End
End If
Result = (time2 - time1) / 1000
If Result < 60 Then
  Label8.caption = "Time to build Index file = " & Result & " sec."
Else
  Label8.caption = "Time to build Index file = " & (CLng(1000 * Result / 60)) / 1000 & " min."
End If
Label8.Refresh
Label9.caption = "Time to build each Tag = " & (CLng(1000 * Result / 7)) / 1000 & " sec."
Label9.Refresh

Command1.caption = "Build Another?"
Command2.Enabled = True
Screen.MousePointer = 0

End Sub

Private Sub Command2_Click()

Form1.Hide
Form2.Show

End Sub

Private Sub Command3_Click()

If (HScroll1.max = 100) Then
  HScroll1.max = 1000
  HScroll1.Value = 1
  Command3.caption = "Decrease Range x10"
  Label1.caption = "Number of Records (Max. 1 000 000)"
Else
  HScroll1.max = 100
  HScroll1.Value = 1
  Command3.caption = "Increase Range x10"
  Label1.caption = "Number of Records (Max. 100 000)"
End If

End Sub

Private Sub Form_Load()

Label10.caption = "56 K"

End Sub

Private Sub Form_Unload(Cancel As Integer)

rc = code4close(cb)
rc = code4initUndo(cb)

End Sub

Private Sub HScroll1_Change()

Label3.caption = Str(HScroll1.Value) & "000"
Label10.caption = Format$(Str$(((Val(Label3.caption) * 28) * 2.03) / 1024), "###,###,###") & " K"

End Sub

Private Sub HScroll1_Scroll()

Label3.caption = Str(HScroll1.Value) & "000"
Label10.caption = Format$(Str$(((Val(Label3.caption) * 28) * 2.03) / 1024), "###,###,###") & " K"

End Sub

