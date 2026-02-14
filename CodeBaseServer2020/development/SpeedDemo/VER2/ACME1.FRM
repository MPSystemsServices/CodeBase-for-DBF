VERSION 5.00
Begin VB.Form Form1 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "CodeBase Speed Demo - Build Database"
   ClientHeight    =   6555
   ClientLeft      =   2025
   ClientTop       =   1170
   ClientWidth     =   6375
   Icon            =   "Acme1.frx":0000
   LinkTopic       =   "Form1"
   LockControls    =   -1  'True
   MaxButton       =   0   'False
   MouseIcon       =   "Acme1.frx":030A
   ScaleHeight     =   6550.966
   ScaleMode       =   0  'User
   ScaleWidth      =   6382.37
   WhatsThisButton =   -1  'True
   WhatsThisHelp   =   -1  'True
   Begin VB.CommandButton cmdExit 
      Caption         =   "E&xit"
      Height          =   435
      Left            =   5100
      TabIndex        =   19
      Top             =   6060
      Width           =   975
   End
   Begin VB.CommandButton cmdBuildAgain 
      Enabled         =   0   'False
      Height          =   435
      Left            =   2325
      Picture         =   "Acme1.frx":074C
      Style           =   1  'Graphical
      TabIndex        =   4
      Top             =   6060
      Width           =   2085
   End
   Begin VB.Frame frmBuild 
      Caption         =   "Build Database"
      Height          =   2235
      Left            =   210
      TabIndex        =   11
      Top             =   2580
      Width           =   5880
      Begin VB.CommandButton cmdBuild 
         Height          =   435
         Left            =   4320
         Picture         =   "Acme1.frx":090F
         Style           =   1  'Graphical
         TabIndex        =   0
         Top             =   1650
         Width           =   1395
      End
      Begin VB.HScrollBar hscrDbSize 
         Height          =   252
         LargeChange     =   10
         Left            =   165
         Max             =   100
         Min             =   1
         TabIndex        =   2
         Top             =   960
         Value           =   1
         Width           =   5550
      End
      Begin VB.CheckBox chkBigTable 
         Caption         =   "&Increase Maximum Database Size"
         Height          =   315
         Left            =   330
         TabIndex        =   1
         Top             =   1680
         Width           =   3240
      End
      Begin VB.Label lblDiskSpace 
         BorderStyle     =   1  'Fixed Single
         Caption         =   "Label10"
         Height          =   300
         Left            =   4605
         TabIndex        =   17
         Top             =   405
         Width           =   1095
      End
      Begin VB.Label lblDiskSpaceCaption 
         Caption         =   "Disk space required:"
         Height          =   255
         Left            =   3030
         TabIndex        =   16
         Top             =   435
         Width           =   1485
      End
      Begin VB.Label lblDbSize 
         BorderStyle     =   1  'Fixed Single
         Caption         =   " "
         Height          =   285
         Left            =   1365
         TabIndex        =   15
         Top             =   420
         Width           =   1350
      End
      Begin VB.Label lblDbSizeCaption 
         Caption         =   "Database Size:"
         Height          =   270
         Left            =   165
         TabIndex        =   14
         Top             =   435
         Width           =   1155
      End
      Begin VB.Label lblMin 
         Caption         =   "1000"
         Height          =   255
         Left            =   390
         TabIndex        =   13
         Top             =   1275
         Width           =   915
      End
      Begin VB.Label lblMax 
         Caption         =   "100000"
         Height          =   240
         Left            =   4815
         TabIndex        =   12
         Top             =   1260
         Width           =   945
      End
   End
   Begin VB.PictureBox Picture1 
      BorderStyle     =   0  'None
      Height          =   375
      Left            =   765
      Picture         =   "Acme1.frx":0C55
      ScaleHeight     =   375
      ScaleWidth      =   4695
      TabIndex        =   9
      Top             =   90
      Width           =   4695
   End
   Begin VB.CommandButton cmdQuery 
      Enabled         =   0   'False
      Height          =   435
      Left            =   210
      Picture         =   "Acme1.frx":5569
      Style           =   1  'Graphical
      TabIndex        =   3
      Top             =   6060
      Width           =   1980
   End
   Begin VB.Label lblResults 
      Caption         =   "Results:"
      Height          =   270
      Left            =   225
      TabIndex        =   18
      Top             =   4980
      Visible         =   0   'False
      Width           =   645
   End
   Begin VB.Label Label4 
      Caption         =   $"Acme1.frx":57DA
      Height          =   615
      Left            =   240
      TabIndex        =   10
      Top             =   1080
      Width           =   5910
   End
   Begin VB.Label lblBuildNdxTime 
      Height          =   255
      Left            =   885
      TabIndex        =   8
      Top             =   5370
      Width           =   5175
      WordWrap        =   -1  'True
   End
   Begin VB.Label lblBuildDbTime 
      Height          =   270
      Left            =   885
      TabIndex        =   7
      Top             =   4980
      Width           =   5160
      WordWrap        =   -1  'True
   End
   Begin VB.Label Label6 
      Caption         =   $"Acme1.frx":58C9
      Height          =   615
      Left            =   240
      TabIndex        =   6
      Top             =   1800
      Width           =   5910
      WordWrap        =   -1  'True
   End
   Begin VB.Label Label5 
      Caption         =   $"Acme1.frx":59B1
      Height          =   450
      Left            =   240
      TabIndex        =   5
      Top             =   525
      Width           =   5910
      WordWrap        =   -1  'True
   End
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False

Private Sub chkBigTable_Click()

    If chkBigTable Then
        lblMin = "10000"
        lblMax = "1000000"
        hscrDbSize.max = 1000
        hscrDbSize.min = 10
        hscrDbSize.Value = 10
    Else
        lblMin = "1000"
        lblMax = "100000"
        hscrDbSize.max = 100
        hscrDbSize.min = 1
        hscrDbSize.Value = 1
    End If
End Sub

Sub EnableBuildFrame(enable As Boolean)
    frmBuild.Enabled = enable
    lblDbSizeCaption.Enabled = enable
    lblDbSize.Enabled = enable
    lblDiskSpace.Enabled = enable
    lblDiskSpaceCaption.Enabled = enable
    lblMin.Enabled = enable
    lblMax.Enabled = enable
    cmdBuild.Enabled = enable
    chkBigTable.Enabled = enable
    'If Not enable Then cmdBuild.caption = "Create"
End Sub

Sub EnableResults(enable As Boolean)
    lblResults.Visible = enable
    lblResults.Refresh
    lblBuildDbTime.Visible = enable
    lblBuildNdxTime.Visible = enable
End Sub

Sub EnableMore(enable As Boolean)
    cmdQuery.Enabled = enable
    cmdBuildAgain.Enabled = enable
End Sub

Private Sub cmdBuild_Click()

Dim ind As Long
Dim X As Long
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
lblBuildDbTime.caption = ""
lblBuildDbTime.Refresh
lblBuildNdxTime.caption = ""
lblBuildNdxTime.Refresh

Call CreateDbf
rc = d4optimize(data, OPT4ALL)

num = CDbl(lblDbSize.caption)
EnableResults (True)
lblBuildDbTime.caption = "Building Database:"
lblBuildDbTime.Refresh

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
For X = 0 To num - 2
  rc = d4appendStart(data, 0)
  Select Case (X Mod 3)
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
Next X

time2 = GetTickCount
Result = (time2 - time1) / 1000

EnableBuildFrame (False)


If Result < 60 Then
  lblBuildDbTime.caption = "Time to build Database = " & Result & " sec."
Else
  lblBuildDbTime.caption = "Time to build Database = " & (CLng(1000 * Result / 60)) / 1000 & " min."
End If
lblBuildDbTime.Refresh

lblBuildNdxTime.caption = "Building 7 Indexes:"
lblBuildNdxTime.Refresh
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
  lblBuildNdxTime.caption = "Time to build 7 Indexes = " & Result & " sec." & " Time per Index = " & (CLng(1000 * Result / 7)) / 1000 & " sec."
Else
  lblBuildNdxTime.caption = "Time to build 7 Indexes = " & (CLng(1000 * Result / 60)) / 1000 & " min." & " Time per Index = " & (CLng(1000 * Result / 7)) / 1000 & " sec."
End If
lblBuildNdxTime.Refresh


EnableMore (True)
Screen.MousePointer = 0
cmdQuery.SetFocus

End Sub

Private Sub cmdBuildAgain_Click()

    EnableBuildFrame (True)
    EnableResults (False)
    EnableMore (False)
    
End Sub


Private Sub cmdExit_Click()
    Unload Me
End Sub

Private Sub cmdQuery_Click()
    Form1.Hide
    Form2.Show
End Sub

Private Sub Form_Load()

Me.Left = (Screen.Width - Me.ScaleWidth) / 2
Me.Top = (Screen.Height - ScaleHeight) / 2
lblDbSize.caption = " " & Str$(hscrDbSize.Value) & "000"
lblDiskSpace.caption = " 56 K"

End Sub

Private Sub Form_Unload(Cancel As Integer)

rc = code4close(cb)
rc = code4initUndo(cb)
End

End Sub

Private Sub hscrDbsize_Change()
    lblDbSize.caption = " " & Str$(hscrDbSize.Value) & "000"
    lblDiskSpace.caption = " " & Str$(CLng(((CDbl(lblDbSize.caption) * 28) * 2.03) / 1024)) & " K"
    
End Sub

Private Sub hscrDbsize_Scroll()
    lblDbSize.caption = " " & Str$(hscrDbSize.Value) & "000"
    lblDiskSpace.caption = " " & Str$(CLng(((CDbl(lblDbSize.caption) * 28) * 2.03) / 1024)) & " K"
End Sub

