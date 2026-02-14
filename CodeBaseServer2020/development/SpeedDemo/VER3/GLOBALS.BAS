Attribute VB_Name = "Module2"
Declare Function GetTickCount Lib "kernel32" () As Long

Public cb As Long

Public data As Long
Public query As Long

Public fieldinfo() As FIELD4INFO
Public tagInfo() As TAG4INFO

Public loc As Long
Public lng As Long
Public wdt As Long
Public hgt As Long
Public wgt As Long
Public qty As Long
Public clr As Long

Public time1, time2 As Variant

Public relation As Long

Public maxval As Integer
Public minval As Integer
Public rc As Long
Public num As Long

Public Sub CreateDbf()

If data <> nil Then rc = d4close(data)
data = d4open(cb, App.Path + "DATA")
If (data <> nil) Then
   rc = d4remove(data)
End If
data = d4createData(cb, App.Path + "\CB6DEMO", fieldinfo())
If data = 0 Then
    MsgBox ("Cannot create database.  Program terminated.")
    code4close (cb)
    code4initUndo (cb)
    End
End If
rc = code4optAll(cb)
rc = code4optStart(cb)

loc = d4field(data, "LOCATION")
If loc = 0 Then
    MsgBox ("Cannot assign field variables.  Program terminated.")
    code4close (cb)
    code4initUndo (cb)
    End
End If
lng = d4field(data, "LENGTH")
wdt = d4field(data, "WIDTH")
hgt = d4field(data, "HEIGHT")
wgt = d4field(data, "WEIGHT")
qty = d4field(data, "QUANTITY")
clr = d4field(data, "COLOUR")

End Sub

Sub Main()

cb = code4init()
If cb = 0 Then
    MsgBox ("An error occured attempting to initialize the CodeBase Demo DLL. Program terminated.")
    End
End If
rc = code4memSizeBuffer(cb, 262144)
rc = code4safety(cb, 0)
rc = code4accessMode(cb, OPEN4DENY_RW)
rc = code4errOpen(cb, 0)
rc = code4errOff(cb, 1)

ReDim fieldinfo(1 To 7) As FIELD4INFO
fieldinfo(1).fName = "LOCATION"
fieldinfo(1).ftype = r4str
fieldinfo(1).flength = 10
fieldinfo(1).fdecimals = 0
fieldinfo(2).fName = "LENGTH"
fieldinfo(2).ftype = r4num
fieldinfo(2).flength = 2
fieldinfo(2).fdecimals = 0
fieldinfo(3).fName = "WIDTH"
fieldinfo(3).ftype = r4num
fieldinfo(3).flength = 2
fieldinfo(3).fdecimals = 0
fieldinfo(4).fName = "HEIGHT"
fieldinfo(4).ftype = r4num
fieldinfo(4).flength = 2
fieldinfo(4).fdecimals = 0
fieldinfo(5).fName = "WEIGHT"
fieldinfo(5).ftype = r4num
fieldinfo(5).flength = 3
fieldinfo(5).fdecimals = 0
fieldinfo(6).fName = "QUANTITY"
fieldinfo(6).ftype = r4num
fieldinfo(6).flength = 2
fieldinfo(6).fdecimals = 0
fieldinfo(7).fName = "COLOUR"
fieldinfo(7).ftype = r4str
fieldinfo(7).flength = 6
fieldinfo(7).fdecimals = 0

ReDim tagInfo(1 To 7) As TAG4INFO
tagInfo(1).name = "LOCTAG"
tagInfo(1).expression = "LOCATION"
tagInfo(2).name = "LENTAG"
tagInfo(2).expression = "LENGTH"
tagInfo(3).name = "WIDTAG"
tagInfo(3).expression = "WIDTH"
tagInfo(4).name = "HEITAG"
tagInfo(4).expression = "HEIGHT"
tagInfo(5).name = "WEITAG"
tagInfo(5).expression = "WEIGHT"
tagInfo(6).name = "QUATAG"
tagInfo(6).expression = "QUANTITY"
tagInfo(6).descending = 0
tagInfo(7).name = "COLTAG"
tagInfo(7).expression = "COLOUR"

maxval = 25
minval = 11
Form1.Show

End Sub
