VERSION 5.00
Begin VB.Form Form9
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Server Information"
   ClientHeight    =   1905
   ClientLeft      =   1200
   ClientTop       =   1965
   ClientWidth     =   5850
   Icon            =   "sus_info.frx":0000
   LinkTopic       =   "Form9"
   MaxButton       =   0   'False
   MDIChild        =   -1  'True
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   1905
   ScaleWidth      =   5850
   ShowInTaskbar   =   0   'False
   Begin VB.CommandButton Command2
      Caption         =   "&Refresh Now"
      Height          =   375
      Left            =   4500
      TabIndex        =   0
      Top             =   900
      Width           =   1140
   End
   Begin VB.CommandButton Command1
      Cancel          =   -1  'True
      Caption         =   "&Close"
      Default         =   -1  'True
      Height          =   375
      Left            =   4500
      TabIndex        =   1
      Top             =   1365
      Width           =   1140
   End
   Begin VB.Timer Timer1
      Interval        =   1000
      Left            =   4050
      Top             =   1200
   End
   Begin VB.Label lblFileCount
      Caption         =   "2"
      Height          =   195
      Left            =   2040
      TabIndex        =   9
      Top             =   1560
      Width           =   1695
   End
   Begin VB.Label lblConnectionCount
      Caption         =   "8"
      Height          =   195
      Left            =   2040
      TabIndex        =   8
      Top             =   1080
      Width           =   1935
   End
   Begin VB.Label lblIndexFormat
      Caption         =   "FoxPro (CDX)"
      Height          =   195
      Left            =   2040
      TabIndex        =   7
      Top             =   600
      Width           =   2295
   End
   Begin VB.Label lblTime
      Caption         =   "22 Days, 15 Hours, 22 Min, 40 Sec"
      Height          =   195
      Left            =   2040
      TabIndex        =   6
      Top             =   120
      Width           =   3375
   End
   Begin VB.Label Label5
      AutoSize        =   -1  'True
      Caption         =   "File Compatibility:"
      Height          =   195
      Left            =   240
      TabIndex        =   5
      Top             =   600
      Width           =   1200
   End
   Begin VB.Label Label1
      AutoSize        =   -1  'True
      Caption         =   "Server Up Time:"
      Height          =   195
      Left            =   240
      TabIndex        =   4
      Top             =   120
      Width           =   1155
   End
   Begin VB.Label Label2
      AutoSize        =   -1  'True
      Caption         =   "Number of Connections:"
      Height          =   195
      Left            =   240
      TabIndex        =   3
      Top             =   1080
      Width           =   1710
   End
   Begin VB.Label Label3
      AutoSize        =   -1  'True
      Caption         =   "Number of Open Files:"
      Height          =   195
      Left            =   240
      TabIndex        =   2
      Top             =   1560
      Width           =   1575
   End
End
Attribute VB_Name = "Form9"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Public Sub refreshInfo()
    Timer1.Enabled = False

    Dim upTimeSec As Long
    Dim memAlloc&
    Dim numFilesOpen As Integer
    Dim numClients As Integer
    Dim days&, hours&, minutes&, seconds As Long
    Dim daySec&, hourSec&, minSec As Long

    'get the server information
    Call code4errOff(gCode4, 1)
    If code4infoRetrieve(gCode4, memAlloc, numClients, upTimeSec, numFilesOpen) <> r4success Then
        ' error
        Dim errno%
        errno = code4errorCode(gCode4, 0)
        Select Case errno
            Case -1399 To -1300
                ' connection error; probably lost connection to server
                Call MsgBox("Connection to host lost.", vbExclamation, App.title)
                Call frmMain2.mnuDisconnect_Click
            Case Else
                Call MsgBox("CodeBase error " & errno & " getting server information.", vbExclamation, App.title)
        End Select
        Call CloseWindow(Me.hWnd)
        Exit Sub
    End If

    'format the time
    days = upTimeSec \ secInDay
    hourSec = upTimeSec Mod secInDay
    hours = hourSec \ 3600
    minSec = hourSec Mod 3600
    minutes = minSec \ 60
    seconds = minSec Mod 60
    lblTime.caption = Trim(str(days) + " Days, " & hours & " Hours, " & minutes & " Min, " & seconds & " Sec")

    ' CS 1999/12/09 removed memory report
    lblConnectionCount.caption = Trim(str(numClients))
    lblFileCount.caption = Trim(str(numFilesOpen))

    Timer1.Enabled = True
End Sub
Private Sub Command1_Click()
    Unload Me
End Sub
Private Sub Command2_Click()
    refreshTime = 0
    If gCode4 <> 0 Then
        refreshInfo
    End If
End Sub
Private Sub Form_Load()
    HelpContextID = Server_Information

    Left = (frmMain2.ScaleWidth - Width) / 2   ' Center form horizontally.
    Top = (frmMain2.ScaleHeight - Height) / 2  ' Center form vertically.

    ResetCode4

    refreshTime = 0
    refreshInfo
    DoEvents
    Select Case UCase$(code4indexExtension(gCode4))
        Case "CDX"
            lblIndexFormat.caption = "FoxPro (CDX)"
        Case "MDX"
            lblIndexFormat.caption = "dBASE (MDX)"
        Case "NTX"
            lblIndexFormat.caption = "Clipper (NTX)"
    End Select
End Sub

Private Sub Timer1_Timer()
    refreshTime = refreshTime + 1
    If refreshTime >= IntervalInfo And gCode4 <> 0 Then
        refreshInfo
        refreshTime = 0
    End If
End Sub
