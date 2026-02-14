VERSION 5.00
Begin VB.Form frmAbout
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "About CodeBase Administrator"
   ClientHeight    =   3075
   ClientLeft      =   1140
   ClientTop       =   1515
   ClientWidth     =   4140
   Icon            =   "suabout.frx":0000
   LinkTopic       =   "Form28"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   3075
   ScaleWidth      =   4140
   ShowInTaskbar   =   0   'False
   StartUpPosition =   2  'CenterScreen
   Begin VB.CommandButton cmdOK
      Caption         =   "&OK"
      Default         =   -1  'True
      Height          =   330
      Left            =   1695
      TabIndex        =   2
      Top             =   2670
      Width           =   1245
   End
   Begin VB.PictureBox Picture2
      BorderStyle     =   0  'None
      Height          =   855
      Left            =   195
      ScaleHeight     =   855
      ScaleWidth      =   735
      TabIndex        =   4
      Top             =   45
      Width           =   735
   End
   Begin VB.Label lblContactInfo
      AutoSize        =   -1  'True
      Height          =   195
      Left            =   1125
      TabIndex        =   3
      Top             =   960
      Width           =   2910
      WordWrap        =   -1  'True
   End
   Begin VB.Line Line1
      X1              =   158
      X2              =   3968
      Y1              =   2460
      Y2              =   2460
   End
   Begin VB.Label Label2
      Caption         =   "Copyright © 2002 Sequiter Software Inc."
      Height          =   240
      Left            =   1125
      TabIndex        =   1
      Top             =   502
      Width           =   2985
   End
   Begin VB.Label lblTitle
      Caption         =   "CodeBase Administrator"
      Height          =   225
      Left            =   1125
      TabIndex        =   0
      Top             =   60
      Width           =   2775
   End
End
Attribute VB_Name = "frmAbout"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub cmdOK_Click()
    Unload Me
End Sub

Private Sub Form_Load()
    Me.caption = "About " & App.title

    lblTitle.caption = App.title & " " & App.Major & "." & App.Minor & "." & App.Revision

    Picture2.Picture = frmMain2.Icon

    lblContactInfo.caption = _
            "Suite 209, 9644 54 Avenue" & vbCrLf & _
            "Edmonton, AB  T6E 5V1" & vbCrLf & _
            "Tel: (780) 437-2410" & vbCrLf & _
            "Fax: (780) 436-2999" & vbCrLf & vbCrLf & _
            "Web: http://www.codebase.com"

    ' override contact info from file
    On Error GoTo aftercaption
    Dim fs As Variant
    Set fs = CreateObject("Scripting.FileSystemObject")
    If fs.FileExists(App.path & "\" & "ContactInfo.txt") Then
        Dim f
        Set f = fs.OpenTextFile(App.path & "\" & "ContactInfo.txt", 1)
        lblContactInfo.caption = f.ReadAll
        f.Close
    End If
aftercaption:
    On Error GoTo 0

    ' adjust size and position based on contact info size
    Dim diff%
    diff = Line1.Y1 - (lblContactInfo.Top + lblContactInfo.Height) - 80

    Line1.Y1 = Line1.Y1 - diff
    Line1.Y2 = Line1.Y1
    cmdOK.Top = cmdOK.Top - diff
    Height = Height - diff
End Sub

