VERSION 5.00
Begin VB.Form Form23
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Select Path"
   ClientHeight    =   4080
   ClientLeft      =   750
   ClientTop       =   2235
   ClientWidth     =   6510
   ControlBox      =   0   'False
   LinkTopic       =   "Form23"
   LockControls    =   -1  'True
   MaxButton       =   0   'False
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   4080
   ScaleWidth      =   6510
   ShowInTaskbar   =   0   'False
   Begin VB.ComboBox Combo1
      Height          =   315
      ItemData        =   "suopen.frx":0000
      Left            =   150
      List            =   "suopen.frx":000A
      Style           =   2  'Dropdown List
      TabIndex        =   7
      Top             =   3525
      Width           =   2415
   End
   Begin VB.CommandButton Command2
      Cancel          =   -1  'True
      Caption         =   "Cancel"
      Height          =   315
      Left            =   5250
      TabIndex        =   5
      Top             =   1350
      Width           =   1140
   End
   Begin VB.CommandButton Command1
      Caption         =   "OK"
      Default         =   -1  'True
      Height          =   315
      Left            =   5250
      TabIndex        =   4
      Top             =   900
      Width           =   1140
   End
   Begin VB.FileListBox File1
      Enabled         =   0   'False
      Height          =   2040
      Left            =   150
      TabIndex        =   1
      Top             =   900
      Width           =   2415
   End
   Begin VB.TextBox txtPath
      Height          =   345
      Left            =   705
      TabIndex        =   0
      Text            =   "Text1"
      Top             =   105
      Width           =   5670
   End
   Begin VB.DirListBox Dir1
      Height          =   2280
      Left            =   2700
      TabIndex        =   2
      Top             =   900
      Width           =   2415
   End
   Begin VB.DriveListBox Drive1
      Height          =   315
      Left            =   2700
      TabIndex        =   3
      Top             =   3525
      Width           =   2415
   End
   Begin VB.Label Label5
      Caption         =   "Drive"
      Height          =   240
      Left            =   2700
      TabIndex        =   11
      Top             =   3300
      Width           =   1365
   End
   Begin VB.Label Label4
      Caption         =   "File Type"
      Height          =   240
      Left            =   150
      TabIndex        =   10
      Top             =   3300
      Width           =   1740
   End
   Begin VB.Label Label3
      Caption         =   "Directories"
      Height          =   315
      Left            =   2700
      TabIndex        =   9
      Top             =   675
      Width           =   1740
   End
   Begin VB.Label Label2
      Caption         =   "Files"
      Height          =   240
      Left            =   150
      TabIndex        =   8
      Top             =   675
      Width           =   1815
   End
   Begin VB.Label Label1
      Caption         =   "&Path:"
      Height          =   240
      Left            =   120
      TabIndex        =   6
      Top             =   150
      Width           =   555
   End
End
Attribute VB_Name = "Form23"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub Combo1_Change()
    File1.Pattern = getExt()
End Sub
Private Sub Combo1_Click()
    File1.Pattern = getExt()
End Sub
Private Function getExt() As String
' return the string between the parentheses
' e.g. passing "xBASE Data File (*.dbf)"
' will return "*.dbf"
' If there are no parentheses, the entire string is returned
' If there is no closing bracket, the string right of the
' open bracket is returned
    Dim s1 As Integer, s2 As Integer
    Dim item As String
    item = Combo1.Text
    s1 = InStr(1, item, "(")
    If s1 = 0 Then
        getExt = item
    Else
        s2 = InStr(s1, item, ")")
        If s2 = 0 Then
            getExt = Mid(item, s1 + 1)
        Else
            getExt = Mid(item, s1 + 1, s2 - s1 - 1)
        End If
    End If
End Function

Private Sub Command1_Click()
    browseDlgPath = Trim$(txtPath)
    browseDrive = Drive1.Drive
    browseDir = Dir1.path

    Unload Me
End Sub
Private Sub Command2_Click()
    Unload Me
End Sub
Private Sub Dir1_Change()
    File1.path = Dir1.path
End Sub
Private Sub Drive1_Change()
    Dir1.path = Drive1.Drive
End Sub
Private Sub File1_PathChange()
    txtPath = File1.path
End Sub
Private Sub Form_Load()
    showWarn

    Left = (Screen.Width - Width) / 2   ' Center form horizontally.
    Top = (Screen.Height - Height) / 2  ' Center form vertically.

    If Trim(browseDrive) <> "" Then Drive1.Drive = browseDrive
    If Trim(browseDir) <> "" Then Dir1.path = browseDir

    txtPath = File1.path
    browseDlgPath = ""
    Combo1.ListIndex = 0
End Sub
