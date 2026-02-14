VERSION 5.00
Begin VB.Form Form26
   Caption         =   "Warning"
   ClientHeight    =   3675
   ClientLeft      =   1335
   ClientTop       =   3195
   ClientWidth     =   9330
   Icon            =   "suchrule.frx":0000
   LinkTopic       =   "Form26"
   LockControls    =   -1  'True
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   3675
   ScaleWidth      =   9330
   Begin VB.PictureBox Picture1
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      BorderStyle     =   0  'None
      ForeColor       =   &H80000008&
      Height          =   570
      Left            =   180
      Picture         =   "suchrule.frx":030A
      ScaleHeight     =   570
      ScaleWidth      =   570
      TabIndex        =   5
      Top             =   210
      Width           =   570
   End
   Begin VB.CommandButton Command2
      Caption         =   "Cancel"
      Height          =   345
      Left            =   4905
      TabIndex        =   4
      Top             =   2910
      Width           =   1125
   End
   Begin VB.CommandButton Command1
      Caption         =   "OK"
      Height          =   345
      Left            =   3615
      TabIndex        =   3
      Top             =   2910
      Width           =   1125
   End
   Begin VB.OptionButton optDelPrivRecs
      Caption         =   "Delete all records in the Privilege database that match the previous Account Id and save the current changes"
      Height          =   405
      Left            =   1035
      TabIndex        =   2
      Top             =   2160
      Width           =   8265
   End
   Begin VB.OptionButton optChangePrivRecs
      Caption         =   "Modify the Privileges database records to match the new Account Id value and save the current changes."
      Height          =   435
      Left            =   1035
      TabIndex        =   1
      Top             =   1680
      Width           =   7830
   End
   Begin VB.Label Label2
      Caption         =   $"suchrule.frx":077C
      Height          =   525
      Left            =   960
      TabIndex        =   6
      Top             =   1005
      Width           =   8265
   End
   Begin VB.Label Label1
      Caption         =   $"suchrule.frx":0869
      Height          =   870
      Left            =   945
      TabIndex        =   0
      Top             =   195
      Width           =   8175
   End
End
Attribute VB_Name = "Form26"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Dim bCancelClick As Boolean
Dim bOkClick As Boolean

Private Sub Command1_Click()

    bOkClick = True

    If optChangePrivRecs = 0 And optDelPrivRecs = 0 Then
        MsgBox "Please select one of the two options, or press Cancel to return to editing this record"
        Exit Sub
    End If

    If optChangePrivRecs Then
        gOptChangePrivRecs = True
        gOptDelPrivRecs = False
    Else
        gOptChangePrivRecs = False
        gOptDelPrivRecs = True
    End If

    Unload Me

End Sub


Private Sub Command2_Click()

    bCancelClick = True

    'cancel operation
    gOptChangePrivRecs = False
    gOptDelPrivRecs = False

    Unload Me

End Sub


Private Sub Form_Load()
    Left = (Screen.Width - Width) / 2   ' Center form horizontally.
    Top = (Screen.Height - Height) / 2  ' Center form vertically.
End Sub

Private Sub Form_Unload(Cancel As Integer)

    'If user just closes form without clicking a button
    'and an option has been selected, then verify selection

    If Not (bOkClick Or bCancelClick) Then
        If gOptChangePrivRecs Or gOptDelPrivRecs Then
            rc = MsgBox("Are you sure you want to continue with the selected option?", vbYesNo)
            If rc = vbNo Then Cancel = 1
        End If
    End If


End Sub


