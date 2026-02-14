VERSION 5.00
Begin VB.Form frmTagSelect
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Select Index Tag"
   ClientHeight    =   2610
   ClientLeft      =   1140
   ClientTop       =   1515
   ClientWidth     =   3225
   ControlBox      =   0   'False
   LinkTopic       =   "Form26"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   2610
   ScaleWidth      =   3225
   ShowInTaskbar   =   0   'False
   Begin VB.CommandButton cmdCancel
      Cancel          =   -1  'True
      Caption         =   "Cancel"
      Height          =   375
      Left            =   1920
      TabIndex        =   2
      Top             =   600
      Width           =   1215
   End
   Begin VB.CommandButton cmdSelect
      Caption         =   "Select"
      Default         =   -1  'True
      Height          =   375
      Left            =   1920
      TabIndex        =   1
      Top             =   120
      Width           =   1215
   End
   Begin VB.ListBox lstTags
      Height          =   2595
      Left            =   0
      TabIndex        =   0
      Top             =   0
      Width           =   1815
   End
End
Attribute VB_Name = "frmTagSelect"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
' This form will change the selected tag of the DATA4
' passed to it.
' It must be started from the init() method.
' It will not work if the show() method is called
' from outside the form.

Private data&
Public Function init(ByRef db&) As Boolean
    Dim tag&, currentTag&, selection%
    If db = 0 Then
        Unload Me
    End If
    data = db
    tag = d4tagNext(data, 0)
    If tag = 0 Then
        lstTags.AddItem "NO TAGS AVAILABLE"
        cmdSelect.Enabled = 0
        lstTags.ListIndex = -1
    Else
        selection = 0
        currentTag = d4tagSelected(data)
        lstTags.AddItem "NATURAL ORDER"
        Do
            lstTags.AddItem t4Alias(tag)
            If currentTag = tag Then
                selection = lstTags.ListCount - 1
            End If
            tag = d4tagNext(data, tag)
        Loop While tag <> 0
    End If
    lstTags.ListIndex = selection
    Me.Show 1
End Function
Private Sub cmdCancel_Click()
    Unload Me
End Sub

Private Sub cmdSelect_Click()
    Dim selection%
    selection = lstTags.ListIndex
    If selection = 0 Then
        Call d4tagSelect(data, 0)
    Else
        Call d4tagSelect(data, d4tag(data, lstTags.List(selection)))
    End If
    Unload Me
End Sub
Private Sub Form_GotFocus()
    If data = 0 Then
        Unload Me
    End If
End Sub

Private Sub Form_Load()
    Left = (Screen.Width - Width) / 2   ' Center form horizontally.
    Top = (Screen.Height - Height) / 2  ' Center form vertically.
End Sub


Private Sub Form_Resize()
    With lstTags
        .Top = ScaleTop
        .Height = ScaleHeight
        .Left = ScaleLeft
    End With
End Sub


Private Sub lstTags_DblClick()
    Call cmdSelect_Click
End Sub


