VERSION 5.00
Begin VB.Form frmGetServerFile
   Caption         =   "Add Tables"
   ClientHeight    =   3450
   ClientLeft      =   1905
   ClientTop       =   2055
   ClientWidth     =   7185
   LinkTopic       =   "Form29"
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   3450
   ScaleWidth      =   7185
   Begin VB.Frame Frame1
      Caption         =   "Directory"
      Height          =   1095
      Left            =   2880
      TabIndex        =   4
      Top             =   120
      Width           =   4215
      Begin VB.TextBox txtPath
         Height          =   300
         Left            =   120
         TabIndex        =   7
         Top             =   240
         Width           =   3975
      End
      Begin VB.CommandButton cmdGetPWD
         Caption         =   "&Server Directory"
         Height          =   375
         Left            =   2760
         TabIndex        =   6
         Top             =   600
         Width           =   1335
      End
      Begin VB.CommandButton cmdBrowse
         Caption         =   "&Browse"
         Height          =   375
         Left            =   1320
         TabIndex        =   5
         Top             =   600
         Width           =   1335
      End
   End
   Begin VB.CommandButton cmdOK
      Caption         =   "OK"
      Default         =   -1  'True
      Height          =   375
      Left            =   4320
      TabIndex        =   3
      Top             =   3000
      Width           =   1335
   End
   Begin VB.CommandButton cmdCancel
      Cancel          =   -1  'True
      Caption         =   "&Cancel"
      Height          =   375
      Left            =   5760
      TabIndex        =   2
      Top             =   3000
      Width           =   1335
   End
   Begin VB.CommandButton cmdRefresh
      Caption         =   "&Refresh"
      Height          =   375
      Left            =   120
      TabIndex        =   1
      Top             =   3000
      Width           =   1335
   End
   Begin VB.ListBox lstServerTables
      Height          =   2790
      Left            =   120
      MultiSelect     =   2  'Extended
      TabIndex        =   0
      Top             =   120
      Width           =   2655
   End
End
Attribute VB_Name = "frmGetServerFile"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private keepPath As String

Private Sub cmdGetPWD_Click()
    Call ResetCode4
    Call ErrOff
    txtPath.Text = code4serverCurrentDirectory(gCode4)
End Sub

Private Sub cmdBrowse_Click()
    BrowseForFolder.SetDefaults
    BrowseForFolder.Display Me.hWnd, ""
    If BrowseForFolder.successful Then
        txtPath.Text = BrowseForFolder.folderName

        'automatically generate list if server is on local machine
        If bHostIsLocal Then
            cmdRefresh_Click
        End If
    End If
End Sub

Public Sub RegenerateTables()
    Dim rc As Integer

    Call code4singleOpen(gCode4, 0)
    Call code4safety(gCode4, 0)
    Call ErrOff

    Dim dbGenTable As Long
    dbGenTable = code4directory(gCode4, txtPath.Text)
    If dbGenTable = 0 Then
        ' could not open the file list
        Dim errno%
        errno = code4errorCode(gCode4, 0)
        Select Case errno
            Case -1399 To -1300
                ' connection error; probably lost connection to server
                Call MsgBox("Connection to host lost.", vbExclamation, App.title)
                Call frmMain2.mnuDisconnect_Click
            Case Else
                Call MsgBox("Unable to access the file list. CodeBase error" & errno & ".", vbExclamation, App.title)
        End Select
        Call CloseWindow(Me.hWnd)
        Call CloseWindow(Form7.hWnd)
        Exit Sub
    End If

'''        pwd = txtPath.Text
    Dim field&
    field = d4fieldJ(dbGenTable, 1)

    'Dim tagName$, tag&
    'tagName = "FILENAME"   ' this may change
    'tag = d4tag(dbGenTable, tagName)
    'If tag = 0 Then
    '    ShowError Space(5) + "Tag " + tagName + " not found. Using default."
        Tag = d4tagDefault(dbGenTable)
    'End If
    Call d4tagSelect(dbGenTable, Tag)

    lstServerTables.Clear
    rc = d4top(dbGenTable)
    Do While rc = r4success
        lstServerTables.AddItem Trim(f4str(field))
        rc = d4skip(dbGenTable, 1)
    Loop
    Call d4close(dbGenTable)

    Call ErrOn
End Sub

Private Sub cmdOK_Click()
    Dim i%
    For i = 0 To (lstServerTables.ListCount - 1)
        If lstServerTables.Selected(i) Then
            Form7.lstTables.AddItem keepPath + lstServerTables.List(i)
        End If
    Next i
    Unload Me
End Sub

Private Sub cmdRefresh_Click()
    MousePointer = vbHourglass
'    CloseTableFile
    RegenerateTables
    keepPath = txtPath
    If Len(keepPath) > 0 Then
        If Right(keepPath, 1) <> SepChar Then
            keepPath = keepPath + SepChar
        End If
    End If
    cmdOK.Enabled = False

    MousePointer = vbDefault
End Sub

Private Sub cmdCancel_Click()
    Unload Me
End Sub

Private Sub Form_Load()
    Call cmdGetPWD_Click
    If bHostIsLocal Then
        cmdRefresh_Click
    End If
End Sub

Private Sub Form_Resize()
    If Me.Height < 2325 Then Me.Height = 2325
    If Me.Width < 5580 Then Me.Width = 5580

    Dim diff As Integer
    diff = cmdCancel.Left - cmdOK.Left - cmdOK.Width

    cmdCancel.Left = Me.ScaleWidth - diff - cmdCancel.Width
    cmdCancel.Top = Me.ScaleHeight - diff - cmdCancel.Height

    cmdOK.Left = cmdCancel.Left - diff - cmdOK.Width
    cmdOK.Top = cmdCancel.Top

    Frame1.Left = ScaleWidth - diff - Frame1.Width

    cmdRefresh.Top = cmdCancel.Top

    lstServerTables.Height = ScaleHeight - diff - cmdRefresh.Height - diff - lstServerTables.Top
    lstServerTables.Width = Frame1.Left - diff - lstServerTables.Left
End Sub

Private Sub lstServerTables_Click()
    If lstServerTables.SelCount > 0 Then
        cmdOK.Enabled = True
    Else
        cmdOK.Enabled = False
    End If
End Sub

Private Sub lstServerTables_DblClick()
    Call cmdOK_Click
End Sub

Private Sub txtPath_GotFocus()
    TxtGotFocus Me
End Sub

Private Sub txtPath_KeyPress(KeyAscii As Integer)
    If KeyAscii = 13 Then
        Call cmdRefresh_Click
        lstServerTables.SetFocus
        KeyAscii = 0
    End If
End Sub
