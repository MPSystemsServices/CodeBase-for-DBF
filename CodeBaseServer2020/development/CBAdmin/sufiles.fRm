VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "comdlg32.ocx"
Begin VB.Form Form7
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Table Maintenance"
   ClientHeight    =   3600
   ClientLeft      =   720
   ClientTop       =   1530
   ClientWidth     =   8640
   Icon            =   "sufiles.frx":0000
   LinkTopic       =   "Form7"
   LockControls    =   -1  'True
   MaxButton       =   0   'False
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   3600
   ScaleWidth      =   8640
   Begin VB.CommandButton cmdClose
      Caption         =   "Close"
      Default         =   -1  'True
      Height          =   375
      Left            =   7050
      TabIndex        =   11
      Top             =   3150
      Width           =   1215
   End
   Begin VB.PictureBox container
      BorderStyle     =   0  'None
      Height          =   2892
      Left            =   3600
      ScaleHeight     =   2895
      ScaleWidth      =   4950
      TabIndex        =   12
      Top             =   120
      Width           =   4944
      Begin VB.Frame Frame1
         Caption         =   "Actions for Selected Tables"
         Height          =   2880
         Left            =   1320
         TabIndex        =   13
         Top             =   0
         Width           =   3626
         Begin VB.CommandButton cmdStartJob
            Caption         =   "&Start"
            Enabled         =   0   'False
            Height          =   375
            Left            =   720
            TabIndex        =   9
            Top             =   2400
            Width           =   1215
         End
         Begin VB.OptionButton optValidIndex
            Caption         =   "&Validate Indexes"
            Height          =   240
            Left            =   150
            TabIndex        =   3
            Top             =   330
            Width           =   2715
         End
         Begin VB.OptionButton optReindex
            Caption         =   "&Reindex Table"
            Height          =   240
            Left            =   150
            TabIndex        =   4
            Top             =   660
            Width           =   2715
         End
         Begin VB.OptionButton optCompress
            Caption         =   "Compress &Memo"
            Height          =   240
            Left            =   150
            TabIndex        =   5
            Top             =   990
            Width           =   2715
         End
         Begin VB.OptionButton optCompressReindex
            Caption         =   "Compress M&emo && Reindex"
            Height          =   240
            Left            =   150
            TabIndex        =   6
            Top             =   1320
            Width           =   2715
         End
         Begin VB.OptionButton optPack
            Caption         =   "Compress &Table (Auto Reindex)"
            Height          =   240
            Left            =   150
            TabIndex        =   7
            Top             =   1650
            Width           =   3165
         End
         Begin VB.OptionButton optAll
            Caption         =   "&Compress Table && Memo (Auto Reindex)"
            Height          =   240
            Left            =   150
            TabIndex        =   8
            Top             =   1980
            Width           =   3372
         End
      End
      Begin VB.CommandButton cmdAdd
         Caption         =   "A&dd..."
         Height          =   375
         Left            =   0
         TabIndex        =   0
         Top             =   0
         Width           =   1215
      End
      Begin VB.CommandButton cmdRemove
         Caption         =   "&Remove"
         Enabled         =   0   'False
         Height          =   375
         Left            =   0
         TabIndex        =   1
         Top             =   480
         Width           =   1215
      End
      Begin VB.CommandButton cmdSelectAll
         Caption         =   "Select &All"
         Enabled         =   0   'False
         Height          =   375
         Left            =   0
         TabIndex        =   2
         Top             =   960
         Width           =   1215
      End
      Begin MSComDlg.CommonDialog dlgBrowse
         Left            =   360
         Top             =   2160
         _ExtentX        =   847
         _ExtentY        =   847
         _Version        =   393216
         DialogTitle     =   "Add Table"
         Filter          =   "xBASE Data Files (*.dbf)|*.dbf|All Files (*.*)|*.*"
         MaxFileSize     =   2048
      End
   End
   Begin VB.ListBox lstTables
      Height          =   3180
      Left            =   105
      MultiSelect     =   2  'Extended
      Sorted          =   -1  'True
      TabIndex        =   10
      Top             =   120
      Width           =   3375
   End
End
Attribute VB_Name = "Form7"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Private bChkMouseDn As Boolean
Private bMouseDown As Boolean
Private bConnected As Boolean
Public pwd As String
Private codeGenTable&

Private Sub lstAppend(lst As ListBox, str As String)
' append text to the last line of a listbox
    lst.List(lst.ListCount - 1) = lst.List(lst.ListCount - 1) + str
End Sub

Private Sub setButtons()
    If lstTables.ListCount = 0 Then
        cmdSelectAll.Enabled = False
        cmdStartJob.Enabled = False
    Else
        cmdSelectAll.Enabled = True
        cmdStartJob.Enabled = True
    End If

    If lstTables.SelCount = 0 Then
        cmdRemove.Enabled = False
    Else
        cmdRemove.Enabled = True
    End If
End Sub

Private Sub cmdAdd_Click()
    If u4switch() And &H80 Then
        'STAND_ALONE
        Dim files As Collection
        Dim Dir$, i%
        dlgBrowse.fileName = ""
        dlgBrowse.ShowOpen
        If Len(dlgBrowse.fileName) > 0 Then
            Set files = ParseDelimitedString(dlgBrowse.fileName)
            If files.count > 1 Then
                Dir = files(1)
                If Right(Dir, 1) <> SepChar Then
                    Dir = Dir + SepChar
                End If
                files.Remove 1
            End If
            For i = 1 To files.count  ' CS 2000/01/25 can't use "for each x in Collection"; possible W98 bug???
                lstTables.AddItem Dir + files.Item(i)
            Next i
        End If
    Else
        'CLIENT/SERVER
        frmGetServerFile.Show 1
    End If
    lstTables.ListIndex = lstTables.NewIndex
    setButtons
End Sub

Private Sub cmdremove_Click()
    Dim i%
    Do While lstTables.SelCount > 0
        For i = 0 To (lstTables.ListCount - 1)
            If lstTables.Selected(i) Then
                lstTables.RemoveItem i
                Exit For
            End If
        Next i
    Loop
    setButtons
End Sub

Private Sub cmdStartJob_Click()
    Me.Enabled = False
    Me.MousePointer = 11
    frmProgress.lstReport.Clear
    If optValidIndex.value Then SelectedTablesAction 0
    If optReindex.value Then SelectedTablesAction 1
    If optCompress.value Then SelectedTablesAction 3
    If optCompressReindex.value Then
        SelectedTablesAction 3
        SelectedTablesAction 1
    End If
    If optPack.value Then SelectedTablesAction 2
    If optAll.value Then SelectedTablesAction 4
    Me.MousePointer = 0
    Me.Enabled = True 'cj -26/10/99- program freezes and allows not input if no radio buttons are checked unless this function is here.
End Sub

Private Sub cmdClose_Click()
    Unload Me
End Sub
Private Sub Form_Load()
    HelpContextID = File_Maintenance

    Left = (Screen.Width - Width) / 2   ' Center form horizontally.
    Top = (Screen.Height - Height) / 2  ' Center form vertically.

    dlgBrowse.flags = cdlOFNAllowMultiselect + cdlOFNExplorer + cdlOFNFileMustExist + cdlOFNHideReadOnly + cdlOFNNoReadOnlyReturn + cdlOFNPathMustExist
End Sub
Public Sub SelectedTablesAction(Action As Integer)
    'get selected tables
    'start checking
    Dim selFiles As Collection
    Set selFiles = GetItems(lstTables)
    Dim i As Integer, j As Integer
    Dim tableName As String, results As String
    Dim resultSuccess As String, resultError As String
    Dim resultCancel As String
    Dim resultRemain As String
    Dim resultAbort As String
    Dim code&, db&
    Dim rc, oldAccess As Integer

'''    Set selbks = Form7.TDBGrid2.SelBookmarks
    resultSuccess = ""
    resultError = ""
    resultCancel = ""
    resultRemain = ""

    'display message box
    userAbort = False

    'code = code4init
    code = gCode4   ' use global CODE4
    If code <> 0 Then
        Call ResetCode4
        Call ErrOff
        rc = code4singleOpen(code, 0)
        oldAccess = code4accessMode(code, OPEN4DENY_RW) 'cj -26/10/99- save old access to set return
        frmProgress.Show
        frmProgress.caption = "Maintenance In Progress"

        frmProgress.MousePointer = 13
        For i = 1 To selFiles.count
            tableName = selFiles(i)
            If Not userAbort Then
                Select Case Action
                    Case Is = 0
                        frmProgress.caption = "Checking Indexes"
                    Case Is = 1
                        frmProgress.caption = "Reindexing"
                    Case Is = 2
                        frmProgress.caption = "Compressing Tables"
                    Case Is = 3
                        frmProgress.caption = "Compressing Memos"
                    Case Is = 4
                        frmProgress.caption = "Reindexing, Compressing Tables and Memo Files"
                End Select

                'open the table
                Call code4errorCode(code, 0)
                DoEvents
                db = d4open(code, tableName)
                DoEvents
                If db <> 0 Then
                    DoEvents
                    Select Case Action
                        Case 0  ' check index
                            frmProgress.lstReport.AddItem tableName + ": Check Indexes... "
                            If d4tagDefault(db) = 0 Then
                                lstAppend frmProgress.lstReport, "No Associated Indexes"
                                rc = r4success
                            Else
                                DoEvents
                                rc = d4check(db)
                                If rc = r4success Then
                                    lstAppend frmProgress.lstReport, "Success"
                                End If
                            End If
                        Case 1
                            frmProgress.lstReport.AddItem tableName + ": Reindex... "
                            If d4tagDefault(db) = 0 Then
                                lstAppend frmProgress.lstReport, "No Associated Indexes"
                                rc = r4success
                            Else
                                DoEvents
                                rc = d4reindex(db)
                                If rc = r4success Then
                                    lstAppend frmProgress.lstReport, "Success"
                                End If
                            End If
                        Case 2
                            frmProgress.lstReport.AddItem tableName + ": Table Compression/Reindex... "
                            DoEvents
                            rc = d4pack(db)
                            If rc = r4success Then
                                lstAppend frmProgress.lstReport, "Success"
                            End If
                        Case 3
                            frmProgress.lstReport.AddItem tableName + ": Memo Compression... "
                            DoEvents
                            rc = d4memoCompress(db)
                            If rc = r4success Then
                                lstAppend frmProgress.lstReport, "Success"
                            End If
                        Case 4
                            frmProgress.lstReport.AddItem tableName + ": Table Compression..."
                            DoEvents
                            rc = d4pack(db)
                            If rc = r4success Then
                                lstAppend frmProgress.lstReport, "Success"
                                frmProgress.lstReport.AddItem tableName + ": Memo Compression... "
                                DoEvents
                                rc = d4memoCompress(db)
                                If rc = r4success Then
                                    lstAppend frmProgress.lstReport, "Success"
                                    frmProgress.lstReport.AddItem tableName + ": Reindex... "
                                    If d4tagDefault(db) = 0 Then
                                        lstAppend frmProgress.lstReport, "No Associated Indexes"
                                        rc = r4success
                                    Else
                                    DoEvents
                                    rc = d4reindex(db)
                                        If rc = r4success Then
                                            lstAppend frmProgress.lstReport, "Success"
                                        End If
                                    End If
                                End If
                            End If
                    End Select

                    'check results
                    If rc <> r4success Then
                        Select Case rc
                            Case r4locked
                                lstAppend frmProgress.lstReport, "LOCKED"
                                results = tableName + ": Required lock did not succeed."
                            Case r4unique
                                lstAppend frmProgress.lstReport, "DUPLICATE KEY"
                                results = tableName + ": Duplicate key occurred in a unique tag."
                            Case e4authorize
                                lstAppend frmProgress.lstReport, "ERROR (" + Trim(str(rc)) + ")"
                                ShowError "You do not have the required privileges to perform this operation."
                            Case Else
                                lstAppend frmProgress.lstReport, "ERROR (" + Trim(str(rc)) + ")"
                                'results = tableName + ": An error occurred. (" + Trim(str(rc)) + ")"
                                ShowError
                        End Select

                        'If more tables to process, check if user wants to continue
                        If i < (selFiles.count - 1) Then
                            rc = MsgBox("Do you wish to continue processing the remainder of the selected tables?", vbYesNo + vbInformation, frmProgress.caption)
                            If rc = vbNo Then
                                userAbort = True
                                resultAbort = tableName
                            End If
                        End If
                    End If

    '                If rc <> r4success Then
    '                    If resultError = "" Then
    '                        resultError = tableName
    '                    Else
    '                        'changed by DF, was 'resultErr' before, causing undefined error
    '                        resultError = resultError & ", " & tableName
    '                    End If
    '                End If

                    Call code4errorCode(code, 0)
                    Call d4close(db)
                    DoEvents
                Else 'else the data file could not be opened
                    Select Case code4errorCode(code, r4check)
                        Case -64:
                            results = "Could not find one or more files belonging to this table."
                        Case -61, -62:
                            results = "Could not open the table exclusively."
                        Case Else:
                            results = "Could not open the table. CodeBase error " & code4errorCode(code, r4check) & "."
                    End Select
                    Call code4errorCode(code, 0)
                    frmProgress.lstReport.AddItem tableName + ": COULD NOT OPEN"
                    If resultError = "" Then
                        resultError = tableName
                    Else
                        resultError = resultError & ", " & tableName
                    End If
                    rc = MsgBox(results & vbCrLf & "Do you wish to continue?", vbYesNo, tableName)
                    If rc = vbNo Then
                        userAbort = True
                        resultAbort = tableName
                    End If
                End If
                Call code4errorCode(code, 0)
            End If 'userAbort = false
        Next i
        frmProgress.MousePointer = 0
        rc = code4accessMode(code, oldAccess)
        'cj - 26/10/99 need to set the access mode back to the default so that reports that access
        'the system server dbfs do not fail because they try to open the files exclusively
    End If 'code <> 0

    'If resultSuccess = "" And resultError = "" Then
    '    Form8.lstReport.AddItem "No tables were selected from the list."
    'ElseIf resultSuccess = "" Then
    '    Form8.lstReport.AddItem resultError & " had errors " & Form8.caption & "."
    'ElseIf resultError = "" Then
    '    Form8.lstReport.AddItem Form8.caption & " was successfully completed on " & resultSuccess & "."
    'Else
    '    Form8.lstReport.AddItem Form8.caption & " was successfully completed on " & resultSuccess & "." & vbCrLf & vbCrLf & resultError & " had errors."
    'End If

    If resultAbort <> "" Then
    '    Form8.lstReport.AddItem Form8.cbedit1.Text & vbCrLf & vbCrLf & "Cancelled after completing " & resultAbort & "."
    End If

    If resultRemain <> "" Then
    '    Form8.lstReport.AddItem Form8.cbedit1.Text & vbCrLf & vbCrLf & Form8.caption & " was not attempted on " & resultRemain & "."
    End If
    frmProgress.caption = "Maintenance Results"
    frmProgress.Command1.caption = "OK"
End Sub

Private Sub Form_Resize()
    If Width < 6000 Then Width = 6000
    If Height < 4005 Then Height = 4005

    Dim diff As Integer
    diff = Frame1.Left - cmdAdd.Left - cmdAdd.Width

    Do While diff + container.Height + diff + cmdClose + diff > Me.ScaleHeight
        Me.Height = Me.Height + 1
    Loop

    If Me.ScaleHeight < 3600 Then
        Me.ScaleHeight = 3600
        Me.Refresh
    End If


    cmdClose.Left = Me.ScaleWidth - diff - cmdClose.Width
    cmdClose.Top = Me.ScaleHeight - diff - cmdClose.Height

    container.Left = Me.ScaleWidth - diff - container.Width
    container.Top = diff

    lstTables.Left = diff
    lstTables.Top = diff
    lstTables.Width = container.Left - diff - diff
    lstTables.Height = Me.ScaleHeight - diff - diff
End Sub


Private Sub lstTables_Click()
    setButtons
End Sub
Private Sub cmdSelectAll_Click()
    'select all the rows in the list
    Dim i%
    For i = 0 To (lstTables.ListCount - 1)
        lstTables.Selected(i) = True
    Next i
    setButtons
End Sub



