VERSION 5.00
Object = "{3A0EBF03-62A1-101C-A69C-040224009C02}#1.0#0"; "MTR4.OCX"
Object = "{564BFFC3-B5F3-101C-A2C8-A27C829646CD}#1.0#0"; "EDT4.OCX"
Object = "{BB149643-466A-11CF-9C13-0020AF0783D2}#1.0#0"; "CMB4.OCX"
Object = "{0BA686C6-F7D3-101A-993E-0000C0EF6F5E}#1.0#0"; "THREED32.OCX"
Begin VB.Form Form5
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Privileges"
   ClientHeight    =   5085
   ClientLeft      =   1185
   ClientTop       =   1980
   ClientWidth     =   7110
   Icon            =   "supriv4.frx":0000
   LinkTopic       =   "Form5"
   MaxButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   5085
   ScaleWidth      =   7110
   ShowInTaskbar   =   0   'False
   Begin VB.CommandButton cmdBrowse
      Caption         =   "..."
      Height          =   315
      Left            =   5546
      TabIndex        =   31
      Top             =   672
      Width           =   300
   End
   Begin VB.CommandButton cmdGetTables
      Caption         =   "&Generate Table List"
      Height          =   345
      Left            =   975
      TabIndex        =   30
      Top             =   3165
      Width           =   2640
   End
   Begin VB.CommandButton cmdServerPath
      Caption         =   "&Server Path"
      Height          =   315
      Left            =   5895
      TabIndex        =   29
      Top             =   672
      Width           =   1005
   End
   Begin VB.Frame Frame2
      Caption         =   "Default Table-level Priveleges"
      Height          =   1140
      Left            =   3900
      TabIndex        =   23
      Top             =   1110
      Width           =   3000
      Begin Edt4Lib.CbEdit Index
         Height          =   315
         Left            =   2340
         TabIndex        =   3
         ToolTipText     =   "Determines if the account can reindex the selected table"
         Top             =   270
         Width           =   375
         DataField       =   "INDEX"
         DataSource      =   "CbMaster1"
         FormatType      =   2
         Mask            =   "L"
         PromptChar      =   " "
         _Version        =   65536
         _ExtentX        =   661
         _ExtentY        =   556
         _StockProps     =   125
         Text            =   " "
         BackColor       =   -2147483643
         Text            =   " "
         ContainerID     =   24527900
      End
      Begin Edt4Lib.CbEdit Compress
         Height          =   315
         Left            =   2340
         TabIndex        =   4
         ToolTipText     =   "Determines if the account can compress the selected table's memo file, if one exists"
         Top             =   720
         Width           =   375
         DataField       =   "COMPRESS"
         DataSource      =   "CbMaster1"
         FormatType      =   2
         Mask            =   "L"
         PromptChar      =   " "
         _Version        =   65536
         _ExtentX        =   661
         _ExtentY        =   556
         _StockProps     =   125
         Text            =   " "
         BackColor       =   -2147483643
         Text            =   " "
         ContainerID     =   24527900
      End
      Begin VB.Label Label13
         Caption         =   "Create/Modify Indexes:"
         Height          =   240
         Left            =   270
         TabIndex        =   25
         Top             =   330
         Width           =   1965
      End
      Begin VB.Label Label14
         Caption         =   "Compress/Reindex:"
         Height          =   240
         Left            =   270
         TabIndex        =   24
         Top             =   765
         Width           =   2040
      End
   End
   Begin VB.CommandButton Command3
      Caption         =   "Cancel"
      Height          =   345
      Left            =   5910
      TabIndex        =   11
      Top             =   4230
      Width           =   1000
   End
   Begin VB.CommandButton Command2
      Caption         =   "OK"
      Default         =   -1  'True
      Height          =   345
      Left            =   5910
      TabIndex        =   10
      Top             =   3795
      Width           =   1000
   End
   Begin VB.Frame Frame1
      Caption         =   "Default Record-level Priveleges"
      Height          =   1200
      Left            =   3900
      TabIndex        =   16
      Top             =   2370
      Width           =   3015
      Begin Edt4Lib.CbEdit Update
         Height          =   315
         Left            =   2340
         TabIndex        =   8
         ToolTipText     =   "Determines if the account can modify existing records in the selected table"
         Top             =   735
         Width           =   375
         DataField       =   "UPDATE"
         DataSource      =   "CbMaster1"
         FormatType      =   2
         Mask            =   "L"
         PromptChar      =   " "
         _Version        =   65536
         _ExtentX        =   661
         _ExtentY        =   556
         _StockProps     =   125
         Text            =   " "
         BackColor       =   -2147483643
         Text            =   " "
         ContainerID     =   24527900
      End
      Begin Edt4Lib.CbEdit Delete
         Height          =   315
         Left            =   2340
         TabIndex        =   7
         ToolTipText     =   "Determines if the account can delete records in the selected table"
         Top             =   285
         Width           =   375
         DataField       =   "DELETE"
         DataSource      =   "CbMaster1"
         FormatType      =   2
         Mask            =   "L"
         PromptChar      =   " "
         _Version        =   65536
         _ExtentX        =   661
         _ExtentY        =   556
         _StockProps     =   125
         Text            =   " "
         BackColor       =   -2147483643
         Text            =   " "
         ContainerID     =   24527900
      End
      Begin Edt4Lib.CbEdit Append
         Height          =   315
         Left            =   945
         TabIndex        =   6
         ToolTipText     =   "Determines if the account can append new records to the selected table"
         Top             =   735
         Width           =   375
         DataField       =   "APPEND"
         DataSource      =   "CbMaster1"
         FormatType      =   2
         Mask            =   "L"
         PromptChar      =   " "
         _Version        =   65536
         _ExtentX        =   661
         _ExtentY        =   556
         _StockProps     =   125
         Text            =   " "
         BackColor       =   -2147483643
         Text            =   " "
         ContainerID     =   24527900
      End
      Begin Edt4Lib.CbEdit Read
         Height          =   315
         Left            =   945
         TabIndex        =   5
         ToolTipText     =   "Determines if the account can read the selected table"
         Top             =   285
         Width           =   375
         DataField       =   "READ"
         DataSource      =   "CbMaster1"
         FormatType      =   2
         Mask            =   "L"
         PromptChar      =   " "
         _Version        =   65536
         _ExtentX        =   661
         _ExtentY        =   556
         _StockProps     =   125
         Text            =   " "
         BackColor       =   -2147483643
         Text            =   " "
         ContainerID     =   24527900
      End
      Begin VB.Label Label12
         Caption         =   "Update:"
         Height          =   240
         Left            =   1680
         TabIndex        =   20
         Top             =   765
         Width           =   645
      End
      Begin VB.Label Label11
         Caption         =   "Delete:"
         Height          =   240
         Left            =   1680
         TabIndex        =   19
         Top             =   330
         Width           =   630
      End
      Begin VB.Label Label10
         Caption         =   "Append:"
         Height          =   240
         Left            =   270
         TabIndex        =   18
         Top             =   765
         Width           =   705
      End
      Begin VB.Label Label9
         Caption         =   "Read:"
         Height          =   240
         Left            =   270
         TabIndex        =   17
         Top             =   330
         Width           =   615
      End
   End
   Begin Threed.SSPanel panelRecno
      Height          =   255
      Left            =   2160
      TabIndex        =   22
      Top             =   4800
      Width           =   2430
      _Version        =   65536
      _ExtentX        =   4276
      _ExtentY        =   445
      _StockProps     =   15
      BackColor       =   12632256
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851}
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BevelOuter      =   1
      Alignment       =   1
   End
   Begin Threed.SSPanel panelStatus
      Height          =   255
      Left            =   210
      TabIndex        =   26
      Top             =   4800
      Width           =   1950
      _Version        =   65536
      _ExtentX        =   3440
      _ExtentY        =   450
      _StockProps     =   15
      BackColor       =   12632256
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851}
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BevelOuter      =   1
      Alignment       =   1
      Begin VB.Label Label8
         Caption         =   "Status:"
         Height          =   180
         Left            =   75
         TabIndex        =   27
         Top             =   15
         Width           =   525
      End
   End
   Begin Threed.SSPanel panelReccount
      Height          =   255
      Left            =   4590
      TabIndex        =   28
      Top             =   4800
      Width           =   2325
      _Version        =   65536
      _ExtentX        =   4101
      _ExtentY        =   450
      _StockProps     =   15
      BackColor       =   12632256
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851}
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BevelOuter      =   1
      Alignment       =   1
   End
   Begin Cmb4Lib.CbCombo cmbTable
      Height          =   1890
      Left            =   975
      TabIndex        =   2
      Top             =   1200
      Width           =   2640
      DataFieldEdit   =   "TABLE"
      DataSourceEdit  =   "CbMaster1"
      Style           =   1
      _Version        =   65536
      _ExtentX        =   4657
      _ExtentY        =   3334
      _StockProps     =   221
      Text            =   "cmbTable"
      BackColor       =   -2147483643
      Text            =   "cmbTable"
      ContainerID     =   24527900
   End
   Begin Mtr4LibCtl.CbMaster CbMaster3
      Height          =   360
      Left            =   0
      TabIndex        =   21
      TabStop         =   0   'False
      Top             =   2640
      Width           =   975
      _Version        =   65536
      _ExtentX        =   1720
      _ExtentY        =   635
      _StockProps     =   111
      Caption         =   "CbMaster3"
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851}
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      AppendVisible   =   0   'False
      BottomVisible   =   0   'False
      DataTag         =   ""
      DeletedVisible  =   0   'False
      FlushVisible    =   0   'False
      NextPageVisible =   0   'False
      NextVisible     =   0   'False
      PackVisible     =   0   'False
      PreviousPageVisible=   0   'False
      PreviousVisible =   0   'False
      ReadOnly        =   -1  'True
      RecNoEnabled    =   0   'False
      RefreshVisible  =   0   'False
      ReindexVisible  =   0   'False
      SearchVisible   =   0   'False
      SliderEnabled   =   0   'False
      TagListEnabled  =   0   'False
      TopVisible      =   0   'False
      UndoVisible     =   0   'False
      Visibility      =   0   'False
      ContainerID     =   24527900
      NameUnique      =   "CbMaster3"
   End
   Begin Cmb4Lib.CbCombo cmbAcctId
      Height          =   315
      Left            =   975
      TabIndex        =   0
      ToolTipText     =   "List of Valid Account IDs"
      Top             =   150
      Width           =   3315
      DataFieldEdit   =   "ACCOUNTID"
      DataSourceEdit  =   "CbMaster1"
      DataFieldExprList=   "ACCOUNTID"
      DataSourceList  =   "CbMaster3"
      DataTagList     =   "NAME"
      Style           =   2
      _Version        =   65536
      _ExtentX        =   5847
      _ExtentY        =   556
      _StockProps     =   221
      Text            =   "cmbAcctId"
      BackColor       =   -2147483643
      ContainerID     =   24527900
   End
   Begin Mtr4LibCtl.CbMaster CbMaster1
      Height          =   780
      Left            =   210
      TabIndex        =   9
      Top             =   3795
      Width           =   5175
      _Version        =   65536
      _ExtentX        =   9128
      _ExtentY        =   1376
      _StockProps     =   111
      Caption         =   "Privileges"
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851}
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      AppendRecordType=   1
      CaptionVisible  =   0   'False
      DataTag         =   ""
      PackVisible     =   0   'False
      PackEnabled     =   0   'False
      ProcessId       =   ""
      RecNoEnabled    =   0   'False
      ReindexVisible  =   0   'False
      ReindexEnabled  =   0   'False
      ServerId        =   ""
      TagListEnabled  =   0   'False
      UserName        =   ""
      Unique          =   1
      ContainerID     =   24527900
      NameUnique      =   "CbMaster1"
   End
   Begin Edt4Lib.CbEdit Path
      Height          =   312
      Left            =   972
      TabIndex        =   1
      ToolTipText     =   "Current path of tables shown in Table List"
      Top             =   672
      Width           =   4524
      DataField       =   "PATH"
      DataSource      =   "CbMaster1"
      _Version        =   65536
      _ExtentX        =   7980
      _ExtentY        =   550
      _StockProps     =   125
      Text            =   "Path"
      BackColor       =   -2147483643
      ContainerID     =   24527900
   End
   Begin VB.Label Label4
      Caption         =   "Table List:"
      Height          =   225
      Left            =   90
      TabIndex        =   15
      Top             =   1725
      Width           =   900
   End
   Begin VB.Label Label3
      Caption         =   "Path:"
      Height          =   225
      Left            =   90
      TabIndex        =   14
      Top             =   720
      Width           =   525
   End
   Begin VB.Label Label2
      Caption         =   "Table:"
      Height          =   225
      Left            =   90
      TabIndex        =   13
      Top             =   1215
      Width           =   675
   End
   Begin VB.Label Label1
      Caption         =   "Account Id:"
      Height          =   225
      Left            =   90
      TabIndex        =   12
      Top             =   210
      Width           =   900
   End
End
Attribute VB_Name = "Form5"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Private browseDir As String
Private bFirstRecord As Boolean
Private masterAction%, prevRecno&, currentRecno&
Private delCount&
Private bIsAppend As Boolean
Private bIsDupeKey As Boolean
Private bCmbTableChanged As Boolean
Sub CheckTableExt()

    'ensure table has .dbf extension if no extension was specified
    'otherwise we could end up with two logically identical records,
    'one with an extension, and one without
    If bCmbTableChanged Then
        If Len(Trim$(cmbTable.Text)) <> 0 And InStr(cmbTable.Text, ".") = 0 Then
            cmbTable.Text = Trim$(cmbTable.Text) & ".dbf"
        End If
        bCmbTableChanged = False
    End If

End Sub

Private Sub Append_GotFocus()
    If Not bMouseDown Then SelIt Append
End Sub
Private Sub Append_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
    bMouseDown = True
End Sub
Private Sub Append_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
    bMouseDown = False
End Sub


Private Sub CbMaster1_ButtonClick(Action As Integer)

    masterAction = Action
    Dim currentCnt&

    Select Case Action
        Case 0 To 6
            prevRecno = CbMaster1.recNo
            If Action = 3 Then
                Action = 0      'don't allow default search dialog to appear
                ShowSearchDlg CbMaster1, Me
            End If
        Case 8     ' delete
            If CbMaster1.Deleted = False Then
                delCount = delCount + 1
            Else
                delCount = delCount - 1
            End If
            currentCnt = d4recCount(CbMaster1.DATA4) - delCount
            If currentCnt < 0 Then
               panelReccount = " Total Records: Undetermined"
            Else
               panelReccount = " Total Records: " & Format(currentCnt, "###,##0")
            End If

            'If CbMaster1.Deleted = False Then
            '    Dim mb As Integer
            '    mb = MsgBox("Are you sure you want to permanently remove " _
            '              + "the privelages for this file?", _
            '                vbExclamation + vbOKCancel, "Delete Privelages")
            '    If mb = vbCancel Then
            '        Action = 0
            '    End If
            'End If
    End Select
End Sub
Private Sub CbMaster1_DatafileOpened(ByVal DatafileOpened As String, Response As Integer)


    If d4top(CbMaster1.DATA4) = r4eof Then
        Dim rc As Integer
        rc = MsgBox("The Privileges database contains no records." + vbCrLf _
                  + "Do you wish to add a new record?", vbYesNo + vbInformation, _
                    "Privilege Database empty")
        If rc = vbNo Then
            Unload Me
        Else
            Response = 0
            bFirstRecord = True
            CbMaster1.Append
            delCount = GetDelCount(CbMaster1.DATA4)
            panelReccount = " Total Records: " & Format(d4recCount(CbMaster1.DATA4) - delCount, "###,##0")
        End If
    Else
        delCount = GetDelCount(CbMaster1.DATA4)
        panelReccount = " Total Records: " & Format(d4recCount(CbMaster1.DATA4) - delCount, "###,##0")
    End If

End Sub

Private Sub CbMaster1_DuplicateKey(ByVal Action As Integer, Response As Integer)

    If bIsAppend Then
        MsgBox "You have attempted to add a privilege that already exists." & vbCrLf & vbCrLf & "Each privileges must be a unique combination of the Acccount Id, Path, and Table fields. " & _
        "Please modify your changes and try to save the record again, or press the Navigator Undo button or the Cancel button to cancel your changes.", vbExclamation, "Duplicate Entry Error"
    Else
        MsgBox "You have changed the current privilege, so that it now is a duplicate of an existing privilege." & vbCrLf & vbCrLf & "Each privilege must be a unique combination of the Acccount Id, Path, and Table fields. " & _
        "Please modify your changes and try to save the record again, or press the Navigator Undo button or the Cancel button to cancel your changes.", vbExclamation, "Duplicate Entry Error"
    End If

    Response = 2
    bIsDupeKey = True

End Sub

Private Sub CbMaster1_Error(ErrorCode As Long, Description As String, Response As Integer)


FrmDbErr5 = True

MasterErr CbMaster1, Me, PRIVILEGE, ErrorCode, Description
Response = 0

If ErrorCode = -10 Then
    Unload Me
ElseIf ErrorCode = -20 Or ErrorCode = -30 Then
    CbMaster1.SearchEnabled = False
End If

'   If ErrorCode = -10 Then
'        'could not open the data file
'        MsgBox "Could not open the ACCOUNT4 file" & Chr(13) & Chr(10) & "Ensure that your account has the necessary authorization to open this file", vbOKOnly + vbExclamation, "Error Opening File"
'        Response = 0

End Sub
Private Sub CbMaster1_Reposition(ByVal Action As Integer)


    Dim statusMsg$, recnoMsg$
    Dim statusForeColor&
    Dim recCount&

    bIsAppend = False

    currentRecno = CbMaster1.recNo
    'defaults
    statusMsg = " Status:  Browse"
    recnoMsg = " Current Record No: " & Format(currentRecno, "###,###,##0")
    statusForeColor = vbButtonText

    Select Case Action
    Case 0 To 2  'moving backwards in table
        If prevRecno = currentRecno Then statusMsg = " Status:  Top of File"

    Case 4 To 6  'moving forwards in table
        If prevRecno = currentRecno Then statusMsg = " Status:  End of File"

    Case 7  'the append button or method called

        '***** NOTE *******'
        'The panelStatus control has a label positioned directly on top
        'of the panel text, that matches the first part of the panel
        'string, namely the word 'Status'. This is so I can make the
        'status 'Append' appear red, without the word 'Status' also
        'becoming red - DF

        statusForeColor = &HFF
        statusMsg = " Status:  Append"
        recnoMsg = " Current Record Number:  --"
        bIsAppend = True
        With CbMaster1
            If bFirstRecord Then
                bFirstRecord = False
                .SetField "APPEND", "F"
                .SetField "COMPRESS", "F"
                .SetField "DELETE", "F"
                .SetField "INDEX", "F"
                .SetField "READ", "F"
                .SetField "UPDATE", "F"
                .SetField "PATH", defPath
                cmbAcctId.ListIndex = 0
                If bHostIsLocal Then
                    RegenerateTables
                End If
            End If
            .SetField "TABLE", ""
        End With
        cmbTable.SetFocus

    Case 12
        'update record count if flushing (may be append)
        recCount = d4recCount(CbMaster1.DATA4)
        If recCount < 0 Then
           'In case of error
           panelReccount = " Total Records: Undetermined"
        Else
           panelReccount = " Total Records: " & Format(d4recCount(CbMaster1.DATA4) - delCount, "###,##0")
        End If

        'panelStatus.ForeColor = vbButtonText
        'PanelStatus = " Status:  Browse"
        'panelRecno = " Current Record No: " & Format(str$(CbMaster1.recNo), "###,###,###")

    End Select

    panelStatus.ForeColor = statusForeColor
    panelStatus = statusMsg
    panelRecno = recnoMsg

End Sub
Private Sub CbMaster1_Validate(ByVal Action As Integer, Response As Integer)

    If CbMaster1.RecordChanged Then
        'if they didn't use 'Save' button to save changes,
        'validate that they want to change record
        If Action <> 12 Then
            Dim rc%

            If Len(Trim(cmbTable.Text)) = 0 Then
                'If it's a Path privilege, don't mention table names
                rc = MsgBox("Save privilege changes for account: " & Trim$(cmbAcctId.Text) & " ?", vbYesNoCancel + vbQuestion, App.title)
            Else
                rc = MsgBox("Save privilege changes for table: " & UCase$(Trim$(cmbTable.Text)) & " for account: " & Trim$(cmbAcctId.Text) & " ?", vbYesNoCancel + vbQuestion, App.title)
            End If

            If rc <> vbYes Then
                'Don't save changes, just move to specified record
                If rc = vbNo Then
                    CbMaster1.Undo
                    Exit Sub
                ElseIf rc = vbCancel Then
                    'Stay where we are, leave current changes intact
                    Response = 0
                    Exit Sub
                End If
            End If
        End If

        ' strip backslach(es) from path
        Dim path As Variant
        path = "a"
        CbMaster1.GetField "PATH", path
        path = Trim(path)
        Do While Right(path, 1) = SepChar
            path = Left(path, Len(path) - 1)
        Loop
        If Right(path, 1) = ":" Then  ' path to root needs backslash
            path = path + SepChar
        End If


        'convert blank permission fields to false
        If Read.Text = " " Then
            Call CbMaster1.SetField("READ", False)
        End If

        If Append.Text = " " Then
            Call CbMaster1.SetField("APPEND", False)
        End If

        If Delete.Text = " " Then
            Call CbMaster1.SetField("DELETE", False)
        End If

        If Update.Text = " " Then
            Call CbMaster1.SetField("UPDATE", False)
        End If

        If Compress.Text = " " Then
            Call CbMaster1.SetField("COMPRESS", False)
        End If

        If Index.Text = " " Then
            Call CbMaster1.SetField("INDEX", False)
        End If

    End If

End Sub

Private Sub CbMaster1_Validation(ByVal Action As Integer, Response As Integer)
    If Right(Trim(path.Text), 1) <> SepChar Then
        path.Text = Trim(path.Text) + SepChar
    End If
End Sub

Private Sub cmbTable_Change()

    bCmbTableChanged = True

End Sub

Private Sub cmbTable_KeyPress(KeyAscii As Integer)
   KeyAscii = UpperKey(KeyAscii)
End Sub

Private Sub cmbTable_LostFocus()

    'ensure table has .dbf if not extension specified
    CheckTableExt

End Sub

Private Sub cmdBrowse_Click()
    Form23.Show 1
    If browseDlgPath <> "" Then
        path.Text = browseDlgPath
        If bHostIsLocal Then
            RegenerateTables
        End If
    End If
End Sub

Private Sub cmdGetTables_Click()
    Dim currentTable$
    currentTable = cmbTable.Text
    RegenerateTables
    'cmbTable.Text = currentTable
End Sub

Private Sub cmdServerPath_Click()
    Call ResetCode4
    Call ErrOff
    path.Text = code4serverCurrentDirectory(gCode4)
    If bHostIsLocal Then
        RegenerateTables
    End If
End Sub

Private Sub Command2_Click()

    'ensure table has .dbf if no extension specified.
    'This code is also in the LostFocus event of cmbTable,
    'but if user hits <return> key, the LostFocus event
    'isn't called, so we must check it here as well
    CheckTableExt

    Unload Me
End Sub
Private Sub Command3_Click()
    CbMaster1.Undo
    Unload Me
End Sub
Private Sub Compress_GotFocus()
    If Not bMouseDown Then SelIt Compress
End Sub
Private Sub Compress_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
    bMouseDown = True
End Sub
Private Sub Compress_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
    bMouseDown = False
End Sub
Private Sub Delete_GotFocus()
    If Not bMouseDown Then SelIt Delete
End Sub
Private Sub Delete_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
    bMouseDown = True
End Sub
Private Sub Delete_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
    bMouseDown = False
End Sub
Private Sub Form_Load()
    HelpContextID = File_Privileges

    Left = (Screen.Width - Width) / 2   ' Center form horizontally.
    Top = (Screen.Height - Height) / 2  ' Center form vertically.

    srchDbfName = systemPath & "PRIV4.dbf"
    srchIndxName = systemPath & "PRIV4"

    bFirstRecord = False

    'open the PRIV4.dbf file with the master,
    'set Cbmaster DatabaseName, Indexfiles, DataTag
    With CbMaster1
        .serverId = svrId
        .processId = portId
        .password = passwrd
        .userName = accountId
        .DatabaseName = srchDbfName
        .IndexFiles = srchIndxName
        .DataTag = "PRIVILEG"
        .RefreshFiles
    End With

    With CbMaster3
        .serverId = svrId
        .processId = portId
        .password = passwrd
        .userName = accountId
        .DatabaseName = systemPath + "ACCOUNT4"
        .IndexFiles = systemPath & "ACCOUNT4"
        .DataTag = "NAMEPASS"
        .RefreshFiles
    End With
End Sub
Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
    Dim rc%

    If CbMaster1.RecordChanged Then
        'Flush record explicitly, otherwise our DuplicateKey code
        'will not be called
        CbMaster1.FlushRecord

        'if dupe record, cancel form close
        If bIsDupeKey Then
           Cancel = True
           bIsDupeKey = False

           'we have to simulate a change to the data file to
           'reset the .RecordChanged flag because calling
           'the FlushRecord method sets it to false, even though
           'the current record still hasn't been flushed. Might
           'be a CC3 bug.
           path.Text = path.Text & ""
        End If
    End If
End Sub
Private Sub Form_Unload(Cancel As Integer)
    FrmDbErr5 = False
End Sub
Private Sub Index_GotFocus()
    If Not bMouseDown Then SelIt Index
End Sub
Private Sub Index_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
    bMouseDown = True
End Sub
Private Sub Index_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
    bMouseDown = False
End Sub

Private Sub path_GotFocus()
    If Not bMouseDown Then SelIt path
End Sub

Private Sub path_KeyPress(KeyAscii As Integer)
   ' convert to uppercase
   KeyAscii = UpperKey(KeyAscii)
End Sub
Private Sub path_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
    bMouseDown = True
End Sub
Private Sub path_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
    bMouseDown = False
End Sub
Private Sub Read_GotFocus()
    If Not bMouseDown Then SelIt Read
End Sub
Private Sub Read_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
    bMouseDown = True
End Sub
Private Sub Read_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
    bMouseDown = False
End Sub
Public Sub RegenerateTables()
'get path information so update the TABLES5.dbf which
'lists the available tables in the directory
'the tables are listed in the list portion of the
'combo
    Dim table As Long
    table = code4directory(gCode4, Trim(path.Text))
    If table = 0 Then
        If code4errorCode(gCode4, r4check) = e4authorize Then
            cmbTable.Enabled = False
            Call code4errorCode(gCode4, 0)
        Else
            ShowError
        End If
    Else
        Dim rc As Integer
        Dim TagName$, tag&
        'tagName = "FILENAME"   ' this may change
        'tag = d4tag(table, tagName)
        'If tag = 0 Then
        '    ShowError Space(5) + "Tag " + tagName + " not found. Using default."
            tag = d4tagDefault(table)
        'End If
        Call d4tagSelect(table, tag)

        Dim fileName As Long
        fileName = d4field(table, "FILE_NAME")
        cmbTable.Clear
        rc = d4top(table)
        Do While rc = r4success
            cmbTable.AddItem Trim(f4str(fileName)), -1
            rc = d4skip(table, 1)
        Loop
        cmbTable.Refresh
    End If
End Sub

Private Sub cmbTable_GotFocus()
    If Not bMouseDown Then SelIt cmbTable
End Sub
Private Sub cmbTable_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
    bMouseDown = True
End Sub
Private Sub cmbTable_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
    bMouseDown = False
End Sub

Private Sub Update_GotFocus()
    If Not bMouseDown Then SelIt Update
End Sub
Private Sub Update_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
    bMouseDown = True
End Sub
Private Sub Update_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
    bMouseDown = False
End Sub
