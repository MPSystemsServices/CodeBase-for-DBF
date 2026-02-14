VERSION 5.00
Object = "{3A0EBF03-62A1-101C-A69C-040224009C02}#1.0#0"; "MTR4.OCX"
Object = "{564BFFC3-B5F3-101C-A2C8-A27C829646CD}#1.0#0"; "EDT4.OCX"
Object = "{0BA686C6-F7D3-101A-993E-0000C0EF6F5E}#1.0#0"; "THREED32.OCX"
Begin VB.Form Form4
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Accounts"
   ClientHeight    =   4905
   ClientLeft      =   1005
   ClientTop       =   855
   ClientWidth     =   6825
   Icon            =   "suacct4.frx":0000
   LinkTopic       =   "Form4"
   LockControls    =   -1  'True
   MaxButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   4905
   ScaleWidth      =   6825
   ShowInTaskbar   =   0   'False
   Begin VB.Frame Frame4
      Caption         =   "Administration Permissions"
      Height          =   780
      Left            =   3885
      TabIndex        =   31
      Top             =   75
      Width           =   2790
      Begin Edt4Lib.CbEdit Disconnect
         Height          =   315
         Left            =   2175
         TabIndex        =   8
         ToolTipText     =   "Determines if the account can access server shutdown options in this program"
         Top             =   300
         Width           =   465
         DataField       =   "DISCONNECT"
         DataSource      =   "CbMaster1"
         FormatType      =   2
         Mask            =   "L"
         PromptChar      =   " "
         _Version        =   65536
         _ExtentX        =   820
         _ExtentY        =   556
         _StockProps     =   125
         Text            =   " "
         BackColor       =   -2147483643
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851}
            Name            =   "MS Sans Serif"
            Size            =   8.26
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Text            =   " "
         ContainerID     =   24518896
      End
      Begin VB.Label Label15
         Caption         =   "Disconnect Permission:"
         Height          =   240
         Left            =   105
         TabIndex        =   32
         Top             =   360
         Width           =   1875
      End
   End
   Begin VB.CommandButton Command2
      Caption         =   "Cancel"
      Height          =   345
      Left            =   5595
      TabIndex        =   16
      Top             =   4020
      Width           =   1065
   End
   Begin VB.CommandButton Command1
      Caption         =   "OK"
      Default         =   -1  'True
      Height          =   345
      Left            =   5595
      TabIndex        =   15
      Top             =   3585
      Width           =   1065
   End
   Begin VB.Frame Frame3
      Caption         =   "Default Table-level Privileges"
      Height          =   2370
      Left            =   3885
      TabIndex        =   27
      Top             =   1020
      Width           =   2790
      Begin Edt4Lib.CbEdit CreateDel
         Height          =   315
         Left            =   2190
         TabIndex        =   12
         ToolTipText     =   "Determines if the account can create tables and indexes that overwrite existing versions on the server"
         Top             =   1500
         Width           =   465
         DataField       =   "CREATE_DEL"
         DataSource      =   "CbMaster1"
         FormatType      =   2
         Mask            =   "L"
         PromptChar      =   " "
         _Version        =   65536
         _ExtentX        =   820
         _ExtentY        =   556
         _StockProps     =   125
         Text            =   " "
         BackColor       =   -2147483643
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851}
            Name            =   "MS Sans Serif"
            Size            =   8.26
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Text            =   " "
         ContainerID     =   24518896
      End
      Begin Edt4Lib.CbEdit Compress
         Height          =   315
         Left            =   2190
         TabIndex        =   13
         ToolTipText     =   "Determines if the account can compress memo files used by the server"
         Top             =   1890
         Width           =   465
         DataField       =   "COMPRESS"
         DataSource      =   "CbMaster1"
         FormatType      =   2
         Mask            =   "L"
         PromptChar      =   " "
         _Version        =   65536
         _ExtentX        =   820
         _ExtentY        =   556
         _StockProps     =   125
         Text            =   " "
         BackColor       =   -2147483643
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851}
            Name            =   "MS Sans Serif"
            Size            =   8.26
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Text            =   " "
         ContainerID     =   24518896
      End
      Begin Edt4Lib.CbEdit Index
         Height          =   315
         Left            =   2190
         TabIndex        =   11
         ToolTipText     =   "Determines if the account can  reindex server tables"
         Top             =   1110
         Width           =   465
         DataField       =   "INDEX"
         DataSource      =   "CbMaster1"
         FormatType      =   2
         Mask            =   "L"
         PromptChar      =   " "
         _Version        =   65536
         _ExtentX        =   820
         _ExtentY        =   556
         _StockProps     =   125
         Text            =   " "
         BackColor       =   -2147483643
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851}
            Name            =   "MS Sans Serif"
            Size            =   8.26
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Text            =   " "
         ContainerID     =   24518896
      End
      Begin Edt4Lib.CbEdit Create
         Height          =   315
         Left            =   2190
         TabIndex        =   9
         ToolTipText     =   "Determines if the account can create permanent data and index files"
         Top             =   330
         Width           =   465
         DataField       =   "CREATE"
         DataSource      =   "CbMaster1"
         FormatType      =   2
         Mask            =   "L"
         PromptChar      =   " "
         _Version        =   65536
         _ExtentX        =   820
         _ExtentY        =   556
         _StockProps     =   125
         Text            =   " "
         BackColor       =   -2147483643
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851}
            Name            =   "MS Sans Serif"
            Size            =   8.26
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Text            =   " "
         ContainerID     =   24518896
      End
      Begin Edt4Lib.CbEdit CreateTmp
         Height          =   315
         Left            =   2190
         TabIndex        =   10
         ToolTipText     =   "Determines if the account can create temporary data and index files"
         Top             =   720
         Width           =   465
         DataField       =   "CREATE_TMP"
         DataSource      =   "CbMaster1"
         FormatType      =   2
         Mask            =   "L"
         PromptChar      =   " "
         _Version        =   65536
         _ExtentX        =   820
         _ExtentY        =   556
         _StockProps     =   125
         Text            =   " "
         BackColor       =   -2147483643
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851}
            Name            =   "MS Sans Serif"
            Size            =   8.26
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Text            =   " "
         ContainerID     =   24518896
      End
      Begin VB.Label Label5
         Caption         =   "Remove/Overwrite Existing:"
         Height          =   240
         Left            =   120
         TabIndex        =   34
         Top             =   1530
         Width           =   2010
      End
      Begin VB.Label Label14
         Caption         =   "Compress/Reindex:"
         Height          =   240
         Left            =   120
         TabIndex        =   33
         Top             =   1920
         Width           =   1965
      End
      Begin VB.Label Label13
         Caption         =   "Create/Modify Indexes:"
         Height          =   240
         Left            =   120
         TabIndex        =   30
         Top             =   1140
         Width           =   1815
      End
      Begin VB.Label Label6
         Caption         =   "Create Permanent:"
         Height          =   240
         Left            =   120
         TabIndex        =   29
         Top             =   360
         Width           =   1965
      End
      Begin VB.Label Label7
         Caption         =   "Create Temporary:"
         Height          =   240
         Left            =   120
         TabIndex        =   28
         Top             =   750
         Width           =   1980
      End
   End
   Begin VB.Frame Frame2
      Caption         =   "Default Record-level Privileges"
      Height          =   1215
      Left            =   135
      TabIndex        =   17
      Top             =   2190
      Width           =   3540
      Begin Edt4Lib.CbEdit Read
         Height          =   315
         Left            =   1125
         TabIndex        =   4
         ToolTipText     =   "Determines if the account can read data files opened by the server. Valid entries are T or F"
         Top             =   300
         Width           =   420
         DataField       =   "READ"
         DataSource      =   "CbMaster1"
         FormatType      =   2
         Mask            =   "L"
         PromptChar      =   " "
         _Version        =   65536
         _ExtentX        =   741
         _ExtentY        =   556
         _StockProps     =   125
         Text            =   " "
         BackColor       =   -2147483643
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851}
            Name            =   "MS Sans Serif"
            Size            =   8.26
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Text            =   " "
         ContainerID     =   24518896
      End
      Begin Edt4Lib.CbEdit Append
         Height          =   315
         Left            =   1125
         TabIndex        =   5
         ToolTipText     =   "Determines if the account can append new records to data files used by the server. Valid entries are T or F"
         Top             =   750
         Width           =   420
         DataField       =   "APPEND"
         DataSource      =   "CbMaster1"
         FormatType      =   2
         Mask            =   "L"
         PromptChar      =   " "
         _Version        =   65536
         _ExtentX        =   741
         _ExtentY        =   556
         _StockProps     =   125
         Text            =   " "
         BackColor       =   -2147483643
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851}
            Name            =   "MS Sans Serif"
            Size            =   8.26
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Text            =   " "
         ContainerID     =   24518896
      End
      Begin Edt4Lib.CbEdit Delete
         Height          =   315
         Left            =   2730
         TabIndex        =   6
         ToolTipText     =   "Determines if the account can delete records in server data files. Valid entries are T or F"
         Top             =   315
         Width           =   420
         DataField       =   "DELETE"
         DataSource      =   "CbMaster1"
         FormatType      =   2
         Mask            =   "L"
         PromptChar      =   " "
         _Version        =   65536
         _ExtentX        =   741
         _ExtentY        =   556
         _StockProps     =   125
         Text            =   " "
         BackColor       =   -2147483643
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851}
            Name            =   "MS Sans Serif"
            Size            =   8.26
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Text            =   " "
         ContainerID     =   24518896
      End
      Begin Edt4Lib.CbEdit Update
         Height          =   315
         Left            =   2730
         TabIndex        =   7
         ToolTipText     =   "Determines if the account can modify existing records in server data files. Valid entries are T or F"
         Top             =   750
         Width           =   420
         DataField       =   "UPDATE"
         DataSource      =   "CbMaster1"
         FormatType      =   2
         Mask            =   "L"
         PromptChar      =   " "
         _Version        =   65536
         _ExtentX        =   741
         _ExtentY        =   556
         _StockProps     =   125
         Text            =   " "
         BackColor       =   -2147483643
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851}
            Name            =   "MS Sans Serif"
            Size            =   8.26
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Text            =   " "
         ContainerID     =   24518896
      End
      Begin VB.Label Label9
         Caption         =   "Read:"
         Height          =   240
         Left            =   390
         TabIndex        =   26
         Top             =   345
         Width           =   690
      End
      Begin VB.Label Label10
         Caption         =   "Append:"
         Height          =   240
         Left            =   405
         TabIndex        =   25
         Top             =   780
         Width           =   705
      End
      Begin VB.Label Label11
         Caption         =   "Delete:"
         Height          =   240
         Left            =   2040
         TabIndex        =   24
         Top             =   345
         Width           =   600
      End
      Begin VB.Label Label12
         Caption         =   "Update:"
         Height          =   240
         Left            =   2025
         TabIndex        =   23
         Top             =   780
         Width           =   615
      End
   End
   Begin VB.Frame Frame1
      Caption         =   "Login Information"
      Height          =   1965
      Left            =   135
      TabIndex        =   18
      Top             =   75
      Width           =   3540
      Begin Edt4Lib.CbEdit TcpBegin
         Height          =   315
         Left            =   1395
         TabIndex        =   2
         ToolTipText     =   "Specifies the lower range of valid IP addresses under which the account can log into the server"
         Top             =   1065
         Width           =   1995
         DataField       =   "TCP_BEGIN"
         DataSource      =   "CbMaster1"
         FormatStripping =   0
         FormatType      =   2
         Mask            =   "###.###.###.###"
         PromptChar      =   " "
         _Version        =   65536
         _ExtentX        =   3519
         _ExtentY        =   556
         _StockProps     =   125
         BackColor       =   -2147483643
         Text            =   ""
         ContainerID     =   24518896
      End
      Begin Edt4Lib.CbEdit TcpEnd
         Height          =   315
         Left            =   1395
         TabIndex        =   3
         ToolTipText     =   "Specifies the upper range of valid IP addresses under which the account can log into the server"
         Top             =   1455
         Width           =   1995
         DataField       =   "TCP_END"
         DataSource      =   "CbMaster1"
         FormatStripping =   0
         FormatType      =   2
         Mask            =   "###.###.###.###"
         PromptChar      =   " "
         _Version        =   65536
         _ExtentX        =   3519
         _ExtentY        =   556
         _StockProps     =   125
         BackColor       =   -2147483643
         Text            =   ""
         ContainerID     =   24518896
      End
      Begin Edt4Lib.CbEdit AcctId
         Height          =   315
         Left            =   1395
         TabIndex        =   0
         ToolTipText     =   "The unique name of the account to be edited, created, etc."
         Top             =   285
         Width           =   1995
         DataField       =   "ACCOUNTID"
         DataSource      =   "CbMaster1"
         PromptChar      =   " "
         _Version        =   65536
         _ExtentX        =   3519
         _ExtentY        =   556
         _StockProps     =   125
         BackColor       =   -2147483643
         MaxLength       =   20
         Text            =   ""
         ContainerID     =   24518896
      End
      Begin Edt4Lib.CbEdit Password
         Height          =   315
         Left            =   1395
         TabIndex        =   1
         ToolTipText     =   "Password for associated Account Id"
         Top             =   675
         Width           =   1995
         DataField       =   "PASSWORD"
         DataSource      =   "CbMaster1"
         _Version        =   65536
         _ExtentX        =   3519
         _ExtentY        =   556
         _StockProps     =   125
         BackColor       =   -2147483643
         MaxLength       =   20
         PasswordChar    =   "*"
         Text            =   ""
         ContainerID     =   24518896
      End
      Begin VB.Label Label3
         Caption         =   "Start IP Address:"
         Height          =   240
         Left            =   105
         TabIndex        =   22
         Top             =   1095
         Width           =   1305
      End
      Begin VB.Label Label4
         Caption         =   "End IP Address:"
         Height          =   285
         Left            =   105
         TabIndex        =   21
         Top             =   1470
         Width           =   1305
      End
      Begin VB.Label Label1
         Caption         =   "Account Id:"
         Height          =   240
         Left            =   100
         TabIndex        =   20
         Top             =   345
         Width           =   1300
      End
      Begin VB.Label Label2
         Caption         =   "Password:"
         Height          =   240
         Left            =   105
         TabIndex        =   19
         Top             =   705
         Width           =   1305
      End
   End
   Begin Threed.SSPanel panelRecno
      Height          =   255
      Left            =   2100
      TabIndex        =   35
      Top             =   4575
      Width           =   2400
      _Version        =   65536
      _ExtentX        =   4233
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
      BorderWidth     =   1
      BevelOuter      =   1
      Alignment       =   1
   End
   Begin Threed.SSPanel panelStatus
      Height          =   255
      Left            =   150
      TabIndex        =   36
      Top             =   4575
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
         TabIndex        =   37
         Top             =   15
         Width           =   525
      End
   End
   Begin Threed.SSPanel panelReccount
      Height          =   255
      Left            =   4500
      TabIndex        =   38
      Top             =   4575
      Width           =   2160
      _Version        =   65536
      _ExtentX        =   3810
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
   Begin Mtr4LibCtl.CbMaster CbMaster1
      Height          =   780
      Left            =   135
      TabIndex        =   14
      Top             =   3585
      Width           =   5175
      _Version        =   65536
      _ExtentX        =   9128
      _ExtentY        =   1376
      _StockProps     =   111
      Caption         =   "Accounts"
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
      DataTag         =   "ACCOUNT"
      PackVisible     =   0   'False
      RecNoEnabled    =   0   'False
      ReindexVisible  =   0   'False
      TagListEnabled  =   0   'False
      Unique          =   1
      ContainerID     =   24518896
      NameUnique      =   "CbMaster1"
   End
End
Attribute VB_Name = "Form4"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Dim bSetLen As Boolean
Dim bMasterSetFocus As Boolean
Dim bMouseClick As Boolean
Dim origAcctId$

Private bIsDupeKey As Boolean
Private masterAction%, prevRecno&, currentRecno&
Private delCount&

Private Sub AcctId_Change()

    If bMasterSetFocus = True Then
        bMasterSetFocus = False
        SelIt AcctId
        'acctId.SelStart = 1
        'acctId.SelLength = 20
        bMasterSetFocus = False
    End If
End Sub
Private Sub AcctId_GotFocus()
    If Not bMouseDown Then SelIt AcctId
End Sub

Private Sub acctId_KeyPress(KeyAscii As Integer)
    KeyAscii = UpperKey(KeyAscii)
End Sub

Private Sub AcctId_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
    bMouseDown = True
End Sub
Private Sub AcctId_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
    bMouseDown = False
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
        Case 0 To 6     'move/search
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

            panelReccount = " Total Records: " & Format(currentCnt, "###,##0")
    End Select
End Sub
Private Sub CbMaster1_DatafileOpened(ByVal DatafileOpened As String, Response As Integer)

   'Set mask and maxlength for fields
   AcctId.MaxLength = CbMaster1.GetFieldLength("ACCOUNTID")
   AcctId.Mask = String(AcctId.MaxLength, "!")
   Password.MaxLength = CbMaster1.GetFieldLength("PASSWORD")

   delCount = GetDelCount(CbMaster1.DATA4)
   panelReccount = " Total Records: " & Format(d4recCount(CbMaster1.DATA4) - delCount, "###,##0")

End Sub

Private Sub CbMaster1_DuplicateKey(ByVal Action As Integer, Response As Integer)

    MsgBox "You have attempted to add an Account ID that already exists in the Accounts data file." & vbCrLf & vbCrLf & "The CodeBase server requires that each Account ID be unique within the Accounts data file. Please modify your changes and try " & _
    "to save the record again, or press the Navigator Undo button or the Cancel button to cancel your changes.", vbExclamation, "Duplicate Entry Error"
    Response = 2

    bIsDupeKey = True

End Sub

Private Sub CbMaster1_Error(ErrorCode As Long, Description As String, Response As Integer)

FrmDbErr4 = True

Call MasterErr(CbMaster1, Me, ACCOUNT, ErrorCode, Description)
Response = 0

Select Case ErrorCode
    Case -10
        Unload Me
    Case -20, -30
        CbMaster1.SearchEnabled = False
End Select

'Select Case ErrorCode
'Case -10
'    ShowErrLow EOPEN, , , systemPath & "ACCOUNT4.dbf", "This file is required for administrating account information. This error may be caused if you don't have appropriate access priviliges for this file."
'    Unload Me
'Case -20
'    ShowErrLow EOPEN, , , systemPath & "ACCOUNT4" & ndxFmt, "This file is required for administrating account information. There may be in internal problem in the index file."
'Case -30
'    ShowErrLow ETAG, , , "Index File: " & systemPath & "ACCOUNT4" & ndxFmt & vbLf & space(5) & "Tag Name: " & GetMst4ErrObject(Description), "There may be an internal problem in the index file.."
'Response = 0
'End Select
'   If ErrorCode = -10 Then
'        'could not open the data file
'        MsgBox "Could not open the ACCOUNT4 file" & Chr(13) & Chr(10) & "Ensure that your account has the necessary authorization to open this file", vbOKOnly + vbExclamation, "Error Opening File"
'        Response = 0
End Sub
Private Sub CbMaster1_Reposition(ByVal Action As Integer)

    Dim statusMsg$, recnoMsg$
    Dim statusForeColor&


    currentRecno = CbMaster1.recNo
    'currentRecno = CbMaster1.recNo - delCount
    If currentRecno < 0 Then currentRecno = 0

    'defaults
    statusMsg = " Status:  Browse"
    recnoMsg = " Current Record No: " & Format(currentRecno, "###,###,##0")
    statusForeColor = vbButtonText

    Select Case Action
    Case 0 To 2  'moving backwards in table
        If prevRecno = currentRecno Then statusMsg = " Status:  Top of File"

    Case 4 To 6  'moving forwards in table
        If prevRecno = currentRecno Then statusMsg = " Status:  End of File"

    Case 7      'append

        '***** NOTE *******'
        'The panelStatus control has a label positioned directly on top
        'of the panel text, that matches the first part of the panel
        'string, namely the word 'Mode'. This is so I can make the
        'status 'Append' appear red, without the word 'Mode' also
        'becoming red - DF
        statusForeColor = &HFF
        statusMsg = " Status:  Append"
        recnoMsg = " Current Record No:  --"

        With CbMaster1  ' set default values
            .SetField "ACCOUNTID", "NEW ACCOUNT"
            .SetField "PASSWORD", ""
            '.SetField "CREATE", "F"
            '.SetField "CREATE_DEL", "F"
            '.SetField "CREATE_TMP", "F"
            '.SetField "READ", "F"
            '.SetField "APPEND", "F"
            '.SetField "DELETE", "F"
            '.SetField "UPDATE", "F"
            '.SetField "INDEX", "F"
            '.SetField "COMPRESS", "F"
            '.SetField "DISCONNECT", "F"
        End With

    Case 12
        'update record count if flushing (may be append)
        panelReccount = " Total Records: " & Format(d4recCount(CbMaster1.DATA4) - delCount, "###,##0")

        'panelStatus.ForeColor = vbButtonText
        'panelStatus = " Mode:   Browse"
        'panelRecno = " Current Record No.  " & Format(str$(CbMaster1.recNo), "###,###,###")
    End Select

    'used to track if account id changes
    origAcctId = AcctId.Text

    panelStatus.ForeColor = statusForeColor
    panelStatus = statusMsg
    panelRecno = recnoMsg

    bMasterSetFocus = True
    AcctId.SetFocus

End Sub
Private Sub CbMaster1_Validate(ByVal Action As Integer, Response As Integer)

Dim dbPriv4&, dbTagAcct&
Dim accountId$, rc%, rcSeek%, actionCount&, actionName$, actionNamed$
Dim endErrMsg$, isDeleted As Boolean

If CbMaster1.RecordChanged Then


    'if they didn't use 'Save' button to save changes,
    'validate that they want to change record
    If Action <> 12 Then

        rc = MsgBox("Save privilege changes for account: " & Trim$(AcctId.Text) & " ?", vbYesNoCancel + vbQuestion, App.title)

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

    Dim mb As Integer
    Dim IPaddress As Variant
    IPaddress = "123"

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

    If Disconnect.Text = " " Then
        Call CbMaster1.SetField("DISCONNECT", False)
    End If

    If CreateDel.Text = " " Then
        Call CbMaster1.SetField("CREATE_DEL", False)
    End If

    If CreateTmp.Text = " " Then
        Call CbMaster1.SetField("CREATE_TMP", False)
    End If

    If Create.Text = " " Then
        Call CbMaster1.SetField("CREATE", False)
    End If

    CbMaster1.GetField "TCP_BEGIN", IPaddress
    If validateIP(IPaddress) = False Then
        mb = MsgBox("Invalid TCP/IP address." + vbCrLf + "Discard changes?", vbYesNo + vbDefaultButton2 + vbExclamation, _
                    "Lower TCP/IP Address Range Limit")
        If mb = vbYes Then
            CbMaster1.Undo
        Else
            Response = 0
            TcpBegin.SetFocus
        End If
    End If

    CbMaster1.GetField "TCP_END", IPaddress
    If validateIP(IPaddress) = False Then
        mb = MsgBox("Invalid TCP/IP address." + vbCrLf + "Discard changes?", vbYesNo + vbDefaultButton2 + vbExclamation, _
                    "Upper TCP/IP Address Range Limit")
        If mb = vbYes Then
            CbMaster1.Undo
        Else
            Response = 0
            TcpBegin.SetFocus
        End If
    End If

    If getBoolean(CbMaster1, "DELETE") = True _
       And (getBoolean(CbMaster1, "READ") = False _
            Or getBoolean(CbMaster1, "UPDATE") = False) Then
        mb = MsgBox("The Deletion privelage also requires the Read and Update privelages. " _
                  + "Do you want to enable the Read and Update privelages for this account?", _
                    vbOKCancel, "Save Account Privelages")
        If mb = vbCancel Then
            Action = 0
            Exit Sub
        Else
            Call CbMaster1.SetField("UPDATE", True)
            Call CbMaster1.SetField("READ", True)
        End If
    End If

    If getBoolean(CbMaster1, "DELETE") = True _
        And getBoolean(CbMaster1, "READ") = False Then
        mb = MsgBox("The Update privelage also requires the Read privelage." _
           + "Do you want to enable the Read privelage for this account?", _
             vbOKCancel, "Save Account Privelages")
        If mb = vbCancel Then
            Action = 0
            Exit Sub
        Else
            Call CbMaster1.SetField("READ", True)
        End If
    End If

     'if account name has changed, or if a record is being deleted check if privileges exist for the
    'account name and prompt user for action if needed
    If CbMaster1.Deleted Then
        isDeleted = True
    End If
    accountId = UCase$(PadRight(Trim$(AcctId.Text), CbMaster1.GetFieldLength("ACCOUNTID")))
    If StrICmp(Trim$(origAcctId), accountId) = False Or isDeleted Then
        dbPriv4 = d4open(gCode4, SystemPath + "PRIV4")
        If dbPriv4 = 0 Then
            ShowError , "Since an error ocurred attempting to open the Privileges data file, the selected action cannot be executed. You can attempt to either save the record again, or press the Navigator Undo button to cancel changes to the record."
            Response = 0
            Screen.MousePointer = 0
            Exit Sub
        Else
            dbTagAcct = d4tag(dbPriv4, "ACCOUNT")
            If dbTagAcct = 0 Then
                ShowError , "Since an error ocurred attempting to open the Index for the Privileges database, the selected action cannot be executed. You can attempt to either save the record again, or press the Navigator Undo button to cancel " _
                                    + "changes to the record."
                Response = 0
                Screen.MousePointer = 0
                rc = d4close(dbPriv4)
                Exit Sub
            Else
                Call d4tagSelect(dbPriv4, dbTagAcct)
            End If
        End If
        rcSeek = d4seek(dbPriv4, origAcctId)
        If rcSeek = r4success Then
            endErrMsg = " You can either attempt to save your changes again, or press the Navigator Undo or the Cancel button on the Privileges screen to abort the changes to the record."
            If isDeleted Then
                Form27.Show 1
            Else
                Form26.Show 1
            End If
            If Not (gOptDelPrivRecs Or gOptChangePrivRecs) Then
                'user hasn't decided what to do
                Response = 0
                rc = d4close(dbPriv4)
                Exit Sub
            Else
                'user wants to delete PRIV4 recs or change them to match new acct id
                Screen.MousePointer = 11
                ResetCode4
                ErrOff
                rc = code4logOpen(gCode4, "", accountId)
                If rc < 0 Then
                    ShowError , "Because the server log file cannot be opened, the transaction required to update the Privileges database cannot be performed." & endErrMsg
                    Response = 0
                    Screen.MousePointer = 0
                    ErrReset
                    rc = d4close(dbPriv4)
                    Exit Sub
                Else
                    rc = code4tranStart(gCode4)
                    If rc < 0 Then
                        ShowError , "Because the transaction required to update the Privileges database cannot be inititated, the update of the Privileges database cannot be performed." & endErrMsg
                        Response = 0
                        Screen.MousePointer = 0
                        ErrReset
                        rc = d4close(dbPriv4)
                        Exit Sub
                    Else
                        Dim fldPtr&
                        fldPtr = d4field(dbPriv4, "ACCOUNTID")
                        If gOptDelPrivRecs Then
                            actionName = "delete"
                            actionNamed = "deleted"
                            Do While rcSeek = r4success
                                Call d4delete(dbPriv4)
                                rc = d4flush(dbPriv4)
                                If rc <> r4success Then
                                    ShowError , "Because an error ocurred attempting to delete a record in the Privileges database, the operation to update has been cancelled." & endErrMsg
                                    rc = code4tranRollback(gCode4)
                                    Response = 0
                                    Screen.MousePointer = 0
                                    ErrReset
                                    rc = d4close(dbPriv4)
                                    Exit Sub
                                Else
                                    actionCount = actionCount + 1
                                    rcSeek = d4seek(dbPriv4, origAcctId)
                                End If
                            Loop
                        Else
                            actionName = "update"
                            actionNamed = "updated"
                            Do While rcSeek = r4success
                                actionCount = actionCount + 1
                                Call f4assign(fldPtr, AcctId.Text)
                                'is this needed?
                                rc = d4flush(dbPriv4)
                                If rc <> r4success Then
                                    If rc < 0 Then
                                        ShowError , "Because an error ocurred attempting to update a record in the Privileges database, the request to update has been cancelled." & endErrMsg
                                    ElseIf rc = r4locked Then
                                        MsgBox "A record in the Privileges database could not be updated because another process has locked it. Due to this condition, the requested update has been cancelled." & endErrMsg, vbExclamation
                                    End If
                                    rc = code4tranRollback(gCode4)
                                    Response = 0
                                    Screen.MousePointer = 0
                                    ErrReset
                                    rc = d4close(dbPriv4)
                                    Exit Sub
                                Else
                                    rcSeek = d4seek(dbPriv4, origAcctId)
                                End If
                            Loop
                        End If
                        rc = code4tranCommit(gCode4)
                        If rc <> r4success Then
                            ShowError , "A critical error ocurred attempting to commit the transaction for the specified changes, and the transaction cannot be reversed. Because of this the Privileges database may only be partially updated with the " _
                                             + "specified changes. It is recommended that you save the current changes to the Account, then shut down the server when possible, and manually " & actionName & " the balance of the Privilege records. " _
                                             + vbCrLf & "Do you wish to save the current changes made to this Account?"
                            Response = 0
                            Screen.MousePointer = 0
                            ErrReset
                            rc = d4close(dbPriv4)
                            Exit Sub
                        End If
                        Screen.MousePointer = 0
                        MsgBox str$(actionCount) & " record(s) in the Privileges database were successfully " & actionNamed & ".", , "Results"
                        origAcctId = AcctId.Text
                    End If
                End If
            End If
        Else
            If rcSeek < 0 Then
                ShowError , "Due to this error, the selected action cannot be executed. You can either attempt to save the record again, or press the Navigator Undo button or the Cancel button to abort changes made to the record."
            End If
        End If
        rc = d4close(dbPriv4)
    End If
End If
End Sub
Private Sub Command1_Click()
    CbMaster1.FlushRecord
    Unload Me
End Sub
Private Sub Command2_Click()
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
Private Sub Create_GotFocus()
    If Not bMouseDown Then SelIt Create
End Sub
Private Sub Create_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
    bMouseDown = True
End Sub
Private Sub Create_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
    bMouseDown = False
End Sub
Private Sub CreateDel_GotFocus()
    If Not bMouseDown Then SelIt CreateDel
End Sub
Private Sub CreateDel_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
    bMouseDown = True
End Sub
Private Sub CreateDel_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
    bMouseDown = False
End Sub
Private Sub CreateTmp_GotFocus()
    If Not bMouseDown Then SelIt CreateTmp
End Sub
Private Sub CreateTmp_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
    bMouseDown = True
End Sub
Private Sub CreateTmp_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
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
Private Sub Disconnect_GotFocus()
    If Not bMouseDown Then SelIt Disconnect
End Sub
Private Sub Disconnect_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
    bMouseDown = True
End Sub
Private Sub Disconnect_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
    bMouseDown = False
End Sub
Private Sub Form_Activate()
    bMouseClick = False
End Sub
Private Sub Form_Load()

    HelpContextID = Accounts

    FrmDbErr4 = False

    Left = (Screen.Width - Width) / 2   ' Center form horizontally.
    Top = (Screen.Height - Height) / 2  ' Center form vertically.

    srchDbfName = SystemPath & "ACCOUNT4"
    srchIndxName = SystemPath & "ACCOUNT4"

    'open the ACCOUNT4.dbf file with the master,
    'set Cbmaster DatabaseName, Indexfiles, DataTag
    CbMaster1.serverId = svrId
    CbMaster1.ProcessId = portId
    CbMaster1.Password = passwrd
    CbMaster1.userName = accountId

    CbMaster1.DatabaseName = srchDbfName
    CbMaster1.IndexFiles = srchIndxName
    CbMaster1.DataTag = "NAMEPASS"
    CbMaster1.RefreshFiles
End Sub
Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
   Dim rc%

   If CbMaster1.RecordChanged Then
      rc = MsgBox("Save changes made to the account: " & UCase$(Trim$(AcctId.Text)) & " ?", vbYesNoCancel + vbQuestion, App.title)
      If rc = vbNo Then
         CbMaster1.Undo
      ElseIf rc = vbCancel Then
         Cancel = 1
      Else
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
            AcctId.Text = AcctId.Text & ""
         End If
      End If
   End If
End Sub
Private Sub Form_Unload(Cancel As Integer)
    FrmDbErr4 = False
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
Private Sub Password_GotFocus()
    If Not bMouseDown Then
        Dim slen%
        slen% = CbMaster1.GetFieldLength("ACCOUNTID")
        Password.SelStart = 0
        If slen > 0 Then Password.SelLength = slen + 1 'cc3 patch for now
    End If
End Sub
Private Sub Password_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
    bMouseDown = True
End Sub
Private Sub Password_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
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
Private Sub TcpBegin_GotFocus()
    If Not bMouseDown Then SelIt TcpBegin
End Sub
Private Sub TcpBegin_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
    bMouseDown = True
End Sub
Private Sub TcpBegin_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
    bMouseDown = False
End Sub
Private Sub TcpEnd_GotFocus()
    If Not bMouseDown Then SelIt TcpEnd
End Sub
Private Sub TcpEnd_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
    bMouseDown = True
End Sub
Private Sub TcpEnd_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
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
