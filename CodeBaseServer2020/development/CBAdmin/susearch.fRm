VERSION 5.00
Object = "{3A0EBF03-62A1-101C-A69C-040224009C02}#1.0#0"; "MTR4.OCX"
Object = "{BB149643-466A-11CF-9C13-0020AF0783D2}#1.0#0"; "CMB4.OCX"
Begin VB.Form Form25
   Caption         =   "Search Database"
   ClientHeight    =   3390
   ClientLeft      =   540
   ClientTop       =   2310
   ClientWidth     =   6435
   Icon            =   "susearch.frx":0000
   LinkTopic       =   "Form25"
   LockControls    =   -1  'True
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   3390
   ScaleWidth      =   6435
   ShowInTaskbar   =   0   'False
   Begin VB.CommandButton Command2
      Caption         =   "OK"
      Default         =   -1  'True
      Height          =   345
      Left            =   5115
      TabIndex        =   4
      Top             =   2895
      Width           =   1155
   End
   Begin VB.CommandButton Command1
      Caption         =   "Cancel"
      Height          =   345
      Left            =   3855
      TabIndex        =   3
      Top             =   2895
      Width           =   1155
   End
   Begin VB.TextBox txtRecno
      Enabled         =   0   'False
      Height          =   285
      Left            =   4935
      TabIndex        =   5
      TabStop         =   0   'False
      Text            =   "Text1"
      Top             =   915
      Width           =   1320
   End
   Begin VB.ComboBox cmbTagList
      Height          =   315
      Left            =   3855
      Style           =   2  'Dropdown List
      TabIndex        =   1
      Top             =   360
      Width           =   2400
   End
   Begin Mtr4LibCtl.CbMaster CbMaster1
      Height          =   360
      Left            =   3885
      TabIndex        =   8
      Top             =   2055
      Width           =   975
      _Version        =   65536
      _ExtentX        =   1720
      _ExtentY        =   635
      _StockProps     =   111
      Caption         =   "CbMaster1"
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
      DeletedVisible  =   0   'False
      FlushVisible    =   0   'False
      NextPageVisible =   0   'False
      NextVisible     =   0   'False
      PackVisible     =   0   'False
      PreviousPageVisible=   0   'False
      PreviousVisible =   0   'False
      RecNoEnabled    =   0   'False
      RefreshVisible  =   0   'False
      ReindexVisible  =   0   'False
      SearchVisible   =   0   'False
      SliderEnabled   =   0   'False
      TagListEnabled  =   0   'False
      TopVisible      =   0   'False
      UndoVisible     =   0   'False
      Visibility      =   0   'False
      ContainerID     =   24436624
      NameUnique      =   "CbMaster1"
   End
   Begin Cmb4Lib.CbCombo Search
      Height          =   2865
      Left            =   165
      TabIndex        =   0
      ToolTipText     =   "Search"
      Top             =   375
      Width           =   3465
      DataFieldExprList=   ""
      DataSourceList  =   "CbMaster1"
      DataTagList     =   ""
      Style           =   1
      _Version        =   65536
      _ExtentX        =   6112
      _ExtentY        =   5054
      _StockProps     =   221
      Text            =   "          "
      BackColor       =   -2147483643
      Text            =   "          "
      ContainerID     =   24436624
   End
   Begin VB.Label Label3
      Caption         =   "Search By:"
      Height          =   240
      Left            =   3855
      TabIndex        =   7
      Top             =   90
      Width           =   1320
   End
   Begin VB.Label Label2
      Caption         =   "Search For:"
      Height          =   240
      Left            =   180
      TabIndex        =   6
      Top             =   90
      Width           =   2265
   End
   Begin VB.Label Label1
      Caption         =   "Record No."
      Height          =   225
      Left            =   3855
      TabIndex        =   2
      Top             =   960
      Width           =   960
   End
End
Attribute VB_Name = "Form25"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Dim leftGutter&, rightGutter&, midGutter&, midColumnOffset&, bottomGutter&, topGutter&
Dim startWidth&, startHeight&

Private Sub CbMaster1_DatafileOpened(ByVal DatafileOpened As String, Response As Integer)
    cmbTagList.ListIndex = 0
End Sub
Private Sub CbMaster1_Error(ErrorCode As Long, Description As String, Response As Integer)
    MasterErr CbMaster1, Me, UNSPECIFIED, ErrorCode, Description
    Response = 0

    Select Case ErrorCode
    Case 10
        Unload Me
    End Select
End Sub
Private Sub CbMaster1_Reposition(ByVal Action As Integer)
    txtRecno = str$(CbMaster1.recno)
End Sub
Private Sub cmbTagList_Click()
    Dim li%, TagName$
    li = cmbTagList.ListIndex

    Select Case UCase$(callingFormId)
        Case "FORM4", "FORM10"  ' ACCOUNT4.dbf
            Select Case li
            Case 0
                TagName = "NAME"
            End Select
        Case "FORM5", "FORM11", "FORM11A" 'PRIV4.dbf
            Select Case li
                Case 0
                    TagName = "account"
                Case 1
                    TagName = "table"
                Case 2
                    TagName = "path_tbl"
                Case 3
                    TagName = "acctqpth"
            End Select
        Case "FORM6", "FORM12"
            Select Case li
                Case 0
                    TagName = "account"
                Case 1
                    TagName = "tcpaddress"
            End Select
        Case "FORM13", "FORM14", "FORM15", "FORM16"
            Select Case li
                Case 0
                    TagName = "table_tag"
                Case 1
                    TagName = "tableonly"
            End Select
        Case "FORM17", "FORM18"
            Select Case li
                    Case 0
                        TagName = "account"
                    Case 1
                        TagName = "tcpaddress"
            End Select
    End Select

    Search.DataTagList = TagName
    Search.DataExprList = t4expr(d4tag(CbMaster1.DATA4, TagName))
    Search.Refresh

    Search.SetsFocus

    SelIt Search
End Sub
Private Sub Command1_Click()

    searchResultsRecno = -1
    Unload Me

End Sub

Private Sub Command2_Click()

    searchResultsRecno = CbMaster1.recno
    Unload Me

End Sub

Private Sub Form_Load()

Left = (Screen.Width - Width) / 2   ' Center form horizontally.
Top = (Screen.Height - Height) / 2  ' Center form vertically.

startWidth = Me.Width
startHeight = Me.Height
midColumnOffset = Me.ScaleWidth - Label3.Left
leftGutter = Search.Left
midGutter = Label1.Left - leftGutter - Search.Width
rightGutter = Me.ScaleWidth - txtRecno.Width - txtRecno.Left
bottomGutter = Me.ScaleHeight - Command1.Top - Command1.Height
topGutter = Me.ScaleHeight - Search.Height - bottomGutter
'recNoTxtSpace = txtRecno.Left - Search.Width - leftGutter
'comm2Space = Command2.Left - Search.Width - leftGutter

CbMaster1.serverId = svrId
CbMaster1.ProcessId = portId
CbMaster1.password = passwrd
CbMaster1.userName = accountId

CbMaster1.DatabaseName = srchDbfName
CbMaster1.IndexFiles = srchIndxName
'CbMaster1.DataTag = "ACCOUNTID"
CbMaster1.RefreshFiles

Select Case UCase$(callingFormId)
Case "FORM4", "FORM10"
    cmbTagList.AddItem "Account Id"
Case "FORM5", "FORM11", "FORM11A"
    cmbTagList.AddItem "Account Id"
    cmbTagList.AddItem "Table Name"
    cmbTagList.AddItem "Path & Table Name"
    cmbTagList.AddItem "Account Id & Path & Table"
Case "FORM6", "FORM12"
    cmbTagList.AddItem "Account Id"
    cmbTagList.AddItem "IP Address"
Case "FORM13", "FORM14", "FORM15", "FORM16"
    cmbTagList.AddItem "Path & Table Name"
    cmbTagList.AddItem "Table Name"
Case "FORM17", "FORM18"
    cmbTagList.AddItem "Account Id"
    cmbTagList.AddItem "IP Address"
End Select

End Sub







Private Sub Form_Resize()

If Me.Width < startWidth Then
    Me.Width = startWidth
Else
    Dim lft&
    lft = Me.ScaleWidth - midColumnOffset
    Label3.Left = lft
    cmbTagList.Left = lft
    Label1.Left = lft
    Command1.Left = lft
    txtRecno.Left = Me.ScaleWidth - txtRecno.Width - rightGutter
    Command2.Left = Me.ScaleWidth - Command2.Width - rightGutter
    Search.Width = Label3.Left - midGutter - leftGutter
End If

If Me.Height < startHeight Then
    Me.Height = startHeight
Else
    Command1.Top = Me.ScaleHeight - Command1.Height - bottomGutter
    Command2.Top = Me.ScaleHeight - Command1.Height - bottomGutter
    Search.Height = Me.ScaleHeight - topGutter - bottomGutter
End If


End Sub


Private Sub Search_LookUp(ByVal str As String, newStr As String)
newStr = UCase$(str)
End Sub


