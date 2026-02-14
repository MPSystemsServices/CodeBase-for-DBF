VERSION 5.00
Object = "{3A0EBF03-62A1-101C-A69C-040224009C02}#1.0#0"; "MTR4.OCX"
Begin VB.Form Form10
   Caption         =   "Accounts Report"
   ClientHeight    =   3795
   ClientLeft      =   780
   ClientTop       =   1920
   ClientWidth     =   8790
   Icon            =   "suacct4r.frx":0000
   LinkTopic       =   "Form10"
   LockControls    =   -1  'True
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   3795
   ScaleWidth      =   8790
   ShowInTaskbar   =   0   'False
   Visible         =   0   'False
   Begin Mtr4LibCtl.CbMaster CbMaster1
      Height          =   480
      Left            =   4470
      TabIndex        =   0
      Top             =   3150
      Width           =   555
      _Version        =   65536
      _ExtentX        =   979
      _ExtentY        =   847
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
      CaptionVisible  =   0   'False
      DataTag         =   ""
      DeletedVisible  =   0   'False
      FlushVisible    =   0   'False
      NextPageVisible =   0   'False
      NextVisible     =   0   'False
      PackVisible     =   0   'False
      PreviousPageVisible=   0   'False
      PreviousVisible =   0   'False
      ProcessId       =   ""
      RecNoEnabled    =   0   'False
      RefreshVisible  =   0   'False
      ReindexVisible  =   0   'False
      ServerId        =   ""
      SliderEnabled   =   0   'False
      TagListEnabled  =   0   'False
      TopVisible      =   0   'False
      UndoVisible     =   0   'False
      UserName        =   ""
      ContainerID     =   24527900
      NameUnique      =   "CbMaster1"
   End
End
Attribute VB_Name = "Form10"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private cb&
Private dbAcct&
Private Sub CbMaster1_Error(ErrorCode As Long, Description As String, Response As Integer)

MasterErr CbMaster1, Me, ACCOUNT, ErrorCode, Description
Response = 0

If ErrorCode = -10 Then
    Unload Me
ElseIf ErrorCode = -20 Or ErrorCode = -30 Then
'    mnuSearch.Enabled = False
End If

'If ErrorCode = -10 Then
'    'could not open the data file
'    MsgBox "Could not open the ACCOUNT4 file for the searching feature" & Chr(13) & Chr(10) & "Ensure that your account has the necessary authorization to open this file", vbOKOnly + vbExclamation, "Error Opening File"
'    Response = 0
'    mnuSearch.Enabled = False
'End If

End Sub
Private Sub CbMaster1_Reposition(ByVal Action As Integer)
Dim recNo As Long

'If Action = 3 Then
    recNo = d4recNo(CbMaster1.DATA4)
    MyData1.MoveTo recNo
'End If

End Sub
Private Sub Form_Load()

HelpContextID = Account_Reports

Left = (Screen.Width - Width) / 2   ' Center form horizontally.
Top = (Screen.Height - Height) / 2  ' Center form vertically.

Dim rc As Integer
Dim mySplit As TrueDBGrid45.Split

'For resizing grid if form size changes
frmStartHt = 1
frmStartWd = 1
grdHtPer = 100
grdWdPer = 100
grdMinHt = 495    'size of one row, plus menu height
grdMaxHt = 100000 'big no. means no real max height
grdMinWd = 1
grdMaxWd = 30000

'open the ACCOUNT4.dbf file with the master,
'set Cbmaster DatabaseName, Indexfiles, DataTag
With CbMaster1
    .serverId = svrId
    .processId = portId
    .password = passwrd
    .userName = accountId
    .DatabaseName = systemPath + "ACCOUNT4.dbf"
    .IndexFiles = systemPath + "ACCOUNT4"
    .DataTag = "NAMEPASS"
    .RefreshFiles
End With

'open the ACCOUNT4 file with the MyData control

'cb = code4init()
cb = gCode4 'use the global CODE4
If cb = 0 Then
    showErrInit
Else
    dbAcct = d4open(cb, systemPath + "ACCOUNT4.dbf")
    If dbAcct = 0 Then
        ShowError "File Name: " & systemPath & "ACCOUNT4.dbf", "This file is required for administrating account information. This error may be caused if you don't have appropriate access priviliges for this data file."
    Else
        'select tag
        Call d4tagSelect(dbAcct, d4tag(dbAcct, "NAMEPASS"))
        If code4errorCode(cb, r4check) <> r4success Then
            ShowError
            Exit Sub
        End If

        MyData1.RecordSource = d4encodeHandle(dbAcct)

        ' Create an additional split:
        Set mySplit = TDBGrid2.Splits.Add(0)
        ' Hide all columns in the leftmost split, Splits(0),
        ' except for columns 0 and 1

        Dim i As Integer
        Dim Cols As Variant
        Set Cols = TDBGrid2.Splits(0).Columns
        For i = 0 To Cols.count - 1
            Cols(i).Visible = False
        Next i
        Cols(0).Visible = True

        ' Configure Splits(0) to display exactly two columns,
        ' and disable resizing
        With TDBGrid2.Splits(0)
            .SizeMode = dbgNumberOfColumns
            .Size = 1
            .AllowSizing = False
        End With

        ' Usually, if you fix columns 0 and 1 from scrolling
        ' in a split, you will want to make them invisible in
        ' other splits:
        Set Cols = TDBGrid2.Splits(1).Columns
        Cols(0).Visible = False
        ' Turn off the record selectors in Split 1:
        TDBGrid2.Splits(1).RecordSelectors = False
    End If
End If
End Sub
Private Sub Form_Resize()
    ReSizeGrid TDBGrid2, Me
End Sub
Private Sub Form_Unload(Cancel As Integer)
    'If codeAcct <> 0 Then
    '    code4close (codeAcct)
    '    code4initUndo (codeAcct)
    '    dbAcct = 0
    '    codeAcct = 0
    'End If
    CloseDB dbAcct
End Sub
Private Sub mnuSearch_Click()
    ShowSearchDlg CbMaster1, Me
End Sub

