VERSION 5.00
Object = "{3A0EBF03-62A1-101C-A69C-040224009C02}#1.0#0"; "MTR4.OCX"
Object = "{564BFFC3-B5F3-101C-A2C8-A27C829646CD}#1.0#0"; "EDT4.OCX"
Object = "{BB149643-466A-11CF-9C13-0020AF0783D2}#1.0#0"; "CMB4.OCX"
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "COMDLG32.OCX"
Begin VB.Form frmConfig2
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Server Configuration"
   ClientHeight    =   3600
   ClientLeft      =   990
   ClientTop       =   1605
   ClientWidth     =   7830
   LinkTopic       =   "Form2"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   3600
   ScaleWidth      =   7830
   ShowInTaskbar   =   0   'False
   Begin VB.PictureBox boxODBC
      BorderStyle     =   0  'None
      Height          =   2295
      Left            =   2400
      ScaleHeight     =   2295
      ScaleWidth      =   5055
      TabIndex        =   7
      Top             =   120
      Width           =   5055
      Begin VB.Frame Frame1
         Caption         =   "Collation"
         Height          =   975
         Left            =   0
         TabIndex        =   54
         Top             =   0
         Width           =   2055
         Begin VB.OptionButton optCollateGeneral
            Caption         =   "General"
            Height          =   255
            Left            =   240
            TabIndex        =   56
            Top             =   540
            Width           =   1095
         End
         Begin VB.OptionButton optCollateMachine
            Caption         =   "Machine"
            Height          =   255
            Left            =   240
            TabIndex        =   55
            Top             =   240
            Width           =   1095
         End
      End
      Begin Edt4Lib.CbEdit edtODBCLockAttempts
         Height          =   315
         Left            =   1560
         TabIndex        =   39
         ToolTipText     =   "CbEdit1"
         Top             =   1080
         Width           =   855
         DataField       =   "ODBCNMLOCK"
         DataSource      =   "CbMaster1"
         FormatStripping =   0
         NegativeColor   =   -2147483630
         _Version        =   65536
         _ExtentX        =   1508
         _ExtentY        =   556
         _StockProps     =   125
         BackColor       =   -2147483643
         Decimals        =   0
         Text            =   ""
         ContainerID     =   24436624
      End
      Begin VB.Label Label15
         Caption         =   "Lock Attempts:"
         Height          =   255
         Left            =   0
         TabIndex        =   40
         Top             =   1155
         Width           =   1335
      End
   End
   Begin VB.ListBox lstCategory
      Height          =   3375
      ItemData        =   "suconfig2.frx":0000
      Left            =   120
      List            =   "suconfig2.frx":0013
      TabIndex        =   3
      Top             =   120
      Width           =   2055
   End
   Begin VB.CommandButton Command1
      Caption         =   "OK"
      Default         =   -1  'True
      Height          =   350
      Left            =   5420
      TabIndex        =   0
      Top             =   3160
      Width           =   1130
   End
   Begin VB.CommandButton Command2
      Cancel          =   -1  'True
      Caption         =   "&Cancel"
      Height          =   350
      Left            =   6625
      TabIndex        =   1
      Top             =   3160
      Width           =   1130
   End
   Begin VB.PictureBox boxMemory
      BorderStyle     =   0  'None
      Height          =   2895
      Left            =   2400
      ScaleHeight     =   2895
      ScaleWidth      =   3735
      TabIndex        =   41
      Top             =   120
      Width           =   3735
      Begin Edt4Lib.CbEdit CbEdit6
         Height          =   315
         Left            =   2520
         TabIndex        =   53
         Top             =   2100
         Width           =   1000
         DataField       =   "MemSzSp"
         DataSource      =   "CbMaster1"
         _Version        =   65536
         _ExtentX        =   1773
         _ExtentY        =   556
         _StockProps     =   125
         BackColor       =   -2147483643
         Text            =   ""
         ContainerID     =   24436624
      End
      Begin Edt4Lib.CbEdit CbEdit5
         Height          =   315
         Left            =   2520
         TabIndex        =   52
         Top             =   1680
         Width           =   1000
         DataField       =   "MemSzSb"
         DataSource      =   "CbMaster1"
         _Version        =   65536
         _ExtentX        =   1773
         _ExtentY        =   556
         _StockProps     =   125
         BackColor       =   -2147483643
         Text            =   ""
         ContainerID     =   24436624
      End
      Begin Edt4Lib.CbEdit CbEdit4
         Height          =   315
         Left            =   2520
         TabIndex        =   51
         Top             =   1260
         Width           =   1000
         DataField       =   "MemSzMemo"
         DataSource      =   "CbMaster1"
         _Version        =   65536
         _ExtentX        =   1773
         _ExtentY        =   556
         _StockProps     =   125
         BackColor       =   -2147483643
         Text            =   ""
         ContainerID     =   24436624
      End
      Begin Edt4Lib.CbEdit CbEdit3
         Height          =   315
         Left            =   2520
         TabIndex        =   50
         Top             =   840
         Width           =   1000
         DataField       =   "MemSzBuf"
         DataSource      =   "CbMaster1"
         _Version        =   65536
         _ExtentX        =   1773
         _ExtentY        =   556
         _StockProps     =   125
         BackColor       =   -2147483643
         Text            =   ""
         ContainerID     =   24436624
      End
      Begin Edt4Lib.CbEdit CbEdit2
         Height          =   315
         Left            =   2520
         TabIndex        =   49
         Top             =   420
         Width           =   1000
         DataField       =   "MemSzBlock"
         DataSource      =   "CbMaster1"
         _Version        =   65536
         _ExtentX        =   1773
         _ExtentY        =   556
         _StockProps     =   125
         BackColor       =   -2147483643
         Text            =   ""
         ContainerID     =   24436624
      End
      Begin Edt4Lib.CbEdit CbEdit1
         Height          =   315
         Left            =   2520
         TabIndex        =   48
         Top             =   0
         Width           =   1000
         DataField       =   "MemStMax"
         DataSource      =   "CbMaster1"
         _Version        =   65536
         _ExtentX        =   1773
         _ExtentY        =   556
         _StockProps     =   125
         BackColor       =   -2147483643
         Text            =   ""
         ContainerID     =   24436624
      End
      Begin VB.Label Label25
         Caption         =   "Memory for Optimization:"
         Height          =   255
         Left            =   0
         TabIndex        =   47
         Top             =   75
         Width           =   2415
      End
      Begin VB.Label Label27
         Caption         =   "Size of Memory Block:"
         Height          =   255
         Left            =   0
         TabIndex        =   46
         Top             =   495
         Width           =   2415
      End
      Begin VB.Label Label28
         Caption         =   "Size of Memory Buffers:"
         Height          =   255
         Left            =   0
         TabIndex        =   45
         Top             =   915
         Width           =   2415
      End
      Begin VB.Label Label29
         Caption         =   "Size of Memory Block for Memos:"
         Height          =   255
         Left            =   0
         TabIndex        =   44
         Top             =   1335
         Width           =   2415
      End
      Begin VB.Label Label30
         Caption         =   "Size of Sort Buffers:"
         Height          =   255
         Left            =   0
         TabIndex        =   43
         Top             =   1755
         Width           =   2415
      End
      Begin VB.Label Label31
         Caption         =   "Amount of Memory for Sorting:"
         Height          =   255
         Left            =   0
         TabIndex        =   42
         Top             =   2175
         Width           =   2415
      End
   End
   Begin VB.PictureBox boxAccess
      BorderStyle     =   0  'None
      Height          =   2175
      Left            =   2400
      ScaleHeight     =   2175
      ScaleWidth      =   4575
      TabIndex        =   5
      Top             =   120
      Width           =   4575
      Begin Edt4Lib.CbEdit KeepOpen
         Height          =   315
         Left            =   1560
         TabIndex        =   24
         ToolTipText     =   "Determines if the server keeps data files open when they are not currently in use by any application"
         Top             =   420
         Width           =   495
         DataField       =   "KEEPOPEN"
         DataSource      =   "CbMaster1"
         FormatType      =   2
         Mask            =   "L"
         PromptChar      =   " "
         _Version        =   65536
         _ExtentX        =   873
         _ExtentY        =   556
         _StockProps     =   125
         Text            =   " "
         BackColor       =   -2147483643
         Text            =   " "
         ContainerID     =   24436624
      End
      Begin Cmb4Lib.CbCombo cmbLogOptions
         Height          =   315
         Left            =   1560
         TabIndex        =   21
         ToolTipText     =   "Determines when the database server logs database updates"
         Top             =   840
         Width           =   2295
         DataFieldEdit   =   ""
         Style           =   2
         ListItems       =   $"suconfig2.frx":0051
         _Version        =   65536
         _ExtentX        =   4048
         _ExtentY        =   556
         _StockProps     =   221
         Text            =   "cmbLogOptions"
         BackColor       =   -2147483643
         Text            =   "cmbLogOptions"
         ContainerID     =   24436624
      End
      Begin Cmb4Lib.CbCombo cmbOpenMode
         Height          =   315
         Left            =   1560
         TabIndex        =   20
         ToolTipText     =   "Determines the access mode used by server to open tables"
         Top             =   0
         Width           =   1815
         DataFieldEdit   =   ""
         ListItems       =   $"suconfig2.frx":0075
         _Version        =   65536
         _ExtentX        =   3201
         _ExtentY        =   556
         _StockProps     =   221
         Text            =   "cmbOpenMode"
         BackColor       =   -2147483643
         Text            =   "cmbOpenMode"
         ContainerID     =   24436624
      End
      Begin Cmb4Lib.CbCombo cmbOptimize
         Height          =   315
         Left            =   1560
         TabIndex        =   19
         ToolTipText     =   "Determines when server should optimize (cache) disk reads"
         Top             =   1260
         Width           =   2895
         DataFieldEdit   =   ""
         Style           =   2
         ListItems       =   $"suconfig2.frx":0097
         _Version        =   65536
         _ExtentX        =   5106
         _ExtentY        =   556
         _StockProps     =   221
         Text            =   "cmbOptimize"
         BackColor       =   -2147483643
         Text            =   "cmbOptimize"
         ContainerID     =   24436624
      End
      Begin Cmb4Lib.CbCombo cmbOptimizeWr
         Height          =   315
         Left            =   1560
         TabIndex        =   18
         ToolTipText     =   "Determines when server should optimize (cache) disk writes"
         Top             =   1680
         Width           =   2895
         DataFieldEdit   =   ""
         Style           =   2
         ListItems       =   $"suconfig2.frx":00C7
         _Version        =   65536
         _ExtentX        =   5106
         _ExtentY        =   556
         _StockProps     =   221
         Text            =   "cmbOptimizeWr"
         BackColor       =   -2147483643
         Text            =   "cmbOptimizeWr"
         ContainerID     =   24436624
      End
      Begin VB.Label Label11
         Caption         =   "Optimize Writes:"
         Height          =   375
         Left            =   0
         TabIndex        =   26
         Top             =   1755
         Width           =   1335
      End
      Begin VB.Label Label10
         Caption         =   "Optimize Reads:"
         Height          =   255
         Left            =   0
         TabIndex        =   25
         Top             =   1335
         Width           =   1335
      End
      Begin VB.Label Label3
         Caption         =   "Keep Files Open:"
         Height          =   255
         Left            =   0
         TabIndex        =   23
         Top             =   495
         Width           =   1335
      End
      Begin VB.Label Label4
         Caption         =   "Log File Changes:"
         Height          =   255
         Left            =   0
         TabIndex        =   22
         Top             =   915
         Width           =   1455
      End
      Begin VB.Label Label6
         Caption         =   "Open/Create Mode:"
         Height          =   255
         Left            =   0
         TabIndex        =   17
         Top             =   75
         Width           =   1575
      End
   End
   Begin VB.PictureBox boxNetworking
      BorderStyle     =   0  'None
      Height          =   2295
      Left            =   2400
      ScaleHeight     =   2295
      ScaleWidth      =   3330
      TabIndex        =   6
      Top             =   120
      Width           =   3330
      Begin VB.Frame Frame3
         Caption         =   "IP Address Range"
         Height          =   1215
         Left            =   0
         TabIndex        =   12
         Top             =   1000
         Width           =   3195
         Begin Edt4Lib.CbEdit edtTCPbegin
            Height          =   315
            Left            =   1560
            TabIndex        =   14
            ToolTipText     =   "Lowest allowed IP address"
            Top             =   260
            Width           =   1515
            DataField       =   "TCPBEGIN"
            DataSource      =   "CbMaster1"
            FormatStripping =   0
            FormatType      =   2
            Mask            =   "###.###.###.###"
            PromptChar      =   " "
            _Version        =   65536
            _ExtentX        =   2672
            _ExtentY        =   556
            _StockProps     =   125
            Text            =   "   .   .   .   "
            BackColor       =   -2147483643
            Text            =   "   .   .   .   "
            ContainerID     =   24436624
         End
         Begin Edt4Lib.CbEdit edtTCPend
            Height          =   315
            Left            =   1560
            TabIndex        =   13
            ToolTipText     =   "Highest allowed IP address"
            Top             =   680
            Width           =   1515
            DataField       =   "TCPEND"
            DataSource      =   "CbMaster1"
            FormatStripping =   0
            FormatType      =   2
            Mask            =   "###.###.###.###"
            PromptChar      =   " "
            _Version        =   65536
            _ExtentX        =   2672
            _ExtentY        =   556
            _StockProps     =   125
            Text            =   "   .   .   .   "
            BackColor       =   -2147483643
            Text            =   "   .   .   .   "
            ContainerID     =   24436624
         End
         Begin VB.Label Label9
            Caption         =   "End:"
            Height          =   240
            Left            =   120
            TabIndex        =   16
            Top             =   755
            Width           =   540
         End
         Begin VB.Label Label8
            Caption         =   "Begin:"
            Height          =   240
            Left            =   120
            TabIndex        =   15
            Top             =   335
            Width           =   540
         End
      End
      Begin Edt4Lib.CbEdit ProcessId
         Height          =   315
         Left            =   1560
         TabIndex        =   11
         ToolTipText     =   "The process identification (or port id) for the server"
         Top             =   0
         Width           =   1500
         DataField       =   "PROCESSID"
         DataSource      =   "CbMaster1"
         FormatType      =   3
         _Version        =   65536
         _ExtentX        =   2646
         _ExtentY        =   556
         _StockProps     =   125
         Text            =   " 0"
         BackColor       =   -2147483643
         Decimals        =   0
         Text            =   " 0"
         ContainerID     =   24436624
      End
      Begin Edt4Lib.CbEdit edtODBCport
         Height          =   315
         Left            =   1560
         TabIndex        =   9
         ToolTipText     =   "CbEdit1"
         Top             =   420
         Width           =   1500
         DataField       =   "ODBCPortNo"
         DataSource      =   "CbMaster1"
         _Version        =   65536
         _ExtentX        =   2646
         _ExtentY        =   556
         _StockProps     =   125
         BackColor       =   -2147483643
         Text            =   ""
         ContainerID     =   24436624
      End
      Begin VB.Label Label7
         Caption         =   "Port Id:"
         Height          =   240
         Left            =   0
         TabIndex        =   10
         Top             =   75
         Width           =   540
      End
      Begin VB.Label Label13
         Caption         =   "ODBC Port Id:"
         Height          =   255
         Left            =   0
         TabIndex        =   8
         Top             =   495
         Width           =   1095
      End
   End
   Begin VB.PictureBox boxFiles
      BorderStyle     =   0  'None
      Height          =   2895
      Left            =   2400
      ScaleHeight     =   2895
      ScaleWidth      =   5295
      TabIndex        =   4
      Top             =   120
      Width           =   5295
      Begin VB.CommandButton cmdBrowseLog
         Caption         =   "..."
         BeginProperty Font
            Name            =   "Arial"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   315
         Index           =   0
         Left            =   4905
         TabIndex        =   30
         Top             =   0
         Width           =   315
      End
      Begin VB.CommandButton cmdBrowseErr
         Caption         =   "..."
         BeginProperty Font
            Name            =   "Arial"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   315
         Index           =   0
         Left            =   4905
         TabIndex        =   29
         Top             =   420
         Width           =   315
      End
      Begin VB.CommandButton cmdBrowse
         Caption         =   "..."
         BeginProperty Font
            Name            =   "Arial"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   315
         Index           =   3
         Left            =   4905
         TabIndex        =   28
         Top             =   1260
         Width           =   315
      End
      Begin VB.CommandButton cmdBrowse
         Caption         =   "..."
         BeginProperty Font
            Name            =   "Arial"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   315
         Index           =   2
         Left            =   4905
         TabIndex        =   27
         Top             =   840
         Width           =   315
      End
      Begin Edt4Lib.CbEdit ErrName
         Height          =   315
         Left            =   1560
         TabIndex        =   38
         ToolTipText     =   "Name of the file used by server to log error information"
         Top             =   420
         Width           =   3300
         DataField       =   "ERRNAME"
         DataSource      =   "CbMaster1"
         _Version        =   65536
         _ExtentX        =   5821
         _ExtentY        =   556
         _StockProps     =   125
         BackColor       =   -2147483643
         Text            =   ""
         ContainerID     =   24436624
      End
      Begin Edt4Lib.CbEdit SystemPath
         Height          =   315
         Left            =   1560
         TabIndex        =   37
         ToolTipText     =   "Default directory where server looks for Account and Privileges system files"
         Top             =   1260
         Width           =   3300
         DataField       =   "SYSTEMPATH"
         DataSource      =   "CbMaster1"
         _Version        =   65536
         _ExtentX        =   5821
         _ExtentY        =   556
         _StockProps     =   125
         BackColor       =   -2147483643
         Text            =   ""
         ContainerID     =   24436624
      End
      Begin Edt4Lib.CbEdit edtDefPath
         Height          =   315
         Left            =   1560
         TabIndex        =   35
         ToolTipText     =   "Default directory where server looks for data files and log files"
         Top             =   840
         Width           =   3300
         DataField       =   "DEFPATH"
         DataSource      =   "CbMaster1"
         _Version        =   65536
         _ExtentX        =   5821
         _ExtentY        =   556
         _StockProps     =   125
         BackColor       =   -2147483643
         MaxLength       =   128
         Text            =   ""
         ContainerID     =   24436624
      End
      Begin Edt4Lib.CbEdit LogName
         Height          =   315
         Left            =   1560
         TabIndex        =   32
         ToolTipText     =   "Name of the file used by server to log transactions and updates"
         Top             =   0
         Width           =   3300
         DataField       =   "LOGNAME"
         DataSource      =   "CbMaster1"
         _Version        =   65536
         _ExtentX        =   5821
         _ExtentY        =   556
         _StockProps     =   125
         BackColor       =   -2147483643
         Text            =   ""
         ContainerID     =   24436624
      End
      Begin VB.Label Label20
         Caption         =   "System Directory:"
         Height          =   225
         Left            =   0
         TabIndex        =   36
         Top             =   1335
         Width           =   1350
      End
      Begin VB.Label Label1
         Caption         =   "Default Directory:"
         Height          =   240
         Left            =   0
         TabIndex        =   34
         Top             =   915
         Width           =   1350
      End
      Begin VB.Label Label5
         Caption         =   "Log File Name:"
         Height          =   225
         Left            =   0
         TabIndex        =   33
         Top             =   75
         Width           =   1350
      End
      Begin VB.Label Label2
         Caption         =   "Error Log File:"
         Height          =   300
         Left            =   0
         TabIndex        =   31
         Top             =   495
         Width           =   1350
      End
   End
   Begin Mtr4LibCtl.CbMaster CbMaster1
      Height          =   360
      Left            =   2445
      TabIndex        =   2
      TabStop         =   0   'False
      Top             =   3090
      Width           =   930
      _Version        =   65536
      _ExtentX        =   1640
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
      DataTag         =   ""
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
   Begin MSComDlg.CommonDialog dlgLogFile
      Left            =   4320
      Top             =   3105
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
      DialogTitle     =   "Browse"
      Filter          =   "Log files (*.log)|*.log|xBASE data files|*.dbf;*.cdx;*.fpt;*.mdx;*.dbt;*.ntx|All Files (*.*)|*.*"
      FilterIndex     =   1
      Flags           =   4
      MaxFileSize     =   128
   End
   Begin MSComDlg.CommonDialog dlgErrFile
      Left            =   3645
      Top             =   3060
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
      DialogTitle     =   "Browse"
      Filter          =   "Error Log files (*.err)|*.err|xBASE data files|*.dbf;*.cdx;*.fpt;*.mdx;*.dbt;*.ntx|All Files (*.*)|*.*"
      FilterIndex     =   1
      Flags           =   4
      MaxFileSize     =   128
   End
End
Attribute VB_Name = "frmConfig2"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub CbMaster1_DatafileOpened(ByVal DatafileOpened As String, Response As Integer)
    lstCategory.ListIndex = 0
End Sub

Private Sub edtODBCLockAttempts_KeyPress(KeyAscii As Integer)
    If (KeyAscii < Asc("0") Or KeyAscii > Asc("9")) And KeyAscii <> Asc("-") Then
        KeyAscii = 0
    End If
End Sub

Private Sub Form_Load()
    HelpContextID = Edit_Configuration_File

    Left = (Screen.Width - Width) / 2   ' Center form horizontally.
    Top = (Screen.Height - Height) / 2  ' Center form vertically.

    FrmDbErr3 = False

    'open the configuration file with the master
    CbMaster1.serverId = svrId
    CbMaster1.ProcessId = portId
    CbMaster1.password = passwrd
    CbMaster1.userName = accountId

    CbMaster1.DatabaseName = configName
    CbMaster1.RefreshFiles
    'cbCmbLogOptions.object.ListIndex = 0

    DoEvents
'    lstCategory.ListIndex = 1
End Sub

Private Sub cmbLogOptions_SelChange()
    If cmbLogOptions.object.ListIndex = 0 Then
        CbMaster1.SetField "log", LOG4ALWAYS
    ElseIf cmbLogOptions.ListIndex = 1 Then
        CbMaster1.SetField "log", LOG4TRANS
    End If
End Sub

Private Sub cmbOpenMode_SelChange()
    Select Case cmbOpenMode.object.ListIndex
        Case 0
            CbMaster1.SetField "openmode", OPEN4DENY_NONE
        Case 1
            CbMaster1.SetField "openmode", OPEN4DENY_WRITE
        Case 2
            CbMaster1.SetField "openmode", OPEN4DENY_RW
    End Select
End Sub

Private Sub cmbOptimize_SelChange()
    Select Case cmbOptimize.object.ListIndex
        Case 0
            CbMaster1.SetField "optimize", OPT4ALL
        Case 1
            CbMaster1.SetField "optimize", OPT4OFF
        Case 2
            CbMaster1.SetField "optimize", OPT4EXCLUSIVE
    End Select
End Sub

Private Sub cmbOptimizeWr_SelChange()
    Select Case cmbOptimizeWr.object.ListIndex
        Case 0
            CbMaster1.SetField "optimizewr", OPT4ALL
        Case 1
            CbMaster1.SetField "optimizewr", OPT4OFF
        Case 2
            CbMaster1.SetField "optimizewr", OPT4EXCLUSIVE
    End Select
End Sub

Private Sub cmdBrowse_Click(Index As Integer)
    Select Case Index
        Case 2   ' DEFPATH
            Form23.Show 1
            If Len(Trim(browseDlgPath)) > 0 Then
                edtDefPath.Text = browseDlgPath
            End If
        Case 3   ' SYSPATH
            Form23.Show 1
            If Len(Trim(browseDlgPath)) > 0 Then
                SystemPath.Text = browseDlgPath
            End If
    End Select
End Sub

Private Sub cmdBrowseErr_Click(Index As Integer)
    showWarn
    dlgErrFile.fileName = Trim(ErrName.Text)
    dlgErrFile.ShowOpen
    If Trim(dlgErrFile.fileName) <> "" Then
        ErrName.Text = UCase(dlgErrFile.fileName)
    End If
End Sub

Private Sub cmdBrowseLog_Click(Index As Integer)
    showWarn
    If Len(Trim(LogName.Text)) > 0 Then
        dlgLogFile.fileName = Trim(LogName.Text)
    Else
        dlgLogFile.fileName = "S4SERVER.LOG"
        If Len(Trim(SystemPath.Text)) > 0 Then
            dlgLogFile.InitDir = Trim(SystemPath.Text)
        Else
            dlgLogFile.InitDir = Trim(code4serverCurrentDirectory(gCode4))
        End If
    End If
    dlgLogFile.fileName = Trim(LogName.Text)
    dlgLogFile.ShowOpen
    If Trim(dlgLogFile.fileName) <> "" Then
        LogName.Text = UCase(dlgLogFile.fileName)
    End If
End Sub

Private Sub Command1_Click()
' OK button clicked
    If CbMaster1.RecordChanged Then
'    If d4changed(CbMaster1.DATA4, -1) <> 0 Then
        Dim mb As Integer
        Dim IPaddress As Variant
        IPaddress = "123"

        CbMaster1.GetField "TCPBEGIN", IPaddress
        If validateIP(IPaddress) = False Then
            mb = MsgBox("You have specified an invalid IP address." + vbCrLf + "Discard changes?", vbExclamation, _
                        "Lower IP Address Range Limit")
            edtTCPbegin.SetFocus
            Exit Sub
        End If

        CbMaster1.GetField "TCPEND", IPaddress
        If validateIP(IPaddress) = False Then
            mb = MsgBox("You have specified an invalid IP address.", vbExclamation, _
                        "Upper IP Address Range Limit")
            edtTCPend.SetFocus
            Exit Sub
        End If

        Dim la&
        la = val(edtODBCLockAttempts.Text)
        If la < 1 And la <> -1 Then
            MsgBox "ODBC Lock Attempts is invalid. It must be -1 or greater than 0.", vbExclamation, "ODBC Lock Attempts"
            Exit Sub
        Else
            edtODBCLockAttempts.Text = Trim(str(la))
        End If

        Call CbMaster1.SetField("LOGNAME", LTrim(LogName.Text))
        Call CbMaster1.SetField("ERRNAME", LTrim(ErrName.Text))
        Call CbMaster1.SetField("DEFPATH", LTrim(edtDefPath.Text))
        Call CbMaster1.SetField("SYSTEMPATH", LTrim(SystemPath.Text))

        MsgBox "Changes will take effect the next time the server is started.", vbInformation, Me.caption
    End If

    CbMaster1.FlushRecord
    Unload Me
End Sub

Private Sub Command2_Click()
   CbMaster1.Undo
   Unload Me
End Sub

Private Sub CbMaster1_Reposition(ByVal Action As Integer)
    Dim Result
    Result = ""

    CbMaster1.GetField "log", Result

    If val(Result) = LOG4TRANS Then
       cmbLogOptions.object.ListIndex = 1
    ElseIf val(Result) = LOG4ALWAYS Then
       cmbLogOptions.object.ListIndex = 0
    End If

    CbMaster1.GetField "openmode", Result
    If val(Result) = OPEN4DENY_NONE Then
       cmbOpenMode.object.ListIndex = 0
    ElseIf val(Result) = OPEN4DENY_WRITE Then
       cmbOpenMode.object.ListIndex = 1
    ElseIf val(Result) = OPEN4DENY_RW Then
       cmbOpenMode.object.ListIndex = 2
    End If

    CbMaster1.GetField "optimize", Result
    If val(Result) = OPT4OFF Then
       cmbOptimize.object.ListIndex = 1
    ElseIf val(Result) = OPT4ALL Then
       cmbOptimize.object.ListIndex = 0
    ElseIf val(Result) = OPT4EXCLUSIVE Then
       cmbOptimize.object.ListIndex = 2
    End If

    CbMaster1.GetField "optimizewr", Result
    If val(Result) = OPT4OFF Then
       cmbOptimizeWr.object.ListIndex = 1
    ElseIf val(Result) = OPT4ALL Then
       cmbOptimizeWr.object.ListIndex = 0
    ElseIf val(Result) = OPT4EXCLUSIVE Then
       cmbOptimizeWr.object.ListIndex = 2
    End If

    CbMaster1.GetField "Collate", Result
    Select Case UCase(Trim(Result))
        Case "GENERAL"
            optCollateGeneral.value = True
        Case "MACHINE"
            optCollateMachine.value = True
        Case Else
            optCollateGeneral.value = False
            optCollateMachine.value = False
    End Select
End Sub

Private Sub lstCategory_Click()
    With lstCategory
        Select Case .List(.ListIndex)
            Case "Files/Directories"
                boxFiles.Visible = True
                boxAccess.Visible = False
                boxNetworking.Visible = False
                boxMemory.Visible = False
                boxODBC.Visible = False
            Case "File Access"
                boxAccess.Visible = True
                boxFiles.Visible = False
                boxNetworking.Visible = False
                boxMemory.Visible = False
                boxODBC.Visible = False
            Case "Networking"
                boxNetworking.Visible = True
                boxFiles.Visible = False
                boxAccess.Visible = False
                boxMemory.Visible = False
                boxODBC.Visible = False
            Case "Memory"
                boxMemory.Visible = True
                boxFiles.Visible = False
                boxAccess.Visible = False
                boxNetworking.Visible = False
                boxODBC.Visible = False
            Case "ODBC"
                boxODBC.Visible = True
                boxFiles.Visible = False
                boxAccess.Visible = False
                boxNetworking.Visible = False
                boxMemory.Visible = False
        End Select
    End With
End Sub

Private Sub optCollateGeneral_Click()
    If optCollateGeneral.value Then
        Dim currentVal
        currentVal = "ff"
        CbMaster1.GetField "Collate", currentVal
        If StrComp(Trim(currentVal), "General") <> 0 Then
            CbMaster1.SetField "Collate", "General"
        End If
    End If
End Sub

Private Sub optCollateMachine_Click()
    If optCollateMachine.value Then
        Dim currentVal
        currentVal = "ff"
        CbMaster1.GetField "Collate", currentVal
        If StrComp(Trim(currentVal), "Machine") <> 0 Then
            CbMaster1.SetField "Collate", "Machine"
        End If
    End If
End Sub
