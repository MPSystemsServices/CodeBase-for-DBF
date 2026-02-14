VERSION 4.00
Begin VB.Form Form3
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Server Configuration File"
   ClientHeight    =   5370
   ClientLeft      =   2835
   ClientTop       =   2565
   ClientWidth     =   8730
   Height          =   5775
   Icon            =   "suconfig.frx":0000
   Left            =   2775
   LinkTopic       =   "Form3"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   5370
   ScaleWidth      =   8730
   ShowInTaskbar   =   0   'False
   Top             =   2220
   Width           =   8850
   Begin VB.Frame Frame4
      Caption         =   "ODBC Properties"
      Height          =   1815
      Left            =   5160
      TabIndex        =   38
      Top             =   2760
      Width           =   3495
      Begin VB.OptionButton optCollate
         Caption         =   "General Gollation"
         Height          =   255
         Index           =   1
         Left            =   240
         TabIndex        =   42
         Top             =   840
         Width           =   1815
      End
      Begin VB.OptionButton optCollate
         Caption         =   "Machine Collation"
         Height          =   255
         Index           =   0
         Left            =   240
         TabIndex        =   41
         Top             =   480
         Width           =   1815
      End
      Begin VB.Label Label15
         Caption         =   "Lock Attempts:"
         Height          =   255
         Left            =   120
         TabIndex        =   40
         Top             =   1320
         Width           =   1335
      End
      Begin Edt4Lib.CbEdit edtODBCLockAttempts
         Height          =   315
         Left            =   1800
         TabIndex        =   39
         Top             =   1275
         Width           =   855
         AllowPrompt     =   -1  'True
         AutoMonthFillin =   -1  'True
         DataField       =   "ODBCNMLOCK"
         DataSource      =   "CbMaster1"
         FormatStripping =   0
         FormatType      =   3
         NegativeColor   =   -2147483630
         _Version        =   65536
         _ExtentX        =   1508
         _ExtentY        =   556
         _StockProps     =   125
         BackColor       =   -2147483643
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851}
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Decimals        =   0
         HideSelection   =   -1  'True
         Text            =   ""
         ToolTipText     =   "CbEdit1"
         ContainerID     =   21799376
      End
   End
   Begin VB.CommandButton cmdMemory
      Caption         =   "&Memory"
      Height          =   375
      Left            =   7440
      TabIndex        =   1
      Top             =   4920
      Width           =   1215
   End
   Begin VB.Frame Frame1
      Caption         =   "File Access"
      Height          =   2535
      Left            =   45
      TabIndex        =   32
      Top             =   2220
      Width           =   4710
      Begin VB.Label Label11
         Caption         =   "Optimize Writes:"
         Height          =   375
         Left            =   120
         TabIndex        =   36
         Top             =   2085
         Width           =   1335
      End
      Begin VB.Label Label10
         Caption         =   "Optimize Reads:"
         Height          =   255
         Left            =   120
         TabIndex        =   35
         Top             =   1665
         Width           =   1335
      End
      Begin Edt4Lib.CbEdit KeepOpen
         Height          =   315
         Left            =   1680
         TabIndex        =   13
         Top             =   780
         Width           =   495
         AllowPrompt     =   -1  'True
         AutoMonthFillin =   -1  'True
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
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851}
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         HideSelection   =   -1  'True
         Text            =   " "
         ToolTipText     =   "Determines if the server keeps data files open when they are not currently in use by any application"
         ContainerID     =   21799376
      End
      Begin VB.Label Label3
         Caption         =   "Keep Files Open:"
         Height          =   255
         Left            =   120
         TabIndex        =   34
         Top             =   825
         Width           =   1335
      End
      Begin VB.Label Label4
         Caption         =   "Log File Changes:"
         Height          =   255
         Left            =   120
         TabIndex        =   33
         Top             =   1245
         Width           =   1455
      End
      Begin Cmb4Lib.CbCombo cmbLogOptions
         Height          =   315
         Left            =   1680
         TabIndex        =   14
         Top             =   1200
         Width           =   2295
         AllowPrompt     =   -1  'True
         AutoMonthFillin =   -1  'True
         Style           =   2
         ListItems       =   $"suconfig.frx":030A
         _Version        =   65536
         _ExtentX        =   4048
         _ExtentY        =   556
         _StockProps     =   221
         Text            =   "cmbLogOptions"
         BackColor       =   -2147483643
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851}
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Text            =   "cmbLogOptions"
         ToolTipEnabled  =   -1  'True
         ToolTipText     =   "Determines when the database server logs database updates"
         ContainerID     =   21799376
      End
      Begin Cmb4Lib.CbCombo cmbOpenMode
         Height          =   315
         Left            =   1680
         TabIndex        =   12
         Top             =   360
         Width           =   1815
         AllowPrompt     =   -1  'True
         AutoMonthFillin =   -1  'True
         ListItems       =   $"suconfig.frx":032E
         _Version        =   65536
         _ExtentX        =   3201
         _ExtentY        =   556
         _StockProps     =   221
         Text            =   "cmbOpenMode"
         BackColor       =   -2147483643
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851}
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Text            =   "cmbOpenMode"
         ToolTipEnabled  =   -1  'True
         ToolTipText     =   "Determines the access mode used by server to open tables"
         ContainerID     =   21799376
      End
      Begin Cmb4Lib.CbCombo cmbOptimize
         Height          =   315
         Left            =   1680
         TabIndex        =   15
         Top             =   1620
         Width           =   2895
         AllowPrompt     =   -1  'True
         AutoMonthFillin =   -1  'True
         Style           =   2
         ListItems       =   $"suconfig.frx":0350
         _Version        =   65536
         _ExtentX        =   5106
         _ExtentY        =   556
         _StockProps     =   221
         Text            =   "cmbOptimize"
         BackColor       =   -2147483643
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851}
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Text            =   "cmbOptimize"
         ToolTipEnabled  =   -1  'True
         ToolTipText     =   "Determines when server should optimize (cache) disk reads"
         ContainerID     =   21799376
      End
      Begin Cmb4Lib.CbCombo cmbOptimizeWr
         Height          =   315
         Left            =   1680
         TabIndex        =   16
         Top             =   2040
         Width           =   2895
         AllowPrompt     =   -1  'True
         AutoMonthFillin =   -1  'True
         Style           =   2
         ListItems       =   $"suconfig.frx":0380
         _Version        =   65536
         _ExtentX        =   5106
         _ExtentY        =   556
         _StockProps     =   221
         Text            =   "cmbOptimizeWr"
         BackColor       =   -2147483643
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851}
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Text            =   "cmbOptimizeWr"
         ToolTipEnabled  =   -1  'True
         ToolTipText     =   "Determines when server should optimize (cache) disk writes"
         ContainerID     =   21799376
      End
      Begin VB.Label Label6
         Caption         =   "Open/Create Mode:"
         Height          =   255
         Left            =   120
         TabIndex        =   37
         Top             =   405
         Width           =   1575
      End
   End
   Begin VB.Frame Frame2
      Caption         =   "Server File/Path Information"
      Height          =   2130
      Left            =   45
      TabIndex        =   27
      Top             =   45
      Width           =   5430
      Begin VB.CommandButton cmdBrowse
         Caption         =   "..."
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851}
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
         TabIndex        =   9
         Top             =   1155
         Width           =   315
      End
      Begin VB.CommandButton cmdBrowse
         Caption         =   "..."
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851}
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
         TabIndex        =   11
         Top             =   1575
         Width           =   315
      End
      Begin VB.CommandButton cmdBrowseErr
         Caption         =   "..."
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851}
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
         TabIndex        =   7
         Top             =   720
         Width           =   315
      End
      Begin VB.CommandButton cmdBrowseLog
         Caption         =   "..."
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851}
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
         TabIndex        =   5
         Top             =   345
         Width           =   315
      End
      Begin VB.Label Label2
         Caption         =   "Error Log File:"
         Height          =   300
         Left            =   150
         TabIndex        =   31
         Top             =   780
         Width           =   1350
      End
      Begin Edt4Lib.CbEdit LogName
         Height          =   315
         Left            =   1560
         TabIndex        =   4
         Top             =   345
         Width           =   3300
         AllowPrompt     =   -1  'True
         AutoMonthFillin =   -1  'True
         DataField       =   "LOGNAME"
         DataSource      =   "CbMaster1"
         _Version        =   65536
         _ExtentX        =   5821
         _ExtentY        =   556
         _StockProps     =   125
         BackColor       =   -2147483643
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851}
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         HideSelection   =   -1  'True
         Text            =   ""
         ToolTipText     =   "Name of the file used by server to log transactions and updates"
         ContainerID     =   21799376
      End
      Begin VB.Label Label5
         Caption         =   "Log File Name:"
         Height          =   225
         Left            =   150
         TabIndex        =   30
         Top             =   390
         Width           =   1350
      End
      Begin VB.Label Label1
         Caption         =   "Default Directory:"
         Height          =   240
         Left            =   150
         TabIndex        =   29
         Top             =   1200
         Width           =   1350
      End
      Begin Edt4Lib.CbEdit edtDefPath
         Height          =   315
         Left            =   1560
         TabIndex        =   8
         Top             =   1155
         Width           =   3300
         AllowPrompt     =   -1  'True
         AutoMonthFillin =   -1  'True
         DataField       =   "DEFPATH"
         DataSource      =   "CbMaster1"
         _Version        =   65536
         _ExtentX        =   5821
         _ExtentY        =   556
         _StockProps     =   125
         BackColor       =   -2147483643
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851}
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         HideSelection   =   -1  'True
         MaxLength       =   128
         Text            =   ""
         ToolTipText     =   "Default directory where server looks for data files and log files"
         ContainerID     =   21799376
      End
      Begin VB.Label Label20
         Caption         =   "System Directory:"
         Height          =   225
         Left            =   150
         TabIndex        =   28
         Top             =   1605
         Width           =   1350
      End
      Begin Edt4Lib.CbEdit SystemPath
         Height          =   315
         Left            =   1560
         TabIndex        =   10
         Top             =   1575
         Width           =   3300
         AllowPrompt     =   -1  'True
         AutoMonthFillin =   -1  'True
         DataField       =   "SYSTEMPATH"
         DataSource      =   "CbMaster1"
         _Version        =   65536
         _ExtentX        =   5821
         _ExtentY        =   556
         _StockProps     =   125
         BackColor       =   -2147483643
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851}
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         HideSelection   =   -1  'True
         Text            =   ""
         ToolTipText     =   "Default directory where server looks for Account and Privileges system files"
         ContainerID     =   21799376
      End
      Begin Edt4Lib.CbEdit ErrName
         Height          =   315
         Left            =   1560
         TabIndex        =   6
         Top             =   750
         Width           =   3300
         AllowPrompt     =   -1  'True
         AutoMonthFillin =   -1  'True
         DataField       =   "ERRNAME"
         DataSource      =   "CbMaster1"
         _Version        =   65536
         _ExtentX        =   5821
         _ExtentY        =   556
         _StockProps     =   125
         BackColor       =   -2147483643
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851}
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         HideSelection   =   -1  'True
         Text            =   ""
         ToolTipText     =   "Name of the file used by server to log error information"
         ContainerID     =   21799376
      End
   End
   Begin VB.Frame Frame3
      Caption         =   "Networking Parameters"
      Height          =   2535
      Left            =   5640
      TabIndex        =   21
      Top             =   45
      Width           =   3015
      Begin Edt4Lib.CbEdit ProcessId
         Height          =   315
         Left            =   1365
         TabIndex        =   17
         Top             =   345
         Width           =   1500
         AllowPrompt     =   -1  'True
         AutoMonthFillin =   -1  'True
         DataField       =   "PROCESSID"
         DataSource      =   "CbMaster1"
         FormatType      =   3
         _Version        =   65536
         _ExtentX        =   2646
         _ExtentY        =   556
         _StockProps     =   125
         Text            =   " 0"
         BackColor       =   -2147483643
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851}
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Decimals        =   0
         HideSelection   =   -1  'True
         Text            =   " 0"
         ToolTipText     =   "The process identification (or port id) for the server"
         ContainerID     =   21799376
      End
      Begin VB.Label Label7
         Caption         =   "Port Id:"
         Height          =   240
         Left            =   120
         TabIndex        =   26
         Top             =   390
         Width           =   540
      End
      Begin VB.Label Label9
         Caption         =   "End:"
         Height          =   240
         Left            =   120
         TabIndex        =   25
         Top             =   2085
         Width           =   540
      End
      Begin VB.Label Label8
         Caption         =   "Begin:"
         Height          =   240
         Left            =   120
         TabIndex        =   24
         Top             =   1680
         Width           =   540
      End
      Begin Edt4Lib.CbEdit edtTCPbegin
         Height          =   315
         Left            =   1365
         TabIndex        =   19
         Top             =   1635
         Width           =   1515
         AllowPrompt     =   -1  'True
         AutoMonthFillin =   -1  'True
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
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851}
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         HideSelection   =   -1  'True
         Text            =   "   .   .   .   "
         ToolTipText     =   "Lowest allowed IP address"
         ContainerID     =   21799376
      End
      Begin Edt4Lib.CbEdit edtTCPend
         Height          =   315
         Left            =   1365
         TabIndex        =   20
         Top             =   2055
         Width           =   1515
         AllowPrompt     =   -1  'True
         AutoMonthFillin =   -1  'True
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
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851}
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         HideSelection   =   -1  'True
         Text            =   "   .   .   .   "
         ToolTipText     =   "Highest allowed IP address"
         ContainerID     =   21799376
      End
      Begin VB.Label Label12
         Caption         =   "--- Default IP Address Range ---"
         Height          =   195
         Left            =   765
         TabIndex        =   23
         Top             =   1260
         Width           =   2115
      End
      Begin Edt4Lib.CbEdit edtODBCport
         Height          =   315
         Left            =   1365
         TabIndex        =   18
         Top             =   840
         Width           =   1500
         AllowPrompt     =   -1  'True
         AutoMonthFillin =   -1  'True
         DataField       =   "ODBCPortNo"
         DataSource      =   "CbMaster1"
         _Version        =   65536
         _ExtentX        =   2646
         _ExtentY        =   556
         _StockProps     =   125
         BackColor       =   -2147483643
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851}
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         HideSelection   =   -1  'True
         Text            =   ""
         ToolTipText     =   "CbEdit1"
         ContainerID     =   21799376
      End
      Begin VB.Label Label13
         Caption         =   "ODBC Port Id:"
         Height          =   255
         Left            =   120
         TabIndex        =   22
         Top             =   885
         Width           =   1095
      End
   End
   Begin VB.CommandButton Command2
      Cancel          =   -1  'True
      Caption         =   "&Cancel"
      Height          =   375
      Left            =   6120
      TabIndex        =   3
      Top             =   4920
      Width           =   1215
   End
   Begin VB.CommandButton Command1
      Caption         =   "OK"
      Default         =   -1  'True
      Height          =   375
      Left            =   4800
      TabIndex        =   0
      Top             =   4920
      Width           =   1215
   End
   Begin MSComDlg.CommonDialog dlgErrFile
      Left            =   2520
      Top             =   5040
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
      DialogTitle     =   "Browse"
      Filter          =   "Error Log files (*.err)|*.err|xBASE data files|*.dbf;*.cdx;*.fpt;*.mdx;*.dbt;*.ntx|All Files (*.*)|*.*"
      FilterIndex     =   1
      Flags           =   4
      MaxFileSize     =   128
   End
   Begin MSComDlg.CommonDialog dlgLogFile
      Left            =   2040
      Top             =   5040
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
      DialogTitle     =   "Browse"
      Filter          =   "Log files (*.log)|*.log|xBASE data files|*.dbf;*.cdx;*.fpt;*.mdx;*.dbt;*.ntx|All Files (*.*)|*.*"
      FilterIndex     =   1
      Flags           =   4
      MaxFileSize     =   128
   End
   Begin Mtr4LibCtl.CbMaster CbMaster1
      Height          =   360
      Left            =   1080
      TabIndex        =   2
      TabStop         =   0   'False
      Top             =   5160
      Width           =   735
      _Version        =   65536
      _ExtentX        =   1296
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
      AppendEnabled   =   -1  'True
      BottomVisible   =   0   'False
      BottomEnabled   =   -1  'True
      CaptionVisible  =   -1  'True
      DataTag         =   ""
      DeletedVisible  =   0   'False
      DeletedEnabled  =   -1  'True
      FlushVisible    =   0   'False
      FlushEnabled    =   -1  'True
      NextPageVisible =   0   'False
      NextPageEnabled =   -1  'True
      NextVisible     =   0   'False
      NextEnabled     =   -1  'True
      PackVisible     =   0   'False
      PackEnabled     =   -1  'True
      PreviousPageVisible=   0   'False
      PreviousPageEnabled=   -1  'True
      PreviousVisible =   0   'False
      PreviousEnabled =   -1  'True
      RecNoEnabled    =   0   'False
      RefreshVisible  =   0   'False
      RefreshEnabled  =   -1  'True
      ReindexVisible  =   0   'False
      ReindexEnabled  =   -1  'True
      SearchVisible   =   0   'False
      SearchEnabled   =   -1  'True
      SliderEnabled   =   0   'False
      TagListEnabled  =   0   'False
      TopVisible      =   0   'False
      ToolTipsEnabled =   -1  'True
      TopEnabled      =   -1  'True
      UndoVisible     =   0   'False
      UndoEnabled     =   -1  'True
      Visibility      =   0   'False
      ContainerID     =   21799376
      NameUnique      =   "CbMaster1"
   End
End
Attribute VB_Name = "Form3"
Attribute VB_Creatable = False
Attribute VB_Exposed = False
Option Explicit
Dim bMouseDown As Boolean

'used in IP address text handling routines
Dim location&, curStart%, curEnd%, bIsBackspace As Boolean


Private Sub CbMaster1_Reposition(ByVal Action As Integer)
    Dim Result
    Result = ""

    CbMaster1.GetField "log", Result

    If Val(Result) = LOG4TRANS Then
       cmbLogOptions.object.ListIndex = 1
    ElseIf Val(Result) = LOG4ALWAYS Then
       cmbLogOptions.object.ListIndex = 0
    End If

    CbMaster1.GetField "openmode", Result
    If Val(Result) = OPEN4DENY_NONE Then
       cmbOpenMode.object.ListIndex = 0
    ElseIf Val(Result) = OPEN4DENY_WRITE Then
       cmbOpenMode.object.ListIndex = 1
    ElseIf Val(Result) = OPEN4DENY_RW Then
       cmbOpenMode.object.ListIndex = 2
    End If

    CbMaster1.GetField "optimize", Result
    If Val(Result) = OPT4OFF Then
       cmbOptimize.object.ListIndex = 1
    ElseIf Val(Result) = OPT4ALL Then
       cmbOptimize.object.ListIndex = 0
    ElseIf Val(Result) = OPT4EXCLUSIVE Then
       cmbOptimize.object.ListIndex = 2
    End If

    CbMaster1.GetField "optimizewr", Result
    If Val(Result) = OPT4OFF Then
       cmbOptimizeWr.object.ListIndex = 1
    ElseIf Val(Result) = OPT4ALL Then
       cmbOptimizeWr.object.ListIndex = 0
    ElseIf Val(Result) = OPT4EXCLUSIVE Then
       cmbOptimizeWr.object.ListIndex = 2
    End If
End Sub

Private Sub CbMaster1_Validation(ByVal Action As Integer, Response As Integer)
    If CbMaster1.RecordChanged Then
        Dim mb As Integer
        Dim IPaddress As Variant
        IPaddress = "123"

        CbMaster1.GetField "TCPBEGIN", IPaddress
        If validateIP(IPaddress) = False Then
            mb = MsgBox("You have specified an invalid IP address." + vbCrLf + "Discard changes?", vbYesNo + vbDefaultButton2 + vbExclamation, _
                        "Lower IP Address Range Limit")
            If mb = vbYes Then
                CbMaster1.Undo
            Else
                Response = 0
                edtTCPbegin.SetFocus
            End If
        End If

        CbMaster1.GetField "TCPEND", IPaddress
        If validateIP(IPaddress) = False Then
            mb = MsgBox("You have specified an invalid IP address." + vbCrLf + "Discard changes?", vbYesNo + vbDefaultButton2 + vbExclamation, _
                        "Upper IP Address Range Limit")
            If mb = vbYes Then
                CbMaster1.Undo
            Else
                Response = 0
                edtTCPend.SetFocus
            End If
        End If

        Call CbMaster1.SetField("LOGNAME", LTrim(logName.Text))
        Call CbMaster1.SetField("ERRNAME", LTrim(ErrName.Text))
        Call CbMaster1.SetField("DEFPATH", LTrim(edtDefPath.Text))
        Call CbMaster1.SetField("SYSTEMPATH", LTrim(systemPath.Text))

        If Response <> 0 Then
            MsgBox "Changes made to the server configuration file will take effect the next time the server is started.", vbInformation, Me.caption
        End If
    End If
End Sub



Private Sub cmbLogOptions_SelChange()
    If cmbLogOptions.object.ListIndex = 0 Then
        CbMaster1.SetField "log", LOG4ALWAYS
    ElseIf cmbLogOptions.ListIndex = 1 Then
        CbMaster1.SetField "log", LOG4TRANS
    End If
End Sub

Private Sub CbMaster1_Error(ErrorCode As Long, Description As String, Response As Integer)
    FrmDbErr3 = True

    MasterErr CbMaster1, Me, TYPE_SERVER, ErrorCode, Description
    Response = 0

    If ErrorCode = -10 Then Unload Me

    'If ErrorCode = -10 Then
    '    'could not open the data file
    '    MsgBox "Could not open the server configuration file" & Chr(13) & Chr(10) & "Ensure that your account has the necessary authorization to open this file", vbOKOnly + vbExclamation, "Error Opening File"
    '    Response = 0
    '    Unload Me
    'End If
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

Private Sub cmbOptimizeWr_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
    bMouseDown = True
End Sub

Private Sub cmbOptimizeWr_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
    bMouseDown = False
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
                systemPath.Text = browseDlgPath
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
    If Len(Trim(logName.Text)) > 0 Then
        dlgLogFile.fileName = Trim(logName.Text)
    Else
        dlgLogFile.fileName = "S4SERVER.LOG"
        If Len(Trim(systemPath.Text)) > 0 Then
            dlgLogFile.InitDir = Trim(systemPath.Text)
        Else
            dlgLogFile.InitDir = Trim(code4serverCurrentDirectory(gCode4))
        End If
    End If
    dlgLogFile.fileName = Trim(logName.Text)
    dlgLogFile.ShowOpen
    If Trim(dlgLogFile.fileName) <> "" Then
        logName.Text = UCase(dlgLogFile.fileName)
    End If
End Sub

Private Sub cmdMemory_Click()
    frmMemory.Show 1
End Sub

Private Sub Command1_Click()
' OK button clicked
    CbMaster1.FlushRecord
    Unload Me
End Sub

Private Sub Command1_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
    bMouseDown = True
End Sub

Private Sub Command1_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
    bMouseDown = False
End Sub

Private Sub Command2_Click()
   CbMaster1.Undo
   Unload Me
End Sub

Private Sub edtDefPath_GotFocus()
    If Not bMouseDown Then SelIt edtDefPath
End Sub

Private Sub edtDefPath_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
    bMouseDown = True
End Sub

Private Sub edtDefPath_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
    bMouseDown = False
End Sub


Private Sub ErrName_GotFocus()
    If Not bMouseDown Then SelIt ErrName
End Sub

Private Sub ErrName_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
    If Not bMouseDown Then SelIt ErrName
End Sub

Private Sub ErrName_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
    bMouseDown = False
End Sub

Private Sub Form_Load()
    HelpContextID = Edit_Configuration_File

    Left = (Screen.Width - Width) / 2   ' Center form horizontally.
    Top = (Screen.Height - Height) / 2  ' Center form vertically.

    FrmDbErr3 = False

    'open the configuration file with the master
    CbMaster1.serverId = svrId
    CbMaster1.processId = portId
    CbMaster1.password = passwrd
    CbMaster1.userName = accountId

    CbMaster1.DatabaseName = configName
    CbMaster1.RefreshFiles
    'cbCmbLogOptions.object.ListIndex = 0
End Sub

Private Sub Form_Unload(Cancel As Integer)
    FrmDbErr3 = False
End Sub

Private Sub KeepOpen_GotFocus()
    If Not bMouseDown Then SelIt KeepOpen
End Sub

Private Sub KeepOpen_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
    bMouseDown = True
End Sub

Private Sub KeepOpen_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
    bMouseDown = False
End Sub

Private Sub logName_GotFocus()
    If Not bMouseDown Then SelIt logName
End Sub

Private Sub logName_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
    bMouseDown = True
End Sub

Private Sub logName_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
    bMouseDown = False
End Sub

Private Sub processId_GotFocus()
    If Not bMouseDown Then SelIt processId
End Sub

Private Sub processId_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
    bMouseDown = True
End Sub

Private Sub processId_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
    bMouseDown = False
End Sub

Private Sub systemPath_GotFocus()
    If Not bMouseDown Then SelIt systemPath
End Sub

Private Sub systemPath_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
    bMouseDown = True
End Sub

Private Sub systemPath_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
    bMouseDown = False
End Sub
