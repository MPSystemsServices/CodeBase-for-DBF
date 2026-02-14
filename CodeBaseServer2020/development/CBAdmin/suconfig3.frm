VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "COMDLG32.OCX"
Begin VB.Form frmConfig
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Server Settings"
   ClientHeight    =   3570
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   7800
   Icon            =   "suconfig3.frx":0000
   LinkTopic       =   "Form2"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   3570
   ScaleWidth      =   7800
   ShowInTaskbar   =   0   'False
   StartUpPosition =   2  'CenterScreen
   Begin VB.CommandButton cmdCancel
      Cancel          =   -1  'True
      Caption         =   "&Cancel"
      Height          =   350
      Left            =   6615
      TabIndex        =   69
      Top             =   3150
      Width           =   1130
   End
   Begin VB.CommandButton cmdOK
      Caption         =   "OK"
      Default         =   -1  'True
      Height          =   350
      Left            =   5400
      TabIndex        =   68
      Top             =   3150
      Width           =   1130
   End
   Begin VB.ListBox lstCategory
      Height          =   3180
      ItemData        =   "suconfig3.frx":000C
      Left            =   105
      List            =   "suconfig3.frx":0022
      TabIndex        =   0
      Top             =   105
      Width           =   2055
   End
   Begin MSComDlg.CommonDialog dlgLogFile
      Left            =   4305
      Top             =   3090
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
      DefaultExt      =   "log"
      DialogTitle     =   "Browse"
      Filter          =   "Log files (*.log)|*.log|xBASE data files|*.dbf;*.cdx;*.fpt;*.mdx;*.dbt;*.ntx|All Files (*.*)|*.*"
      FilterIndex     =   1
      Flags           =   4
      MaxFileSize     =   128
   End
   Begin MSComDlg.CommonDialog dlgErrFile
      Left            =   3630
      Top             =   3045
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
      DialogTitle     =   "Browse"
      Filter          =   "Error Log files (*.err)|*.err|xBASE data files|*.dbf;*.cdx;*.fpt;*.mdx;*.dbt;*.ntx|All Files (*.*)|*.*"
      FilterIndex     =   1
      Flags           =   4
      MaxFileSize     =   128
   End
   Begin MSComDlg.CommonDialog dlgBLog
      Left            =   2700
      Top             =   2940
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
      DefaultExt      =   "log"
      DialogTitle     =   "Browse"
      Filter          =   "Log files (*.log)|*.log|xBASE data files|*.dbf;*.cdx;*.fpt;*.mdx;*.dbt;*.ntx|All Files (*.*)|*.*"
      FilterIndex     =   1
      Flags           =   4
      MaxFileSize     =   128
   End
   Begin VB.PictureBox boxAccess
      BorderStyle     =   0  'None
      Height          =   2790
      Left            =   2385
      ScaleHeight     =   2790
      ScaleWidth      =   4890
      TabIndex        =   70
      Top             =   105
      Width           =   4890
      Begin VB.CheckBox chkLogFileFlush
         Caption         =   "Log File &Flush"
         Height          =   255
         Left            =   0
         TabIndex        =   72
         Tag             =   "LOGFLUSH"
         Top             =   1680
         Width           =   1455
      End
      Begin VB.TextBox txtMaxLogSize
         Height          =   315
         Left            =   1875
         TabIndex        =   41
         Tag             =   "MAXLOGSIZE"
         Top             =   1240
         Width           =   1335
      End
      Begin VB.ComboBox cmbOpenMode
         Height          =   315
         ItemData        =   "suconfig3.frx":0068
         Left            =   1875
         List            =   "suconfig3.frx":0075
         Style           =   2  'Dropdown List
         TabIndex        =   36
         Tag             =   "OPENMODE"
         Top             =   60
         Width           =   1905
      End
      Begin VB.ComboBox cmbOptimizeWrite
         Height          =   315
         ItemData        =   "suconfig3.frx":0098
         Left            =   1875
         List            =   "suconfig3.frx":00A6
         Style           =   2  'Dropdown List
         TabIndex        =   46
         Tag             =   "OPTIMIZEWR"
         Top             =   2440
         Width           =   2850
      End
      Begin VB.ComboBox cmbOptimize
         Height          =   315
         ItemData        =   "suconfig3.frx":00D8
         Left            =   1875
         List            =   "suconfig3.frx":00E6
         Style           =   2  'Dropdown List
         TabIndex        =   44
         Tag             =   "OPTIMIZE"
         Top             =   2040
         Width           =   2850
      End
      Begin VB.ComboBox cmbLogOptions
         Height          =   315
         ItemData        =   "suconfig3.frx":0118
         Left            =   1875
         List            =   "suconfig3.frx":0128
         Style           =   2  'Dropdown List
         TabIndex        =   39
         Tag             =   "LOG"
         Top             =   840
         Width           =   2850
      End
      Begin VB.CheckBox chkKeepOpen
         Caption         =   "&Keep Files Open"
         Height          =   225
         Left            =   0
         TabIndex        =   37
         Tag             =   "KEEPOPEN"
         Top             =   495
         Width           =   2640
      End
      Begin VB.Label Label17
         AutoSize        =   -1  'True
         Caption         =   "bytes"
         Height          =   195
         Left            =   3300
         TabIndex        =   42
         Top             =   1300
         Width           =   375
      End
      Begin VB.Label Label16
         AutoSize        =   -1  'True
         Caption         =   "Maximum Log File &Size:"
         Height          =   195
         Left            =   0
         TabIndex        =   40
         Top             =   1300
         Width           =   1650
      End
      Begin VB.Label Label6
         Caption         =   "&Open/Create Mode:"
         Height          =   255
         Left            =   0
         TabIndex        =   35
         Top             =   120
         Width           =   1575
      End
      Begin VB.Label Label4
         Caption         =   "&Log File Changes:"
         Height          =   255
         Left            =   0
         TabIndex        =   38
         Top             =   915
         Width           =   1455
      End
      Begin VB.Label Label10
         Caption         =   "Optimize &Reads:"
         Height          =   255
         Left            =   0
         TabIndex        =   43
         Top             =   2080
         Width           =   1335
      End
      Begin VB.Label Label11
         Caption         =   "Optimize &Writes:"
         Height          =   255
         Left            =   0
         TabIndex        =   45
         Top             =   2515
         Width           =   1335
      End
   End
   Begin VB.PictureBox boxFiles
      BorderStyle     =   0  'None
      Height          =   1755
      Left            =   2385
      ScaleHeight     =   1755
      ScaleWidth      =   5295
      TabIndex        =   71
      Top             =   105
      Width           =   5295
      Begin VB.TextBox txtSystemPath
         Height          =   315
         Left            =   1875
         TabIndex        =   57
         Tag             =   "SYSTEMPATH"
         Top             =   1260
         Width           =   2970
      End
      Begin VB.TextBox txtDefPath
         Height          =   315
         Left            =   1875
         TabIndex        =   54
         Tag             =   "DEFPATH"
         Top             =   840
         Width           =   2970
      End
      Begin VB.TextBox txtErrorLogName
         Height          =   315
         Left            =   1875
         TabIndex        =   51
         Tag             =   "ERRNAME"
         Top             =   420
         Width           =   2970
      End
      Begin VB.TextBox txtLogName
         Height          =   315
         Left            =   1875
         TabIndex        =   48
         Tag             =   "LOGNAME"
         Top             =   0
         Width           =   2970
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
         Left            =   4920
         TabIndex        =   55
         Top             =   840
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
         Left            =   4920
         TabIndex        =   58
         Top             =   1260
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
         Left            =   4920
         TabIndex        =   52
         Top             =   420
         Width           =   315
      End
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
         Left            =   4920
         TabIndex        =   49
         Top             =   0
         Width           =   315
      End
      Begin VB.Label Label2
         Caption         =   "&Error Log File:"
         Height          =   300
         Left            =   0
         TabIndex        =   50
         Top             =   480
         Width           =   1350
      End
      Begin VB.Label Label5
         Caption         =   "&Log File Name:"
         Height          =   225
         Left            =   0
         TabIndex        =   47
         Top             =   75
         Width           =   1350
      End
      Begin VB.Label Label1
         Caption         =   "&Default Directory:"
         Height          =   240
         Left            =   0
         TabIndex        =   53
         Top             =   915
         Width           =   1350
      End
      Begin VB.Label Label20
         Caption         =   "&System Directory:"
         Height          =   225
         Left            =   0
         TabIndex        =   56
         Top             =   1335
         Width           =   1350
      End
   End
   Begin VB.PictureBox boxNetworking
      BorderStyle     =   0  'None
      Height          =   2760
      Left            =   2385
      ScaleHeight     =   2760
      ScaleWidth      =   3615
      TabIndex        =   1
      Top             =   105
      Width           =   3615
      Begin VB.CheckBox chkRequireEncryption
         Caption         =   "Require E&ncryption"
         Height          =   255
         Left            =   0
         TabIndex        =   11
         Tag             =   "Encrypt"
         Top             =   2400
         Width           =   1935
      End
      Begin VB.TextBox txtODBCPortNo
         Height          =   315
         Left            =   1920
         TabIndex        =   5
         Tag             =   "ODBCPORTNO"
         Top             =   420
         Width           =   1485
      End
      Begin VB.TextBox txtProcessId
         Height          =   315
         Left            =   1920
         TabIndex        =   3
         Tag             =   "PROCESSID"
         Top             =   0
         Width           =   1485
      End
      Begin VB.Frame Frame3
         Caption         =   "&IP Address Range"
         Height          =   1215
         Left            =   0
         TabIndex        =   6
         Top             =   1000
         Width           =   3495
         Begin VB.TextBox txtTCPEnd
            Height          =   315
            Left            =   1875
            TabIndex        =   10
            Tag             =   "TCPEND"
            Top             =   680
            Width           =   1485
         End
         Begin VB.TextBox txtTCPBegin
            Height          =   315
            Left            =   1875
            TabIndex        =   8
            Tag             =   "TCPBEGIN"
            Top             =   240
            Width           =   1485
         End
         Begin VB.Label Label8
            Caption         =   "&Begin:"
            Height          =   240
            Left            =   120
            TabIndex        =   7
            Top             =   335
            Width           =   540
         End
         Begin VB.Label Label9
            Caption         =   "&End:"
            Height          =   240
            Left            =   120
            TabIndex        =   9
            Top             =   755
            Width           =   540
         End
      End
      Begin VB.Label Label13
         Caption         =   "&ODBC Port Id:"
         Height          =   255
         Left            =   0
         TabIndex        =   4
         Top             =   495
         Width           =   1095
      End
      Begin VB.Label Label7
         Caption         =   "&Port Id:"
         Height          =   240
         Left            =   0
         TabIndex        =   2
         Top             =   75
         Width           =   540
      End
   End
   Begin VB.PictureBox boxODBC
      BorderStyle     =   0  'None
      Height          =   2640
      Left            =   2385
      ScaleHeight     =   2640
      ScaleWidth      =   5055
      TabIndex        =   59
      Top             =   105
      Width           =   5055
      Begin VB.CheckBox chkODBCTrans
         Caption         =   "Enable &Transaction Processing"
         Height          =   255
         Left            =   0
         TabIndex        =   67
         Tag             =   "odbctrans"
         Top             =   2280
         Width           =   3015
      End
      Begin VB.CheckBox chkExpRecno
         Caption         =   "Expose &Record Number"
         Height          =   255
         Left            =   0
         TabIndex        =   66
         Tag             =   "exprecno"
         Top             =   1920
         Width           =   2415
      End
      Begin VB.CheckBox chkTrimBlanks
         Caption         =   "Trim Trailing &Blanks"
         Height          =   255
         Left            =   0
         TabIndex        =   65
         Tag             =   "trimblanks"
         Top             =   1560
         Width           =   2295
      End
      Begin VB.TextBox txtODBCLockAttempts
         Height          =   315
         Left            =   1575
         TabIndex        =   64
         Tag             =   "ODBCNMLOCK"
         Top             =   1080
         Width           =   855
      End
      Begin VB.Frame Frame1
         Caption         =   "&Collation"
         Height          =   975
         Left            =   0
         TabIndex        =   60
         Top             =   0
         Width           =   2055
         Begin VB.OptionButton optCollate
            Caption         =   "&General"
            Height          =   255
            Index           =   1
            Left            =   240
            TabIndex        =   62
            Tag             =   "COLLATE"
            Top             =   540
            Width           =   1095
         End
         Begin VB.OptionButton optCollate
            Caption         =   "&Machine"
            Height          =   255
            Index           =   0
            Left            =   240
            TabIndex        =   61
            Tag             =   "COLLATE"
            Top             =   240
            Width           =   1095
         End
      End
      Begin VB.Label Label15
         Caption         =   "&Lock Attempts:"
         Height          =   255
         Left            =   0
         TabIndex        =   63
         Top             =   1155
         Width           =   1335
      End
   End
   Begin VB.PictureBox boxBackup
      BorderStyle     =   0  'None
      Height          =   1395
      Left            =   2385
      ScaleHeight     =   1395
      ScaleWidth      =   5295
      TabIndex        =   12
      Top             =   105
      Width           =   5295
      Begin VB.CommandButton cmdBrowseBLog
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
         Left            =   4920
         TabIndex        =   21
         Top             =   840
         Width           =   315
      End
      Begin VB.CommandButton cmdBrowseBLog
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
         Left            =   4920
         TabIndex        =   15
         Top             =   0
         Width           =   315
      End
      Begin VB.CommandButton cmdBrowseBLog
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
         Index           =   1
         Left            =   4920
         TabIndex        =   18
         Top             =   420
         Width           =   315
      End
      Begin VB.TextBox txtBackLog1
         Height          =   315
         Left            =   1875
         TabIndex        =   14
         Tag             =   "BACKLOG1"
         Top             =   0
         Width           =   2970
      End
      Begin VB.TextBox txtBackLog2
         Height          =   315
         Left            =   1875
         TabIndex        =   17
         Tag             =   "BACKLOG2"
         Top             =   420
         Width           =   2970
      End
      Begin VB.TextBox txtPreBackLog
         Height          =   315
         Left            =   1875
         TabIndex        =   20
         Tag             =   "PREBACKLOG"
         Top             =   840
         Width           =   2970
      End
      Begin VB.Label Label14
         AutoSize        =   -1  'True
         Caption         =   "P&ending Transaction File:"
         Height          =   195
         Left            =   0
         TabIndex        =   19
         Top             =   915
         Width           =   1800
      End
      Begin VB.Label Label12
         Caption         =   "&Primary Log File:"
         Height          =   225
         Left            =   0
         TabIndex        =   13
         Top             =   75
         Width           =   1350
      End
      Begin VB.Label Label3
         AutoSize        =   -1  'True
         Caption         =   "&Secondary Log File:"
         Height          =   195
         Left            =   0
         TabIndex        =   16
         Top             =   495
         Width           =   1410
      End
   End
   Begin VB.PictureBox boxMemory
      BorderStyle     =   0  'None
      Height          =   2475
      Left            =   2385
      ScaleHeight     =   2475
      ScaleWidth      =   3630
      TabIndex        =   22
      Top             =   105
      Width           =   3630
      Begin VB.TextBox txtMemSzSp
         Height          =   315
         Left            =   2520
         TabIndex        =   34
         Tag             =   "MEMSZSP"
         Top             =   2100
         Width           =   1000
      End
      Begin VB.TextBox txtMemSzSb
         Height          =   315
         Left            =   2520
         TabIndex        =   32
         Tag             =   "MEMSZSB"
         Top             =   1680
         Width           =   1000
      End
      Begin VB.TextBox txtMemSzMemo
         Height          =   315
         Left            =   2520
         TabIndex        =   30
         Tag             =   "MEMSZMEMO"
         Top             =   1260
         Width           =   1000
      End
      Begin VB.TextBox txtMemSzBuf
         Height          =   315
         Left            =   2520
         TabIndex        =   28
         Tag             =   "MEMSZBUF"
         Top             =   840
         Width           =   1000
      End
      Begin VB.TextBox txtMemSzBlock
         Height          =   315
         Left            =   2520
         TabIndex        =   26
         Tag             =   "MEMSZBLOCK"
         Top             =   420
         Width           =   1000
      End
      Begin VB.TextBox txtMemStMax
         Height          =   315
         Left            =   2520
         TabIndex        =   24
         Tag             =   "MEMSTMAX"
         Top             =   0
         Width           =   1000
      End
      Begin VB.Label Label31
         AutoSize        =   -1  'True
         Caption         =   "Amount of Memory for &Sorting:"
         Height          =   195
         Left            =   0
         TabIndex        =   33
         Top             =   2160
         Width           =   2130
      End
      Begin VB.Label Label30
         AutoSize        =   -1  'True
         Caption         =   "Size of Sort &Buffers:"
         Height          =   195
         Left            =   0
         TabIndex        =   31
         Top             =   1755
         Width           =   1395
      End
      Begin VB.Label Label29
         AutoSize        =   -1  'True
         Caption         =   "Size of Memory Block for &Memos:"
         Height          =   195
         Left            =   0
         TabIndex        =   29
         Top             =   1335
         Width           =   2355
      End
      Begin VB.Label Label28
         AutoSize        =   -1  'True
         Caption         =   "Size of Memory Bu&ffers:"
         Height          =   195
         Left            =   0
         TabIndex        =   27
         Top             =   915
         Width           =   1665
      End
      Begin VB.Label Label27
         AutoSize        =   -1  'True
         Caption         =   "Size of Memory Bloc&k:"
         Height          =   195
         Left            =   0
         TabIndex        =   25
         Top             =   495
         Width           =   1575
      End
      Begin VB.Label Label25
         AutoSize        =   -1  'True
         Caption         =   "Memory for &Optimization:"
         Height          =   195
         Left            =   0
         TabIndex        =   23
         Top             =   75
         Width           =   1725
      End
   End
End
Attribute VB_Name = "frmConfig"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Private config&  'DATA4 pointer for S4SERVER.DBF

Private Sub edtODBCLockAttempts_KeyPress(KeyAscii As Integer)
    If (KeyAscii < Asc("0") Or KeyAscii > Asc("9")) And KeyAscii <> Asc("-") Then
        KeyAscii = 0
    End If
End Sub

Private Sub initText(tb As TextBox)
    Dim field&
    With tb
        If .Enabled Then
            field = d4field(config, .Tag)
            If field = 0 Then
                .Enabled = False
                .BackColor = vbButtonFace
            Else
                .Text = Trim(f4str(field))
            End If
        Else
            .BackColor = vbButtonFace
        End If
    End With
End Sub

Private Sub PositionComboGivenData(cmb As VB.ComboBox, itemdata As Long)
    ' Search for itemdata in the itemdata property
    ' of the combo and position there.
    Dim i%
    For i = 0 To cmb.ListCount - 1
        If cmb.itemdata(i) = itemdata Then
            cmb.ListIndex = i
            Exit For
        End If
    Next i
End Sub

Private Sub chkExpRecno_Click()
    If Me.Visible Then
        With chkExpRecno
            If .Enabled Then
                If .value = vbUnchecked Then
                    Call f4assign(d4field(config, .Tag), "F")
                Else
                    Call f4assign(d4field(config, .Tag), "T")
                End If
            End If
        End With
    End If
End Sub

Private Sub chkKeepOpen_Click()
    If Me.Visible Then
        With chkKeepOpen
            If .value = vbUnchecked Then
                Call f4assign(d4field(config, .Tag), "F")
            Else
                Call f4assign(d4field(config, .Tag), "T")
            End If
        End With
    End If
End Sub

Private Sub chkLogFileFlush_Click()
    If Me.Visible Then
        With chkLogFileFlush
            If .Enabled Then
                If .value = vbUnchecked Then
                    Call f4assign(d4field(config, .Tag), "F")
                Else
                    Call f4assign(d4field(config, .Tag), "T")
                End If
            End If
        End With
    End If
End Sub

Private Sub chkODBCTrans_Click()
    If Me.Visible Then
        With chkODBCTrans
            If .Enabled Then
                If .value = vbUnchecked Then
                    Call f4assign(d4field(config, .Tag), "F")
                Else
                    Call f4assign(d4field(config, .Tag), "T")
                End If
            End If
        End With
    End If
End Sub

Private Sub chkRequireEncryption_Click()
    If Me.Visible Then
        With chkRequireEncryption
            If .Enabled Then
                If .value = vbUnchecked Then
                    Call f4assign(d4field(config, .Tag), "F")
                Else
                    Call f4assign(d4field(config, .Tag), "T")
                End If
            End If
        End With
    End If
End Sub


Private Sub chkTrimBlanks_Click()
    If Me.Visible Then
        With chkTrimBlanks
            If .Enabled Then
                If .value = vbUnchecked Then
                    Call f4assign(d4field(config, .Tag), "F")
                Else
                    Call f4assign(d4field(config, .Tag), "T")
                End If
            End If
        End With
    End If
End Sub

Private Sub cmdBrowseBLog_Click(Index As Integer)
    dlgBLog.ShowOpen
    If Trim(dlgBLog.fileName) <> "" Then
        Select Case Index
            Case 0
                ' backlog 1
                txtBackLog1.Text = dlgBLog.fileName
            Case 1
                ' backlog 2
                txtBackLog2.Text = dlgBLog.fileName
            Case 2
                ' pending
                txtPreBackLog.Text = dlgBLog.fileName
        End Select
    End If
End Sub

Private Sub Form_Load()
    HelpContextID = Server_Settings

    Call code4errOff(gCode4, 1)  ' no error messages
    config = d4open(gCode4, configName)
    If config = 0 Then
        ' could not open the file list
        Dim errno%
        errno = code4errorCode(gCode4, 0)
        Select Case errno
            Case -1399 To -1300
                ' connection error; probably lost connection to server
                Call MsgBox("Connection to host lost.", vbExclamation, App.title)
                Call frmMain2.mnuDisconnect_Click
            Case e4fileFind
                Call MsgBox("Unable to find server settings file.", vbExclamation, App.title)
            Case Else
                Call MsgBox("Unable to access the file list. CodeBase error" & errno & ".", vbExclamation, App.title)
        End Select
        Call CloseWindow(Me.hWnd)
        Exit Sub
    Else
        ' server configuration file successfully opened
        Call d4top(config)

        Dim field&
        Call code4errFieldName(gCode4, 0)

        initText txtLogName
        initText txtErrorLogName
        initText txtDefPath
        initText txtSystemPath
        initText txtMaxLogSize
        initText txtMemStMax
        initText txtMemSzBlock
        initText txtMemSzBuf
        initText txtMemSzMemo
        initText txtMemSzSb
        initText txtMemSzSp
        initText txtODBCLockAttempts
        initText txtProcessId
        initText txtODBCPortNo
        initText txtTCPBegin
        initText txtTCPEnd
        initText txtBackLog1
        initText txtBackLog2
        initText txtPreBackLog

        With optCollate
            If .Item(1).Enabled Then
                field = d4field(config, optCollate(0).Tag)
                If field = 0 Then
                    .Item(0).Enabled = False
                    .Item(1).Enabled = False
                Else
                    Select Case UCase(Trim(f4str(field)))
                        Case "MACHINE"
                            .Item(0).value = True
                        Case "GENERAL"
                            .Item(1).value = True
                    End Select
                End If
            End If
        End With

        With cmbOptimize
            If .Enabled Then
                field = d4field(config, .Tag)
                If field = 0 Then
                    .Enabled = False
                Else
                    Call PositionComboGivenData(cmbOptimize, f4int(field))
                End If
            End If
        End With

        With cmbOptimizeWrite
            If .Enabled Then
                field = d4field(config, .Tag)
                If field = 0 Then
                    .Enabled = False
                Else
                    Call PositionComboGivenData(cmbOptimizeWrite, f4int(field))
                End If
            End If
        End With

        With cmbLogOptions
            If .Enabled Then
                field = d4field(config, .Tag)
                If field = 0 Then
                    .Enabled = False
                Else
                    Call PositionComboGivenData(cmbLogOptions, f4int(field))
                End If
            End If
        End With

        With cmbOpenMode
            If .Enabled Then
                field = d4field(config, .Tag)
                If field = 0 Then
                    .Enabled = False
                Else
                    Call PositionComboGivenData(cmbOpenMode, f4int(field))
                End If
            End If
        End With

        With chkRequireEncryption
            If .Enabled Then
                field = d4field(config, .Tag)
                If field = 0 Then
                    .Enabled = False
                Else
                    .value = f4true(field)
                End If
            End If
        End With

        With chkKeepOpen
            If .Enabled Then
                field = d4field(config, .Tag)
                If field = 0 Then
                    .Enabled = False
                Else
                    .value = f4true(field)
                End If
            End If
        End With

        With chkLogFileFlush
            If .Enabled Then
                field = d4field(config, .Tag)
                If field = 0 Then
                    .Enabled = False
                Else
                    .value = f4true(field)
                End If
            End If
        End With

        With chkTrimBlanks
            If .Enabled Then
                field = d4field(config, .Tag)
                If field = 0 Then
                    .Enabled = False
                Else
                    .value = f4true(field)
                End If
            End If
        End With

        With chkExpRecno
            If .Enabled Then
                field = d4field(config, .Tag)
                If field = 0 Then
                    .Enabled = False
                Else
                    Dim expRecVal As Integer
                    expRecVal = f4true(field)   ' LY Jun 24/04 : exprecno field in s4server.dbf changed to r4log
                    If expRecVal = 0 Then
                        .value = 0
                    Else
                        .value = 1
                    End If
                End If
            End If
        End With

        With chkODBCTrans
            If .Enabled Then
                field = d4field(config, .Tag)
                If field = 0 Then
                    .Enabled = False
                Else
                    .value = f4true(field)
                End If
            End If
        End With

        lstCategory.ListIndex = 0
    End If
End Sub

Private Sub cmbLogOptions_Click()
    With cmbLogOptions
        If Me.Visible Then
            Call f4assignLong(d4field(config, .Tag), .itemdata(.ListIndex))
        End If

        ' if the Enabled property of txtMaxLogSize can be toggled
        If d4field(config, txtMaxLogSize.Tag) Then
            ' if auto-truncate
            If .itemdata(.ListIndex) = 5 Then
                txtMaxLogSize.Enabled = True
                txtMaxLogSize.BackColor = vbWindowBackground
            Else
                txtMaxLogSize.Enabled = False
                txtMaxLogSize.BackColor = vbButtonFace
            End If
        End If
    End With
End Sub

Private Sub cmbOpenMode_Click()
    If Me.Visible Then
        With cmbOpenMode
            Call f4assignLong(d4field(config, .Tag), .itemdata(.ListIndex))
        End With
    End If
End Sub

Private Sub cmbOptimize_Click()
    If Me.Visible Then
        With cmbOptimize
            Call f4assignLong(d4field(config, .Tag), .itemdata(.ListIndex))
        End With
    End If
End Sub

Private Sub cmbOptimizeWrite_Click()
    If Me.Visible Then
        With cmbOptimizeWrite
            Call f4assignLong(d4field(config, .Tag), .itemdata(.ListIndex))
        End With
    End If
End Sub

Private Sub cmdBrowse_Click(Index As Integer)
    showWarn
    BrowseForFolder.SetDefaults
    Select Case Index
        Case 2   ' DEFPATH
            BrowseForFolder.Display Me.hWnd, "Locate Default Path"
            If BrowseForFolder.successful Then
                txtDefPath.Text = BrowseForFolder.folderName
            End If
        Case 3   ' SYSPATH
            BrowseForFolder.Display Me.hWnd, "Locate System Path"
            If BrowseForFolder.successful Then
                txtSystemPath.Text = BrowseForFolder.folderName
            End If
    End Select
End Sub

Private Sub cmdBrowseErr_Click(Index As Integer)
    showWarn
    dlgErrFile.fileName = Trim(txtErrorLogName.Text)
    dlgErrFile.ShowOpen
    If Trim(dlgErrFile.fileName) <> "" Then
        txtErrorLogName.Text = dlgErrFile.fileName
    End If
End Sub

Private Sub cmdBrowseLog_Click(Index As Integer)
    showWarn
    If Len(Trim(txtLogName.Text)) > 0 Then
        dlgLogFile.fileName = Trim(txtLogName.Text)
    Else
        dlgLogFile.fileName = "S4SERVER.LOG"
        If Len(Trim(txtSystemPath.Text)) > 0 Then
            dlgLogFile.InitDir = Trim(txtSystemPath.Text)
        Else
            dlgLogFile.InitDir = Trim(code4serverCurrentDirectory(gCode4))
        End If
    End If
    dlgLogFile.fileName = Trim(txtLogName.Text)
    dlgLogFile.ShowOpen
    If Trim(dlgLogFile.fileName) <> "" Then
        txtLogName.Text = dlgLogFile.fileName
    End If
End Sub

Private Sub cmdOK_Click()
    ' OK button clicked
    If d4changed(config, -1) <> 0 Then
        If validateIP(txtTCPBegin.Text) = False Then
            Call MsgBox("You have specified an invalid IP address.", vbExclamation, _
                        "Lower IP Address Range Limit")
            txtTCPBegin.SetFocus
            Exit Sub
        End If

        If validateIP(txtTCPEnd) = False Then
            Call MsgBox("You have specified an invalid IP address.", vbExclamation, _
                        "Upper IP Address Range Limit")
            txtTCPEnd.SetFocus
            Exit Sub
        End If

        If txtODBCLockAttempts.Enabled Then
            Dim la&
            la = val(txtODBCLockAttempts.Text)
            If la < 1 And la <> -1 Then
                MsgBox "ODBC Lock Attempts is invalid. It must be -1 or greater than 0.", vbExclamation, "ODBC Lock Attempts"
                Exit Sub
            End If
        End If

        Call d4flush(config)
        MsgBox "Changes will take effect the next time the server is started.", vbInformation, Me.caption
    End If

    Unload Me
End Sub

Private Sub cmdCancel_Click()
   Call d4changed(config, 0)
   Unload Me
End Sub

Private Sub Form_Unload(Cancel As Integer)
    If config <> 0 Then
        Call d4changed(config, 0)
        Call d4close(config)
    End If
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
                boxBackup.Visible = False
                HelpContextID = Files_Directories
            Case "File Access"
                boxAccess.Visible = True
                boxFiles.Visible = False
                boxNetworking.Visible = False
                boxMemory.Visible = False
                boxODBC.Visible = False
                boxBackup.Visible = False
                HelpContextID = File_Access
            Case "Networking"
                boxNetworking.Visible = True
                boxFiles.Visible = False
                boxAccess.Visible = False
                boxMemory.Visible = False
                boxODBC.Visible = False
                boxBackup.Visible = False
                HelpContextID = Networking_Config
            Case "Memory"
                boxMemory.Visible = True
                boxFiles.Visible = False
                boxAccess.Visible = False
                boxNetworking.Visible = False
                boxODBC.Visible = False
                boxBackup.Visible = False
                HelpContextID = Memory
            Case "ODBC"
                boxODBC.Visible = True
                boxFiles.Visible = False
                boxAccess.Visible = False
                boxNetworking.Visible = False
                boxMemory.Visible = False
                boxBackup.Visible = False
                HelpContextID = ODBC_Config
            Case "Backup"
                boxBackup.Visible = True
                boxFiles.Visible = False
                boxAccess.Visible = False
                boxNetworking.Visible = False
                boxMemory.Visible = False
                boxODBC.Visible = False
                HelpContextID = Backup_Settings
        End Select
    End With
End Sub

Private Sub optCollate_Click(Index As Integer)
    If Me.Visible Then
        With optCollate(Index)
            If Index = 1 Then
                ' general
                Call f4assign(d4field(config, .Tag), "General")
            Else
                ' machine
                Call f4assign(d4field(config, .Tag), "Machine")
            End If
        End With
    End If
End Sub

Private Sub txtBackLog1_Change()
    If Me.Visible Then
        With txtBackLog1
            Call f4assign(d4field(config, .Tag), Trim(.Text))
        End With
    End If
End Sub

Private Sub txtBackLog1_GotFocus()
    TxtGotFocus Me
End Sub

Private Sub txtBackLog2_Change()
    If Me.Visible Then
        With txtBackLog2
            Call f4assign(d4field(config, .Tag), Trim(.Text))
        End With
    End If
End Sub

Private Sub txtBackLog2_GotFocus()
    TxtGotFocus Me
End Sub

Private Sub txtDefPath_Change()
    If Me.Visible Then
        With txtDefPath
            Call f4assign(d4field(config, .Tag), Trim(.Text))
        End With
    End If
End Sub

Private Sub txtDefPath_GotFocus()
    TxtGotFocus Me
End Sub

Private Sub txtErrorLogName_Change()
    If Me.Visible Then
        With txtErrorLogName
            Call f4assign(d4field(config, .Tag), Trim(.Text))
        End With
    End If
End Sub

Private Sub txtErrorLogName_GotFocus()
    TxtGotFocus Me
End Sub

Private Sub txtLogName_Change()
    If Me.Visible Then
        With txtLogName
            Call f4assign(d4field(config, .Tag), Trim(.Text))
        End With
    End If
End Sub

Private Sub txtLogName_GotFocus()
    TxtGotFocus Me
End Sub

Private Sub txtMaxLogSize_Change()
    If Me.Visible Then
        With txtMaxLogSize
            Call f4assign(d4field(config, .Tag), Trim(.Text))
        End With
    End If
End Sub

Private Sub txtMaxLogSize_GotFocus()
    TxtGotFocus Me
End Sub

Private Sub txtMemStMax_Change()
    If Me.Visible Then
        With txtMemStMax
            Call f4assign(d4field(config, .Tag), Trim(.Text))
        End With
    End If
End Sub

Private Sub txtMemStMax_GotFocus()
    TxtGotFocus Me
End Sub

Private Sub txtMemSzBlock_Change()
    If Me.Visible Then
        With txtMemSzBlock
            Call f4assign(d4field(config, .Tag), Trim(.Text))
        End With
    End If
End Sub

Private Sub txtMemSzBlock_GotFocus()
    TxtGotFocus Me
End Sub

Private Sub txtMemSzBuf_Change()
    If Me.Visible Then
        With txtMemSzBuf
            Call f4assign(d4field(config, .Tag), Trim(.Text))
        End With
    End If
End Sub

Private Sub txtMemSzBuf_GotFocus()
    TxtGotFocus Me
End Sub

Private Sub txtMemSzMemo_Change()
    If Me.Visible Then
        With txtMemSzMemo
            Call f4assign(d4field(config, .Tag), Trim(.Text))
        End With
    End If
End Sub

Private Sub txtMemSzMemo_GotFocus()
    TxtGotFocus Me
End Sub

Private Sub txtMemSzSb_Change()
    If Me.Visible Then
        With txtMemSzSb
            Call f4assign(d4field(config, .Tag), Trim(.Text))
        End With
    End If
End Sub

Private Sub txtMemSzSb_GotFocus()
    TxtGotFocus Me
End Sub

Private Sub txtMemSzSp_Change()
    If Me.Visible Then
        With txtMemSzSp
            Call f4assign(d4field(config, .Tag), Trim(.Text))
        End With
    End If
End Sub

Private Sub txtMemSzSp_GotFocus()
    TxtGotFocus Me
End Sub

Private Sub txtODBCLockAttempts_Change()
    If Me.Visible Then
        With txtODBCLockAttempts
            Call f4assign(d4field(config, .Tag), Trim(.Text))
        End With
    End If
End Sub

Private Sub txtODBCLockAttempts_GotFocus()
    TxtGotFocus Me
End Sub

Private Sub txtODBCPortNo_Change()
    If Me.Visible Then
        With txtODBCPortNo
            Call f4assign(d4field(config, .Tag), Trim(.Text))
        End With
    End If
End Sub

Private Sub txtODBCPortNo_GotFocus()
    TxtGotFocus Me
End Sub

Private Sub txtPreBackLog_Change()
    If Me.Visible Then
        With txtPreBackLog
            Call f4assign(d4field(config, .Tag), Trim(.Text))
        End With
    End If
End Sub

Private Sub txtPreBackLog_GotFocus()
    TxtGotFocus Me
End Sub

Private Sub txtProcessId_Change()
    If Me.Visible Then
        With txtProcessId
            Call f4assign(d4field(config, .Tag), Trim(.Text))
        End With
    End If
End Sub

Private Sub txtProcessId_GotFocus()
    TxtGotFocus Me
End Sub

Private Sub txtSystemPath_Change()
    If Me.Visible Then
        With txtSystemPath
            Call f4assign(d4field(config, .Tag), Trim(.Text))
        End With
    End If
End Sub

Private Sub txtSystemPath_GotFocus()
    TxtGotFocus Me
End Sub

Private Sub txtTCPBegin_Change()
    If Me.Visible Then
        With txtTCPBegin
            Call f4assign(d4field(config, .Tag), Trim(.Text))
        End With
    End If
End Sub

Private Sub txtTCPBegin_GotFocus()
    TxtGotFocus Me
End Sub

Private Sub txtTCPEnd_Change()
    If Me.Visible Then
        With txtTCPEnd
            Call f4assign(d4field(config, .Tag), Trim(.Text))
        End With
    End If
End Sub

Private Sub txtTCPEnd_GotFocus()
    TxtGotFocus Me
End Sub
