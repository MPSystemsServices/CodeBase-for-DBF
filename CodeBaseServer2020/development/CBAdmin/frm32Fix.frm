VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "COMDLG32.OCX"
Begin VB.Form frm32Fix
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Recover Utility"
   ClientHeight    =   3375
   ClientLeft      =   45
   ClientTop       =   345
   ClientWidth     =   5100
   Icon            =   "frm32Fix.frx":0000
   LinkTopic       =   "Form1"
   LockControls    =   -1  'True
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   3375
   ScaleWidth      =   5100
   ShowInTaskbar   =   0   'False
   StartUpPosition =   3  'Windows Default
   Tag             =   "LOG4FIX"
   Begin MSComDlg.CommonDialog dlgMisc
      Left            =   3960
      Top             =   1260
      _ExtentX        =   688
      _ExtentY        =   688
      _Version        =   393216
   End
   Begin VB.CommandButton cmdOpenDialog
      Caption         =   "..."
      Height          =   300
      Index           =   1
      Left            =   3420
      TabIndex        =   8
      Top             =   1920
      Width           =   300
   End
   Begin VB.TextBox txtPreLogName
      Height          =   300
      Left            =   120
      TabIndex        =   7
      Top             =   1920
      Width           =   3255
   End
   Begin VB.Frame Frame1
      Caption         =   "Log File &Type"
      Height          =   855
      Left            =   120
      TabIndex        =   0
      Top             =   60
      Width           =   3615
      Begin VB.OptionButton optTypePrimary
         Caption         =   "&Primary Log File"
         Height          =   255
         Left            =   240
         TabIndex        =   1
         Top             =   240
         Value           =   -1  'True
         Width           =   2535
      End
      Begin VB.OptionButton optTypeBackup
         Caption         =   "&Backup Log File"
         Height          =   255
         Left            =   240
         TabIndex        =   2
         Top             =   480
         Width           =   2295
      End
   End
   Begin VB.Frame Frame2
      Caption         =   "&Inconsistency Detected"
      Height          =   855
      Left            =   120
      TabIndex        =   9
      Top             =   2340
      Width           =   3615
      Begin VB.OptionButton optForce
         Caption         =   "Use log &data"
         Height          =   255
         Index           =   0
         Left            =   240
         TabIndex        =   10
         Top             =   240
         Width           =   2175
      End
      Begin VB.OptionButton optForce
         Caption         =   "&Warning message"
         Height          =   255
         Index           =   1
         Left            =   240
         TabIndex        =   11
         Top             =   480
         Value           =   -1  'True
         Width           =   2655
      End
   End
   Begin VB.CommandButton cmdOpenDialog
      Caption         =   "..."
      Height          =   300
      Index           =   0
      Left            =   3420
      TabIndex        =   5
      Top             =   1260
      Width           =   300
   End
   Begin VB.TextBox txtLogName
      Height          =   300
      Left            =   120
      TabIndex        =   4
      Top             =   1260
      Width           =   3255
   End
   Begin VB.CommandButton cmdGo
      Caption         =   "Process"
      Default         =   -1  'True
      Height          =   350
      Left            =   3900
      TabIndex        =   12
      Top             =   120
      Width           =   1130
   End
   Begin VB.CommandButton cmdClose
      Cancel          =   -1  'True
      Caption         =   "Close"
      Height          =   350
      Left            =   3900
      TabIndex        =   13
      Top             =   600
      Width           =   1130
   End
   Begin VB.Label Label2
      AutoSize        =   -1  'True
      Caption         =   "Pending &Transaction File Name:"
      Height          =   195
      Left            =   120
      TabIndex        =   6
      Top             =   1680
      Width           =   2265
   End
   Begin VB.Label Label1
      AutoSize        =   -1  'True
      Caption         =   "&Log File Name:"
      Height          =   195
      Left            =   120
      TabIndex        =   3
      Top             =   1005
      Width           =   1065
   End
End
Attribute VB_Name = "frm32Fix"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
'===================================================================================
'
' LOG4FIX Switch Constants
'
'===================================================================================

Const cConfigFile = 1   'STANDALONE    version: this is the logfile name
                        'CONCURRENCY version: this is the configuration file name
Const cOptforce = 2
Const cPrimary = 3
Const cPrepend = 4
Const cEncKey = 5

Const ctotalParams = 11
Const cArrayTotal = ctotalParams - 1

'===================================================================================
'
' LOG4FIX Switch Variables
'
'===================================================================================

Dim vSwitches(5) As String
Dim vData(5) As String

Private Sub FormatData()
    'This Procedure calls a C function to obtain a
    'pointer to a string. This string is then assigned
    'to an array of string pointers
    'NOTE: GLOBAL VARIABLE ACCESSED :vnumDataParams

    Dim i, skip As Integer
    skip = 0
    'formatting the program name
    vDllData(cProgram) = v4Cstring(vData(cProgram))
    'formatting the switches and the data
    For i = 1 To numParams
        If (Len(Trim(vData(i))) > 0) Then
            vDllData(((i - skip) * 2 - 1)) = v4Cstring(vSwitches(i))
            vDllData((i - skip) * 2) = v4Cstring(vData(i))
        Else
            skip = skip + 1
        End If
    Next i

    'multiply by 2 to include the switch and data
    vnumDataParams = ctotalParams - (skip * 2)
End Sub

Private Sub GetUserData()
    'vData(cConfigFile) = Trim$(frm32Fix.data(cConfigFile).Text)
    vData(cConfigFile) = Trim$(txtLogName.Text)

    If optTypePrimary.value Then
        vData(cPrimary) = "ON"
    Else
        vData(cPrimary) = "OFF"
    End If

    vData(cPrepend) = Trim(txtPreLogName.Text)

    If optForce(0).value Then
        vData(cOptforce) = "ON"
    Else
        vData(cOptforce) = "OFF"
    End If

    If Len(encryptionKeyFileName) > 0 Then
        vData(cEncKey) = "@" & encryptionKeyFileName
    Else
        vData(cEncKey) = ""
    End If
End Sub

Private Sub InitializeFix()
    ReDim vDllData(cArrayTotal) As Long 'redimensioning global array

    numParams = 5   'Input parameters for Log4Back
    currentUtility = LOG4FIX
    'vtotalParams = ctotalParams 'initializing global variable

    'storing utility name
    vSwitches(cProgram) = ""
    vData(cProgram) = "LOG4FIX"

    'setup the abbreviated switches
    vSwitches(cConfigFile) = "-l"    'CONCURRENCY version: switch should be "-c"
    vSwitches(cOptforce) = "-F"
    vSwitches(cPrimary) = "-primary"
    vSwitches(cPrepend) = "-prepend"
    vSwitches(cEncKey) = "-enckey"

    Call SetUserData
End Sub

Private Sub SetUserData()
    txtLogName.Text = GetSetting(App.title, Tag, "LogName")
    txtPreLogName.Text = GetSetting(App.title, Tag, "PreLogName")
    optForce(0).value = GetSetting(App.title, Tag, "UseLog", False)
    optForce(1).value = GetSetting(App.title, Tag, "Warning", True)
    optTypePrimary.value = GetSetting(App.title, Tag, "TypePrimary", True)
    optTypeBackup.value = GetSetting(App.title, Tag, "TypeBackup", False)
End Sub

Private Function ValidateForm%()
    Dim aLog$, rc%

    rc = True
    aLog = Trim(txtLogName.Text)

    If aLog = "" Then
        errMsg cErrNullName, "Log File"
        txtLogName.SetFocus
        rc = False
    ElseIf Not Exist(aLog) Then
        errMsg cErrFileExist, aLog
        txtLogName.SetFocus
        rc = False
    End If

    ValidateForm = rc
End Function

Private Sub cmdClose_Click()
    Unload Me
End Sub

Private Sub cmdGo_Click()
    If Not ValidateForm() Then Exit Sub

    Call GetUserData
    Call FormatData

    Call SaveSetting(App.title, Tag, "LogName", Trim(txtLogName.Text))
    Call SaveSetting(App.title, Tag, "PreLogName", Trim(txtPreLogName.Text))
    Call SaveSetting(App.title, Tag, "UseLog", optForce(0).value)
    Call SaveSetting(App.title, Tag, "Warning", optForce(1).value)
    Call SaveSetting(App.title, Tag, "TypePrimary", optTypePrimary.value)
    Call SaveSetting(App.title, Tag, "TypeBackup", optTypeBackup.value)

    frm32Status.Show 1
End Sub

Private Sub cmdOpenDialog_Click(Index As Integer)
    If Index = 0 Then
        InitFileDialog dlgMisc, Trim(txtLogName.Text), True
    Else
        InitFileDialog dlgMisc, Trim(txtPreLogName.Text), True
    End If

    dlgMisc.ShowOpen

    If Trim(dlgMisc.fileName) <> "" Then
        If Index = 0 Then
            txtLogName.Text = dlgMisc.fileName
        Else
            txtPreLogName.Text = dlgMisc.fileName
        End If
    End If
End Sub

Private Sub Form_Load()
    HelpContextID = Log_Recover

    Call InitializeFix
    Call SetDeviceIndependentWindow(frm32Fix)
    Call checkFields
End Sub

Private Sub checkFields()
    If optTypePrimary.value Then
        cmdOpenDialog(1).Enabled = False
        txtPreLogName.Enabled = False
        txtPreLogName.BackColor = vbButtonFace
    Else
        cmdOpenDialog(1).Enabled = True
        txtPreLogName.Enabled = True
        txtPreLogName.BackColor = vbWindowBackground
    End If

    If Len(Trim(txtLogName)) = 0 Then
        cmdGo.Enabled = False
    Else
        cmdGo.Enabled = True
    End If
End Sub

Private Sub optTypeBackup_Click()
    checkFields
End Sub

Private Sub optTypePrimary_Click()
    checkFields
End Sub

Private Sub txtLogName_Change()
    checkFields
End Sub

Private Sub txtLogName_GotFocus()
    TxtGotFocus Me
End Sub

Private Sub txtPreLogName_Change()
    checkFields
End Sub

Private Sub txtPreLogName_GotFocus()
    TxtGotFocus Me
End Sub
