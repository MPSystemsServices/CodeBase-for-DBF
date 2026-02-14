VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "COMDLG32.OCX"
Begin VB.Form frm32Back
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Backup Utility"
   ClientHeight    =   1935
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   4650
   Icon            =   "frm32Back.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   1935
   ScaleWidth      =   4650
   ShowInTaskbar   =   0   'False
   StartUpPosition =   3  'Windows Default
   Tag             =   "LOG4BACK"
   Begin VB.CheckBox chkOverwrite
      Caption         =   "&Overwrite Existing Backup Log File"
      Height          =   195
      Left            =   60
      TabIndex        =   7
      Top             =   1200
      Width           =   2955
   End
   Begin VB.CheckBox chkCompress
      Caption         =   "&Compress Active Log File"
      Height          =   255
      Left            =   60
      TabIndex        =   6
      Top             =   900
      Width           =   2355
   End
   Begin VB.TextBox data
      Height          =   300
      Index           =   2
      Left            =   1380
      TabIndex        =   4
      Top             =   540
      Width           =   2800
   End
   Begin MSComDlg.CommonDialog dlgMisc
      Left            =   3180
      Top             =   900
      _ExtentX        =   688
      _ExtentY        =   688
      _Version        =   393216
   End
   Begin VB.TextBox data
      Height          =   300
      Index           =   1
      Left            =   1380
      TabIndex        =   1
      Top             =   120
      Width           =   2800
   End
   Begin VB.CommandButton cmdOpenDialog
      Caption         =   "..."
      Height          =   300
      Index           =   1
      Left            =   4260
      TabIndex        =   5
      Top             =   540
      Width           =   300
   End
   Begin VB.CommandButton cmdOpenDialog
      Caption         =   "..."
      Height          =   300
      Index           =   0
      Left            =   4260
      TabIndex        =   2
      Top             =   120
      Width           =   300
   End
   Begin VB.CommandButton cmdGo
      Caption         =   "Process"
      Default         =   -1  'True
      Height          =   350
      Left            =   2160
      TabIndex        =   8
      Top             =   1530
      Width           =   1130
   End
   Begin VB.CommandButton cmdClose
      Cancel          =   -1  'True
      Caption         =   "Close"
      Height          =   350
      Left            =   3430
      TabIndex        =   9
      Top             =   1530
      Width           =   1130
   End
   Begin VB.Label Label1
      AutoSize        =   -1  'True
      Caption         =   "&Backup Log File:"
      Height          =   195
      Index           =   0
      Left            =   60
      TabIndex        =   0
      Top             =   160
      Width           =   1200
   End
   Begin VB.Label Label1
      AutoSize        =   -1  'True
      Caption         =   "&Active Log File:"
      Height          =   195
      Index           =   1
      Left            =   60
      TabIndex        =   3
      Top             =   580
      Width           =   1095
   End
End
Attribute VB_Name = "frm32Back"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
'===================================================================================
'
' LOG4BACK Switch Constants
'
'===================================================================================

Const cReturnKey = 13
Const cBackup = 1
Const cConfigFile = 2   'STANDALONE    version: this is the logfile name
         'CONCURRENCY version: this is the configuration file name
Const cCompress = 3
Const cOverwrite = 4
Const cEncKey = 5

Const ctotalParams = 11
Const cArrayTotal = 10 'always ctotalParams - 1
'===================================================================================
'
' LOG4BACK Switch Variables
'
'===================================================================================

Dim vSwitches(5) As String
Dim vData(5) As String
'Dim vDllData(cArrayTotal) As Long
Private vTotalParams%

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
    Dim retstr As String

    vData(cBackup) = Trim$(frm32Back.data(cBackup).Text)
    vData(cConfigFile) = Trim$(frm32Back.data(cConfigFile).Text)

    If chkCompress.value = vbChecked Then
       retstr = "ON"
    Else
       retstr = "OFF"
    End If
    vData(cCompress) = retstr

    If chkOverwrite.value = vbChecked Then
       retstr = "ON"
    Else
       retstr = "OFF"
    End If
    vData(cOverwrite) = retstr

    If Len(encryptionKeyFileName) > 0 Then
        vData(cEncKey) = "@" & encryptionKeyFileName
    Else
        vData(cEncKey) = ""
    End If
End Sub

Private Sub InitializeBackup()
    ReDim vDllData(cArrayTotal) As Long 'redimensioning global array

    numParams = 5   'Input parameters for Log4Back
    currentUtility = LOG4BACK
    vTotalParams = ctotalParams 'initializing global variable


    'setup the abbreviated switches
    vSwitches(cBackup) = "-b"
    vSwitches(cConfigFile) = "-l"  'CONCURRENCY version use: "-con"
    vSwitches(cCompress) = "-com"
    vSwitches(cOverwrite) = "-ov"
    vSwitches(cEncKey) = "-enckey"

    'storing utility name
    vSwitches(cProgram) = ""
    vData(cProgram) = "LOG4BACK"

    Call SetUserData
End Sub

Private Sub SetUserData()
    data(cBackup).Text = GetSetting(App.title, Tag, "BackupLog")
    data(cConfigFile).Text = GetSetting(App.title, Tag, "ActiveLog")
    chkCompress.value = CInt(GetSetting(App.title, Tag, "Compress", vbUnchecked))
    chkOverwrite.value = CInt(GetSetting(App.title, Tag, "Overwrite", vbUnchecked))

    checkFields
End Sub

Private Function ValidateForm%()
   Dim aLog$, bLog$, rc%

   rc = True
   bLog = Trim(Me.data(cBackup))
   aLog = Trim(Me.data(cConfigFile))

   If bLog = "" Then
      errMsg cErrNullName, "Backup Log File"
      Me.data(cBackup).SetFocus
      rc = False
   ElseIf aLog = "" Then
      errMsg cErrNullName, "Active Log File"
      Me.data(cConfigFile).SetFocus
      rc = False
   ElseIf bLog = aLog Then
      errMsg cErrSameFile, ""
      Me.data(cBackup).SetFocus
      rc = False
   ElseIf Not Exist(aLog) Then
      errMsg cErrFileExist, aLog
      Me.data(cConfigFile).SetFocus
      rc = False
   End If

   ValidateForm = rc
End Function

Private Sub cmdClose_Click()
    Unload frm32Back
End Sub

Private Sub cmdGo_Click()
    Dim paramsSkipped As Integer

    If Not ValidateForm() Then Exit Sub

    Call GetUserData
    Call FormatData

    Call SaveSetting(App.title, Tag, "BackupLog", Trim(data(cBackup).Text))
    Call SaveSetting(App.title, Tag, "ActiveLog", Trim(data(cConfigFile).Text))
    Call SaveSetting(App.title, Tag, "Compress", chkCompress.value)
    Call SaveSetting(App.title, Tag, "Overwrite", chkOverwrite.value)

    frm32Status.Show 1
End Sub

Private Sub cmdOpenDialog_Click(Index As Integer)
   Dim dlgId As Integer, fileMustExist As Boolean

   If Index = 0 Then
      dlgId = cBackup
      dlgMisc.Flags = cdlOFNOverwritePrompt
      fileMustExist = False
   Else
      dlgId = cConfigFile
      dlgMisc.Flags = cdlOFNFileMustExist
      fileMustExist = True
   End If

   'DF Set Dialog Options
   InitFileDialog dlgMisc, Trim(data(dlgId)), fileMustExist
'   dlgMisc.Action = cActionOpen
   dlgMisc.ShowOpen

   If Trim(dlgMisc.fileName) <> "" Then
      frm32Back.data(dlgId).Text = LCase(dlgMisc.fileName)
   End If
End Sub

Private Sub data_Change(Index As Integer)
    checkFields
End Sub

Private Sub data_GotFocus(Index As Integer)
    TxtGotFocus Me
End Sub

Private Sub Form_Load()
    HelpContextID = Log_Backup

    Call InitializeBackup
    Call SetDeviceIndependentWindow(frm32Back)
End Sub

Private Sub Form_Unload(Cancel As Integer)
    currentUtility = 0
End Sub

Private Sub checkFields()
    If Len(Trim(data(2).Text)) = 0 Or Len(Trim(data(1).Text)) = 0 Then
   cmdGo.Enabled = False
    Else
   cmdGo.Enabled = True
    End If
End Sub
