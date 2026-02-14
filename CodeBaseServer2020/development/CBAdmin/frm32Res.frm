VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "COMDLG32.OCX"
Begin VB.Form frm32Res
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Restore Utility"
   ClientHeight    =   3495
   ClientLeft      =   45
   ClientTop       =   345
   ClientWidth     =   4350
   Icon            =   "frm32Res.frx":0000
   LinkTopic       =   "Form1"
   LockControls    =   -1  'True
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   3495
   ScaleWidth      =   4350
   ShowInTaskbar   =   0   'False
   StartUpPosition =   3  'Windows Default
   Tag             =   "LOG4RES"
   Begin VB.CheckBox chkIndex
      Caption         =   "Update &Index Files"
      Height          =   195
      Left            =   60
      TabIndex        =   9
      Top             =   2760
      Width           =   2055
   End
   Begin VB.Frame Frame2
      Caption         =   "Files to &Include"
      Height          =   1095
      Index           =   4
      Left            =   60
      TabIndex        =   3
      Top             =   540
      Width           =   4215
      Begin VB.CheckBox chkAll
         Caption         =   "&All"
         Height          =   255
         Left            =   120
         TabIndex        =   4
         Top             =   240
         Value           =   1  'Checked
         Width           =   1335
      End
      Begin VB.CommandButton cmdSelect
         Caption         =   "&Select..."
         Enabled         =   0   'False
         Height          =   375
         Left            =   120
         TabIndex        =   5
         Top             =   600
         Width           =   1095
      End
      Begin VB.Label lblFiles
         AutoSize        =   -1  'True
         Caption         =   "0 Files Selected"
         Height          =   195
         Left            =   1440
         TabIndex        =   12
         Top             =   690
         Width           =   1125
      End
   End
   Begin VB.Frame Frame2
      Caption         =   "File &Access"
      Height          =   915
      Index           =   1
      Left            =   60
      TabIndex        =   6
      Top             =   1740
      Width           =   4215
      Begin VB.OptionButton optExclusive
         Caption         =   "&Exclusive"
         Height          =   255
         Index           =   0
         Left            =   120
         TabIndex        =   7
         Top             =   240
         Width           =   1215
      End
      Begin VB.OptionButton optExclusive
         Caption         =   "&Shared"
         Height          =   255
         Index           =   1
         Left            =   120
         TabIndex        =   8
         Top             =   540
         Value           =   -1  'True
         Width           =   975
      End
   End
   Begin VB.CommandButton cmdOpenDialog
      Caption         =   "..."
      Height          =   300
      Left            =   3960
      TabIndex        =   2
      Top             =   90
      Width           =   300
   End
   Begin VB.TextBox data
      Height          =   300
      Left            =   960
      TabIndex        =   1
      Top             =   90
      Width           =   2930
   End
   Begin MSComDlg.CommonDialog dlgMisc
      Left            =   3480
      Top             =   2580
      _ExtentX        =   688
      _ExtentY        =   688
      _Version        =   393216
   End
   Begin VB.CommandButton cmdGo
      Caption         =   "Process"
      Default         =   -1  'True
      Height          =   350
      Left            =   1830
      TabIndex        =   10
      Top             =   3060
      Width           =   1095
   End
   Begin VB.CommandButton cmdClose
      Cancel          =   -1  'True
      Caption         =   "Close"
      Height          =   350
      Left            =   3150
      TabIndex        =   11
      Top             =   3060
      Width           =   1095
   End
   Begin VB.Label Label1
      AutoSize        =   -1  'True
      Caption         =   "&Log File:"
      Height          =   195
      Left            =   60
      TabIndex        =   0
      Top             =   130
      Width           =   600
   End
End
Attribute VB_Name = "frm32Res"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

'===================================================================================
'
' LOG4RES Switch Constants
'
'===================================================================================

'Const cAdd1 = 1
'Const cAdd2 = 2
Const cLogFile = 1
Const cExclusive = 2
Const cIndex = 3
'Const cRemove = 4
Const ccAll = 4
Const cRestore = 5
'Const cExclude = 6
Const cEncKey = 6

Const ctotalParams = 13
Const cArrayTotal = 12 'always ctotalParams - 1

Const cOptAll = 0
Const cOptSelect = 1

Const cCmdButProcess = "Process"
Const cCmdButSelect = "Select"

Const cDoProcess = 0
Const cDoSelect = 1

'===================================================================================
'
' LOG4RES Switch Variables
'
'===================================================================================

Dim vfilesSelected As Integer    'specific files selected for restoration
Dim vSwitches(8) As String
Dim vData(8) As String
'Dim vOrigLogFileString As String
Private vTotalParams%

Private Sub FormatData()
'This Procedure calls a C function to obtain a
'pointer to a string. This string is then assigned
'to an array of string pointers
'NOTE: GLOBAL VARIABLE ACCESSED :vnumDataParams


Dim i, skip, destNames As Integer
Dim bGetRestoreFiles, currentIndex As Integer

If (Trim$(vData(ccAll)) = "OFF") Then     'restore files specified
   bGetRestoreFiles = True
Else
   bGetRestoreFiles = False
End If

If (bGetRestoreFiles = True) Then
   'obtaining the number of destination names
   'therefore increase the vDllData array by this amount
   destNames = 0
   For i = 0 To (vRestoreItems - 1)
      If Not (Trim$(vDataRestore(i, cDest)) = cNoItems) Then
         destNames = destNames + 1
      End If
   Next i

   'redimension array with the original size (cArrayTotal)
   '+ the number of files being restored (vRestoreItems)
   '+ the "-r" for each file being restored (* 2)
   '+ the number of files being renamed (destNames)
   '- the original allocation of a space for restoration (-1)
   ReDim vDllData((cArrayTotal + (vRestoreItems * 2) + destNames - 1))
End If

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

'obtaining the specific files to be restored
If (bGetRestoreFiles = True) Then
   currentIndex = (numParams - skip) * 2
   For i = 0 To (vRestoreItems - 1)
      currentIndex = currentIndex + 1
      vDllData(currentIndex - skip) = v4Cstring(vSwitches(cRestore))
      currentIndex = currentIndex + 1
      vDllData(currentIndex - skip) = v4Cstring(vDataRestore(i, cTarget))
      If Not (Trim$(vDataRestore(i, cDest)) = cNoItems) Then
         vDllData(currentIndex - skip + 1) = v4Cstring(vDataRestore(i, cDest))
         currentIndex = currentIndex + 1
      End If
   Next i

   'adding additional parameter for the selected files and their destination names
   vnumDataParams = (numParams * 2 + 1) + (vRestoreItems * 2) + destNames - (skip * 2)
Else
   'multiply by 2 to include the switch and data
   vnumDataParams = (numParams * 2 + 1) - (skip * 2)
End If

End Sub

Private Sub GetUserData()
    Dim retstr As String

    'vData(cAdd1) = frmRes.data(cAdd1).Text
    'vData(cAdd2) = frmRes.data(cAdd2).Text
    'vData(cAdd1) = ""
    'vData(cAdd2) = ""
    'vData(cLogFile) = frm32Res.data(cLogFile).Text
    vData(cLogFile) = frm32Res.data.Text
    'vData(cRemove) = frmRes.data(cRemove).Text
    'vData(cRemove) = ""

    'If (frmRes.optAll(0).Value = True) Then
    '   retstr = "ON"
    'Else
    '   retstr = "OFF"
    'End If
    'vData(ccAll) = retstr
    'vData(ccAll) = ""

    If (frm32Res.optExclusive(0).value = True) Then
       retstr = "ON"
    Else
       retstr = "OFF"
    End If
    vData(cExclusive) = retstr

    If chkIndex.value = vbChecked Then
       retstr = "ON"
    Else
       retstr = "OFF"
    End If
    vData(cIndex) = retstr

    If chkAll.value = vbChecked Then
       retstr = "ON"
    Else
       retstr = "OFF"
    End If
    vData(ccAll) = retstr

    If Len(encryptionKeyFileName) > 0 Then
        vData(cEncKey) = "@" & encryptionKeyFileName
    Else
        vData(cEncKey) = ""
    End If
End Sub

Private Sub InitializeRestore()
    ReDim vDllData(cArrayTotal) As Long 'redimensioning global array

    vfilesSelected = 0
    numParams = 6   'Input parameters for Log4Res
    currentUtility = LOG4RES
    vTotalParams = ctotalParams 'initializing global variable


    'setup the abbreviated switches
    'vSwitches(cAdd1) = "-addr"
    'vSwitches(cAdd2) = "-cadd2" 'must have a dummy name for INI info
    vSwitches(cLogFile) = "-l"
    vSwitches(cExclusive) = "-e"
    vSwitches(cIndex) = "-i"
    'vSwitches(cRemove) = "-r"
    vSwitches(ccAll) = "-al"
    vSwitches(cRestore) = "-addr"
    'vSwitches(cExclude) = "-r"
    vSwitches(cEncKey) = "-enckey"

    'storing utility name
    vSwitches(cProgram) = ""
    vData(cProgram) = "LOG4RES"

    Call SetUserData
    frm32Res.lblFiles.caption = "All" & cFilesSelected 'Setting the Files Selected caption
End Sub

Private Sub performProcessOrSelect(operation As Integer)
    If (operation = cDoProcess) Then    'Process option selected
        Call GetUserData
        Call FormatData

        Call SaveSetting(App.title, Tag, "LogFile", Trim(data.Text))
        Call SaveSetting(App.title, Tag, "Exclusive", optExclusive(0).value)
        Call SaveSetting(App.title, Tag, "Shared", optExclusive(1).value)
        Call SaveSetting(App.title, Tag, "UpdateIndex", chkIndex.value)

        frm32Status.Show 1
    Else    'Select option chosen
        If setLogFileName(data.Text) = True Then
            Call setFrmSelectCaptions
            If (DisplayForm(cFrmSelectCaption, 1) = False) Then
                Load frm32Select
            End If
            If (vRestoreItems > 0) Then
                vfilesSelected = 1
            Else
                vfilesSelected = 0
                Call chkAll_Click   'reset command buttons
            End If
            If vRestoreItems = 1 Then
                frm32Res.lblFiles.caption = str$(vRestoreItems) & cFileSelected
            Else
                frm32Res.lblFiles.caption = str$(vRestoreItems) & cFilesSelected
            End If
        Else
            MsgBox "Log File Name incorrect", 16 + 0, "Utitily Error"
        End If
    End If
End Sub

Private Sub setFrmSelectCaptions()
    'This procedure initializes the frmSelect's caption
    'variables by setting the form's global variables

    vlblList0 = "&Exclude Files"
    vlblList1 = "&Include Files"
    vlblList2 = "&Output File"
    vlblList3 = ""
    vcmdAction6 = "Destination File Name"
End Sub

Private Sub SetUserData()
    data.Text = GetSetting(App.title, Tag, "LogFile")
    optExclusive(0).value = GetSetting(App.title, Tag, "Exclusive", True)
    optExclusive(1).value = GetSetting(App.title, Tag, "Shared", True)
    chkIndex.value = CInt(GetSetting(App.title, Tag, "UpdateIndex", 0))
End Sub

Private Function ValidateForm()
    Dim aLog$, rc%

    rc = True
    aLog = Trim(Me.data.Text)

    If aLog = "" Then
        errMsg cErrNullName, "Log File"
        Me.data.SetFocus
        rc = False
    ElseIf Not Exist(aLog) Then
        errMsg cErrFileExist, aLog
        Me.data.SetFocus
        rc = False
    End If

    ValidateForm = rc
End Function

Private Sub chkAll_Click()
    If chkAll.value = vbChecked Then
        cmdSelect.Enabled = False
    Else
        cmdSelect.Enabled = True
    End If

    If chkAll.value = vbChecked Then
        lblFiles.caption = "All" & cFilesSelected
    Else
        If vfilesSelected = 0 Then
            lblFiles.caption = "0" & cFilesSelected
        Else
            If vRestoreItems = 1 Then
                lblFiles.caption = str$(vRestoreItems) & cFileSelected
            Else
                lblFiles.caption = str$(vRestoreItems) & cFilesSelected
            End If
        End If
    End If
End Sub

Private Sub cmdClose_Click()
    Unload frm32Res
End Sub

Private Sub cmdGo_Click()
    If Not ValidateForm() Then Exit Sub

    If cmdGo.caption = cCmdButProcess Then
        Call performProcessOrSelect(cDoProcess)
    Else
        Call performProcessOrSelect(cDoSelect)
    End If
End Sub

Private Sub cmdOpenDialog_Click()
    'DF Set Dialog Options
    InitFileDialog dlgMisc, Trim(data.Text), True
   'dlgMisc.Action = cActionOpen
    dlgMisc.ShowOpen

    If Trim(dlgMisc.fileName) <> "" Then
        frm32Res.data.Text = dlgMisc.fileName
    End If
End Sub

Private Sub cmdSelect_Click()
    If Not ValidateForm() Then Exit Sub
    Call performProcessOrSelect(cDoSelect)
End Sub

Private Sub data_Change()
    Call setListItemCount(0, 0)
    Unload frm32Select
    chkAll.value = vbChecked    'select the "All" option button
    checkFields
End Sub

Private Sub data_GotFocus()
    TxtGotFocus Me
End Sub

Private Sub Form_Load()
    HelpContextID = Log_Restore

    Call InitializeRestore
    Call SetDeviceIndependentWindow(frm32Res)
    checkFields
End Sub

Private Sub Form_Unload(Cancel As Integer)
    currentUtility = 0
    Unload frm32Select
    Call clearFrmSelectArrays
End Sub

Private Sub checkFields()
    If Len(Trim(data.Text)) = 0 Then
        chkAll.Enabled = False
        cmdSelect.Enabled = False
    Else
        chkAll.Enabled = True
        If chkAll.value = vbChecked Then
            cmdSelect.Enabled = False
        Else
            cmdSelect.Enabled = True
        End If
    End If
End Sub

