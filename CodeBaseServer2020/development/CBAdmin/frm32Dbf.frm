VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "COMDLG32.OCX"
Begin VB.Form frm32Dbf
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Analysis Utility"
   ClientHeight    =   4500
   ClientLeft      =   45
   ClientTop       =   345
   ClientWidth     =   8175
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   4500
   ScaleWidth      =   8175
   ShowInTaskbar   =   0   'False
   StartUpPosition =   2  'CenterScreen
   Tag             =   "LOG4DBF"
   Begin VB.CheckBox chkTime
      Caption         =   "Include Time Info"
      Height          =   195
      Left            =   120
      TabIndex        =   19
      Top             =   3600
      Width           =   1815
   End
   Begin VB.CheckBox chkOverwrite
      Caption         =   "Over&write Files"
      Height          =   195
      Left            =   120
      TabIndex        =   18
      Top             =   3240
      Width           =   1815
   End
   Begin VB.Frame Frame2
      Caption         =   "Files to Include"
      Height          =   1095
      Index           =   4
      Left            =   5160
      TabIndex        =   31
      Top             =   2040
      Width           =   2895
      Begin VB.CommandButton cmdSelect
         Caption         =   "Select..."
         Enabled         =   0   'False
         Height          =   350
         Left            =   120
         TabIndex        =   25
         Top             =   600
         Width           =   1095
      End
      Begin VB.CheckBox chkAll
         Caption         =   "All"
         Height          =   255
         Left            =   120
         TabIndex        =   24
         Top             =   240
         Value           =   1  'Checked
         Width           =   1335
      End
      Begin VB.Label lblFiles
         AutoSize        =   -1  'True
         Caption         =   "0 Files Selected"
         Height          =   195
         Left            =   1440
         TabIndex        =   32
         Top             =   690
         Width           =   1125
      End
   End
   Begin VB.Frame Frame2
      Caption         =   "Different DataBase Structure Found"
      Height          =   855
      Index           =   0
      Left            =   5160
      TabIndex        =   30
      Top             =   120
      Width           =   2880
      Begin VB.OptionButton optMultiple
         Caption         =   "Increment file name"
         Height          =   255
         Index           =   0
         Left            =   120
         TabIndex        =   20
         Top             =   240
         Width           =   1815
      End
      Begin VB.OptionButton optMultiple
         Caption         =   "Warning "
         Height          =   255
         Index           =   1
         Left            =   120
         TabIndex        =   21
         Top             =   480
         Value           =   -1  'True
         Width           =   1215
      End
   End
   Begin VB.Frame Frame2
      Caption         =   "Unable to Find DataBase File"
      Height          =   855
      Index           =   1
      Left            =   5160
      TabIndex        =   29
      Top             =   1080
      Width           =   2880
      Begin VB.OptionButton optOriginal
         Caption         =   "Save Data"
         Height          =   255
         Index           =   0
         Left            =   120
         TabIndex        =   22
         Top             =   240
         Width           =   1335
      End
      Begin VB.OptionButton optOriginal
         Caption         =   "Error Message"
         Height          =   255
         Index           =   1
         Left            =   120
         TabIndex        =   23
         Top             =   480
         Value           =   -1  'True
         Width           =   1695
      End
   End
   Begin VB.CommandButton cmdOpenDialog
      Caption         =   "..."
      Height          =   300
      Index           =   0
      Left            =   4680
      TabIndex        =   2
      Top             =   120
      Width           =   300
   End
   Begin VB.TextBox data
      Height          =   300
      Index           =   5
      Left            =   1800
      TabIndex        =   10
      Top             =   1560
      Width           =   2775
   End
   Begin VB.TextBox data
      Height          =   300
      Index           =   4
      Left            =   1800
      TabIndex        =   8
      Top             =   1200
      Width           =   615
   End
   Begin VB.TextBox data
      Height          =   300
      Index           =   3
      Left            =   1800
      TabIndex        =   6
      Top             =   840
      Width           =   615
   End
   Begin VB.TextBox data
      Height          =   300
      Index           =   2
      Left            =   1800
      TabIndex        =   4
      Top             =   480
      Width           =   615
   End
   Begin VB.TextBox data
      Height          =   300
      Index           =   1
      Left            =   1800
      TabIndex        =   1
      Top             =   120
      Width           =   2775
   End
   Begin VB.CommandButton cmdOpenDialog
      Caption         =   "..."
      Height          =   300
      Index           =   1
      Left            =   4680
      TabIndex        =   11
      Top             =   1560
      Width           =   300
   End
   Begin VB.CommandButton cmdGo
      Caption         =   "Process"
      Default         =   -1  'True
      Height          =   350
      Left            =   5655
      TabIndex        =   26
      Top             =   4050
      Width           =   1095
   End
   Begin VB.CommandButton cmdClose
      Cancel          =   -1  'True
      Caption         =   "Close"
      Height          =   350
      Left            =   6975
      TabIndex        =   27
      Top             =   4050
      Width           =   1095
   End
   Begin VB.Frame Frame3
      Caption         =   "Transactions to be Translated"
      Height          =   975
      Left            =   120
      TabIndex        =   28
      Top             =   2160
      Width           =   4860
      Begin VB.CheckBox chkInclude
         Caption         =   "&Initial"
         Height          =   255
         Index           =   0
         Left            =   240
         TabIndex        =   12
         Top             =   240
         Value           =   1  'Checked
         Width           =   1095
      End
      Begin VB.CheckBox chkInclude
         Caption         =   "&Memo"
         Height          =   255
         Index           =   1
         Left            =   1740
         TabIndex        =   14
         Top             =   240
         Value           =   1  'Checked
         Width           =   1095
      End
      Begin VB.CheckBox chkInclude
         Caption         =   "&Open"
         Height          =   255
         Index           =   2
         Left            =   3240
         TabIndex        =   16
         Top             =   240
         Value           =   1  'Checked
         Width           =   1095
      End
      Begin VB.CheckBox chkInclude
         Caption         =   "&Rollback"
         Height          =   255
         Index           =   3
         Left            =   240
         TabIndex        =   13
         Top             =   600
         Value           =   1  'Checked
         Width           =   1095
      End
      Begin VB.CheckBox chkInclude
         Caption         =   "Tem&p"
         Height          =   255
         Index           =   4
         Left            =   1740
         TabIndex        =   15
         Top             =   600
         Value           =   1  'Checked
         Width           =   1095
      End
      Begin VB.CheckBox chkInclude
         Caption         =   "&Update"
         Height          =   255
         Index           =   5
         Left            =   3240
         TabIndex        =   17
         Top             =   600
         Value           =   1  'Checked
         Width           =   1095
      End
   End
   Begin MSComDlg.CommonDialog dlgMisc
      Left            =   3720
      Top             =   840
      _ExtentX        =   688
      _ExtentY        =   688
      _Version        =   393216
   End
   Begin VB.Label Label1
      AutoSize        =   -1  'True
      Caption         =   "&Log File"
      Height          =   195
      Index           =   0
      Left            =   120
      LinkTimeout     =   45
      TabIndex        =   0
      Top             =   165
      Width           =   555
   End
   Begin VB.Label Label1
      AutoSize        =   -1  'True
      Caption         =   "Misc. &Field Len."
      Height          =   195
      Index           =   1
      Left            =   120
      TabIndex        =   3
      Top             =   525
      Width           =   1110
   End
   Begin VB.Label Label1
      AutoSize        =   -1  'True
      Caption         =   "&Network User Id Len."
      Height          =   195
      Index           =   2
      Left            =   120
      TabIndex        =   5
      Top             =   885
      Width           =   1515
   End
   Begin VB.Label Label1
      AutoSize        =   -1  'True
      Caption         =   "Output &Directory"
      Height          =   195
      Index           =   3
      Left            =   120
      TabIndex        =   9
      Top             =   1605
      Width           =   1155
   End
   Begin VB.Label Label1
      AutoSize        =   -1  'True
      Caption         =   "&User Id Len."
      Height          =   195
      Index           =   4
      Left            =   120
      TabIndex        =   7
      Top             =   1245
      Width           =   870
   End
End
Attribute VB_Name = "frm32Dbf"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

'===================================================================================
'
' LOG4DBF Switch Constants
'
'===================================================================================

Const cLogFile = 1
Const cMiscLen = 2
Const cNetIdLen = 3
Const cUserIdLen = 4
Const cOutDir = 5
Const cInclude = 6
Const cOverwrite = 7
Const cTime = 8
Const cMultiple = 9
Const cOriginal = 10
Const ccAll = 11
Const cRestore = 12
Const cEncKey = 13
'Const cConfigFile = 14    'STANDALONE version: no need for this
                           'CONCURRENCY version: reqd
                           'CONCURRENCY version: cConfigFile = 6, shift everything else down

'STANDALONE version value
Const ctotalParams = 27
Const cArrayTotal = 26 'always ctotalParams - 1

'CONCURRENCY version values
'   Const ctotalParams = 27
'   Const cArrayTotal = 26 'always ctotalParams - 1

Const cOptAll = 0
Const cOptSelect = 1

Const cCmdButSelect = "Select"

Const cDoProcess = 0
Const cDoSelect = 1

Const csIncludeAll = "All"
Const csIncludeInitial = "Initial"
Const csIncludeMemo = "Memo"
Const csIncludeOpen = "Open"
Const csIncludeRollback = "Rollback"
Const csIncludeTemp = "Temp"
Const csIncludeUpdate = "Update"

Const ciIncludeAll = 0
Const ciIncludeInitial = 1
Const ciIncludeMemo = 2
Const ciIncludeOpen = 3
Const ciIncludeRollback = 4
Const ciIncludeTemp = 5
Const ciIncludeUpdate = 6

Const cIncludeStr = 0
Const cIncludeSelected = 1
Const cnumInclude = 7
'===================================================================================
'
' LOG4DBF Switch Variables
'
'===================================================================================

Dim vfilesSelected As Integer    'specific files selected for restoration
Dim vSwitches(14) As String
Dim vData(14) As String
'Dim vcmboInclude As String
Dim vInclude(7)
Dim vOrigLogFileString As String
Dim includeResult As String * 10

Private Function CountIncludeOptions%()
    'Counts the number of '-in' include options
    'set. This amount is used to determine how
    'larger the vDllData array should be expanded to

    Dim count%, i%

    For i = (ciIncludeInitial - 1) To (ciIncludeUpdate - 1)
        If chkInclude(i).value = vbChecked Then
            count = count + 1
        End If
    Next i

    CountIncludeOptions = count
End Function

Private Sub FormatData()
    'This Procedure calls a C function to obtain a
    'pointer to a string. This string is then assigned
    'to an array of string pointers. These pointers are
    'freed in the function FrmStatus.RunUtility()
    'NOTE: GLOBAL VARIABLE ACCESSED :vnumDataParams

    Dim i, j, skip, destNames, templateNames As Integer
    Dim bGetRestoreFiles, currentIndex As Integer

    Dim vDllDataSize%       'total final size of C paramter array - DF
    Dim includeOptions%, includeOptionsVal     'no. of transaction include options
    Dim position%  'used for parsing the '-include' string
    Dim vBasicIndex%, vDllIndex%
    Dim Result$

    If (Trim$(vData(ccAll)) = "OFF") Then     'restore files specified
       bGetRestoreFiles = True
    Else
       bGetRestoreFiles = False
    End If

    If (bGetRestoreFiles = True) Then
       'obtaining the number of field template and destination names
       'therefore increase the vDllData array by this amount
       destNames = 0
       templateNames = 0
       For i = 0 To (vRestoreItems - 1)
          If Not (Trim$(vDataRestore(i, cDest)) = cNoItems) Then
             destNames = destNames + 1
          End If

          If Not (Trim$(vDataRestore(i, cTemplate)) = cNoItems) Then
             templateNames = templateNames + 1
          End If
       Next i

       For i = 0 To (vRestoreItems - 1)
       Next i

       'redimension array with the original size (cArrayTotal)
       '+ the number of files being restored (vRestoreItems)
       '+ the "-ad" for each file being restored (* 2), less 1 for orig. allocation
       '+ the number of field template files to obtain (destNames)
       '+ the number of files being renamed (templateNames)
       '- the original allocation of a space for restoration (-1)
       vDllDataSize = cArrayTotal + ((vRestoreItems - 1) * 2) + destNames + templateNames  'DF
       'ReDim vDllData((cArrayTotal + (vRestoreItems * 2) + destNames + templateNames - 1))
    Else
       vDllDataSize = cArrayTotal - 2 ' don't need space for any '-add' switches
    End If

    'DF must increase the C array if user is not selecting default of ALL transactions
    'so we can add individual switches, one per entry found in vData(cInclude), which
    'is one string item, containing all options.
    If Not vData(cInclude) = csIncludeAll Then
       includeOptions = CountIncludeOptions() - 1  'Already have one item allocated, default - 'ALL'
       vDllDataSize = vDllDataSize + includeOptions
    End If

    ReDim vDllData(vDllDataSize)  'DF

    skip = 0
    'formatting the program name
    vDllData(cProgram) = v4Cstring(vData(cProgram))

    'don't adjust the vSwitches array until the '-in' option has been processed.
    includeOptionsVal = 0

    vBasicIndex = 1
    vDllIndex = 1

    For i = 1 To numParams
       If Len(Trim(vData(vBasicIndex))) > 0 Then
          vDllData(vDllIndex) = v4Cstring(vSwitches(vBasicIndex)) 'switch

          'If user isn't including 'ALL' transactions, then we must implement
          'special handling to put each transaction type (specified in one string)
          'into it's own array element - DF.
          If i = cInclude And vData(vBasicIndex) <> csIncludeAll Then
             j = 0
             position = 1
             Do While position       'add individual args for switch, each into it's own array element
                position = StrTok(position, vData(cInclude), Result, " ")
                If position Then
                   j = j + 1
                   'includeIndex = LocateIncludeIndex(result)
                   vDllData(vDllIndex + j) = v4Cstring(Result)
                End If
             Loop
             vDllIndex = vDllIndex + 1 + j
          Else
             vDllData(vDllIndex + 1) = v4Cstring(vData(vBasicIndex)) 'data for switch
             vDllIndex = vDllIndex + 2
          End If
       End If
       vBasicIndex = vBasicIndex + 1
    Next i

    'obtaining the specific files to be restored
    If (bGetRestoreFiles = True) Then
       'currentIndex = (numparams - skip) * 2
       For i = 0 To (vRestoreItems - 1)
          'currentIndex = currentIndex + 1
          vDllData(vDllIndex) = v4Cstring(vSwitches(cRestore))
          'currentIndex = currentIndex + 1
          vDllIndex = vDllIndex + 1
          vDllData(vDllIndex) = v4Cstring(vDataRestore(i, cTarget))

          If Not (Trim$(vDataRestore(i, cDest)) = cNoItems) Then
             vDllIndex = vDllIndex + 1
             vDllData(vDllIndex) = v4Cstring(vDataRestore(i, cDest))
             'currentIndex = currentIndex + 1
          End If

          If Not (Trim$(vDataRestore(i, cTemplate)) = cNoItems) Then
             vDllIndex = vDllIndex + 1
             vDllData(vDllIndex) = v4Cstring(vDataRestore(i, cTemplate))
             'currentIndex = currentIndex + 1
          End If
          If i < (vRestoreItems - 1) Then
             vDllIndex = vDllIndex + 1 ' LY 00/12/12 - new args overwriting old args
          End If
       Next i

       'adding additional parameter for the selected files and their destination names
       'vNumDataParams = (numparams * 2 + 1) - (skip * 2) + (vRestoreItems * 2) + destNames + templateNames
       vnumDataParams = vDllIndex + 1
    Else
       vnumDataParams = vDllIndex
    End If
End Sub

Private Sub GetUserData()
    Dim retstr As String
    Dim i As Integer, allSelected

    'CONCURRENCY version: get frmDbf.data(cConfigFile).text value
    '   vData(cConfigFile) = Trim$(frmDbf.data(cConfigFile).Text)

    vData(cLogFile) = Trim$(frm32Dbf.data(cLogFile).Text)
    vData(cOutDir) = Trim$(frm32Dbf.data(cOutDir).Text)
    'vData(cRemove) = Trim$(frmDbf.data(cRemove).Text)

    If (Len(Trim$(frm32Dbf.data(cMiscLen).Text)) > 0) Then
       vData(cMiscLen) = Trim$(frm32Dbf.data(cNetIdLen).Text)
    Else
       vData(cMiscLen) = "0"
    End If
    If (Len(Trim$(frm32Dbf.data(cNetIdLen).Text)) > 0) Then
       vData(cNetIdLen) = Trim$(frm32Dbf.data(cNetIdLen).Text)
    Else
       vData(cNetIdLen) = "20"
    End If
    If (Len(Trim$(frm32Dbf.data(cUserIdLen).Text)) > 0) Then
       vData(cUserIdLen) = Trim$(frm32Dbf.data(cUserIdLen).Text)
    Else
       vData(cUserIdLen) = "20"
    End If


    vData(cInclude) = ""
    allSelected = True
    For i = 1 To 6
       If (frm32Dbf.chkInclude(i - 1).value = 1) Then
          vData(cInclude) = vData(cInclude) & vInclude(i) & " "
       Else
          allSelected = False
       End If
    Next i
    If (allSelected) Then
       vData(cInclude) = csIncludeAll
    End If

    If chkAll.value = vbChecked Then
       retstr = "ON"
    Else
       retstr = "OFF"
    End If
    vData(ccAll) = retstr

    If chkOverwrite.value = vbChecked Then
       retstr = "ON"
    Else
       retstr = "OFF"
    End If
    vData(cOverwrite) = retstr

    If chkTime.value = vbChecked Then
       retstr = "ON"
    Else
       retstr = "OFF"
    End If
    vData(cTime) = retstr

    If (frm32Dbf.optMultiple(0).value = False) Then
       retstr = "OFF" ' LY 00/12/12 - switched "ON"/"OFF" to match SetUserData()
    Else
       retstr = "ON"
    End If
    vData(cMultiple) = retstr

    If (frm32Dbf.optOriginal(0).value = False) Then
       retstr = "ON"
    Else
       retstr = "OFF"
    End If
    vData(cOriginal) = retstr

    If Len(encryptionKeyFileName) > 0 Then
        vData(cEncKey) = "@" & encryptionKeyFileName
    Else
        vData(cEncKey) = ""
    End If
End Sub

Private Sub hideCmdGo(butHide As Integer)
    If (butHide = 1) Then   'show the Process Command Button
       frm32Dbf.cmdGo.Enabled = True
    End If

    If (vRestoreItems > 0) Then   'files selected for restore, allow processing to occur
       frm32Dbf.cmdGo.Enabled = True
    Else
       If (butHide <> 1) Then
          frm32Dbf.cmdGo.Enabled = False
       End If
    End If
End Sub

Private Sub initializeDbf()
    Dim i As Integer

    ReDim vDllData(cArrayTotal) As Long 'redimensioning global array
    'moved above line to FormatData() function -DF


    'CONCURRENCY version value:
    '   numparams = 12

    numParams = 13   'Input parameters for Log4Back
    'DanF - error in logic for the '-include' option.
    'low-level log function excpects each argument
    'as a separate argument, not as one string
    'consisting of all arguments. 6 possible args,
    'so add 5 to existing number
    'numparams = 16


    currentUtility = LOG4DBF

    'clear the vData array which hold the user input
    'For i = 0 To numparams
    '   vData(i) = ""
    'Next i

    'initialize the Include Option array of option names
    For i = 0 To cnumInclude
       Select Case i
          Case ciIncludeAll
             vInclude(i) = csIncludeAll

          Case ciIncludeInitial
             vInclude(i) = csIncludeInitial

          Case ciIncludeMemo
             vInclude(i) = csIncludeMemo

          Case ciIncludeOpen
             vInclude(i) = csIncludeOpen

          Case ciIncludeRollback
             vInclude(i) = csIncludeRollback

          Case ciIncludeTemp
             vInclude(i) = csIncludeTemp

          Case ciIncludeUpdate
             vInclude(i) = csIncludeUpdate
       End Select
    Next i

    'setup the abbreviated switches

    'CONCURRENCY version: use cConfigFile option
    '   vSwitches(cConfigFile) = "-c"

    vSwitches(cLogFile) = "-log"
    vSwitches(cMiscLen) = "-misclen"
    vSwitches(cNetIdLen) = "-netidlen"
    vSwitches(cUserIdLen) = "-useridlen"
    vSwitches(cOutDir) = "-outdir"
    vSwitches(cInclude) = "-include"
    vSwitches(cOverwrite) = "-overwrite"
    vSwitches(cTime) = "-time"
    vSwitches(cMultiple) = "-multiple"
    vSwitches(cOriginal) = "-original"
    vSwitches(ccAll) = "-all"
    vSwitches(cRestore) = "-add"   'add files to be deciphered from logfile
    vSwitches(cEncKey) = "-enckey"   'encryption key

    'storing utility name
    vSwitches(cProgram) = ""
    vData(cProgram) = "LOG4DBF"

    'Call initializeCmboInclude
    Call SetUserData
    Call hideCmdGo(1)   'Showing the Process Command Button
    frm32Dbf.lblFiles.caption = "All" & cFilesSelected 'Setting the Files Selected caption
End Sub

Private Function LocateIncludeIndex%(searchStr$)

   'This function returns an index into the 'vInclude' array
   'which contains a list of valid transaction types. Used by
   'FormatData(), which needs a reference to a VB string that
   'won't move around in memory.

   Dim sizeArray%, i%, Result

   sizeArray = UBound(vInclude) - LBound(vInclude) + 1
   Result = 0

   For i = 1 To sizeArray
      If UCase(LTrim(RTrim(searchStr))) = UCase(LTrim(RTrim(vInclude(i)))) Then
         Result = i
         Exit For
      End If
   Next i

   LocateIncludeIndex = Result

End Function

Private Sub performProcessOrSelect(operation As Integer)
    If (operation = cDoProcess) Then    'Process option selected
        Call GetUserData
        Call FormatData

        Call SaveSetting(App.title, Tag, "LogFile", Trim(data(cLogFile).Text))
        Call SaveSetting(App.title, Tag, "MiscLen", Trim(data(cMiscLen).Text))
        Call SaveSetting(App.title, Tag, "UserIdLen", Trim(data(cUserIdLen).Text))
        Call SaveSetting(App.title, Tag, "NetIdLen", Trim(data(cNetIdLen).Text))
        Call SaveSetting(App.title, Tag, "OutputDir", Trim(data(cOutDir).Text))
        Call SaveSetting(App.title, Tag, "Overwrite", chkOverwrite.value)
        Call SaveSetting(App.title, Tag, "Time", chkTime.value)
        Call SaveSetting(App.title, Tag, "Increment", optMultiple(0).value)
        Call SaveSetting(App.title, Tag, "Warning", optMultiple(1).value)
        Call SaveSetting(App.title, Tag, "Save", optOriginal(0).value)
        Call SaveSetting(App.title, Tag, "Error", optOriginal(1).value)

        frm32Status.Show 1, Me
    Else    'Select option chosen
        If (setLogFileName((frm32Dbf.data(cLogFile).Text)) = True) Then
            Call setFrmSelectCaptions
            If (DisplayForm(cFrmSelectCaption, 1) = False) Then
                Load frm32Select
            End If
            If (vRestoreItems > 0) Then
                vfilesSelected = 1
                Call hideCmdGo(0)    'show the appropriate Select button
            Else
                chkAll.value = vbUnchecked
                Call chkAll_Click   'reset command buttons
            End If
            If vRestoreItems = 1 Then
                frm32Dbf.lblFiles.caption = str$(vRestoreItems) & cFileSelected
            Else
                frm32Dbf.lblFiles.caption = str$(vRestoreItems) & cFilesSelected
            End If
       Else
          MsgBox "LogFile Name incorrect", 16 + 0, "Utitily Error"
       End If
    End If
End Sub

Private Sub setFrmSelectCaptions()
    'This procedure initializes the frmSelect's caption
    'variables by setting the form's global variables

    vlblList0 = "&Exclude Files"
    vlblList1 = "&Include Files"
    vlblList2 = "In&put File"
    vlblList3 = "&Output File"
    vcmdAction6 = "Template/Output File"
End Sub

Private Sub SetUserData()
    data(cLogFile).Text = GetSetting(App.title, Tag, "LogFile")
    data(cMiscLen).Text = GetSetting(App.title, Tag, "MiscLen", "0")
    data(cUserIdLen).Text = GetSetting(App.title, Tag, "UserIdLen", "20")
    data(cNetIdLen).Text = GetSetting(App.title, Tag, "NetIdLen", "20")
    data(cOutDir).Text = GetSetting(App.title, Tag, "OutputDir", "log4dbf")
    chkOverwrite.value = GetSetting(App.title, Tag, "Overwrite", vbUnchecked)
    chkTime.value = GetSetting(App.title, Tag, "Time", vbChecked)
    optMultiple(0).value = GetSetting(App.title, Tag, "Increment", False)
    optMultiple(1).value = GetSetting(App.title, Tag, "Warning", True)
    optOriginal(0).value = GetSetting(App.title, Tag, "Save", False)
    optOriginal(1).value = GetSetting(App.title, Tag, "Error", True)
End Sub

Private Function StrTok(pos%, source$, Result$, delim$)
   Dim start%, finish%, char$

   Result = ""
   start = pos

   'trim leading blanks
   Do While Mid$(source, start, 1) = " "
      start = start + 1
   Loop

   If start > Len(source) Then Exit Function

   Do
      char = Mid$(source, start, 1)
      If char = delim Then
         Exit Do
      Else
         Result = Result + char
         start = start + 1
      End If
   Loop Until char = delim Or start > Len(source)

   StrTok = start + 1
End Function

Private Function ValidateForm%()
   Dim aLog$, rc%, outDir$

   rc = True
   aLog = Trim(Me.data(cLogFile))

   If aLog = "" Then
      errMsg cErrNullName, "Log File"
      Me.data(cLogFile).SetFocus
      rc = False
   ElseIf Not Exist(aLog) Then
      errMsg cErrFileExist, aLog
      Me.data(cLogFile).SetFocus
      rc = False
   End If

   outDir = Trim(Me.data(cOutDir))

   If Right$(outDir, 1) = "\" Then
      Me.data(cOutDir) = Left$(outDir, Len(outDir) - 1)
   End If

   ValidateForm = rc
End Function

Private Sub chkInclude_Click(Index As Integer)
    Dim i As Integer, oneStillSelected As Integer

    oneStillSelected = False

    For i = (ciIncludeInitial - 1) To (ciIncludeUpdate - 1)
        If chkInclude(i).value = vbChecked Then
            oneStillSelected = True
            Exit For
        End If
    Next i

    If Not oneStillSelected Then
        chkInclude(Index).value = vbChecked
        MsgBox "WARNING: At least one item must be selected", 0 + 16, "CodeServer Utility Warning"
    End If
End Sub

Private Sub cmdClose_Click()
Unload frm32Dbf
End Sub

Private Sub cmdGo_Click()
    If Not ValidateForm() Then Exit Sub

    Call performProcessOrSelect(cDoProcess)
End Sub

Private Sub cmdOpenDialog_Click(Index As Integer)
    Select Case Index
        Case cDlgOpenLog
            InitFileDialog dlgMisc, Trim(data(cLogFile)), True
            '      dlgMisc.Action = cActionOpen
            dlgMisc.ShowOpen

            If Trim(dlgMisc.fileName) <> "" Then
               frm32Dbf.data(cLogFile).Text = LCase(dlgMisc.fileName)
            End If
        Case cDlgOutDir
            BrowseForFolder.SetDefaults
            BrowseForFolder.Display Me.hWnd, "Select output directory."
            If BrowseForFolder.successful Then
                frm32Dbf.data(cOutDir).Text = BrowseForFolder.folderName
            End If
   End Select
End Sub

Private Sub cmdSelect_Click()
    If Not ValidateForm() Then Exit Sub
    Call performProcessOrSelect(cDoSelect)
End Sub

Private Sub data_GotFocus(Index As Integer)
    TxtGotFocus Me
    Select Case Index
        Case cLogFile
            vOrigLogFileString = Trim$(frm32Dbf.data(cLogFile).Text)
    End Select
End Sub

Private Sub data_LostFocus(Index As Integer)
    Select Case Index
       Case cLogFile
          'logfile name has changed
          If (StrComp(vOrigLogFileString, Trim$(frm32Dbf.data(cLogFile).Text), vbTextCompare) <> 0) Then
             Call setListItemCount(0, 0)
             Unload frm32Select
             Call hideCmdGo(1)    'hiding the Select command
          End If
    End Select
End Sub

Private Sub Form_Load()
    HelpContextID = Log_Analyze

    Call initializeDbf
    Call SetDeviceIndependentWindow(frm32Dbf)
End Sub

Private Sub Form_Unload(Cancel As Integer)
    currentUtility = 0
    Unload frm32Select
    Call clearFrmSelectArrays
End Sub

Private Sub chkAll_Click()
    Call hideCmdGo(1)
    If chkAll.value = vbChecked Then
        cmdSelect.Enabled = False
        lblFiles.caption = "All" & cFilesSelected
    Else
        frm32Dbf.cmdSelect.Enabled = True
        If vfilesSelected = 0 Then
            lblFiles.caption = "0" & cFilesSelected
        Else
            lblFiles.caption = str$(vRestoreItems) & cFilesSelected
        End If
    End If
End Sub
