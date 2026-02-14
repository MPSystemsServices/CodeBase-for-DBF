VERSION 5.00
Begin VB.Form frm32Select
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "File Selection"
   ClientHeight    =   3240
   ClientLeft      =   30
   ClientTop       =   315
   ClientWidth     =   8835
   ControlBox      =   0   'False
   LinkTopic       =   "Form1"
   LockControls    =   -1  'True
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   3240
   ScaleWidth      =   8835
   ShowInTaskbar   =   0   'False
   StartUpPosition =   3  'Windows Default
   Begin VB.TextBox txtPath
      BackColor       =   &H8000000F&
      Height          =   300
      Left            =   780
      Locked          =   -1  'True
      TabIndex        =   3
      Top             =   2400
      Width           =   5655
   End
   Begin VB.TextBox txtInfo
      Height          =   285
      Index           =   3
      Left            =   6600
      TabIndex        =   13
      Top             =   1260
      Width           =   2175
   End
   Begin VB.TextBox txtInfo
      Height          =   285
      Index           =   2
      Left            =   6600
      TabIndex        =   11
      Top             =   540
      Width           =   2175
   End
   Begin VB.Timer timerStatus
      Interval        =   1
      Left            =   3540
      Top             =   2100
   End
   Begin VB.ListBox lstFiles
      Height          =   2010
      Index           =   1
      Left            =   4140
      MultiSelect     =   2  'Extended
      TabIndex        =   9
      Top             =   300
      Width           =   2292
   End
   Begin VB.ListBox lstFiles
      Height          =   2010
      Index           =   0
      Left            =   60
      MultiSelect     =   2  'Extended
      TabIndex        =   1
      Top             =   300
      Width           =   2292
   End
   Begin VB.CommandButton Command1
      Caption         =   "Refresh Files"
      Height          =   375
      Left            =   6900
      TabIndex        =   16
      Top             =   1860
      Visible         =   0   'False
      Width           =   1455
   End
   Begin VB.CommandButton cmdEnd
      Cancel          =   -1  'True
      Caption         =   "Cancel"
      Height          =   350
      Index           =   1
      Left            =   7620
      TabIndex        =   15
      Top             =   2820
      Width           =   1130
   End
   Begin VB.CommandButton cmdEnd
      Caption         =   "OK"
      Default         =   -1  'True
      Height          =   350
      Index           =   0
      Left            =   6360
      TabIndex        =   14
      Top             =   2820
      Width           =   1130
   End
   Begin VB.CommandButton cmdAction
      Caption         =   "Ex&clude All <--"
      Height          =   375
      Index           =   5
      Left            =   2460
      TabIndex        =   7
      Top             =   1560
      Width           =   1575
   End
   Begin VB.CommandButton cmdAction
      Caption         =   "E&xclude <--"
      Height          =   375
      Index           =   3
      Left            =   2460
      TabIndex        =   6
      Top             =   1140
      Width           =   1575
   End
   Begin VB.CommandButton cmdAction
      Caption         =   "&Include -->"
      Height          =   375
      Index           =   2
      Left            =   2460
      TabIndex        =   5
      Top             =   720
      Width           =   1575
   End
   Begin VB.CommandButton cmdAction
      Caption         =   "<-- Select &All"
      Height          =   375
      Index           =   0
      Left            =   2460
      TabIndex        =   4
      Top             =   300
      Width           =   1575
   End
   Begin VB.ListBox lstFiles
      Height          =   2010
      Index           =   2
      Left            =   4140
      MultiSelect     =   2  'Extended
      TabIndex        =   17
      Top             =   300
      Visible         =   0   'False
      Width           =   2292
   End
   Begin VB.ListBox lstFiles
      Height          =   2010
      Index           =   3
      Left            =   4140
      MultiSelect     =   2  'Extended
      TabIndex        =   18
      Top             =   300
      Visible         =   0   'False
      Width           =   2292
   End
   Begin VB.Label lblList
      AutoSize        =   -1  'True
      Caption         =   "Destination File Name"
      Height          =   195
      Index           =   3
      Left            =   6600
      TabIndex        =   12
      Top             =   1020
      Width           =   1545
   End
   Begin VB.Label lblList
      AutoSize        =   -1  'True
      Caption         =   "Destination File Name"
      Height          =   195
      Index           =   2
      Left            =   6600
      TabIndex        =   10
      Top             =   300
      Width           =   1545
   End
   Begin VB.Label lblList
      AutoSize        =   -1  'True
      Caption         =   "Restore Files"
      Height          =   195
      Index           =   1
      Left            =   4140
      TabIndex        =   8
      Top             =   60
      Width           =   915
   End
   Begin VB.Label lblList
      AutoSize        =   -1  'True
      Caption         =   "Exclude Files"
      Height          =   195
      Index           =   0
      Left            =   60
      TabIndex        =   0
      Top             =   60
      Width           =   930
   End
   Begin VB.Label Label1
      AutoSize        =   -1  'True
      Caption         =   "Path:"
      Height          =   195
      Left            =   60
      TabIndex        =   2
      Top             =   2445
      Width           =   375
   End
End
Attribute VB_Name = "frm32Select"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

'===================================================================================
'
' FORM SELECTION Switch Constants
'
'===================================================================================

Const cSelectAll = 0
Const cUnselectAll = 1
Const cRestore = 2
Const cExclude = 3
Const cRestoreAll = 4
Const cExcludeAll = 5
Const cDestName = 6

Const ctxtDest = 2
Const ctxtTemplate = 3
Const cDefaultName = "(default)"

'===================================================================================
'
' FORM SELECTION Switch Variables
'
'===================================================================================
Dim vProperExit%   'Did user select a button to exit form or exiting from system menu
Dim vlstboxSelected% 'currently selected index in the listbox
Dim rc As Integer  ' reqd for error checking on form Load
Dim frmLoaded As Boolean

Private vFormTitle$
Private vlbl0$
Private vlbl1$
Private vlbl2$

Private Sub FreeItemDataString()
    'This procedure frees the memory allocated for the
    'pathnames in the listbox ItemData property
    'The data was allocated in the DLL and the pointer
    'address is stored in the ItemData property
    Dim i, totalExcludeItems, totalRestoreItems As Integer

    totalExcludeItems = frm32Select.lstFiles(cListExclude).ListCount
    totalRestoreItems = frm32Select.lstFiles(cListRestore).ListCount

    For i = 1 To (totalExcludeItems - 1)
        Call u4free((frm32Select.lstFiles(cListExclude).itemdata(i)))
    Next i
    For i = 1 To (totalRestoreItems - 1)
        Call u4free((frm32Select.lstFiles(cListRestore).itemdata(i)))
    Next i
End Sub

Private Sub getFrmSelectCaptions()
    'This procedure initializes the frmSelect's caption
    'variables by obtaining the form's global variables

    frm32Select.lblList(0).caption = vlblList0
    frm32Select.lblList(1).caption = vlblList1
    frm32Select.lblList(2).caption = vlblList2
    If (Len(vlblList3) > 0) Then
        frm32Select.lblList(3).caption = vlblList3
    Else
        frm32Select.lblList(3).Visible = False
        frm32Select.lblList(3).Enabled = False
        frm32Select.txtInfo(3).Visible = False
    End If

'frmSelect.cmdAction(cDestName).Caption = vcmdAction6
End Sub

Private Function GetItemDataString(lb As ListBox, sIndex%) As String
    If (lb.itemdata(sIndex) = 0) Then
        GetItemDataString = ""
    Else
        GetItemDataString = b4String((lb.itemdata(sIndex)))
    End If
End Function

Private Sub initializeSelect()
    Dim cListInclude%
    rc = 0
    'Call getFrmSelectCaptions
    vProperExit = False
    Select Case currentUtility
        Case LOG4RES
            rc = getLogDatabases(getLogFileName(), frm32Select.lstFiles(cListInclude).hWnd)
        Case LOG4DBF
            ' just using the log file name, not the config file info
            cListInclude = 0
            rc = getLogDatabases(getLogFileName(), frm32Select.lstFiles(cListInclude).hWnd)
    End Select

    If (rc < 0) Then  'error has occurred
        If frmLoaded Then
            frm32Select.Hide
        End If
        frm32Select.timerStatus.Enabled = True
        'Call setListItemCount(0, 0)
        'Unload frmSelect
    End If
End Sub

Private Function setFormDestFile() As Integer
    'This procedure sets the entry data variables for frmDest
    'Returns: the listbox index of the filename to be replaced
    '         -1 no file selected
    '        > 0 list box index item number of file to be replaced
    Dim i, targetIndex As Integer

    targetIndex = -1
    i = 0
    Do While (targetIndex = -1) And (i < frm32Select.lstFiles(cListRestore).ListCount)
        If (frm32Select.lstFiles(cListRestore).Selected(i) = True) Then
            targetIndex = i
        End If
        i = i + 1
    Loop
    If (targetIndex > -1) Then
        Select Case currentUtility
            Case LOG4RES
                vFormTitle = "Destination File Name"
                vlbl0 = "File name : " & frm32Select.lstFiles(cListRestore).List(targetIndex)
                vlbl1 = "Enter Destination name <optional>"
                vlbl2 = ""     'third paramter is not used
                'If frm32Select.lstFiles(cListDest).List(targetIndex) = cNoFiles Then
                '    vtxtbox = ""
                'Else
                '    vtxtbox = frm32Select.lstFiles(cListDest).List(targetIndex)
                'End If
                'vtxtbox = Trim$(vDataRestore(targetIndex, cDest))
            Case LOG4DBF
                vFormTitle = "Target/Field Template File Names"
                vlbl0 = "File name : " & frm32Select.lstFiles(cListRestore).List(targetIndex)
                vlbl1 = "Field Template name <optional>"
                vlbl2 = "Enter Destination name <optional>"
                'vtxtbox = ""
                'vtxtbox1 = ""
        End Select
    End If

    setFormDestFile = targetIndex
End Function

Private Sub SetlblPath(pathName$)
    frm32Select.txtPath.Text = Trim$(pathName)
End Sub

Private Sub storeTxtBoxData(Index As Integer)
Dim selectedString, textString As String
Dim selectedIndex As Integer

'frmSelect.txtInfo(Index).BackColor = &HC0C0C0   'change txtbox color to grey

selectedIndex = vlstboxSelected
selectedString = frm32Select.lstFiles(cListRestore).List(selectedIndex)

If (validListIndex(frm32Select.lstFiles(cListRestore), selectedIndex)) Then
   textString = frm32Select.txtInfo(Index).Text
   If (Len(Trim$(textString)) > 0) Then

      'If (StrComp(Trim$(selectedString), Trim$(textString), 1) <> 0) Then   'strings are the same
         'modifying the appropriate hidden listbox
         frm32Select.lstFiles(Index).List(selectedIndex) = Trim$(textString)
      'End If
   Else
      frm32Select.txtInfo(Index).Text = selectedString
      frm32Select.lstFiles(Index).List(selectedIndex) = cNoItems
   End If
Else
  frm32Select.txtInfo(ctxtDest) = ""
  frm32Select.txtInfo(ctxtTemplate) = ""
End If

End Sub

Private Sub updateTxtBoxes()
Dim duplicateFound, i  As Integer
Dim selectedIndex%
Dim templateString, destString, selectedString As String

duplicateFound = False
i = 0
selectedIndex = frm32Select.lstFiles(cListRestore).ListIndex
selectedString = frm32Select.lstFiles(cListRestore).List(selectedIndex)

If (validListIndex(frm32Select.lstFiles(cListRestore), selectedIndex)) Then    'last item being removed from listbox
   If ((selectedIndex < 0) Or (frm32Select.lstFiles(cListRestore).Selected(selectedIndex) = False)) Then
      selectedIndex = -1
   End If
Else
   selectedIndex = -1
End If

If (selectedIndex > -1) Then
   destString = frm32Select.lstFiles(cListDest).List(selectedIndex)
   If (destString = cNoItems) Then
      frm32Select.txtInfo(cListDest).Text = selectedString
   Else
      frm32Select.txtInfo(cListDest).Text = destString
   End If

   templateString = frm32Select.lstFiles(cListTemplate).List(selectedIndex)
   If (templateString = cNoItems) Then
      frm32Select.txtInfo(cListTemplate).Text = selectedString
   Else
      frm32Select.txtInfo(cListTemplate).Text = templateString
   End If

Else
  frm32Select.txtInfo(ctxtDest) = ""
  frm32Select.txtInfo(ctxtTemplate) = ""
End If

'While Not duplicateFound
'   If (frmSelect.lstFiles(cListRestore).Selected(i) = True) Then
'      if( selectedIndex
End Sub

Private Function validListIndex(lstbox As ListBox, Index%) As Integer
    'This procedure checks to ensure the index is in the listbox
    If (Index > -1) And (lstbox.ListCount > Index) Then
        validListIndex = True
    Else
        validListIndex = False
    End If
End Function

Private Sub cmdAction_Click(Index As Integer)
    Dim i, totalExcludeItems, totalRestoreItems As Integer
    Dim totalItems, listBoxType As Integer
    Dim FileNameIndex As Integer

    totalExcludeItems = frm32Select.lstFiles(cListExclude).ListCount
    totalRestoreItems = frm32Select.lstFiles(cListRestore).ListCount

    totalItems = totalExcludeItems
    listBoxType = cListExclude
    If (Index = cExclude) Or (Index = cExcludeAll) Or (Index = cDestName) Then
        totalItems = totalRestoreItems
        listBoxType = cListRestore
    End If

    If (totalItems > 0) And Not (frm32Select.lstFiles(listBoxType).List(0) = cNoItems) Then
        Select Case Index
            Case cSelectAll
                For i = 0 To (totalExcludeItems - 1)
                    frm32Select.lstFiles(cListExclude).Selected(i) = True
                Next i
            Case cUnselectAll
                For i = 0 To (totalExcludeItems - 1)
                    frm32Select.lstFiles(cListExclude).Selected(i) = False
                Next i
            Case cRestore
                i = 0
                Do While (i < totalExcludeItems)
                    If (frm32Select.lstFiles(cListExclude).Selected(i) = True) Then
                        frm32Select.lstFiles(cListRestore).AddItem frm32Select.lstFiles(cListExclude).List(i)
                        frm32Select.lstFiles(cListRestore).itemdata(frm32Select.lstFiles(cListRestore).NewIndex) = frm32Select.lstFiles(cListExclude).itemdata(i)
                        frm32Select.lstFiles(cListDest).AddItem cNoItems
                        frm32Select.lstFiles(cListTemplate).AddItem cNoItems
                        frm32Select.lstFiles(cListExclude).RemoveItem i
                        i = i - 1
                        totalExcludeItems = frm32Select.lstFiles(cListExclude).ListCount
                    End If
                    i = i + 1
                Loop
                Call SetlblPath("")
            Case cExclude
                i = 0
                Do While (i < totalRestoreItems)
                    If (frm32Select.lstFiles(cListRestore).Selected(i) = True) Then
                        frm32Select.lstFiles(cListExclude).AddItem frm32Select.lstFiles(cListRestore).List(i)
                        frm32Select.lstFiles(cListExclude).itemdata(frm32Select.lstFiles(cListExclude).NewIndex) = frm32Select.lstFiles(cListRestore).itemdata(i)
                        frm32Select.lstFiles(cListRestore).RemoveItem i
                        frm32Select.lstFiles(cListDest).RemoveItem i
                        frm32Select.lstFiles(cListTemplate).RemoveItem i
                        i = i - 1
                        totalRestoreItems = frm32Select.lstFiles(cListRestore).ListCount
                    End If
                    i = i + 1
                Loop
            Case cRestoreAll
                i = 0
                Do While (i < totalExcludeItems)
                    frm32Select.lstFiles(cListRestore).AddItem frm32Select.lstFiles(cListExclude).List(i)
                    frm32Select.lstFiles(cListRestore).itemdata(frm32Select.lstFiles(cListRestore).NewIndex) = frm32Select.lstFiles(cListExclude).itemdata(i)
                    frm32Select.lstFiles(cListDest).AddItem cNoItems
                    frm32Select.lstFiles(cListTemplate).AddItem cNoItems
                    frm32Select.lstFiles(cListExclude).RemoveItem i
                    totalExcludeItems = frm32Select.lstFiles(cListExclude).ListCount
                Loop
                Call SetlblPath("")
            Case cExcludeAll
                i = 0
                Do While (i < totalRestoreItems)
                    frm32Select.lstFiles(cListExclude).AddItem frm32Select.lstFiles(cListRestore).List(i)
                    frm32Select.lstFiles(cListExclude).itemdata(frm32Select.lstFiles(cListExclude).NewIndex) = frm32Select.lstFiles(cListRestore).itemdata(i)
                    frm32Select.lstFiles(cListRestore).RemoveItem i
                    frm32Select.lstFiles(cListDest).RemoveItem i
                    frm32Select.lstFiles(cListTemplate).RemoveItem i
                    totalRestoreItems = frm32Select.lstFiles(cListRestore).ListCount
                Loop
'      Case cDestName
'         FileNameIndex = setFormDestFile()
'         If (FileNameIndex > -1) Then
'            frmDest.Show 1
'            If (vfrmDestCancel = False) Then
'               If (Len(frmSelect.lstFiles(cListDest).List(FileNameIndex)) > 0) Then
'                  frmSelect.lstFiles(cListDest).List(FileNameIndex) = Trim$(vtxtbox)
'                  'frmSelect.txtInfo(0).Text = Trim$(vtxtbox)
'               Else
'                  frmSelect.lstFiles(cListDest).List(FileNameIndex) = cNoItems
'                  'frmSelect.txtInfo(0).Text = cDefaultName
'               End If
'               If ((currentUtility = LOG4DBF) And (Len(frmSelect.lstFiles(cListTemplate).List(FileNameIndex))) > 0) Then
'                  frmSelect.lstFiles(cListTemplate).List(FileNameIndex) = Trim$(vtxtbox1)
'               Else
'                  frmSelect.lstFiles(cListTemplate).List(FileNameIndex) = cNoItems
'               End If
'
'               'vDataRestore(FileNameIndex, cDest) = Trim$(vtxtbox)
'               'frmSelect.lstFiles(cListRestore).Tag = Trim$(vtxtbox)
'               frmSelect.lstFiles(cListRestore).Selected(FileNameIndex) = False
'               frmSelect.txtInfo(0).Text = ""
'               frmSelect.txtInfo(1).Text = ""
'            End If

'         End If

        End Select
        Call updateTxtBoxes
    End If
End Sub

Private Sub cmdEnd_Click(Index As Integer)
    Select Case Index
        Case cCmdOk
            Call setListItemCount((frm32Select.lstFiles(cListExclude).ListCount), (frm32Select.lstFiles(cListRestore).ListCount))
        Case cCmdCancel
            Call setListItemCount(0, 0)
    End Select
    vProperExit = True
    Me.Hide
End Sub

Private Sub Command1_Click()
    Call initializeSelect
End Sub

Private Sub Form_GotFocus()
    vProperExit = False
End Sub

Private Sub Form_Load()
    frmLoaded = False
    Call getFrmSelectCaptions
    Call SetDeviceIndependentWindow(frm32Select)
    Call initializeSelect
    If (rc = 0) Then
        frmLoaded = True
        frm32Select.Show 1
        'frmSelect.Visible = True
    End If
End Sub

Private Sub Form_Unload(Cancel As Integer)
    If (vProperExit = False) Then
        Call setListItemCount(0, 0)
    End If
    Call FreeItemDataString
End Sub

Private Sub lstFiles_Click(Index As Integer)
    'This event sets the appropriate info in txtInfo text box
    'This event also may set the path info in the lblPath label

    Dim selectedString, destString, templateString As String
    Dim selectedIndex As Integer

    Select Case Index
        Case cListRestore
            selectedIndex = frm32Select.lstFiles(cListRestore).ListIndex
            selectedString = frm32Select.lstFiles(cListRestore).List(selectedIndex)

            destString = frm32Select.lstFiles(cListDest).List(selectedIndex)
            If (destString = cNoItems) Then
                txtInfo(cListDest).Text = Trim(selectedString)
            Else
                txtInfo(cListDest).Text = Trim(destString)
            End If

            templateString = lstFiles(cListTemplate).List(selectedIndex)
            If (templateString = cNoItems) Then
                txtInfo(cListTemplate).Text = Trim(selectedString)
            Else
                txtInfo(cListTemplate).Text = Trim(templateString)
            End If
        Case cListExclude
            selectedIndex = lstFiles(Index).ListIndex
            Call SetlblPath((GetItemDataString(lstFiles(Index), selectedIndex)))
    End Select
End Sub

Private Sub lstFiles_DblClick(Index As Integer)
    Select Case Index
        Case cListExclude
            Call cmdAction_Click(cRestore)
        Case cListRestore
            Call cmdAction_Click(cExclude)
    End Select
End Sub

Private Sub lstFiles_GotFocus(Index As Integer)
    Call SetlblPath("")
End Sub

Private Sub timerStatus_Timer()
    frm32Select.timerStatus.Enabled = False
    If rc < 0 Then   'error has occurred
        Call setListItemCount(0, 0)
        Unload frm32Select
    End If
End Sub

Private Sub txtInfo_GotFocus(Index As Integer)
    TxtGotFocus Me
    vlstboxSelected = frm32Select.lstFiles(cListRestore).ListIndex
End Sub

Private Sub txtInfo_LostFocus(Index As Integer)
    Call storeTxtBoxData(2) 'storing both textbox data into lists
    If frm32Select.txtInfo(3).Visible = True Then
        Call storeTxtBoxData(3)
    End If
End Sub

Private Sub txtPath_GotFocus()
    TxtGotFocus Me
End Sub


