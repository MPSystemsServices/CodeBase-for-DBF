'===================================================================================
'
'     FILE SELECTION FORM Global Constants
'
'===================================================================================
Global Const cFrmSelectCaption = "File Selection"
Global Const cListExclude = 0
Global Const cListRestore = 1
Global Const cListDest = 2
Global Const cListTemplate = 3

'===================================================================================
'
'     FILE SELECTION FORM Global variables
'
'===================================================================================

'===================================================================================
'        Label Caption variables
'===================================================================================
Global vlblList0 As String
Global vlblList1 As String
Global vlblList2 As String
Global vlblList3 As String

'===================================================================================
'        Command Button Caption variables
'===================================================================================
Global vcmdAction6 As String

Sub setListItemCount(numExcludeItems As Integer, numRestoreItems As Integer)
'This procedure sets the global variables (frmRes & frmDbf) of
'which files are being restored from the logfile (dimension 0),
'the file's are obtaining their field template from (dimension 1),
'and the file's destination name (dimension 2).
'This procedure then saves the data in the appropriate array

Dim i As Integer

ReDim vDataRestore(numRestoreItems, 2) As String
ReDim vDataExclude(numExcludeItems) As String

vRestoreItems = numRestoreItems
vExcludeItems = numExcludeItems

If (numExcludeItems > 0) Then
   For i = 0 To (numExcludeItems - 1)
      vDataExclude(i) = frm32Select.lstFiles(cListExclude).List(i)
   Next i
End If

If (numRestoreItems > 0) Then
   For i = 0 To (numRestoreItems - 1)
      vDataRestore(i, cTarget) = Trim(frm32Select.lstFiles(cListRestore).List(i))
      vDataRestore(i, cDest) = Trim(frm32Select.lstFiles(cListDest).List(i))
      vDataRestore(i, cTemplate) = Trim(frm32Select.lstFiles(cListTemplate).List(i))
   Next i
End If

End Sub

