VERSION 5.00
Begin VB.Form FmBrowse
   Caption         =   "Browse Table"
   ClientHeight    =   5010
   ClientLeft      =   315
   ClientTop       =   1365
   ClientWidth     =   8895
   Icon            =   "subrowse.frx":0000
   LinkTopic       =   "Form8"
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   5010
   ScaleWidth      =   8895
   ShowInTaskbar   =   0   'False
   Begin VB.Menu mnuView
      Caption         =   "&View"
      Begin VB.Menu mnuNatural
         Caption         =   "&Natural Ordering"
         Checked         =   -1  'True
         Shortcut        =   ^N
      End
      Begin VB.Menu mnuSelect
         Caption         =   "&Select Index Tag..."
         Shortcut        =   ^T
      End
   End
End
Attribute VB_Name = "FmBrowse"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private codeBrowse&, dbBrowse&

Private Function getFile() As String
    Dim i As Integer
    With Form7.lstTables
        For i = 1 To .ListCount
            If .Selected(i) Then
                getFile = .List(i)
            End If
        Next i
    End With
    '(Form7.lstTables.ListIndex)
End Function

Private Sub Form_Load()

HelpContextID = File_Maintenance

Left = (Screen.Width - Width) / 2   ' Center form horizontally.
Top = (Screen.Height - Height) / 2  ' Center form vertically.

'For resizing grid if form size changes
frmStartHt = 1
frmStartWd = 1
grdHtPer = 100
grdWdPer = 100
grdMinHt = 495    'size of one row, plus menu height
grdMaxHt = 100000 'big no. means no real max height
grdMinWd = 1
grdMaxWd = 100000

Dim rc As Integer
Dim filePath As String
Dim tag As Long
Dim openFile As Boolean

'codeBrowse = code4init()
codeBrowse = gCode4 'use global CODE4
openFile = True
If codeBrowse <> 0 Then
    ResetCode4
    ErrOff
    Call code4singleOpen(codeBrowse, 0)
    'rc = code4connect(codeBrowse, svrId, portId, accountId, passwrd, "C4SOCK.DLL")
    'If rc = r4success Then
        ' next line removed by CS
'        Form7.TDBGrid2.bookmark = Form7.TDBGrid2.SelBookmarks(0)
        filePath = Trim(Form7.pwd)
        If Len(filePath) > 0 Then
            If Right(filePath, 1) <> SepChar Then
                filePath = filePath + SepChar
            End If
        End If
        filePath = Form7.lstTables.List(Form7.lstTables.ListIndex)
        If UCase(filePath) = SystemPath + "ACCOUNT4.dbf" Then
            MsgBox "Unable to open ACCOUNT4.dbf" + vbCrLf _
                 + "This table contains sensitive information and cannot be browsed." + vbCrLf _
                 + "Select the Report option under the Account menu to view this table.", _
                   vbCritical, "ACCOUNT4.dbf"
            openFile = False
        Else
            dbBrowse = d4open(codeBrowse, filePath)
            If dbBrowse <> 0 Then
                Call d4tagSelect(dbBrowse, 0)

                MyData1.RecordSource = d4encodeHandle(dbBrowse)
            Else
                'could not open the data file
                ShowError "Could not open " & filePath & " file" & vbCrLf & "Ensure that your account has the necessary authorization to open this file"
                openFile = False
            End If
        End If
    Else
        MsgBox "ERROR: Could not connect to the server", vbOKOnly + vbExclamation, "Error Connecting to Server"
        openFile = False
    End If

browseTableOpen = False

If openFile Then
   browseTableOpen = True
Else
   closeFiles
End If

End Sub


Private Sub Form_Resize()
    ReSizeGrid TDBGrid2, Me
End Sub
Private Sub Form_Unload(Cancel As Integer)
    closeFiles
End Sub
Private Sub mnuAcctId_Click()
'    d4tagSelect dbBrowse, d4tag(dbBrowse, "acct")
'    MyData1.Refresh
'    mnuAcctId.Checked = True
'    mnuName.Checked = False
'    mnuNatural.Checked = False
End Sub
Private Sub mnuName_Click()
'    d4tagSelect dbBrowse, d4tag(dbBrowse, "namepass")
'    MyData1.Refresh
'    mnuAcctId.Checked = False
'    mnuName.Checked = True
'    mnuNatural.Checked = False
End Sub
Private Sub mnuNatural_Click()
    d4tagSelect dbBrowse, 0
    MyData1.Refresh
    mnuNatural.Checked = True
End Sub
Public Sub closeFiles()
'    If codeBrowse <> 0 Then
'        code4close (codeBrowse)
'        code4initUndo (codeBrowse)
'        dbBrowse = 0
'        codeBrowse = 0
'    End If
    If dbBrowse <> 0 Then
        Call d4close(dbBrowse)
        dbBrowse = 0
    End If
End Sub

Private Sub mnuSelect_Click()
    Call frmTagSelect.init(dbBrowse)
    MyData1.Refresh
    If d4tagSelected(dbBrowse) = 0 Then
        mnuNatural.Checked = True
    Else
        mnuNatural.Checked = False
    End If
End Sub

