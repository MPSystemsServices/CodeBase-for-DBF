VERSION 5.00
Begin VB.Form Form11
   Caption         =   "Privileges Report- Tables with AccountId "
   ClientHeight    =   3675
   ClientLeft      =   330
   ClientTop       =   1950
   ClientWidth     =   9390
   Icon            =   "suprivr1.frx":0000
   LinkTopic       =   "Form11"
   LockControls    =   -1  'True
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   3675
   ScaleWidth      =   9390
   ShowInTaskbar   =   0   'False
End
Attribute VB_Name = "Form11"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private codePrivTable&
Private dbPrivTable&


Private Sub Form_Load()

HelpContextID = Privileges_Reports

Left = (Screen.Width - Width) / 2   ' Center form horizontally.
Top = (Screen.Height - Height) / 2  ' Center form vertically.

'For resizing grid if form size changes
frmStartHt = 1
frmStartWd = 1
grdHtPer = 100
grdWdPer = 100
grdMinHt = 495    'size of one row, plus menu height
grdMaxHt = 100000 'big no. means no real max height
grdMinWd = 1   'width of just first column
grdMaxWd = 100000

'open the PRIV4 file with the MyData control
Dim rc As Integer
Dim st As String
Dim mySplit As TrueDBGrid45.Split

'codePrivTable = code4init()
codePrivTable = gCode4 ' use global CODE4
If codePrivTable <> 0 Then
    Call ResetCode4
    rc = code4singleOpen(codePrivTable, 0)
    Call ErrOff
    dbPrivTable = d4open(codePrivTable, systemPath & "PRIV4")
    If dbPrivTable <> 0 Then
        'select tag
        Call d4tagSelect(dbPrivTable, d4tag(dbPrivTable, "TABLE"))

        st = d4encodeHandle(dbPrivTable)
        MyData1.RecordSource = st

        ' Create an additional split:
        Set mySplit = TDBGrid2.Splits.Add(0)
        ' Hide all columns in the leftmost split, Splits(0),
        ' except for columns 0 and 1

        Dim i As Integer
        Dim Cols As Variant
        Set Cols = TDBGrid2.Splits(0).Columns
        For i = 0 To Cols.count - 1
            Cols(i).Visible = False
        Next i
        Cols(0).Visible = True
        Cols(1).Visible = True

        ' Configure Splits(0) to display exactly two columns,
        ' and disable resizing
        With TDBGrid2.Splits(0)
            .SizeMode = dbgNumberOfColumns
            .Size = 2
            .AllowSizing = False
        End With

        ' Usually, if you fix columns 0 and 1 from scrolling
        ' in a split, you will want to make them invisible in
        ' other splits:
        Set Cols = TDBGrid2.Splits(1).Columns
        Cols(0).Visible = False
        Cols(1).Visible = False

        ' Turn off the record selectors in Split 1:
        TDBGrid2.Splits(1).RecordSelectors = False

    Else
        'could not open the data file
        ShowError "File Name: " & systemPath & "PRIV4.dbf", "This file is required for administrating file privileges. This error may be caused if you don't have appropriate access priviliges for this data file."
    End If

    Call ErrOn

End If

End Sub
Private Sub Form_Resize()
ReSizeGrid TDBGrid2, Me
End Sub


Private Sub Form_Unload(Cancel As Integer)

'If codePrivTable <> 0 Then
'    code4close (codePrivTable)
'    code4initUndo (codePrivTable)
'    dbPrivTable = 0
'    codePrivTable = 0
'End If
CloseDB dbPrivTable
End Sub

