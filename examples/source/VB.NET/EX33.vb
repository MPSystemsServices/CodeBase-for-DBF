Option Strict Off
Option Explicit On
Module EXAMPLE
    'EX33.VB
	
	Public lf As String 'Line Feed
	Public fPath As String 'Full path name to db files
	
	Public Const MB_OK As Short = 0
	Public Const MB_YESNO As Short = 4
	Public Const MB_ICONQUESTION As Short = 32
	Public Const IDYES As Short = 6
	
    Sub ExCode(ByRef frm As Form1)
        Dim db, cb, secondFile As Integer
        Dim fieldArray() As FIELD4INFO
        Dim rc As Short

        cb = code4init()
        rc = code4safety(cb, 0) 'overwrite the file if it exists

        ReDim fieldArray(3)
        fieldArray(0).fName = "NAME_FIELD"
        fieldArray(0).ftype = r4str
        fieldArray(0).flength = 20
        fieldArray(0).fdecimals = 0

        fieldArray(1).fName = "AGE_FIELD"
        fieldArray(1).ftype = r4num
        fieldArray(1).flength = 2
        fieldArray(1).fdecimals = 0

        fieldArray(2).fName = "BIRTH_DATE"
        fieldArray(2).ftype = r4date
        fieldArray(2).flength = 8
        fieldArray(2).fdecimals = 0

        'Specify full path if stand-alone
        If u4switch() And &H80S Then fPath = VB6.GetPath & "\"

        db = d4createData(cb, fPath & "NEWDBF", fieldArray)

        error4exitTest((cb))

        If code4errorCode(cb, r4check) < 0 Then
            MsgBox("An error occurred, NEWDBF was not created")
        Else
            MsgBox("Created successfully")
        End If

        rc = code4close(cb)
        rc = code4initUndo(cb)
    End Sub
End Module