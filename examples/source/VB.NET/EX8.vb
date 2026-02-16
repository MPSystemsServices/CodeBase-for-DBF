Option Strict Off
Option Explicit On
Module EXAMPLE
    'EX8.VB
	
	Public lf As String 'Line Feed
	Public fPath As String 'Full path name to data files
	
	Public Const MB_OK As Short = 0
	Public Const MB_YESNO As Short = 4
	Public Const MB_ICONQUESTION As Short = 32
	Public Const IDYES As Short = 6
	
    Sub ExCode(ByRef frm As Form1)
        'ex8 example code

        Dim cb, db As Integer
        Dim fieldInfo() As FIELD4INFO
        Dim rc As Short

        cb = code4init()
        ReDim fieldInfo(2)
        fieldInfo(0).fName = "NAME_FLD"
        fieldInfo(0).ftype = r4str
        fieldInfo(0).flength = 20
        fieldInfo(0).fdecimals = 0

        fieldInfo(1).fName = "AGE_FLD"
        fieldInfo(1).ftype = r4num
        fieldInfo(1).flength = 3
        fieldInfo(1).fdecimals = 0

        rc = code4errOpen(cb, 0)

        'Specify full path if stand-alone
        If u4switch() And &H80S Then fPath = VB6.GetPath & "\"

        'no error message is displayed if NO_FILE does not exist
        db = d4open(cb, "NO_FILE")

        If code4errorCode(cb, r4check) = r4noOpen Then
            'the data file does not exist
            rc = code4safety(cb, 0)
            db = d4createData(cb, fPath & "NO_FILE", fieldInfo)
            If db = 0 Then
                MsgBox("Could not create NO_FILE", MsgBoxStyle.OKOnly)
            End If
        End If

        rc = code4initUndo(cb)
    End Sub
End Module