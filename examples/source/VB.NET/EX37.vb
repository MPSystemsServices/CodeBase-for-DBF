Option Strict Off
Option Explicit On
Module EXAMPLE
    'EX37.VB
	
	Public lf As String 'Line Feed
	Public fPath As String 'Full path name to db files
	
	Public Const MB_OK As Short = 0
	Public Const MB_YESNO As Short = 4
	Public Const MB_ICONQUESTION As Short = 32
	Public Const IDYES As Short = 6
	
    Sub ExCode(ByRef frm As Form1)
        Dim db, cb, field As Integer
        Dim rc, num As Short

        cb = code4init()

        'Specify full path if stand-alone
        If u4switch() And &H80S Then fPath = VB6.GetPath & "\"

        db = d4open(cb, fPath & "INFO")
        For num = d4numFields(db) To 1 Step -1
            field = d4fieldJ(db, num)
            If num = d4fieldNumber(db, f4name(field)) Then
                MsgBox("This is always true")
            End If
        Next
        rc = code4initUndo(cb)
    End Sub
End Module