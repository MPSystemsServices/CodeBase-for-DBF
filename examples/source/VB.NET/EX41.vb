Option Strict Off
Option Explicit On
Module EXAMPLE
    'EX41.VB
	
	Public lf As String 'Line Feed
	Public fPath As String 'Full path name to db files
	
	Public Const MB_OK As Short = 0
	Public Const MB_YESNO As Short = 4
	Public Const MB_ICONQUESTION As Short = 32
	Public Const IDYES As Short = 6
	
    Sub ExCode(ByRef frm As Form1)
        Dim cb, db As Integer
        Dim rc As Short

        cb = code4init()

        'Specify full path if stand-alone
        If u4switch() And &H80S Then fPath = VB6.GetPath & "\"

        db = d4open(cb, fPath & "DATA1")
        rc = code4lockAttempts(cb, 4) 'try lock 4 times
        rc = d4lock(db, 2) 'lock record 5
        If rc = r4success Then
            MsgBox("Record 2 is now locked")
        ElseIf rc = r4locked Then
            MsgBox("Record 2 is locked by another user")
        End If

        rc = code4lockAttempts(cb, WAIT4EVER) 'try lock forever
        rc = d4lock(db, 2)
        If rc = r4locked Then
            MsgBox("This should never happen")
        End If

        rc = d4close(db)
        rc = code4initUndo(cb)
    End Sub
End Module