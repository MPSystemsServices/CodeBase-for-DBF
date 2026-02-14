Option Strict Off
Option Explicit On
Module EXAMPLE
    'EX9.VB
	
	Public lf As String 'Line Feed
	Public fPath As String 'Full path name to data files
	
	Public Const MB_OK As Short = 0
	Public Const MB_YESNO As Short = 4
	Public Const MB_ICONQUESTION As Short = 32
	Public Const IDYES As Short = 6
	
    Sub ExCode(ByRef frm As Form1)
        'ex9 example code
        Dim cb, db As Integer
        Dim rc As Short

        cb = code4init()

        db = d4open(cb, fPath & "INFO")

        rc = code4readLock(cb, 1)
        rc = code4lockAttempts(cb, 3)

        If d4top(db) = r4locked Then
            MsgBox("Top rcord locked by another user")
            MsgBox("Lock attempted" & Str(code4lockAttempts(cb, r4check)), MsgBoxStyle.OKOnly)
        Else
            MsgBox("Now the record is locked")
        End If

        rc = code4initUndo(cb)
    End Sub
End Module