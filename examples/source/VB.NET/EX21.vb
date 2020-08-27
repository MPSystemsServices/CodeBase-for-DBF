Option Strict Off
Option Explicit On
Module EXAMPLE
    'EX21.VB
	
	Public lf As String 'Line Feed
	Public fPath As String 'Full path name to data files
	
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

        db = d4open(cb, fPath & "INFO")

        rc = d4lockAll(db)
        rc = d4optimizeWrite(db, OPT4ALL)

        rc = code4optStart(cb)
        '... some other code
        rc = code4optSuspend(cb) 'flush & free optimization memory.

        rc = d4unlock(db) 'let other users make modifications.
        '... some other code
        rc = d4lockAll(db)
        rc = code4optStart(cb)
        '... some other code
        rc = code4initUndo(cb)
    End Sub
End Module