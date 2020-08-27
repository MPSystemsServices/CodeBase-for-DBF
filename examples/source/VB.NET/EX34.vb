Option Strict Off
Option Explicit On
Module EXAMPLE
    'EX34.VB 
	
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

        rc = code4accessMode(cb, OPEN4DENY_RW) 'open file exclusively to speed pack
        db = d4open(cb, fPath & "INFO")
        error4exitTest((cb))

        rc = code4optStart(cb)
        rc = d4top(db)
        Do While rc = r4success
            Call d4delete(db) 'mark record for deletion
            rc = d4skip(db, 2)
        Loop
        'physically remove the deleted records from the disk
        rc = d4pack(db)
        rc = d4close(db)
        rc = code4initUndo(cb)
    End Sub
End Module