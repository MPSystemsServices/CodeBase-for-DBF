Option Strict Off
Option Explicit On
Module EXAMPLE
    'EX20.VB
	
	Public lf As String 'Line Feed
	Public fPath As String 'Full path name to data files
	
	Public Const MB_OK As Short = 0
	Public Const MB_YESNO As Short = 4
	Public Const MB_ICONQUESTION As Short = 32
	Public Const IDYES As Short = 6
	
    Sub ExCode(ByRef frm As Form1)
        Dim cb, db As Integer
        Dim rc, delCount As Short

        cb = code4init()
        rc = code4accessMode(cb, OPEN4DENY_RW)

        'Specify full path if stand-alone
        If u4switch() And &H80S Then fPath = VB6.GetPath & "\"

        db = d4open(cb, fPath & "INFO")
        error4exitTest((cb))

        'initialize optimization with default settings

        rc = code4optStart(cb)

        delCount = 0
        rc = d4top(db)
        Do While rc = r4success
            If d4deleted(db) Then
                delCount = delCount + 1
            End If
            rc = d4skip(db, 1)
        Loop

        frm.ListBox1.Items.Add(Str(delCount) & " records are marked for deletion")

        rc = code4initUndo(cb)
    End Sub
End Module