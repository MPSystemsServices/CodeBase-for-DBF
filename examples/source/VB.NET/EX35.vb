Option Strict Off
Option Explicit On
Module EXAMPLE
    'EX35.VB
	
	Public lf As String 'Line Feed
	Public fPath As String 'Full path name to db files
	
	Public Const MB_OK As Short = 0
	Public Const MB_YESNO As Short = 4
	Public Const MB_ICONQUESTION As Short = 32
	Public Const IDYES As Short = 6
	
    Sub ExCode(ByRef frm As Form1)
        Dim cb, db As Integer
        Dim rc As Short
        Dim count As Integer

        cb = code4init()

        'Specify full path if stand-alone
        If u4switch() And &H80S Then fPath = VB6.GetPath & "\"

        db = d4open(cb, fPath & "INFO")
        rc = code4optStart(cb)
        count = 0

        rc = d4top(db)
        Do While rc = r4success
            If d4deleted(db) = 1 Then
                count = count + 1
            End If
            rc = d4skip(db, 1)
        Loop
        frm.ListBox1.Items.Add("INFO has " & Str(count) & " deleted records")
        rc = code4initUndo(cb)
    End Sub
End Module