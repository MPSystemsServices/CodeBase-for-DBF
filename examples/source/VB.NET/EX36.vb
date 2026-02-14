Option Strict Off
Option Explicit On
Module EXAMPLE
    'EX36.VB	

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

        db = d4open(cb, fPath & "INFO")
        'Go to the end of the file and set the End of File flag
        rc = d4goEof(db)
        'Check if the End of File flag is set
        If d4eof(db) = 1 Then
            MsgBox("This is always true")
            rc = d4bottom(db) 'reset the eof flag
        End If

        rc = d4close(db)
        rc = code4initUndo(cb)
    End Sub
End Module