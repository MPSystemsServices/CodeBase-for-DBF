Option Strict Off
Option Explicit On
Module EXAMPLE
    'EX40.VB
	
	Public lf As String 'Line Feed
	Public fPath As String 'Full path name to db files
	
	Public Const MB_OK As Short = 0
	Public Const MB_YESNO As Short = 4
	Public Const MB_ICONQUESTION As Short = 32
	Public Const IDYES As Short = 6
	
    Sub ExCode(ByRef frm As Form1)
        Dim db, cb, index As Integer
        Dim rc As Short

        cb = code4init()

        'Specify full path if stand-alone
        If u4switch() And &H80S Then fPath = VB6.GetPath & "\"

        db = d4open(cb, fPath & "INFO")
        'Since CODE4.autoOpen is true (non-zero) by default
        'the index file should have been opened
        index = d4index(db, "INFO")
        If index <> 0 Then
            MsgBox("Index file INFO has been opened")
        End If

        index = d4index(db, fPath & "JUNK")
        If index = 0 Then
            MsgBox("Index file JUNK has not been opened")
        End If

        rc = d4close(db)
        rc = code4initUndo(cb)
    End Sub
End Module