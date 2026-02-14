Option Strict Off
Option Explicit On
Module EXAMPLE
    'EX29.VB
	
	Public lf As String 'Line Feed
	Public fPath As String 'Full path name to db files
	
	Public Const MB_OK As Short = 0
	Public Const MB_YESNO As Short = 4
	Public Const MB_ICONQUESTION As Short = 32
	Public Const IDYES As Short = 6
	
    Sub ExCode(ByRef frm As Form1)
        Dim db, cb, field As Integer
        Dim rc As Short

        cb = code4init()

        'Specify full path if stand-alone
        If u4switch() And &H80S Then fPath = VB6.GetPath & "\"

        db = d4open(cb, fPath & "INFO")

        rc = d4bottom(db)
        frm.ListBox1.Items.Add("Last Name added: " & f4str(d4field(db, "NAME")))
        rc = d4close(db)
        rc = code4initUndo(cb)
    End Sub
End Module