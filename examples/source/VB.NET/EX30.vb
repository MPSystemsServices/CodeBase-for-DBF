Option Strict Off
Option Explicit On
Module EXAMPLE
    'EX30.VB
	
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

        db = d4open(cb, fPath & "DATA1")
        field = d4field(db, "L_NAME")
        rc = d4top(db)

        If d4changed(db, -1) = 0 Then 'Displays false
            MsgBox("Changed Status: FALSE")
        Else
            MsgBox("Changed Status: TRUE")
        End If

        Call f4assign(field, "TEMP DATA")
        If d4changed(db, -1) = 0 Then 'Displays true
            MsgBox("Changed Status: FALSE")
        Else
            MsgBox("Changed Status: TRUE")
        End If

        rc = d4changed(db, 0)
        rc = d4close(db) 'top record not flushed
        rc = code4initUndo(cb)
    End Sub
End Module