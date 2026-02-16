Option Strict Off
Option Explicit On
Module EXAMPLE
    'EX6.VB
	
	Public lf As String 'Line Feed
	Public fPath As String 'Full path name to data files
	
	Public Const MB_OK As Short = 0
	Public Const MB_YESNO As Short = 4
	Public Const MB_ICONQUESTION As Short = 32
	Public Const IDYES As Short = 6
	
    Sub ExCode(ByRef frm As Form1)
        'ex6 example code
        Dim db, cb, field As Integer
        Dim rc As Short
        Dim badField As String

        cb = code4init()

        'Use full path in stand-alone version
        If u4switch() And &H80S Then fPath = VB6.GetPath & "\"

        db = d4open(cb, fPath & "INFO")
        badField = "notAField"
        field = d4field(db, badField)

        MsgBox("An error message just displayed", MsgBoxStyle.OKOnly)

        rc = code4errorCode(cb, 0)
        rc = code4errFieldName(cb, 0)

        field = d4field(db, badField)
        MsgBox("No error message displayed.", MsgBoxStyle.OKOnly)

        rc = code4initUndo(cb)
    End Sub
End Module