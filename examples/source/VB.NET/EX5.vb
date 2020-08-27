Option Strict Off
Option Explicit On
Module EXAMPLE
	'EX5.VB
	
	Public lf As String 'Line Feed
	Public fPath As String 'Full path name to data files
	
	Public Const MB_OK As Short = 0
	Public Const MB_YESNO As Short = 4
	Public Const MB_ICONQUESTION As Short = 32
	Public Const IDYES As Short = 6
	
    Sub ExCode(ByRef frm As Form1)
        Dim db, cb, expr As Integer
        Dim badExpr As String
        Dim rc As Short

        cb = code4init()

        'Specify full path if stand-alone
        If u4switch() And &H80S Then fPath = VB6.GetPath & "\"

        db = d4open(cb, fPath & "INFO")

        badExpr = "NAME = 5"
        expr = expr4parse(db, badExpr)
        MsgBox("An error message just displayed", MsgBoxStyle.OKOnly)

        rc = code4errorCode(cb, 0)
        rc = code4errExpr(cb, 0)
        expr = expr4parse(db, badExpr)
        MsgBox("No error message displayed.", MsgBoxStyle.OKOnly)

        rc = code4initUndo(cb)
    End Sub
End Module