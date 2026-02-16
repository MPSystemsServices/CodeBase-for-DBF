Option Strict Off
Option Explicit On
Module EXAMPLE
    'EX15.VB
	
	Public lf As String 'Line Feed
	Public fPath As String 'Full path name to data files
	
	Public Const MB_OK As Short = 0
	Public Const MB_YESNO As Short = 4
	Public Const MB_ICONQUESTION As Short = 32
	Public Const IDYES As Short = 6
	
    Sub ExCode(ByRef frm As Form1)
        'ex15 example code
        Dim cb, db As Integer
        Dim ex, ex2 As Integer
        Dim rc As Short

        cb = code4init()

        'Specify full path if stand-alone
        If u4switch() And &H80S Then fPath = VB6.GetPath & "\"

        db = d4open(cb, fPath & "DATA1")
        rc = d4top(db)

        ex = expr4parse(db, "TRIM(L_NAME)+', '+TRIM(F_NAME)")
        rc = code4calcCreate(cb, ex, "NAMES")
        frm.ListBox1.Items.Add(expr4str(ex))

        ex2 = expr4parse(db, "'HELLO '+NAMES()") 'no space in dBASE function calls
        frm.ListBox1.Items.Add(expr4str(ex2))

        Call expr4free(ex2)
        Call code4calcReset(cb)
        rc = code4initUndo(cb)
    End Sub
End Module