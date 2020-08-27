Option Strict Off
Option Explicit On
Module EXAMPLE 
    'EX83.VB

    Public lf As String 'Line Feed
    Public fPath As String 'Full path name to db files

    Public Const MB_OK As Short = 0
    Public Const MB_YESNO As Short = 4
    Public Const MB_ICONQUESTION As Short = 32
    Public Const IDYES As Short = 6

    Sub ExCode(ByRef frm As Form1)
        Dim info, cb, db, expr As Integer
        Dim rc As Short

        cb = code4init()

        'Specify full path if stand-alone
        If u4switch() And &H80S Then fPath = VB6.GetPath & "\"

        db = d4open(cb, fPath & "DATA1")
        info = d4open(cb, fPath & "INFO")

        expr = expr4parse(db, "F_NAME+' '+DTOS( INFO->BIRTH_DATE)")
        rc = d4top(db)
        rc = d4top(info)
        frm.ListBox1.Items.Add("First name from DATA1 and birth date from INFO: " & expr4str(expr))

        rc = code4close(cb)
        rc = code4initUndo(cb)
    End Sub
End Module