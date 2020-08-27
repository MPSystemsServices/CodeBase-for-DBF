Option Strict Off
Option Explicit On
Module EXAMPLE 
    'EX92.VB

    Public lf As String 'Line Feed
    Public fpath As String 'Full path name to db files

    Public Const MB_OK As Short = 0
    Public Const MB_YESNO As Short = 4
    Public Const MB_ICONQUESTION As Short = 32
    Public Const IDYES As Short = 6

    Sub ExCode(ByRef frm As Form1)
        Dim field1, cb, db, field2 As Integer
        Dim rc As Short

        cb = code4init()

        'Specify full path if stand-alone
        If u4switch() And &H80S Then fpath = VB6.GetPath & "\"

        db = d4open(cb, fpath & "INFO")
        field1 = d4fieldJ(db, 1)
        field2 = d4fieldJ(db, 2)
        rc = d4top(db)
        frm.ListBox1.Items.Add("Field 1: " & f4str(field1))
        frm.ListBox1.Items.Add("Field 2: " & f4str(field2))
        rc = code4initUndo(cb)
    End Sub
End Module