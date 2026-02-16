Option Strict Off
Option Explicit On
Module EXAMPLE 
    'EX82.VB

    Public lf As String 'Line Feed
    Public fPath As String 'Full path name to db files

    Public Const MB_OK As Short = 0
    Public Const MB_YESNO As Short = 4
    Public Const MB_ICONQUESTION As Short = 32
    Public Const IDYES As Short = 6

    Sub ExCode(ByRef frm As Form1)
        Dim db, cb, fullName As Integer
        Dim rc As Short
        Dim nameStr, nextName As String

        cb = code4init()

        'Specify full path if stand-alone
        If u4switch() And &H80S Then fPath = VB6.GetPath & "\"

        db = d4open(cb, fPath & "DATA1")

        rc = d4top(db)
        fullName = expr4parse(db, "TRIM( L_NAME )+', '+F_NAME")
        nameStr = expr4str(fullName)

        rc = d4skip(db, 1)
        nextName = expr4str(fullName)
        'For illustration purposes only: Avoid using the expression module
        'when the field functions will suffice
        frm.ListBox1.Items.Add(nameStr & " is the first person in the data file")
        frm.ListBox1.Items.Add(nextName & " is the second person in the data file ")

        Call expr4free(fullName)
        rc = code4initUndo(cb)
    End Sub
End Module