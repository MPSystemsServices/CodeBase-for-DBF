Option Strict Off
Option Explicit On
Module EXAMPLE 
    'EX53.VB

    Public lf As String 'Line Feed
    Public fPath As String 'Full path name to db files

    Public Const MB_OK As Short = 0
    Public Const MB_YESNO As Short = 4
    Public Const MB_ICONQUESTION As Short = 32
    Public Const IDYES As Short = 6

    Sub ExCode(ByRef frm As Form1)
        Dim db, cb, defaultTag As Integer
        Dim rc As Short
        Dim count As Integer

        cb = code4init()

        'Specify full path if stand-alone
        If u4switch() And &H80S Then fPath = VB6.GetPath & "\"

        db = d4open(cb, fPath & "INFO")
        defaultTag = d4tagDefault(db)
        Call d4tagSelect(db, defaultTag) 'select the default tag
        count = 1
        rc = d4top(db)
        Do While rc = r4success
            frm.ListBox1.Items.Add("Tag position: " & Str(count) & "     Record position: " & Str(d4recNo(db)))
            count = count + 1
            rc = d4skip(db, 1)
        Loop

        rc = code4initUndo(cb)
    End Sub
End Module