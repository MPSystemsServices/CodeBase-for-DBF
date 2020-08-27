Option Strict Off
Option Explicit On
Module EXAMPLE 
    'EX63.VB

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

        Call d4tagSelect(db, defaultTag)
        count = 0
        rc = d4top(db) 'top record in default tag
        Do While d4eof(db) = 0
            count = count + 1
            rc = d4skip(db, 1)
        Loop
        frm.ListBox1.Items.Add(Str(count) & " records in the tag" & t4Alias(defaultTag))

        rc = d4close(db)
        rc = code4initUndo(cb)
    End Sub
End Module