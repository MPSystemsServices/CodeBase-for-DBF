Option Strict Off
Option Explicit On
Module EXAMPLE 
    'EX89.VB

    Public lf As String 'Line Feed
    Public fPath As String 'Full path name to db files

    Public Const MB_OK As Short = 0
    Public Const MB_YESNO As Short = 4
    Public Const MB_ICONQUESTION As Short = 32
    Public Const IDYES As Short = 6

    Sub ExCode(ByRef frm As Form1)
        Dim db, cb, comments As Integer
        Dim rc As Short
        Dim count As Integer

        cb = code4init()
        db = d4open(cb, fPath & "DATA1")
        comments = d4field(db, "COMMENT")
        Call error4exitTest(cb)

        count = 0
        rc = d4top(db)
        rc = f4memoAssign(comments, "There is one memo in this data file")
        rc = d4flush(db)
        Do While d4eof(db) = 0
            If f4memoLen(comments) > 0 Then
                count = count + 1
            End If
            rc = d4skip(db, 1)
            rc = f4memoAssign(comments, "There is a second memo in this data file")
            rc = d4flush(db)

        Loop
        frm.ListBox1.Items.Add("There were " & Str(count) & " memo entries out of ")
        frm.ListBox1.Items.Add(Str(d4recCount(db)) & " records")

        rc = code4initUndo(cb)
    End Sub
End Module