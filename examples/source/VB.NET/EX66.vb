Option Strict Off
Option Explicit On
Module EXAMPLE 
    'EX66.VB

    Dim cb, db As Integer
    Dim rc As Integer

    Public lf As String 'Line Feed
    Public fPath As String 'Full path name to db files

    Public Const MB_OK As Short = 0
    Public Const MB_YESNO As Short = 4
    Public Const MB_ICONQUESTION As Short = 32
    Public Const IDYES As Short = 6

    Sub ExCode(ByRef frm As Form1)
        Dim toDelete As Integer

        cb = code4init()
        rc = code4accessMode(cb, OPEN4DENY_RW)

        'Specify full path if stand-alone
        If u4switch() And &H80S Then fPath = VB6.GetPath & "\"

        db = d4open(cb, fPath & "INFO")
        toDelete = 2
        rc = zapLast(toDelete, frm)
        rc = code4initUndo(cb)
    End Sub

    Function zapLast(ByRef toDelete As Integer, ByRef fm As Form1) As Integer
        fm.ListBox1.Items.Add(d4alias(db) & " has " & Str(d4recCount(db)) & " records")
        rc = d4zap(db, d4recCount(db) - toDelete + 1, 1000000)
        fm.ListBox1.Items.Add(d4alias(db) & " now has " & Str(d4recCount(db)) & " records")
        zapLast = d4recCount(db)
    End Function
End Module