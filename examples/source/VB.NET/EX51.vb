Option Strict Off
Option Explicit On
Module EXAMPLE 
    'EX51.VB

    Dim cb, db As Integer
    Dim rc As Integer

    Public lf As String 'Line Feed
    Public fPath As String 'Full path name to db files

    Public Const MB_OK As Short = 0
    Public Const MB_YESNO As Short = 4
    Public Const MB_ICONQUESTION As Short = 32
    Public Const IDYES As Short = 6

    Sub ExCode(ByRef frm As Form1)
        Dim count As Integer
        Dim lockTries As Short

        lockTries = WAIT4EVER
        cb = code4init()

        'Specify full path if stand-alone
        If u4switch() And &H80S Then fPath = VB6.GetPath & "\"

        db = d4open(cb, fPath & "INFO")
        count = recallAll(lockTries)
        frm.ListBox1.Items.Add("The number of recalled records is " & Str(count))
        rc = code4initUndo(cb)
    End Sub

    Function recallAll(ByRef lockTries As Short) As Integer
        Dim saveSelected As Integer
        Dim count As Integer

        saveSelected = d4tagSelected(db)
        Call d4tagSelect(db, 0) 'use record number ordering
        count = 0
        rc = d4top(db)
        Do While rc = r4success
            Call d4recall(db)
            If d4changed(db, -1) <> 0 Then count = count + 1
            rc = d4skip(db, 1)
        Loop

        Call d4tagSelect(db, saveSelected) 'reset the selected tag
        recallAll = count
    End Function
End Module