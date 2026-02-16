Option Strict Off
Option Explicit On
Module EXAMPLE 
    'EX65.VB

    Public lf As String 'Line Feed
    Public fPath As String 'Full path name to db files

    Public Const MB_OK As Short = 0
    Public Const MB_YESNO As Short = 4
    Public Const MB_ICONQUESTION As Short = 32
    Public Const IDYES As Short = 6

    Sub ExCode(ByRef frm As Form1)
        Dim cb, db As Integer
        Dim rc, i As Short
        Dim numRecs As Integer

        cb = code4init()
        rc = code4accessMode(cb, OPEN4DENY_RW)

        'Specify full path if stand-alone
        If u4switch() And &H80S Then fPath = VB6.GetPath & "\"

        db = d4open(cb, fPath & "INFO1")

        rc = d4go(db, 1)
        'Make all the records in the data file the same as the first record
        numRecs = d4recCount(db)
        For i = numRecs To 2 Step -1
            If d4write(db, numRecs) <> 0 Then Exit For
        Next

        rc = d4close(db)
        rc = code4initUndo(cb)
    End Sub
End Module