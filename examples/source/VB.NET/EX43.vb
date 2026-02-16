Option Strict Off
Option Explicit On
Module EXAMPLE 
    'EX43.VB

    Public lf As String 'Line Feed
    Public fPath As String 'Full path name to db files

    Public Const MB_OK As Short = 0
    Public Const MB_YESNO As Short = 4
    Public Const MB_ICONQUESTION As Short = 32
    Public Const IDYES As Short = 6

    Sub ExCode(ByRef frm As Form1)
        Dim cb, db As Integer
        Dim rc As Short

        cb = code4init()

        'Specify full path if stand-alone
        If u4switch() And &H80S Then fPath = VB6.GetPath & "\"

        db = d4open(cb, fPath & "INFO")
        'Lock all the records in the data file as well as the append bytes
        'all at once. Existing locks are removed according to
        'code4unlockAuto

        rc = d4lockFile(db)
        If rc = r4success Then
            MsgBox("Other users can read this data file, but can not make modifications until the lock is removed")
        End If
        rc = code4initUndo(cb)
    End Sub
End Module