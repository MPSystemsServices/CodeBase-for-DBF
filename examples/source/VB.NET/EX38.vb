Option Strict Off
Option Explicit On
Module EXAMPLE 
    'EX38.VB

    Public lf As String 'Line Feed
    Public fPath As String 'Full path name to db files

    Public Const MB_OK As Short = 0
    Public Const MB_YESNO As Short = 4
    Public Const MB_ICONQUESTION As Short = 32
    Public Const IDYES As Short = 6

    Sub ExCode(ByRef frm As Form1)
        Dim db, cb, age As Integer
        Dim rc As Short

        cb = code4init()

        'Specify full path if stand-alone
        If u4switch() And &H80S Then fPath = VB6.GetPath & "\"

        db = d4open(cb, fPath & "INFO")
        rc = d4go(db, 2)
        age = d4field(db, "AGE")
        Call f4assignLong(age, 49)
        'Explicitly flush the change to disk in case the power goes out
        rc = d4flush(db)
        '... some other code....
        rc = d4close(db)
        rc = code4initUndo(cb)
    End Sub
End Module