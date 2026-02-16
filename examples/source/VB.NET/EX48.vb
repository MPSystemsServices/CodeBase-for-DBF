Option Strict Off
Option Explicit On
Module EXAMPLE 
    'EX48.VB

    Public lf As String 'Line Feed
    Public fPath As String 'Full path name to db files

    Public Const MB_OK As Short = 0
    Public Const MB_YESNO As Short = 4
    Public Const MB_ICONQUESTION As Short = 32
    Public Const IDYES As Short = 6

    Sub ExCode(ByRef frm As Form1)
        Dim db, cb, ageField As Integer
        Dim rc As Short
        Dim age As Integer

        cb = code4init()

        'Specify full path if stand-alone
        If u4switch() And &H80S Then fPath = VB6.GetPath & "\"

        db = d4open(cb, fPath & "INFO")

        rc = d4optimizeWrite(db, OPT4ALL)
        'when doing write optimization on shared files, it is necessary to
        'lock the file, preferably with d4lockAll( )
        rc = d4lockAll(db)

        rc = code4optStart(cb) 'Begin optimization
        ageField = d4field(db, "AGE")
        'append a copy of the first record, assigning the age field's
        'value from 20 to 65
        rc = d4top(db)
        For age = 20 To 65
            rc = d4appendStart(db, 0)
            Call f4assignLong(ageField, age)
            rc = d4append(db)
        Next
        rc = code4initUndo(cb)
    End Sub
End Module