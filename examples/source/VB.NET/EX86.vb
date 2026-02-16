Option Strict Off
Option Explicit On
Module EXAMPLE 
    'EX86.VB

    Public lf As String 'Line Feed
    Public fPath As String 'Full path name to db files

    Public Const MB_OK As Short = 0
    Public Const MB_YESNO As Short = 4
    Public Const MB_ICONQUESTION As Short = 32
    Public Const IDYES As Short = 6

    Sub ExCode(ByRef frm As Form1)
        Dim db, cb, info As Integer
        Dim infoName, dataLname As Integer
        Dim rc As Short

        cb = code4init()
        rc = code4accessMode(cb, OPEN4DENY_RW)

        'Specify full path if stand-alone
        If u4switch() And &H80S Then fPath = VB6.GetPath & "\"

        info = d4open(cb, fPath & "INFO")
        db = d4open(cb, fPath & "DATA1")
        Call error4exitTest(cb)

        infoName = d4field(info, "NAME")
        dataLname = d4field(db, "L_NAME")
        rc = d4top(info)
        rc = d4top(db)
        Do While d4eof(info) = 0 And d4eof(db) = 0
            Call f4assignField(infoName, dataLname) 'copy "L_NAME" into "NAME"
            rc = d4skip(info, 1)
            rc = d4skip(db, 1)
        Loop

        rc = code4initUndo(cb)
    End Sub
End Module