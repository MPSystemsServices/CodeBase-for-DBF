Option Strict Off
Option Explicit On
Module EXAMPLE 
    'EX88.VB

    Public lf As String 'Line Feed
    Public fPath As String 'Full path name to db files

    Public Const MB_OK As Short = 0
    Public Const MB_YESNO As Short = 4
    Public Const MB_ICONQUESTION As Short = 32
    Public Const IDYES As Short = 6

    Function createBufCopy(ByRef f As Integer) As String
        Dim buf As String

        buf = Space(f4len(f) + 1)
        buf = Mid(f4str(f), 1)
        createBufCopy = buf
    End Function

    Sub ExCode(ByRef frm As Form1)
        Dim db, cb, field As Integer
        Dim buffer As String
        Dim rc As Short

        cb = code4init()

        'Specify full path if stand-alone
        If u4switch() And &H80S Then fPath = VB6.GetPath & "\"

        db = d4open(cb, fPath & "INFO")
        field = d4field(db, "NAME")
        rc = d4top(db)
        buffer = createBufCopy(field)
        frm.ListBox1.Items.Add("the copy of the buffer is " & buffer)
        rc = code4initUndo(cb)
    End Sub
End Module