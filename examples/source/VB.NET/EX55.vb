Option Strict Off
Option Explicit On
Module EXAMPLE 
    'EX55.VB

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
        rc = d4optimize(db, OPT4ALL)
        rc = code4optStart(cb)
        rc = d4top(db)
        MsgBox("Click OK when you want to refresh your data", MsgBoxStyle.OKOnly)
        rc = d4refresh(db)
        rc = d4top(db) 're-read the record from disk
        MsgBox("The latest infomation is now in the buffer")
        rc = d4close(db)
        rc = code4initUndo(cb)
    End Sub
End Module