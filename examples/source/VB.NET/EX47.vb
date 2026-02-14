Option Strict Off
Option Explicit On
Module EXAMPLE 
    'EX47.VB

    Public lf As String 'Line Feed
    Public fPath As String 'Full path name to db files

    Public Const MB_OK As Short = 0
    Public Const MB_YESNO As Short = 4
    Public Const MB_ICONQUESTION As Short = 32
    Public Const IDYES As Short = 6

    Sub ExCode(ByRef frm As Form1)
        Dim db, cb, extra As Integer
        Dim rc As Short

        cb = code4init()
        rc = code4accessMode(cb, OPEN4DENY_RW)

        'Specify full path if stand-alone
        If u4switch() And &H80S Then fPath = VB6.GetPath & "\"

        db = d4open(cb, fPath & "INFO")
        'Open the file exclusively, default optimization is the same as if
        'd4optimize( db, OPT4EXCLUSIVE ) were called.

        'open a shared file.
        rc = code4accessMode(cb, OPEN4DENY_NONE)
        extra = d4open(cb, fPath & "DATA1")

        rc = d4optimize(extra, OPT4ALL) 'read optimize the extra "DATA" file

        rc = code4optStart(cb) 'Begin the memory optimizations.

        ' .... Some other code ....

        rc = code4close(cb)
        rc = code4initUndo(cb)
    End Sub
End Module