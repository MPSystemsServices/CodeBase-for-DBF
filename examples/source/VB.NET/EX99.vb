Option Strict Off
Option Explicit On
Module EXAMPLE 
    'EX99.VB

    Public lf As String 'Line Feed
    Public fPath As String 'Full path name to db files

    Public Const MB_OK As Short = 0
    Public Const MB_YESNO As Short = 4
    Public Const MB_ICONQUESTION As Short = 32
    Public Const IDYES As Short = 6

    Sub ExCode(ByRef frm As Form1)
        Dim db, cb, TopMaster As Integer
        Dim rc As Short

        cb = code4init()

        'Specify full path if stand-alone
        If u4switch() And &H80S Then fPath = VB6.GetPath & "\"

        db = d4open(cb, fPath & "INFO")

        TopMaster = relate4init(db)

        ' ... other code ...

        'This relation tree is no longer needed. Create a new one
        rc = relate4free(TopMaster, 0)

        TopMaster = relate4init(db)

        ' ... other code ...
        ' Automatically close all files in the relation
        rc = relate4free(TopMaster, 1)

        rc = code4close(cb) 'close any remaining files
        rc = code4initUndo(cb)
    End Sub
End Module