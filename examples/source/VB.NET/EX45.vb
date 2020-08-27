Option Strict Off
Option Explicit On
Module EXAMPLE 
    'EX45.VB

    Public lf As String 'Line Feed
    Public fPath As String 'Full path name to db files

    Public Const MB_OK As Short = 0
    Public Const MB_YESNO As Short = 4
    Public Const MB_ICONQUESTION As Short = 32
    Public Const IDYES As Short = 6

    Sub ExCode(ByRef frm As Form1)
        Dim db, cb, field As Integer
        Dim rc, fieldNum As Short

        cb = code4init()

        'Specify full path if stand-alone
        If u4switch() And &H80S Then fPath = VB6.GetPath & "\"

        db = d4open(cb, fPath & "INFO")

        rc = code4optStart(cb)
        rc = d4top(db)
        Do While rc <> r4eof
            For fieldNum = 1 To d4numFields(db)
                frm.ListBox1.Items.Add(f4name(d4fieldJ(db, fieldNum)))
            Next
            rc = d4skip(db, 1)
        Loop
        rc = code4initUndo(cb)
    End Sub
End Module