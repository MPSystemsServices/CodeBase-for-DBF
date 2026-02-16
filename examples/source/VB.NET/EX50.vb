Option Strict Off
Option Explicit On
Module EXAMPLE 
    'EX50.VB

    Public lf As String 'Line Feed
    Public fPath As String 'Full path name to db files

    Public Const MB_OK As Short = 0
    Public Const MB_YESNO As Short = 4
    Public Const MB_ICONQUESTION As Short = 32
    Public Const IDYES As Short = 6

    Sub ExCode(ByRef frm As Form1)
        Dim db, cb, defaultTag As Integer
        Dim rc As Short

        cb = code4init()

        'Specify full path if stand-alone
        If u4switch() And &H80S Then fPath = VB6.GetPath & "\"

        db = d4open(cb, fPath & "INFO")
        defaultTag = d4tagDefault(db)
        error4exitTest((cb))

        Call d4tagSelect(db, defaultTag) 'select the default tag
        rc = d4top(db)
        rc = d4positionSet(db, 0.25) 'move one quarter through the index file.
        frm.ListBox1.Items.Add("Record number: " & Str(d4recNo(db)))
        frm.ListBox1.Items.Add("The current position is " & Str(d4position(db)))

        rc = code4initUndo(cb)
    End Sub
End Module