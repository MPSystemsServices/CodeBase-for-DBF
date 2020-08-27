Option Strict Off
Option Explicit On
Module EXAMPLE  
    'EX81.VB

    Public lf As String 'Line Feed
    Public fPath As String 'Full path name to db files

    Public Const MB_OK As Short = 0
    Public Const MB_YESNO As Short = 4
    Public Const MB_ICONQUESTION As Short = 32
    Public Const IDYES As Short = 6

    Sub ExCode(ByRef frm As Form1)
        Dim db, cb, expr As Integer
        Dim rc As Short
        Dim count As Integer
        Dim VoteAge As Double

        VoteAge = 18
        cb = code4init()

        'Specify full path if stand-alone
        If u4switch() And &H80S Then fPath = VB6.GetPath & "\"

        db = d4open(cb, fPath & "INFO")
        expr = expr4parse(db, "AGE")
        count = 0
        rc = d4top(db)
        Do While rc <> r4eof
            If expr4double(expr) >= VoteAge Then
                count = count + 1
            End If
            rc = d4skip(db, 1)
        Loop
        frm.ListBox1.Items.Add("Possible voters: " & Str(count))
        Call expr4free(expr)
        rc = code4initUndo(cb)
    End Sub
End Module