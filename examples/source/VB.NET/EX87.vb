Option Strict Off
Option Explicit On
Module EXAMPLE 
    'EX87.VB

    Public lf As String 'Line Feed
    Public fPath As String 'Full path name to db files

    Public Const MB_OK As Short = 0
    Public Const MB_YESNO As Short = 4
    Public Const MB_ICONQUESTION As Short = 32
    Public Const IDYES As Short = 6

    Sub displayFieldStats(ByRef f As Integer, ByRef fm As Form1 )
        Dim dFile As Integer

        dFile = f4data(f)
        fm.ListBox1.Items.Add("-----------------------------------------------------")
        fm.ListBox1.Items.Add("DataFile: " & d4alias(dFile) & "     Field : " & f4name(f))
        fm.ListBox1.Items.Add("Length: " & Str(f4len(f)) & "            Type : " & Chr(f4type(f)))
        fm.ListBox1.Items.Add("Decimals: " & Str(f4decimals(f)))
        fm.ListBox1.Items.Add("-----------------------------------------------------")
    End Sub

    Sub ExCode(ByRef frm As Form1)
        Dim db, cb, field As Integer
        Dim rc As Short

        cb = code4init()

        'Specify full path if stand-alone
        If u4switch() And &H80S Then fPath = VB6.GetPath & "\"

        db = d4open(cb, fPath & "INFO")
        field = d4field(db, "NAME")

        Call displayFieldStats(field, frm)
        rc = code4initUndo(cb)
    End Sub
End Module