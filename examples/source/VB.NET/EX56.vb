Option Strict Off
Option Explicit On
Module EXAMPLE 
    'EX56.VB

    Dim cb, db As Integer
    Dim rc As Short

    Public lf As String 'Line Feed
    Public fPath As String 'Full path name to db files

    Public Const MB_OK As Short = 0
    Public Const MB_YESNO As Short = 4
    Public Const MB_ICONQUESTION As Short = 32
    Public Const IDYES As Short = 6

    Sub ExCode(ByRef frm As Form1)
        cb = code4init()
        'Specify full path if stand-alone
        If u4switch() And &H80S Then fPath = VB6.GetPath & "\"
        db = d4open(cb, fPath & "DATA1")
        rc = d4top(db)
        Call updateData()
        rc = code4initUndo(cb)
    End Sub

    Sub updateData()
        If d4changed(db, -1) > 0 Then
            frm.ListBox1.Items.Add("Changes not discarded")
        Else
            rc = d4refreshRecord(db)
        End If
    End Sub
End Module