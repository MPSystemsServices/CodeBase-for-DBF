Option Strict Off
Option Explicit On
Module EXAMPLE 
    'EX57.VB

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
        rc = code4accessMode(cb, OPEN4DENY_RW)

        'Specify full path if stand-alone
        If u4switch() And &H80S Then fPath = VB6.GetPath & "\"

        db = d4open(cb, fPath & "INFO")

        frm.ListBox1.Items.Add("Reindexing " & d4alias(db) & " Please Wait")
        If d4reindex(db) <> 0 Then
            MsgBox("Reindex NOT successful")
        Else
            MsgBox("Reindex Successful")
        End If
        rc = code4initUndo(cb)
    End Sub
End Module