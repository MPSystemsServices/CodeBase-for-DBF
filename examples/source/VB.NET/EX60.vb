Option Strict Off
Option Explicit On
Module EXAMPLE 
    'EX60.VB

    Public lf As String 'Line Feed
    Public fPath As String 'Full path name to db files

    Public Const MB_OK As Short = 0
    Public Const MB_YESNO As Short = 4
    Public Const MB_ICONQUESTION As Short = 32
    Public Const IDYES As Short = 6

    Sub ExCode(ByRef frm As Form1)
        Dim db, cb, nameField As Integer
        Dim rc As Short

        cb = code4init()

        'Specify full path if stand-alone
        If u4switch() And &H80S Then fPath = VB6.GetPath & "\"

        db = d4open(cb, fPath & "NAMES")
        'Skip to the last record in the file whose NAME field contains "John"

        rc = code4optStart(cb)
        nameField = d4field(db, "NAME")

        rc = d4bottom(db)
        Do While d4bof(db) = 0 'Do while bof is false
            If InStr(1, f4str(nameField), "White     ") <> 0 Then Exit Do
            rc = d4skip(db, -1)
        Loop

        If d4bof(db) <> 0 Then
            frm.ListBox1.Items.Add("White not located")
        Else
            frm.ListBox1.Items.Add("The last White is located in record # " & Str(d4recNo(db)))
        End If

        rc = d4close(db)
        rc = code4initUndo(cb)
    End Sub
End Module