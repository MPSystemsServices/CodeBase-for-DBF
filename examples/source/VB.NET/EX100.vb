Option Strict Off
Option Explicit On
Module EXAMPLE 
    'EX100.VB

    Public lf As String 'Line Feed
    Public fPath As String 'Full path name to db files

    Public Const MB_OK As Short = 0
    Public Const MB_YESNO As Short = 4
    Public Const MB_ICONQUESTION As Short = 32
    Public Const IDYES As Short = 6

    Sub ExCode(ByRef frm As Form1)
        Dim db, cb, field As Integer
        Dim rc As Short
        Dim j As Short

        cb = code4init()

        'Specify full path if stand-alone
        If u4switch() And &H80S Then fPath = VB6.GetPath & "\"

        db = d4open(cb, fPath & "DATA1")

        If db <> 0 Then
            ' list the fields that are character fields
            For j = 1 To d4numFields(db)
                field = d4fieldJ(db, j)
                If Chr(f4type(field)) = r4str Then
                    frm.ListBox1.Items.Add(f4name(field))
                End If
            Next j
        End If

        rc = code4initUndo(cb)
    End Sub
End Module