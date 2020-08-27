Option Strict Off
Option Explicit On
Module EXAMPLE 
    'EX77.VB

    Public lf As String 'Line Feed
    Public fPath As String 'Full path name to db files

    Public Const MB_OK As Short = 0
    Public Const MB_YESNO As Short = 4
    Public Const MB_ICONQUESTION As Short = 32
    Public Const IDYES As Short = 6

    Sub ExCode(ByRef frm As Form1)
        Dim db, cb, bDate As Integer
        Dim date1, date2 As String
        Dim year1, year2 As Short
        Dim rc As Short

        cb = code4init()

        'Specify full path if stand-alone
        If u4switch() And &H80S Then fPath = VB6.GetPath & "\"

        db = d4open(cb, fPath & "DATA1")
        bDate = d4field(db, "BIRTH_DATE")
        rc = d4top(db)
        date1 = Mid(f4str(bDate), 1) 'make a copy of the field's contents
        rc = d4skip(db, 1)
        date2 = Mid(f4str(bDate), 1)
        year1 = date4year(date1)
        year2 = date4year(date2)
        If year1 <> year2 Then
            frm.ListBox1.Items.Add("The people in the 1st and 2nd records were born in different years " & Str(year1) & ", " & Str(year2))
        Else
            frm.ListBox1.Items.Add("The people in the 1st and 2nd records were born in the same year" & Str(year1))
        End If

        rc = d4close(db)
        rc = code4initUndo(cb)
    End Sub
End Module