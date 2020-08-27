Option Strict Off
Option Explicit On
Module EXAMPLE 
    'EX90.VB

    Public lf As String 'Line Feed
    Public fPath As String 'Full path name to db files

    Public Const MB_OK As Short = 0
    Public Const MB_YESNO As Short = 4
    Public Const MB_ICONQUESTION As Short = 32
    Public Const IDYES As Short = 6

    Sub displayTheRecord(ByRef d As Integer, ByRef fm As Form1)
        Dim numFields, curField As Short
        Dim genericField As Integer

        numFields = d4numFields(d)
        For curField = 1 To numFields
            genericField = d4fieldJ(d, curField)
            fm.ListBox1.Items.Add(f4memoStr(genericField))
        Next
        fm.ListBox1.Items.Add("all records have been displayed")
    End Sub

    Sub ExCode(ByRef frm As Form1)
        Dim cb, db As Integer
        Dim rc As Short

        cb = code4init()

        'Specify full path if stand-alone
        If u4switch() And &H80S Then fPath = VB6.GetPath & "\"

        db = d4open(cb, fPath & "INFO")
        rc = d4top(db)
        displayTheRecord(db, frm)
        rc = code4initUndo(cb)
    End Sub
End Module