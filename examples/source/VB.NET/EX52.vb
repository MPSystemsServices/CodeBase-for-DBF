Option Strict Off
Option Explicit On
Module EXAMPLE 
    'EX52.VB

    Dim cb, db As Integer
    Dim rc As Short

    Public lf As String 'Line Feed
    Public fPath As String 'Full path name to db files

    Public Const MB_OK As Short = 0
    Public Const MB_YESNO As Short = 4
    Public Const MB_ICONQUESTION As Short = 32
    Public Const IDYES As Short = 6

    Sub ExCode(ByRef frm As Form1)
        Dim count As Integer

        cb = code4init()

        'Specify full path if stand-alone
        If u4switch() And &H80S Then fPath = VB6.GetPath & "\"

        db = d4open(cb, fPath & "INFO")

        count = recsInFile()
        frm.ListBox1.Items.Add("the number of records in the file is " & Str(count))
        rc = code4initUndo(cb)
    End Sub

    Function recsInFile() As Integer
        Dim count As Integer

        If code4errorCode(cb, r4check) < 0 Then 'an error occurred
            recsInFile = -1
            Exit Function
        End If
        count = d4recCount(db) 'save the record count
        recsInFile = count
    End Function
End Module