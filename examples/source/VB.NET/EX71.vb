Option Strict Off
Option Explicit On
Module EXAMPLE 
    'EX71.VB

    Public lf As String 'Line Feed
    Public fPath As String 'Full path name to db files

    Public Const MB_OK As Short = 0
    Public Const MB_YESNO As Short = 4
    Public Const MB_ICONQUESTION As Short = 32
    Public Const IDYES As Short = 6

    Sub ExCode(ByRef frm As Form1)
        Dim tillEnd As Short
        Dim dateStr As String

        Call date4today(dateStr)
        tillEnd = 7 - date4dow(dateStr)
        frm.ListBox1.Items.Add(Str(tillEnd) & " days left till the end of the week")
    End Sub
End Module