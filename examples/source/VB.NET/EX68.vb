Option Strict Off
Option Explicit On
Module EXAMPLE 
    'EX68.VB

    Public lf As String 'Line Feed
    Public fPath As String 'Full path name to db files

    Public Const MB_OK As Short = 0
    Public Const MB_YESNO As Short = 4
    Public Const MB_ICONQUESTION As Short = 32
    Public Const IDYES As Short = 6

    Sub ExCode(ByRef frm As Form1)
        Dim result, dateStr, dayBefore, resultBefore As String
        Dim rc As Short

        dateStr = "19900101"

        Call date4assign(dayBefore, date4long(dateStr) - 1)
        Call date4format(dateStr, result, "MMM DD, 'YY")
        Call date4format(dayBefore, resultBefore, "MMM DD, 'YY")
        frm.ListBox1.Items.Add(result & " is after " & resultBefore)
    End Sub
End Module