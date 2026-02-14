Option Strict Off
Option Explicit On
Module EXAMPLE 
    'EX76.VB

    Public lf As String 'Line Feed
    Public fPath As String 'Full path name to db files

    Public Const MB_OK As Short = 0
    Public Const MB_YESNO As Short = 4
    Public Const MB_ICONQUESTION As Short = 32
    Public Const IDYES As Short = 6

    Sub ExCode(ByRef frm As Form1)
        Dim d As String
        Dim daysToWeekEnd As Short

        Call date4today(d)
        frm.ListBox1.Items.Add("Today is " & date4cdow(d))
        daysToWeekEnd = 7 - date4dow(d)
        If daysToWeekEnd = 0 Or daysToWeekEnd = 6 Then
            frm.ListBox1.Items.Add("Better enjoy it!")
        Else
            frm.ListBox1.Items.Add("Only " & Str(daysToWeekEnd) & " days to go till the weekend")
        End If
    End Sub
End Module