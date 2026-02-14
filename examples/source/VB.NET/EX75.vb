Option Strict Off
Option Explicit On
Module EXAMPLE 
    'EX75.VB
    'NOTE: 'today' is a reserved word under VB .NET

    Public lf As String 'Line Feed
    Public fPath As String 'Full path name to db files

    Public Const MB_OK As Short = 0
    Public Const MB_YESNO As Short = 4
    Public Const MB_ICONQUESTION As Short = 32
    Public Const IDYES As Short = 6

    Sub ExCode(ByRef frm As Form1)
        Dim daysInMonth() As Short
        Dim endOfMonth As Short
        Dim today_Renamed As String

        ReDim daysInMonth(12)
        daysInMonth(0) = 31
        daysInMonth(1) = 28
        daysInMonth(2) = 31
        daysInMonth(3) = 30
        daysInMonth(4) = 31
        daysInMonth(5) = 30
        daysInMonth(6) = 31
        daysInMonth(7) = 31
        daysInMonth(8) = 30
        daysInMonth(9) = 31
        daysInMonth(10) = 30
        daysInMonth(11) = 31

        Call date4today(today_Renamed)
        endOfMonth = daysInMonth(date4month(today_Renamed))
        If date4month(today_Renamed) = 2 And date4isLeap(today_Renamed) = 1 Then
            endOfMonth = endOfMonth + 1
        End If
        frm.ListBox1.Items.Add("There are " & Str(endOfMonth - date4day(today_Renamed)) & " days till the end of the month")
    End Sub
End Module