Option Strict Off
Option Explicit On
Module EXAMPLE
    'EX74.VB
    'NOTE: 'today' is a reserved word under VB .NET

    Public lf As String 'Line Feed
    Public fPath As String 'Full path name to db files

    Public Const MB_OK As Short = 0
    Public Const MB_YESNO As Short = 4
    Public Const MB_ICONQUESTION As Short = 32
    Public Const IDYES As Short = 6

    Sub ExCode(ByRef frm As Form1)
        Dim yesterday As Integer
        Dim tomorrow, today_Renamed, result As String

        Call date4today(today_Renamed)
        yesterday = date4long(today_Renamed) - 1
        Call date4assign(tomorrow, yesterday + 2)
        Call date4format(today_Renamed, result, "MMM DD, CCYY")
        frm.ListBox1.Items.Add("Today is " & result)
        frm.ListBox1.Items.Add("The Julian date for yesterday is " & Str(yesterday))
        frm.ListBox1.Items.Add("The Julian date for tomorrow is " & Str(date4long(tomorrow)))
    End Sub
End Module