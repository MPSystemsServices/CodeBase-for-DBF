Option Strict Off
Option Explicit On
Module EXAMPLE 
    'EX70.VB
    'NOTE: 'today' is a reserved word under VB .NET

    Public lf As String 'Line Feed
    Public fPath As String 'Full path name to db files

    Public Const MB_OK As Short = 0
    Public Const MB_YESNO As Short = 4
    Public Const MB_ICONQUESTION As Short = 32
    Public Const IDYES As Short = 6

    Sub ExCode(ByRef frm As Form1)
        Dim today_Renamed As String

        Call date4today(today_Renamed)
        frm.ListBox1.Items.Add("The current month is " & date4cmonth(today_Renamed))
        'displays "The current month is January" if the system clock says that it is
    End Sub
End Module