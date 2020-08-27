Option Strict Off
Option Explicit On
Module EXAMPLE 
    'EX72.VB

    Public lf As String 'Line Feed
    Public fPath As String 'Full path name to db files

    Public Const MB_OK As Short = 0
    Public Const MB_YESNO As Short = 4
    Public Const MB_ICONQUESTION As Short = 32
    Public Const IDYES As Short = 6

    Sub ExCode(ByRef frm As Form1)
        Dim dt, result As String

        dt = "19901002"
        Call date4format(dt, result, "YY.MM.DD") 'result will contain "90.10.02"
        frm.ListBox1.Items.Add(result)
        Call date4format(dt, result, "CCYY.MM.DD") 'result will contain "1990.10.02"
        frm.ListBox1.Items.Add(result)
        Call date4format(dt, result, "MM/DD/YY") 'result will contain "10/02/90"
        frm.ListBox1.Items.Add(result)
        Call date4format(dt, result, "MMM DD/CCYY") 'result will contain "Oct 02/1990"
        frm.ListBox1.Items.Add(result)
    End Sub
End Module