Option Strict Off
Option Explicit On
Module EXAMPLE 
    'EX73.VB

    Public lf As String 'Line Feed
    Public fPath As String 'Full path name to db files

    Public Const MB_OK As Short = 0
    Public Const MB_YESNO As Short = 4
    Public Const MB_ICONQUESTION As Short = 32
    Public Const IDYES As Short = 6

    Sub ExCode(ByRef frm As Form1)
        Dim dayStr As String

        'date4init puts a given date in CCYYMMDD format
        Call date4init(dayStr, "Oct 07/90", "MMM DD/YY")
        frm.ListBox1.Items.Add("Oct 07/90 becomes " & dayStr)
        Call date4init(dayStr, "08/07/1989", "MM/DD/CCYY")
        frm.ListBox1.Items.Add("08/07/1989 becomes " & dayStr)
    End Sub
End Module