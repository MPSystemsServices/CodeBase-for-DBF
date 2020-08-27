Option Strict Off
Option Explicit On
Module EXAMPLE 
    'EX78.VB

    Dim cb As Integer
    Dim rc As Short

    Public lf As String 'Line Feed
    Public fPath As String 'Full path name to db files

    Public Const MB_OK As Short = 0
    Public Const MB_YESNO As Short = 4
    Public Const MB_ICONQUESTION As Short = 32
    Public Const IDYES As Short = 6

    Function display(ByRef p As String, ByRef fm As Form1) As Short
        If p = "" Then
            display = error4describe(cb, e4parm, 0, "Null display string", "", "")
            Exit Function
        End If
        fm.ListBox1.Items.Add(p)
        display = r4success
    End Function

    Sub ExCode(ByRef frm As Form1)
        Dim p As String

        p = "some string"
        cb = code4init()
        rc = display(p, frm)
        rc = display("", frm)
        rc = code4initUndo(cb)
    End Sub
End Module