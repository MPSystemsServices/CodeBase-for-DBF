Option Strict Off
Option Explicit On
Module EXAMPLE 
    'EX80.VB

    Public lf As String 'Line Feed
    Public fPath As String 'Full path name to db files

    Public Const MB_OK As Short = 0
    Public Const MB_YESNO As Short = 4
    Public Const MB_ICONQUESTION As Short = 32
    Public Const IDYES As Short = 6

    Sub ExCode(ByRef frm As Form1)
        Dim db, cb, expr As Integer
        Dim rc As Short

        cb = code4init()

        'Specify full path if stand-alone
        If u4switch() And &H80S Then fPath = VB6.GetPath & "\"

        db = d4open(cb, fPath & "DATA1")
        rc = d4go(db, 1)
        'FNAME and LNAME are character field names of data file DATA1.DBF

        expr = expr4parse(db, "F_NAME+' '+L_NAME")
        If expr <> 0 Then
            frm.ListBox1.Items.Add("F_NAME and L_NAME for record one: " & expr4str(expr))
        End If
        Call expr4free(expr)
        rc = d4close(db)
        rc = code4initUndo(cb)
    End Sub
End Module