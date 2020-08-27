Option Strict Off
Option Explicit On
Module EXAMPLE 
    'EX84.VB

    Dim db, cb, expr As Integer
    Dim rc As Short

    Public lf As String 'Line Feed
    Public fPath As String 'Full path name to db files

    Public Const MB_OK As Short = 0
    Public Const MB_YESNO As Short = 4
    Public Const MB_ICONQUESTION As Short = 32
    Public Const IDYES As Short = 6

    Sub ExCode(ByRef frm As Form1)

        cb = code4init()

        'Specify full path if stand-alone
        If u4switch() And &H80S Then fPath = VB6.GetPath & "\"

        db = d4open(cb, fPath & "INFO")
        rc = d4top(db)

        expr = expr4parse(db, "NAME")
        Call showExpr(frm)
        Call expr4free(expr)

        expr = expr4parse(db, "AGE")
        Call showExpr(frm)
        Call expr4free(expr)

        expr = expr4parse(db, "BIRTH_DATE")
        Call showExpr(frm)
        Call expr4free(expr)

        rc = code4close(cb)
        rc = code4initUndo(cb)
    End Sub

    Sub showExpr(ByRef fm As Form1)
        Dim typeExpr As String
        typeExpr = expr4type(expr)

        Select Case typeExpr
            Case r4date
                fm.ListBox1.Items.Add("type is r4date")
            Case r4dateDoub
                fm.ListBox1.Items.Add("type is r4dateDoub")
            Case r4log
                fm.ListBox1.Items.Add("type is r4log")
            Case r4num
                fm.ListBox1.Items.Add("type is r4num")
            Case r4numDoub
                fm.ListBox1.Items.Add("type is r4numDoub")
            Case r4str
                fm.ListBox1.Items.Add("type is r4str")
            Case r4memo
                fm.ListBox1.Items.Add("type is r4memo")
        End Select
    End Sub
End Module