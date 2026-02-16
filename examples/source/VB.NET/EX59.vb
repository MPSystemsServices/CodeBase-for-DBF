Option Strict Off
Option Explicit On
Module EXAMPLE
	'EX59.VB
	
	Dim cb, db As Integer
	Dim rc As Short
	
	Public lf As String 'Line Feed
	Public fPath As String 'Full path name to db files
	
	Public Const MB_OK As Short = 0
	Public Const MB_YESNO As Short = 4
	Public Const MB_ICONQUESTION As Short = 32
	Public Const IDYES As Short = 6
	
    Sub ExCode(ByRef frm As Form1)
        Dim nameTag As Integer
        Dim i As Short

        cb = code4init()

        'Specify full path if stand-alone
        If u4switch() And &H80S Then fPath = VB6.GetPath & "\"

        db = d4open(cb, fPath & "PEOPLE")
        nameTag = d4tag(db, "PPL_NAME")
        Call d4tagSelect(db, nameTag)
        rc = d4top(db)
        rc = SeekSeries("mickey")
        frm.ListBox1.Items.Add("return code" & Str(rc))
        rc = code4initUndo(cb)
    End Sub

    Function SeekSeries(ByRef s As String) As Short
        rc = d4seekNext(db, s)

        If rc = r4notag Or rc = r4entry Or rc = r4locked Then
            SeekSeries = rc
            Exit Function
        End If

        If rc = r4after Or rc = r4eof Then rc = d4seek(db, s)

        SeekSeries = rc
    End Function
End Module