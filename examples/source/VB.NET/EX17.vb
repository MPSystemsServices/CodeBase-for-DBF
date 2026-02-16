Option Strict Off
Option Explicit On
Module EXAMPLE
    'EX17.VB
	
	Public cb As Integer
	Public rc As Short
	
	Public lf As String 'Line Feed
	Public fPath As String 'Full path name to data files
	
	Public Const MB_OK As Short = 0
	Public Const MB_YESNO As Short = 4
	Public Const MB_ICONQUESTION As Short = 32
	Public Const IDYES As Short = 6
	
    Sub ExCode(ByRef frm As Form1)
        Dim db As Integer

        cb = code4init()

        'Specify full path if stand-alone
        If u4switch() And &H80S Then fPath = VB6.GetPath & "\"

        Call openAFile()

        db = code4data(cb, "INFO") 'obtain a new DATA4 structure
        If db <> 0 Then
            frm.ListBox1.Items.Add("INFO has " & Str(d4recCount(db)) & " records")
            rc = d4top(db)
            'an alternative way to close the file
            rc = d4close(code4data(cb, "INFO"))
        End If

        rc = code4initUndo(cb)
    End Sub

    Sub openAFile()
        Dim db As Integer
        'db falls out of scope.  Data file is still open
        db = d4open(cb, fPath & "INFO")
    End Sub
End Module