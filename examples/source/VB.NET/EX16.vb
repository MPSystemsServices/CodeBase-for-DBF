Option Strict Off
Option Explicit On
Module EXAMPLE
    'EX16.VB
	
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
        rc = code4autoOpen(cb, 0)

        'Specify full path if stand-alone
        If u4switch() And &H80S Then fPath = VB6.GetPath & "\"

        Call openAFile()

        db = d4open(cb, fPath & "DATA1") 'open a second file
        frm.ListBox1.Items.Add("Number of records in DATA1: " & Str(d4recCount(db)))

        rc = code4close(cb) 'INFO and DATA1 are both closed
        rc = code4initUndo(cb)
    End Sub

    Sub openAFile()
        Dim db As Integer
        'db falls out of scope. the data file is still open
        db = d4open(cb, fPath & "INFO")
    End Sub
End Module