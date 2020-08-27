Option Strict Off
Option Explicit On
Module EXAMPLE
    'EX22.VB
	
	Public lf As String 'Line Feed
	Public fPath As String 'Full path name to data files
	
	Public Const MB_OK As Short = 0
	Public Const MB_YESNO As Short = 4
	Public Const MB_ICONQUESTION As Short = 32
	Public Const IDYES As Short = 6
	
    Sub ExCode(ByRef frm As Form1)
        'ex22 example code
        Dim info, cb, db As Integer
        Dim rc As Short

        cb = code4init()

        'Specify full path if stand-alone
        If u4switch() And &H80S Then fPath = VB6.GetPath & "\"

        info = d4open(cb, fPath & "INFO")
        db = d4open(cb, fPath & "DATA1")

        rc = d4lock(info, 1)
        If rc = r4success Then frm.ListBox1.Items.Add("lock successful")
        rc = d4lockAll(db)
        If rc = r4success Then frm.ListBox1.Items.Add("lock successful")
        rc = code4unlock(cb) 'unlocks all open files
        If rc = r4success Then frm.ListBox1.Items.Add("unlock successful")
        rc = code4initUndo(cb)
    End Sub
End Module