Option Strict Off
Option Explicit On
Module EXAMPLE
    'EX18.VB
	
	Public cb As Integer
	Public rc As Short
	
	Public lf As String 'Line Feed
	Public fPath As String 'Full path name to data files
	
	Public Const MB_OK As Short = 0
	Public Const MB_YESNO As Short = 4
	Public Const MB_ICONQUESTION As Short = 32
	Public Const IDYES As Short = 6
	
    Sub ExCode(ByRef frm As Form1)
        cb = code4init()
        Call exitToSystem()
    End Sub

    Sub exitToSystem()
        MsgBox("Shutting down application ... ")

        rc = code4close(cb)
        Call code4exit(cb)
    End Sub
End Module