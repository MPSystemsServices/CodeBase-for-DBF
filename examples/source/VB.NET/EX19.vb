Option Strict Off
Option Explicit On
Module EXAMPLE
    'EX19.VB
	
	Public lf As String 'Line Feed
	Public fPath As String 'Full path name to data files
	
	Public Const MB_OK As Short = 0
	Public Const MB_YESNO As Short = 4
	Public Const MB_ICONQUESTION As Short = 32
	Public Const IDYES As Short = 6
	
    Sub ExCode(ByRef frm As Form1)
        Dim data1, cb, data2 As Integer
        Dim rc As Short
        Dim numRec As Integer

        cb = code4init()

        'Specify full path if stand-alone
        If u4switch() And &H80S Then fPath = VB6.GetPath & "\"

        data1 = d4open(cb, fPath & "DATA1")
        data2 = d4open(cb, fPath & "DATA2")

        rc = d4top(data1)
        rc = d4top(data2)

        rc = d4lockAddFile(data1)
        rc = d4lockAddAppend(data2)

        numRec = d4recCount(data2)

        If numRec > 2 Then
            rc = d4lockAdd(data2, numRec)
            rc = d4lockAdd(data2, numRec - 1)
        End If

        If code4lock(cb) = r4success Then
            MsgBox("All locks were successfully performed")
        End If

        rc = code4initUndo(cb)
    End Sub
End Module