Option Strict Off
Option Explicit On
Module EXAMPLE
	'EX31.VB
	
	Public lf As String 'Line Feed
	Public fPath As String 'Full path name to db files
	
	Public Const MB_OK As Short = 0
	Public Const MB_YESNO As Short = 4
	Public Const MB_ICONQUESTION As Short = 32
	Public Const IDYES As Short = 6
	
    Sub ExCode(ByRef frm As Form1)
        Dim checkData, cb, testIndex As Integer
        Dim rc As Short
        Dim fileName, indexName As String

        cb = code4init()

        'Specify full path if stand-alone
        If u4switch() And &H80S Then fPath = VB6.GetPath & "\"

        rc = code4autoOpen(cb, 0) 'open index file manually
        fileName = InputBox("Enter data file name ", "EX31", "INFO")
        indexName = InputBox("Enter index file name ", "EX31", "INFO")
        If fileName = "" Then
            Exit Sub
        Else
            checkData = d4open(cb, fPath & fileName)
            testIndex = i4open(checkData, fPath & indexName)

            error4exitTest((cb))
            rc = code4optStart(cb)

            If d4check(checkData) = r4success Then
                MsgBox("Index is OK !")
            Else
                MsgBox("Problem with Index")
            End If
        End If

        rc = code4close(cb)
        rc = code4initUndo(cb)
    End Sub
End Module