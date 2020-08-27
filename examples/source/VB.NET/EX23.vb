Option Strict Off
Option Explicit On
Module EXAMPLE
    'EX23.VB
	
	Public lf As String 'Line Feed
	Public fPath As String 'Full path name to data files
	
	Public Const MB_OK As Short = 0
	Public Const MB_YESNO As Short = 4
	Public Const MB_ICONQUESTION As Short = 32
	Public Const IDYES As Short = 6
	
    Sub ExCode(ByRef frm As Form1)
        Dim info, cb, field As Integer
        Dim rc As Short
        Dim iRec, recNum As Integer

        cb = code4init()

        'Specify full path if stand-alone
        If u4switch() And &H80S Then fPath = VB6.GetPath & "\"

        info = d4open(cb, fPath & "DATA2")

        rc = code4optStart(cb)
        field = d4field(info, "NAME")

        recNum = d4recCount(info)
        For iRec = 1 To recNum
            rc = d4go(info, iRec)
            Call f4assign(field, "New Data")
        Next

        rc = d4close(info)
        rc = code4initUndo(cb)
    End Sub
End Module