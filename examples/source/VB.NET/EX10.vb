Option Strict Off
Option Explicit On
Module EXAMPLE
    'EX10.VB
	
	Public lf As String 'Line Feed
	Public fPath As String 'Full path name to data files
	
	Public Const MB_OK As Short = 0
	Public Const MB_YESNO As Short = 4
	Public Const MB_ICONQUESTION As Short = 32
	Public Const IDYES As Short = 6
	
    Sub ExCode(ByRef frm As Form1)
        'ex10 example code
        Dim cb, inventory As Integer
        Dim onHand, minOnHand, stockName As Integer
        Dim rc, count As Short
        Dim oldOpt, oldExcl As Short

        cb = code4init()
        oldOpt = code4optimize(cb, OPT4EXCLUSIVE)
        oldExcl = code4accessMode(cb, OPEN4DENY_RW)

        'Specify full path if stand-alone
        If u4switch() And &H80S Then fPath = VB6.GetPath & "\"

        inventory = d4open(cb, fPath & "INVENT.DBF") 'Read optimized
        minOnHand = d4field(inventory, "MIN_ON_HND")
        onHand = d4field(inventory, "ON_HAND")
        stockName = d4field(inventory, "ITEM")

        count = 0
        If code4errorCode(cb, r4check) >= 0 Then
            rc = code4optStart(cb)
            rc = d4top(inventory)
            Do While d4eof(inventory) = 0
                If f4long(onHand) < f4long(minOnHand) Then
                    count = count + 1
                End If
                rc = d4skip(inventory, 1)
            Loop
            rc = code4optSuspend(cb)
        End If

        rc = code4optimize(cb, oldOpt)
        rc = code4accessMode(cb, oldExcl)
        rc = d4close(inventory)
        MsgBox(Str(count) & " items need to be restocked", MsgBoxStyle.OKOnly)
        rc = code4initUndo(cb)
    End Sub
End Module