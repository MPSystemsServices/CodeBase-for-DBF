Option Strict Off
Option Explicit On
Module EXAMPLE 
    'EX12.VB
	
	Public cb, db As Integer
	Public rc As Short
	
	Public lf As String 'Line Feed
	Public fPath As String 'Full path name to data files
	
	Public Const MB_OK As Short = 0
	Public Const MB_YESNO As Short = 4
	Public Const MB_ICONQUESTION As Short = 32
	Public Const IDYES As Short = 6
	
    Sub ExCode(ByRef frm As Form1)
        cb = code4init()

        'Specify full path if stand-alone
        If u4switch() And &H80S Then fPath = VB6.GetPath & "\"

        db = d4open(cb, fPath & "DATEFILE")
        Call modifyRecordValues()
        rc = code4initUndo(cb)
    End Sub

    Sub modifyRecordValues()
        Dim buf As String
        Dim field As Integer

        rc = code4readLock(cb, 1)
        rc = code4lockAttempts(cb, 3)

        field = d4field(db, "DATE")

        rc = d4top(db)
        Do While rc = r4locked
            If retry() = 1 Then rc = d4top(db)
        Loop

        If rc = r4locked Then Exit Sub

        Do While d4eof(db) = 0
            buf = InputBox("Enter the new record value", "EX12", "19950101")
            Call f4assign(field, buf)

            rc = d4skip(db, 1)
            Do While rc = r4locked
                If retry() = 1 Then rc = d4skip(db, 1)
            Loop
            If rc = r4locked Then Exit Sub
        Loop
    End Sub

    Function retry() As Integer
        rc = MsgBox("Record locked by another user. Retry?", MsgBoxStyle.YesNo)
        If rc = IDYES Then
            retry = 1
        Else
            retry = 0
        End If
    End Function
End Module