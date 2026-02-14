Option Strict Off
Option Explicit On
Module EXAMPLE
    'EX11.VB
	
	Public lf As String 'Line Feed
	Public fPath As String 'Full path name to data files
	
	Public Const MB_OK As Short = 0
	Public Const MB_YESNO As Short = 4
	Public Const MB_ICONQUESTION As Short = 32
	Public Const IDYES As Short = 6
	
    Sub ExCode(ByRef frm As Form1)
        'ex11 example code
        Dim db, cb, dateField As Integer
        Dim rc As Short
        Dim today_Renamed As String
        Dim oldOpt, oldLockAtt, oldOptWrite As Short

        cb = code4init()
        oldLockAtt = code4lockAttempts(cb, WAIT4EVER)
        oldOpt = code4optimize(cb, OPT4ALL)
        oldOptWrite = code4optimizeWrite(cb, OPT4ALL)

        'Specify full path if stand-alone
        If u4switch() And &H80S Then fPath = VB6.GetPath & "\"

        db = d4open(cb, fPath & "DATEFILE")
        If code4errorCode(cb, r4check) Then code4exit((cb))

        rc = d4lockAll(db) 'lock the file for optimizations to take place

        dateField = d4field(db, "DATE")
        Call date4today(today_Renamed)

        rc = code4optStart(cb)
        rc = d4top(db)
        Do While d4eof(db) = 0
            Call f4assign(dateField, today_Renamed)
            rc = d4skip(db, 1)
        Loop
        rc = code4optSuspend(cb)

        rc = d4close(db)
        rc = code4lockAttempts(cb, oldLockAtt)
        rc = code4optimize(cb, oldOpt)
        rc = code4optimizeWrite(cb, oldOptWrite)
        rc = code4initUndo(cb)
    End Sub
End Module