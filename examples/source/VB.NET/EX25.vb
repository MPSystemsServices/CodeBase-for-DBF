Option Strict Off
Option Explicit On
Module EXAMPLE
	'EX25.BAS
	
	Public lf As String 'Line Feed
	Public fPath As String 'Full path name to db files
	
	Public Const MB_OK As Short = 0
	Public Const MB_YESNO As Short = 4
	Public Const MB_ICONQUESTION As Short = 32
	Public Const IDYES As Short = 6
	
    Sub ExCode(ByRef frm As Form1)
        Dim db, cb, field As Integer
        Dim rc As Short

        cb = code4init()

        'Specify full path if stand-alone
        If u4switch() And &H80S Then fPath = VB6.GetPath & "\"

        db = d4open(cb, fPath & "INFO")
        rc = code4optStart(cb)

        rc = d4appendBlank(db)
        rc = d4appendBlank(db)

        'Append a copy of record two, assuming record two exists
        rc = d4go(db, 2)
        rc = d4appendStart(db, 0) 'a false parameter means don't copy memo entries
        rc = d4append(db) 'append a copy of record 2 using a blank memo

        rc = d4go(db, 2)
        rc = d4appendStart(db, 1) 'a true parameter means copy memo entries
        rc = d4append(db) 'append a copy of record 2 with its memo

        'Set the record buffer to blank, change a field's value, and append
        'the resulting record.
        rc = d4appendStart(db, 0)
        Call d4blank(db)

        field = d4field(db, "NAME")
        Call f4assign(field, "New Field Value")
        rc = d4append(db)

        'close all open files and release any allocated memory
        rc = code4initUndo(cb)
    End Sub
End Module