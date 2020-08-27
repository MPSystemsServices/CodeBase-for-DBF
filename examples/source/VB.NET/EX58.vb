Option Strict Off
Option Explicit On
Module EXAMPLE
	'EX58.VB
	
	Public lf As String 'Line Feed
	Public fPath As String 'Full path name to db files
	
	Public Const MB_OK As Short = 0
	Public Const MB_YESNO As Short = 4
	Public Const MB_ICONQUESTION As Short = 32
	Public Const IDYES As Short = 6
	
    Sub ExCode(ByRef frm As Form1)
        Dim db, cb, age As Integer
        Dim rc As Short
        Dim birth, result As String
        Dim birthLong As Integer

        cb = code4init()

        'Specify full path if stand-alone
        If u4switch() And &H80S Then fPath = VB6.GetPath & "\"

        db = d4open(cb, fPath & "PEOPLE.DBF")
        'Assume PEOPLE.DBF has a production index file with
        'tags PPL_NAME, PPL_AGE, PPL_BRTH
        Call d4tagSelect(db, d4tag(db, "PPL_NAME"))

        If d4seek(db, "fred") = r4success Then
            frm.ListBox1.Items.Add("fred is in record # " & Str(d4recNo(db)))
        End If

        If d4seek(db, "HANK STEVENS") = r4success Then
            frm.ListBox1.Items.Add("HANK STEVENS is in record #" & Str(d4recNo(db)))
        End If

        Call d4tagSelect(db, d4tag(db, "PPL_AGE"))
        age = d4field(db, "AGE")

        rc = d4seekDouble(db, 0.0#)

        If rc = r4success Or rc = r4after Then
            frm.ListBox1.Items.Add("The youngest age is " & Str(f4int(age)))
        End If

        'Seek using the character version
        rc = d4seek(db, "0")

        If rc = r4success Or rc = r4after Then
            frm.ListBox1.Items.Add("The youngest age is " & Str(f4int(age)))
        End If

        'Assume PPL_BRTH is a Date key expression
        Call d4tagSelect(db, d4tag(db, "PPL_BRTH"))

        birth = "19600415"

        If d4seek(db, birth) = r4success Then
            Call date4format(birth, result, "MMM DD, CCYY")
            frm.ListBox1.Items.Add("Found " & result)
        End If

        birthLong = date4long(birth)

        If d4seekDouble(db, birthLong) = r4success Then
            frm.ListBox1.Items.Add("Found " & result)
        End If
        rc = d4close(db)
        rc = code4initUndo(cb)
    End Sub
End Module