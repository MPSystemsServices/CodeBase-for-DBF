Option Strict Off
Option Explicit On
Module EXAMPLE
	'EX67.VB
    'NOTE: 'today' is a reserved word under VB .NET	
 
	Dim cb, db As Integer
	Dim rc As Integer
	
	Public lf As String 'Line Feed
	Public fPath As String 'Full path name to db files
	
	Public Const MB_OK As Short = 0
	Public Const MB_YESNO As Short = 4
	Public Const MB_ICONQUESTION As Short = 32
	Public Const IDYES As Short = 6
	
    Sub ExCode(ByRef frm As Form1)
        Dim birthField, cb, db, birthTag As Integer
        Dim result, today_Renamed, tomorrow, yesterday As String
        Dim myBirthDate As String
        Dim rc As Short

        Call date4today(today_Renamed)
        Call date4format(today_Renamed, result, "MMMMMMMM DD, CCYY")
        frm.ListBox1.Items.Add("Today is " & date4cdow(today_Renamed) & " " & result)

        Call date4assign(tomorrow, date4long(today_Renamed) + 1)
        Call date4format(tomorrow, result, "MMMMMMMM DD, CCYY")
        frm.ListBox1.Items.Add("Tomorrow is " & date4cdow(tomorrow) & " " & result)

        Call date4assign(yesterday, date4long(tomorrow) - 2)
        Call date4format(yesterday, result, "MMMMMMMM DD, CCYY")
        frm.ListBox1.Items.Add("Yesterday was " & date4cdow(yesterday) & " " & result)

        cb = code4init()

        'Specify full path if stand-alone
        If u4switch() And &H80S Then fPath = VB6.GetPath & "\"

        db = d4open(cb, fPath & "INFO")
        birthField = d4field(db, "BIRTH_DATE")
        birthTag = d4tag(db, "INF_BRTH")
        Call d4tagSelect(db, birthTag)
        myBirthDate = "19690225"
        If d4seek(db, myBirthDate) = r4success Then
            frm.ListBox1.Items.Add("I'm in record " & Str(d4recNo(db)))
        End If

        'change all birthdate fields to my birth date
        rc = d4top(db)
        Do While d4eof(db) = 0
            Call f4assign(birthField, myBirthDate)
            rc = d4skip(db, 1)
        Loop

        rc = code4initUndo(cb)
    End Sub
End Module