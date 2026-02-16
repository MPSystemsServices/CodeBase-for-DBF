Option Strict Off
Option Explicit On
Module EXAMPLE 
    'EX85.VB

    'NOTE: 'today' is a reserved word under VB .NET

    Public lf As String 'Line Feed
    Public fPath As String 'Full path name to db files

    Public Const MB_OK As Short = 0
    Public Const MB_YESNO As Short = 4
    Public Const MB_ICONQUESTION As Short = 32
    Public Const IDYES As Short = 6

    Sub ExCode(ByRef frm As Form1)
        Dim db, cb, birthDate As Integer
        Dim rc As Short
        Dim today_Renamed, result As String
        Dim ageInDays As Integer

        cb = code4init()
        rc = code4accessMode(cb, OPEN4DENY_RW)

        'Specify full path if stand-alone
        If u4switch() And &H80S Then fPath = VB6.GetPath & "\"

        db = d4open(cb, fPath & "INFO")
        birthDate = d4field(db, "BIRTH_DATE")
        error4exitTest((cb))

        rc = d4top(db)
        If rc <> r4success Then
            MsgBox("No records to display.")
        Else
            Call date4today(today_Renamed)
            ageInDays = date4long(today_Renamed) - date4long(f4str(birthDate))
            frm.ListBox1.Items.Add("Age in days: " & Str(ageInDays))

            'display all current birth dates in formatted form
            rc = d4top(db)
            Do While rc = r4success
                Call date4format(f4str(birthDate), result, "MMM DD, CCYY")
                frm.ListBox1.Items.Add(result)
                rc = d4skip(db, 1)
            Loop

            'assign today's date to all birth dates in the data file
            rc = d4top(db)
            Do While rc = r4success
                Call f4assign(birthDate, today_Renamed)
                rc = d4skip(db, 1)
            Loop
        End If

        rc = code4initUndo(cb)
    End Sub
End Module