Option Strict Off
Option Explicit On
Module EXAMPLE
    'EX4.VB
	
	Public lf As String 'Line Feed
	Public fPath As String 'Full path name to data files
	
	Public Const MB_OK As Short = 0
	Public Const MB_YESNO As Short = 4
	Public Const MB_ICONQUESTION As Short = 32
	Public Const IDYES As Short = 6
	
    Sub ExCode(ByRef frm As Form1)
        'ex4 example code

        Dim cb, db As Integer
        Dim rc As Short

        cb = code4init()

        ' Do not add duplicate records to unique tags or the data file and
        ' return r4unique when attempted.
        rc = code4errDefaultUnique(cb, r4unique)

        'Specify full path if stand-alone
        If u4switch() And &H80S Then fPath = VB6.GetPath & "\"

        db = d4open(cb, fPath & "INFO")
        rc = d4top(db)
        rc = d4appendStart(db, 0)

        rc = d4append(db) ' append a duplicate copy of the top record

        If rc = r4unique Then
            MsgBox("Attempt to add a duplicate record failed.", MsgBoxStyle.OKOnly)
        Else
            MsgBox("Attempt to add a duplicate record succeeded", MsgBoxStyle.OKOnly)
            MsgBox("Record in both data and index file", MsgBoxStyle.OKOnly)
        End If

        rc = d4close(db)
        rc = code4initUndo(cb)
    End Sub
End Module