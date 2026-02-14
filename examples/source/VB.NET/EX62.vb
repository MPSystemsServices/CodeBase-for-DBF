Option Strict Off
Option Explicit On
Module EXAMPLE
	'EX62.VB
	
	Public lf As String 'Line Feed
	Public fPath As String 'Full path name to db files
	
	Public Const MB_OK As Short = 0
	Public Const MB_YESNO As Short = 4
	Public Const MB_ICONQUESTION As Short = 32
	Public Const IDYES As Short = 6
	
    Sub ExCode(ByRef frm As Form1)
        Dim db, cb, tag As Integer
        Dim rc As Short

        cb = code4init()

        'Specify full path if stand-alone
        If u4switch() And &H80S Then fPath = VB6.GetPath & "\"

        db = d4open(cb, fPath & "DBF")
        tag = d4tag(db, "DBF_NAME") 'a tag with a '.NOT.DELETED()' filter

        rc = d4top(db) 'position to the first record that is not deleted
        Call d4delete(db) 'current record is no longer located in the tag

        rc = d4tagSync(db, tag)
        rc = d4skip(db, 1)

        'some other code
        rc = code4initUndo(cb)
    End Sub
End Module