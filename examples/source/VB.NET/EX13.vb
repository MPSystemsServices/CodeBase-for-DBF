Option Strict Off
Option Explicit On
Module EXAMPLE
	'EX13.VB
	
	Public lf As String 'Line Feed
	Public fpath As String 'Full path name to data files
	
	Public Const MB_OK As Short = 0
	Public Const MB_YESNO As Short = 4
	Public Const MB_ICONQUESTION As Short = 32
	Public Const IDYES As Short = 6
	
    Sub ExCode(ByRef frm As Form1)
        Dim db, cb, tag As Integer
        Dim rc As Short

        cb = code4init()
        rc = code4readOnly(cb, 1)

        'open a file on a drive without write access
        db = d4open(cb, "W:\DATA1.DBF")

        If code4errorCode(cb, 0) = r4success Then
            tag = d4tag(db, "NAME_TAG")
            Call d4tagSelect(db, tag)

            If d4seek(db, "Sarah     Webber") = 0 Then
                MsgBox("Sarah is found")
            Else
                MsgBox("Sarah is not found")
            End If

            rc = d4close(db)
        End If

        rc = code4initUndo(cb)
    End Sub
End Module