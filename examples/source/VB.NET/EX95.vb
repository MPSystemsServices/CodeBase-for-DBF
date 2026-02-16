Option Strict Off
Option Explicit On
Module EXAMPLE 
    'EX95.VB

    Public lf As String 'Line Feed
    Public fPath As String 'Full path name to db files

    Public Const MB_OK As Short = 0
    Public Const MB_YESNO As Short = 4
    Public Const MB_ICONQUESTION As Short = 32
    Public Const IDYES As Short = 6

    Sub ExCode(ByRef frm As Form1)
        Dim db, cb, index As Integer
        Dim rc As Short

        cb = code4init()

        'Specify full path if stand-alone
        If u4switch() And &H80S Then fPath = VB6.GetPath & "\"

        db = d4open(cb, fPath & "STUDENT")
        index = i4open(db, "STUDENT2") 'Open a seconday index file

        rc = code4lockAttempts(cb, WAIT4EVER) 'Wait until the lock succeeds
        rc = d4lockAll(db)
        If i4reindex(index) = r4success Then
            frm.ListBox1.Items.Add("Reindexed successfully")
        End If

        rc = code4close(cb)
        rc = code4initUndo(cb)
    End Sub
End Module