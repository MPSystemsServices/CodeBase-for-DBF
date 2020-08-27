Option Strict Off
Option Explicit On
Module EXAMPLE 
    'EX93.VB

    Dim rc As Short

    Public lf As String 'Line Feed
    Public fPath As String 'Full path name to db files

    Public Const MB_OK As Short = 0
    Public Const MB_YESNO As Short = 4
    Public Const MB_ICONQUESTION As Short = 32
    Public Const IDYES As Short = 6

    Function addLotsOfRecords(ByRef d As Integer) As Short
        Dim index As Integer
        Dim i As Short

        index = d4index(d, "INFO2") 'get the secondary index file
        If index <> 0 Then
            rc = i4close(index)
        End If

        rc = d4top(d)
        For i = 1 To 20
            rc = d4appendStart(d, 0)
            rc = d4append(d) 'make 20 copies of the top record
        Next

        'open the index file and update it
        index = i4open(d, "INFO2")
        addLotsOfRecords = i4reindex(index)
    End Function

    Sub ExCode(ByRef frm As Form1)
        Dim db, cb, index As Integer

        cb = code4init()

        'Specify full path if stand-alone
        If u4switch() And &H80S Then fPath = VB6.GetPath & "\"

        db = d4open(cb, fPath & "INFO")
        index = i4open(db, "INFO2")

        rc = addLotsOfRecords(db)
        rc = code4initUndo(cb)
    End Sub
End Module