Option Strict Off
Option Explicit On
Module EXAMPLE 
    'EX54.VB

    Public lf As String 'Line Feed
    Public fPath As String 'Full path name to db files

    Public Const MB_OK As Short = 0
    Public Const MB_YESNO As Short = 4
    Public Const MB_ICONQUESTION As Short = 32
    Public Const IDYES As Short = 6

    Sub ExCode(ByRef frm As Form1)
        Dim fromFile, cb, toFile As Integer
        Dim fromField, toField As Integer
        Dim toStr As String
        Dim rc, i As Short
        Dim iRecs As Integer

        'Copy records from one database to another.

        cb = code4init()

        'Specify full path if stand-alone
        If u4switch() And &H80S Then fPath = VB6.GetPath & "\"

        'Data file TO_DBF and FROM_DBF have the same file structure
        fromFile = d4open(cb, fPath & "FROM_DBF")
        toFile = d4open(cb, fPath & "TO_DBF")
        rc = code4optStart(cb)

        If d4recWidth(fromFile) <> d4recWidth(toFile) Then
            rc = error4describe(cb, e4result, 0, "Structures not identical", "", "")
            code4exit((cb))
        End If

        error4exitTest((cb))
        For iRecs = 1 To d4recCount(fromFile)
            rc = d4go(fromFile, iRecs) 'read the data file record
            rc = d4appendStart(toFile, 0) 'copy the data file buffer
            For i = 1 To d4numFields(fromFile)
                fromField = d4fieldJ(fromFile, i)
                'UPGRADE_WARNING: Couldn't resolve default property of object f4nCpy(). Click for more: 'ms-help://MS.VSCC/commoner/redir/redirect.htm?keyword="vbup1037"'
                rc = f4nCpy(fromField, toStr, f4len(fromField))
                toField = d4fieldJ(toFile, i)
                Call f4assign(toField, toStr)
            Next
            rc = d4append(toFile)
        Next
        rc = code4close(cb)
        rc = code4initUndo(cb)
    End Sub
End Module