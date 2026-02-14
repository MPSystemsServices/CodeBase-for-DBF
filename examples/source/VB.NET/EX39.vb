Option Strict Off
Option Explicit On
Module EXAMPLE 
    'EX39.VB

    Public lf As String 'Line Feed
    Public fPath As String 'Full path name to db files

    Public Const MB_OK As Short = 0
    Public Const MB_YESNO As Short = 4
    Public Const MB_ICONQUESTION As Short = 32
    Public Const IDYES As Short = 6

    Sub ExCode(ByRef frm As Form1)
        Dim cb, db As Integer
        Dim rc As Short

        cb = code4init()

        'Specify full path if stand-alone
        If u4switch() And &H80S Then fPath = VB6.GetPath & "\"

        db = d4open(cb, fPath & "INFO")

        rc = code4lockAttempts(cb, 1) 'do not wait when locking
        rc = code4readLock(cb, 1)

        rc = d4go(db, 2)
        If rc = r4locked Then
            MsgBox("Record 3 was locked by another user")
            rc = code4readLock(cb, 0) 'turn off automatic locking
            rc = d4go(db, 2)
            If rc = r4locked Then
                MsgBox("This will never happen because CODE4.readLock is false")
            End If
        End If
        rc = d4close(db)
        rc = code4initUndo(cb)
    End Sub
End Module