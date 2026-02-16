Option Strict Off
Option Explicit On
Module EXAMPLE 
    'EX61.VB

    Public lf As String 'Line Feed
    Public fPath As String 'Full path name to db files

    Public Const MB_OK As Short = 0
    Public Const MB_YESNO As Short = 4
    Public Const MB_ICONQUESTION As Short = 32
    Public Const IDYES As Short = 6

    Sub ExCode(ByRef frm As Form1)
        Dim ageTag, cb, db, defaultTag As Integer
        Dim rc As Short

        cb = code4init()

        'Specify full path if stand-alone
        If u4switch() And &H80S Then fPath = VB6.GetPath & "\"

        db = d4open(cb, fPath & "data1")
        ageTag = d4tag(db, "AGE_TAG")
        defaultTag = d4tagDefault(db)

        Call d4tagSelect(db, ageTag) 'Select the AGE_TAG tag
        rc = d4seekDouble(db, 32) 'seek using the AGE_TAG tag
        If rc = r4success Then frm.ListBox1.Items.Add("Found age 32")

        Call d4tagSelect(db, defaultTag) 'Select the default tag which is the ADDR_TAG tag
        rc = d4seek(db, "1232") 'seek using the ADDR_TAG tag
        If rc = r4success Then frm.ListBox1.Items.Add("Found address starting with 1232")

        Call d4tagSelect(db, 0) 'Select record number ordering
        rc = d4top(db) 'physical top of the data file

        rc = code4initUndo(cb)
    End Sub
End Module