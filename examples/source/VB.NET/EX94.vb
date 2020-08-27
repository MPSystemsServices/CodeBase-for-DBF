Option Strict Off
Option Explicit On
Module EXAMPLE 
    'EX94.VB

    Public lf As String 'Line Feed
    Public fPath As String 'Full path name to db files

    Public Const MB_OK As Short = 0
    Public Const MB_YESNO As Short = 4
    Public Const MB_ICONQUESTION As Short = 32
    Public Const IDYES As Short = 6

    Sub ExCode(ByRef frm As Form1)
        Dim db, cb, index As Integer
        Dim fieldInfo() As FIELD4INFO
        Dim tagInfo() As TAG4INFO
        Dim rc As Short

        ReDim fieldInfo(2)
        fieldInfo(0).fName = "FIELD_NAME"
        fieldInfo(0).ftype = r4str
        fieldInfo(0).flength = 10
        fieldInfo(0).fdecimals = 0

        fieldInfo(1).fName = "VALUE"
        fieldInfo(1).ftype = r4num
        fieldInfo(1).flength = 7
        fieldInfo(1).fdecimals = 2

        ReDim tagInfo(2)
        tagInfo(0).name = "T_NAME"
        tagInfo(0).expression = "FIELD_NAME"
        tagInfo(0).filter_Renamed = "FIELD_NAME > 'A'"

        tagInfo(1).name = "NAME_TWO"
        tagInfo(1).expression = "VALUE"
        tagInfo(1).unique = e4unique
        tagInfo(1).descending = r4descending

        cb = code4init()
        rc = code4safety(cb, 0)

        'Specify full path if stand-alone
        If u4switch() And &H80S Then fPath = VB6.GetPath & "\"

        db = d4createData(cb, fPath & "DB_NAME", fieldInfo)
        index = i4create(db, fPath & "NAME", tagInfo)

        rc = d4close(db)
        rc = code4initUndo(cb)
    End Sub
End Module