Option Strict Off
Option Explicit On
Module EXAMPLE 
    'EX98.VB

    Dim rc As Short

    Public lf As String 'Line Feed
    Public fPath As String 'Full path name to db files

    Public Const MB_OK As Short = 0
    Public Const MB_YESNO As Short = 4
    Public Const MB_ICONQUESTION As Short = 32
    Public Const IDYES As Short = 6

    Sub ExCode(ByRef frm As Form1)
        'CODE4 pointer
        Dim cb As Integer
        'DATA4 pointers
        Dim enroll, master As Integer
        'TAG4 pointers
        Dim enrollTag, codeTag As Integer
        'RELATE4 pointers
        Dim MasterRelation, relation1 As Integer
        'FIELD4 pointers
        Dim classTitle, classCode, enrollStudentId As Integer

        cb = code4init()

        'Specify full path if stand-alone
        If u4switch() And &H80S Then fPath = VB6.GetPath & "\"

        enroll = d4open(cb, fPath & "ENROLL")
        master = d4open(cb, fPath & "CLASSES")

        enrollTag = d4tag(enroll, "ENR_CODE")
        codeTag = d4tag(master, "CODE_TAG")

        MasterRelation = relate4init(master)
        relation1 = relate4createSlave(MasterRelation, enroll, "CODE", enrollTag)

        rc = relate4type(relation1, relate4scan)

        classCode = d4field(master, "CODE")
        classTitle = d4field(master, "TITLE")
        enrollStudentId = d4field(enroll, "STU_ID_TAG")

        Call error4exitTest(cb)

        rc = seekMaster(master, relation1, codeTag, "MATH521")
        frm.ListBox1.Items.Add(f4str(enrollStudentId))
        frm.ListBox1.Items.Add(f4str(classCode))
        frm.ListBox1.Items.Add(f4str(classTitle))

        rc = relate4free(MasterRelation, 1)
        rc = code4initUndo(cb)
    End Sub

    Function seekMaster(ByRef master As Integer, ByRef r As Integer, ByRef masterTag As Integer, ByRef seekKey As String) As Short
        Call d4tagSelect(master, masterTag)
        rc = d4seek(master, seekKey) 'seek for requested value
        If rc = r4success Then
            rc = relate4doOne(r) 'position the slave data file to the
            'appropriate record, according to its master
        End If
        seekMaster = rc
    End Function
End Module