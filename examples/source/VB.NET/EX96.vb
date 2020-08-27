Option Strict Off
Option Explicit On
Module EXAMPLE
	'EX96.VB
	
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
        Dim master, enroll, student As Integer
        'TAG4 pointers
        Dim enrollTag, studentTag As Integer
        'RELATE4 pointers
        Dim relation1, MasterRelation, relation2 As Integer
        'FIELD4 pointers
        Dim classTitle, classCode, enrollStudentId As Integer
        Dim studentName As Integer

        Dim rc, count As Short

        cb = code4init()

        'Specify full path if stand-alone
        If u4switch() And &H80S Then fPath = VB6.GetPath & "\"

        enroll = d4open(cb, fPath & "ENROLL")
        master = d4open(cb, fPath & "CLASSES")
        student = d4open(cb, fPath & "STUDENT")

        enrollTag = d4tag(enroll, "ENR_CODE")
        studentTag = d4tag(student, "STU_ID")

        MasterRelation = relate4init(master)
        relation1 = relate4createSlave(MasterRelation, enroll, "CODE", enrollTag)
        relation2 = relate4createSlave(relation1, student, "STU_ID_TAG", studentTag)

        rc = relate4type(relation1, relate4scan)
        rc = relate4sortSet(MasterRelation, "STUDENT->L_NAME,8,0+ENROLL->CODE")

        classCode = d4field(master, "CODE")
        classTitle = d4field(master, "TITLE")
        enrollStudentId = d4field(enroll, "STU_ID_TAG")
        studentName = d4field(student, "L_NAME")

        error4exitTest((cb))

        rc = relate4top(MasterRelation)
        Do While rc = r4success 'onle one f4str per statement
            frm.ListBox1.Items.Add(f4str(studentName))
            frm.ListBox1.Items.Add(f4str(enrollStudentId))
            frm.ListBox1.Items.Add(f4str(classCode))
            frm.ListBox1.Items.Add(f4str(classTitle))
            frm.ListBox1.Items.Add("")
            rc = relate4skip(MasterRelation, 1)
            count = count + 1
            If count Mod 4 = 0 Then
                MsgBox("Click for more...")
                frm.ListBox1.Items.Clear()
            End If
        Loop

        frm.ListBox1.Items.Add("the number of records is ")
        frm.ListBox1.Items.Add(Str(d4recCount(master)))

        rc = relate4free(MasterRelation, 1)
        rc = code4initUndo(cb)
    End Sub
End Module