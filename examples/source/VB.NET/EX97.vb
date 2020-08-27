Option Strict Off
Option Explicit On
Module EXAMPLE
	'EX97.VB
	
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
        Dim office, employee, building As Integer
        'TAG4 pointers
        Dim officeNo, buildNo As Integer
        'RELATE4 pointers
        Dim toOffice, master, toBuilding As Integer

        Dim rc As Short

        cb = code4init()

        'Specify full path if stand-alone
        If u4switch() And &H80S Then fPath = VB6.GetPath & "\"

        employee = d4open(cb, fPath & "EMPLOYEE")
        office = d4open(cb, fPath & "OFFICE")
        building = d4open(cb, fPath & "BUILDING")

        'set up the tags
        officeNo = d4tag(office, "OFF_NUM")
        buildNo = d4tag(building, "BUILD_NO")

        'Create the relations
        master = relate4init(employee)
        toOffice = relate4createSlave(master, office, "EMPLOYEE->OFFICE_NO", officeNo)
        toBuilding = relate4createSlave(toOffice, building, "OFFICE->BUILD_NO", buildNo)
        'Go to employee, at record 2
        rc = d4go(employee, 2)

        'Lock the data files and their index files.
        rc = relate4lockAdd(master)
        rc = code4lock(cb)

        'This call causes the corresponding records in data files "OFFICE" and
        '"BUILDING" to be looked up.
        rc = relate4doAll(master)
        frm.ListBox1.Items.Add(Str(d4recNo(employee)))
        frm.ListBox1.Items.Add(" employee rec no")
        frm.ListBox1.Items.Add(Str(d4recNo(office)))
        frm.ListBox1.Items.Add(" office rec no")
        frm.ListBox1.Items.Add(Str(d4recNo(building)))
        frm.ListBox1.Items.Add(" building rec no")

        'Go to office, at record 3
        rc = d4go(office, 3)

        'This call causes the building record to be looked up from the office
        rc = relate4doOne(toBuilding)
        frm.ListBox1.Items.Add(Str(d4recNo(employee)))
        frm.ListBox1.Items.Add(" employee rec no")
        frm.ListBox1.Items.Add(Str(d4recNo(office)))
        frm.ListBox1.Items.Add(" office rec no")
        frm.ListBox1.Items.Add(Str(d4recNo(building)))
        frm.ListBox1.Items.Add(" building rec no")

        '  ..  and so on

        rc = relate4free(master, 1)
        rc = code4initUndo(cb)
    End Sub
End Module