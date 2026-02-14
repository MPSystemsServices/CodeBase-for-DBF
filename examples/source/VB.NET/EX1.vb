Option Strict Off
Option Explicit On
Module EXAMPLE 
    'EX1.VB
	
	Public lf As String 'Line Feed
	Public fpath As String 'Full path name to data files
	
	Public Const MB_OK As Short = 0
	Public Const MB_YESNO As Short = 4
	Public Const MB_ICONQUESTION As Short = 32
	Public Const IDYES As Short = 6
	
   Sub ExCode(ByRef frm As Form1)
      'ex1 example code
      Dim cb, newDataFile As Integer
      Dim fieldInfo() As FIELD4INFO
      Dim rc As Short

      cb = code4init()
      ReDim fieldInfo(3)
      fieldInfo(0).fName = "NAME"
      fieldInfo(0).ftype = r4str
      fieldInfo(0).flength = 20
      fieldInfo(0).fdecimals = 0

      fieldInfo(1).fName = "AGE"
      fieldInfo(1).ftype = r4num
      fieldInfo(1).flength = 3
      fieldInfo(1).fdecimals = 0

      fieldInfo(2).fName = "BIRTH_DATE"
      fieldInfo(2).ftype = r4date
      fieldInfo(2).flength = 8
      fieldInfo(2).fdecimals = 0

      'Specify full path if stand-alone
      If u4switch() And &H80S Then fpath = VB6.GetPath & "\"

      rc = code4accessMode(cb, OPEN4DENY_RW) 'Prevents other applications from
      'having read and write access to any
      'files opened subsequently
      rc = code4safety(cb, 0) 'Ensure the create overwrites any
      'existing file
      newDataFile = d4createData(cb, fpath & "NEWDBF", fieldInfo)

      rc = d4close(newDataFile)

      rc = code4accessMode(cb, OPEN4DENY_NONE) 'Open file in shared mode
      newDataFile = d4open(cb, fpath & "NEWDBF")

      '...some other code...

      rc = code4close(cb)
      rc = code4initUndo(cb)
   End Sub
End Module