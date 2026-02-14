Option Strict Off
Option Explicit On
Module EXAMPLE
   'EX14.VB
	
	Public cb, db As Integer
	Public rc As Short
	
	Public lf As String 'Line Feed
	Public fPath As String 'Full path name to data files
	
	Public Const MB_OK As Short = 0
	Public Const MB_YESNO As Short = 4
	Public Const MB_ICONQUESTION As Short = 32
	Public Const IDYES As Short = 6
	
   Function createFiles() As Short
      Dim fields() As FIELD4INFO
      Dim tags() As TAG4INFO

      ReDim fields(3)
      fields(0).fName = "NAME"
      fields(0).ftype = r4str
      fields(0).flength = 20
      fields(0).fdecimals = 0

      fields(1).fName = "AGE"
      fields(1).ftype = r4num
      fields(1).flength = 3
      fields(1).fdecimals = 0

      fields(2).fName = "BIRTH_DATE"
      fields(2).ftype = r4date
      fields(2).flength = 8
      fields(2).fdecimals = 0

      ReDim tags(3)
      tags(0).name = "INF_AGE"
      tags(0).expression = "AGE"

      tags(1).name = "INF_BRTH"
      tags(1).expression = "BIRTH_DATE"

      tags(2).name = "INF_NAME"
      tags(2).expression = "NAME"

      rc = code4safety(cb, 0) 'Turn off safety -- overwrite files

      'Specify full path if stand-alone
      If u4switch() And &H80S Then fPath = VB6.GetPath & "\"

      db = d4create(cb, fPath & "INFO.DBF", fields, tags)

      createFiles = code4errorCode(cb, r4check)
   End Function

   Sub ExCode(ByRef frm As Form1)
      cb = code4init()

      rc = createFiles()
      rc = code4initUndo(cb)
   End Sub
End Module