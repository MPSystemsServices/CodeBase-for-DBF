Option Strict Off
Option Explicit On
Module EXAMPLE 
    'EX3.VB
	
	Public lf As String 'Line Feed
	Public fPath As String 'Full path name to data files
	
	Public Const MB_OK As Short = 0
	Public Const MB_YESNO As Short = 4
	Public Const MB_ICONQUESTION As Short = 32
	Public Const IDYES As Short = 6
	
   Sub ExCode(ByRef frm As Form1)
      'ex3 example code
      Dim cb, temp As Integer
      Dim rc As Short
      Dim fieldInfo() As FIELD4INFO

      ReDim fieldInfo(1)

      fieldInfo(0).fName = "NAME"
      fieldInfo(0).ftype = r4str
      fieldInfo(0).flength = 10
      fieldInfo(0).fdecimals = 0

      cb = code4init()

      'Specify full path if stand-alone
      If u4switch() And &H80S Then fPath = VB6.GetPath & "\"

      rc = code4errCreate(cb, 0)
      temp = d4createData(cb, fPath & "NEWFILE", fieldInfo)
      If code4errorCode(cb, r4check) = r4noCreate Then
         'File exists. Try in the temp directory.
         temp = d4createData(cb, "C:\TEMP\NEWFILE", fieldInfo) 'TEMP must exist
      End If

      If code4errorCode(cb, 0) < 0 Then Call code4exit(cb)

      'Some other code
      rc = code4initUndo(cb)
   End Sub
End Module