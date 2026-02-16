Option Strict Off
Option Explicit On
Module EXAMPLE 
    'EX2.VB
	
	Public lf As String 'Line Feed
	Public fPath As String 'Full path name to data files
	
	Public Const MB_OK As Short = 0
	Public Const MB_YESNO As Short = 4
	Public Const MB_ICONQUESTION As Short = 32
	Public Const IDYES As Short = 6
	
   Sub ExCode(ByRef frm As Form1)
      'ex2 example code

      Dim db, cb, info, ind As Integer
      Dim rc As Short

      cb = code4init()

      ' Do not automatically open production index file
      rc = code4autoOpen(cb, 0)

      'Specify full path if stand-alone
      If u4switch() And &H80S Then fPath = VB6.GetPath & "\"

      info = d4open(cb, fPath & "INFO")
      ind = i4open(info, fPath & "INFO")

      If ind = 0 Then
         MsgBox("Production index file is not opened", MsgBoxStyle.OKOnly)
      End If

      ' DATA1.DBF has a production index.  Open it
      rc = code4autoOpen(cb, 1)
      db = d4open(cb, fPath & "DATA1")

      ' Some other code

      rc = code4close(cb)
      rc = code4initUndo(cb)
   End Sub
End Module