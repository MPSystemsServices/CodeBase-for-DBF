Option Strict Off
Option Explicit On
Module EXAMPLE
    'EX0.VB 
	
	Public lf As String 'Line Feed
	Public fPath As String 'Full path name to data files
	
	Public Const MB_OK As Short = 0
	Public Const MB_YESNO As Short = 4
	Public Const MB_ICONQUESTION As Short = 32
	Public Const IDYES As Short = 6
	
   Sub ExCode(ByRef frm As Form1)
      'ex0 example code
      Dim cb, db As Integer
      Dim rc As Short

      cb = code4init()
      rc = code4autoOpen(cb, 0) 'Don't open production index
      rc = code4errDefaultUnique(cb, r4unique) 'How to handle duplicate keys
      frm.ListBox1.Items.Add("made setting changes")

      'Specify full path if stand-alone
      If u4switch() And &H80S Then fPath = VB6.GetPath & "\"

      db = d4open(cb, fPath & "data1") 'Open a data file

      ' this is equivalent to calling error4exitTest( )

      If code4errorCode(cb, 0) < 0 Then
         Call code4exit(cb)
      End If


      ' more code  ...
      rc = code4initUndo(cb)
   End Sub
End Module