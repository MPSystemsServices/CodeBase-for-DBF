Option Strict Off
Option Explicit On
Module EXAMPLE 
    'EX69.VB

    Public lf As String 'Line Feed
    Public fPath As String 'Full path name to db files

    Public Const MB_OK As Short = 0
    Public Const MB_YESNO As Short = 4
    Public Const MB_ICONQUESTION As Short = 32
    Public Const IDYES As Short = 6

    Sub ExCode(ByRef frm As Form1)
        Dim birthDate As String

        birthDate = InputBox("Enter your birth date in CCYYMMDD format", "EX69", "19730305")
        frm.ListBox1.Items.Add("You were born on a " & date4cdow(birthDate))
        'displays "You were born on a Monday" if a Monday was entered
    End Sub
End Module