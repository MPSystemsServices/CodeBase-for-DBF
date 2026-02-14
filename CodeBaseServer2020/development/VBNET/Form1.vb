Public Class Form1

    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click
        Dim cb As Long = code4init()
        MsgBox("@" & code4indexExtension(cb) & "@")
    End Sub
End Class
