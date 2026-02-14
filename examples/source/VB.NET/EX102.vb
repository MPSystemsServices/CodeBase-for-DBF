Option Strict Off
Option Explicit On
Module Example
   'ex102 
   
   Private reindexDone As Boolean
   Private gFrm As Form1
   Private gMutex As New System.Threading.Mutex()

    Sub ExCode(ByRef frm As Form1)
        Dim cb, db As Integer
        Dim rc, action As Short
        Dim fPath As String

        gFrm = frm
        cb = code4init()

        'Specify full path if stand-alone
        If u4switch() And &H80S Then fPath = VB6.GetPath & "\"

        db = d4open(cb, "c:\examples\customer")
        If db <> 0 Then
            reindexDone = False
            rc = d4reindexWithProgress(db, AddressOf ReindexProgress, 1200)
            If rc = r4success Then
                Do While True
                    gMutex.WaitOne()
                    If reindexDone = True Then
                       gMutex.ReleaseMutex()
                       Exit Do
                    End If
                    gMutex.ReleaseMutex()
                    System.Threading.Thread.Sleep(100)
                    System.Windows.Forms.Application.DoEvents()
                Loop
            MsgBox("Reindex is complete.", MsgBoxStyle.Information, "Reindex")
            End If
        End If

        Call code4close(cb)
        Call code4initUndo(cb)
    End Sub

    Function ReindexProgress(ByVal percent As Double) As Short
        ' Handle progress here by updating a progress bar,
        ' for example.

        gFrm.ListBox1.Items.Add(VB6.Format(percent * 100, ".##"))
        If percent >= 1.0# Then
            gMutex.WaitOne()
            reindexDone = True
            gMutex.ReleaseMutex()
        End If
        ReindexProgress = 0
    End Function
End Module