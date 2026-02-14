VERSION 5.00
Begin VB.Form Splash
   BackColor       =   &H00FFFF00&
   BorderStyle     =   0  'None
   Caption         =   "Form1"
   ClientHeight    =   3240
   ClientLeft      =   1860
   ClientTop       =   1815
   ClientWidth     =   5940
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   Picture         =   "splash.frx":0000
   ScaleHeight     =   216
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   396
   ShowInTaskbar   =   0   'False
   StartUpPosition =   2  'CenterScreen
   Begin VB.PictureBox Picture1
      AutoSize        =   -1  'True
      BorderStyle     =   0  'None
      Height          =   375
      Left            =   0
      ScaleHeight     =   375
      ScaleWidth      =   1575
      TabIndex        =   0
      Top             =   0
      Visible         =   0   'False
      Width           =   1575
   End
   Begin VB.Timer Timer1
      Left            =   285
      Top             =   2640
   End
End
Attribute VB_Name = "Splash"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Public timerDone As Boolean

Private Sub Form_Click()
    timerDone = True
End Sub

Private Sub Form_KeyDown(KeyCode As Integer, Shift As Integer)
    timerDone = True
End Sub

Private Sub Form_Load()
    timerDone = False
    Timer1.interval = 2000

    On Error GoTo flbottom
    Dim fs As Variant
    Set fs = CreateObject("Scripting.FileSystemObject")
    If fs.FileExists(App.path & "\" & "splash.bmp") Then
        Picture1.Picture = LoadPicture(App.path & "\" & "splash.bmp")
        Height = Picture1.ScaleY(Picture1.Height, vbPixels, vbTwips)
        Width = Picture1.ScaleX(Picture1.Width, vbPixels, vbTwips)
        Picture1.Visible = True
    End If

flbottom:
    On Error GoTo 0
End Sub

Private Sub Form_Unload(Cancel As Integer)
    Timer1.Enabled = False
    timerDone = True
End Sub

Private Sub Picture1_Click()
    timerDone = True
End Sub

Private Sub Picture1_KeyDown(KeyCode As Integer, Shift As Integer)
    timerDone = True
End Sub

Private Sub Timer1_Timer()
    Timer1.Enabled = False
    timerDone = True
End Sub


