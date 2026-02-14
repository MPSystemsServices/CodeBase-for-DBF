VERSION 5.00
Begin VB.Form frm32Main
   Caption         =   "CodeUtil Utilities"
   ClientHeight    =   6072
   ClientLeft      =   132
   ClientTop       =   708
   ClientWidth     =   8772
   Icon            =   "frm32Main.frx":0000
   LinkTopic       =   "Form1"
   ScaleHeight     =   6072
   ScaleWidth      =   8772
   StartUpPosition =   3  'Windows Default
   Begin VB.Menu mnuFile
      Caption         =   "&File"
      Begin VB.Menu mnuDbf
         Caption         =   "&Analysis"
      End
      Begin VB.Menu mnuBack
         Caption         =   "&Backup"
      End
      Begin VB.Menu mnuCat
         Caption         =   "Log4&Cat"
         Enabled         =   0   'False
         Visible         =   0   'False
      End
      Begin VB.Menu mnuFix
         Caption         =   "&Recover"
      End
      Begin VB.Menu mnuRes
         Caption         =   "Re&store"
      End
      Begin VB.Menu mnuSec
         Caption         =   "Log4&Sec"
         Enabled         =   0   'False
         Visible         =   0   'False
      End
      Begin VB.Menu mnuSep
         Caption         =   "-"
      End
      Begin VB.Menu mnuExit
         Caption         =   "E&xit"
      End
   End
   Begin VB.Menu mnuOptions
      Caption         =   "Options"
      Begin VB.Menu mnuLoad
         Caption         =   "&Load Settings"
         Checked         =   -1  'True
      End
      Begin VB.Menu mnuSave
         Caption         =   "&Save Settings"
         Checked         =   -1  'True
      End
   End
   Begin VB.Menu mnuHelp
      Caption         =   "Help"
      Begin VB.Menu mnuContents
         Caption         =   "&Contents"
      End
      Begin VB.Menu mnuAbout
         Caption         =   "About"
      End
   End
End
Attribute VB_Name = "frm32Main"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub Form_Load()
vloadPref = frm32Main.mnuLoad.Checked
vsavePref = frm32Main.mnuSave.Checked
Call SetDeviceIndependentWindow(frm32Main)

End Sub

Private Sub mnuAbout_Click()
frm32About.Show 1

End Sub

Private Sub mnuBack_Click()
frm32Back.Show 1

End Sub

Private Sub mnuContents_Click()
SendKeys "{F1}"

End Sub

Private Sub mnuDbf_Click()
frm32Dbf.Show 1

End Sub

Private Sub mnuExit_Click()
Unload frm32Main

End Sub

Private Sub mnuFix_Click()
frm32Fix.Show 1

End Sub

Private Sub mnuLoad_Click()
frm32Main.mnuLoad.Checked = Not frm32Main.mnuLoad.Checked
vloadPref = frm32Main.mnuLoad.Checked

End Sub

Private Sub mnuRes_Click()
frm32Res.Show 1

End Sub

Private Sub mnuSave_Click()
frm32Main.mnuSave.Checked = Not frm32Main.mnuSave.Checked
vsavePref = frm32Main.mnuSave.Checked

End Sub
