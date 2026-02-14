VERSION 5.00
Object = "{831FDD16-0C5C-11D2-A9FC-0000F8754DA1}#2.0#0"; "MSCOMCTL.OCX"
Begin VB.Form Form6
   Caption         =   "Connections"
   ClientHeight    =   3510
   ClientLeft      =   2115
   ClientTop       =   1935
   ClientWidth     =   2970
   Icon            =   "suconect.frx":0000
   LinkTopic       =   "Form6"
   MDIChild        =   -1  'True
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   3510
   ScaleWidth      =   2970
   ShowInTaskbar   =   0   'False
   Begin MSComctlLib.ListView ListView1
      Height          =   3270
      Left            =   105
      TabIndex        =   0
      Top             =   105
      Width           =   2745
      _ExtentX        =   4842
      _ExtentY        =   5768
      View            =   3
      LabelEdit       =   1
      LabelWrap       =   -1  'True
      HideSelection   =   0   'False
      _Version        =   393217
      ForeColor       =   -2147483640
      BackColor       =   -2147483643
      BorderStyle     =   1
      Appearance      =   1
      NumItems        =   0
   End
End
Attribute VB_Name = "Form6"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub Form_Load()
    HelpContextID = Connections_Report

    Dim fieldNames As New Collection
    fieldNames.Add "Account_ID"
    fieldNames.Add "TCPAddress"

    ReportFormLoad2 ListView1, "CONNECT", fieldNames
End Sub

Private Sub Form_Resize()
    With ListView1
        .Left = Me.ScaleLeft
        .Width = Me.ScaleWidth
        .Top = Me.ScaleTop
        .Height = Me.ScaleHeight
    End With
End Sub
