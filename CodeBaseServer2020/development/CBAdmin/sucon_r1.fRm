VERSION 5.00
Object = "{831FDD16-0C5C-11D2-A9FC-0000F8754DA1}#2.0#0"; "MSCOMCTL.OCX"
Begin VB.Form Form12
   Caption         =   "Connections with Tables"
   ClientHeight    =   5205
   ClientLeft      =   1515
   ClientTop       =   2130
   ClientWidth     =   8880
   Icon            =   "sucon_r1.frx":0000
   LinkTopic       =   "Form12"
   MDIChild        =   -1  'True
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   5205
   ScaleWidth      =   8880
   Visible         =   0   'False
   Begin MSComctlLib.ListView ListView1
      Height          =   2325
      Left            =   105
      TabIndex        =   1
      Top             =   315
      Width           =   3690
      _ExtentX        =   6509
      _ExtentY        =   4101
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
   Begin VB.Label lblNote
      AutoSize        =   -1  'True
      Caption         =   "NOTE: This list does not include tables opened by ODBC clients."
      Height          =   195
      Left            =   0
      TabIndex        =   0
      Top             =   0
      Width           =   4575
   End
End
Attribute VB_Name = "Form12"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub Form_Load()
    HelpContextID = Connections_Tables_Report

    Dim fieldNames As New Collection
    fieldNames.Add "ACCOUNT_ID"
    fieldNames.Add "TCPADDRESS"
    fieldNames.Add "PATH"
    fieldNames.Add "TABLE"
    fieldNames.Add "FILETYPE"
    fieldNames.Add "RECCOUNT"
    fieldNames.Add "NUMFIELDS"
    fieldNames.Add "REC_WIDTH"
    fieldNames.Add "READ_ONLY"
    fieldNames.Add "ACCESSMODE"

    ReportFormLoad2 ListView1, "CONNECTT", fieldNames
End Sub

Private Sub Form_Resize()
    With ListView1
        If Me.ScaleHeight > 0 Then
            .Left = Me.ScaleLeft
            .Width = Me.ScaleWidth
            .Top = Me.ScaleTop + ((lblNote.Top - Me.ScaleTop) * 2) + lblNote.Height
            .Height = Me.ScaleHeight - .Top
        End If
    End With
End Sub
