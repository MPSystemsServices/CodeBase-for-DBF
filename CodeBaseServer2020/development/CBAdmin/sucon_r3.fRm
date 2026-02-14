VERSION 5.00
Object = "{831FDD16-0C5C-11D2-A9FC-0000F8754DA1}#2.0#0"; "MSCOMCTL.OCX"
Begin VB.Form Form14
   Caption         =   "Open Tables with Connections"
   ClientHeight    =   5385
   ClientLeft      =   465
   ClientTop       =   1710
   ClientWidth     =   8880
   Icon            =   "sucon_r3.frx":0000
   LinkTopic       =   "Form14"
   MDIChild        =   -1  'True
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   5385
   ScaleWidth      =   8880
   ShowInTaskbar   =   0   'False
   Begin MSComctlLib.ListView ListView1
      Height          =   2115
      Left            =   315
      TabIndex        =   1
      Top             =   420
      Width           =   3165
      _ExtentX        =   5583
      _ExtentY        =   3731
      LabelWrap       =   -1  'True
      HideSelection   =   -1  'True
      _Version        =   393217
      ForeColor       =   -2147483640
      BackColor       =   -2147483643
      BorderStyle     =   1
      Appearance      =   1
      NumItems        =   0
   End
   Begin VB.Label lblNote
      Caption         =   "NOTE: This list does not include tables opened by ODBC clients."
      Height          =   255
      Left            =   0
      TabIndex        =   0
      Top             =   0
      Width           =   4935
   End
End
Attribute VB_Name = "Form14"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub Form_Load()
    HelpContextID = Open_Tables_con_Report

    Dim fieldNames As New Collection
    fieldNames.Add "PATH"
    fieldNames.Add "TABLE"
    fieldNames.Add "ACCOUNT_ID"
    fieldNames.Add "TCPADDRESS"
    fieldNames.Add "FILETYPE"
    fieldNames.Add "RECCOUNT"
    fieldNames.Add "NUMFIELDS"
    fieldNames.Add "REC_WIDTH"
    fieldNames.Add "READ_ONLY"
    fieldNames.Add "ACCESSMODE"

    ReportFormLoad2 ListView1, "TABLE_C", fieldNames
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

