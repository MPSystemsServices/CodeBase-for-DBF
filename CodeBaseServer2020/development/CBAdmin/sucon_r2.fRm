VERSION 5.00
Object = "{831FDD16-0C5C-11D2-A9FC-0000F8754DA1}#2.0#0"; "MSCOMCTL.OCX"
Begin VB.Form Form13
   Caption         =   "Open Tables"
   ClientHeight    =   5550
   ClientLeft      =   690
   ClientTop       =   1800
   ClientWidth     =   8820
   Icon            =   "sucon_r2.frx":0000
   LinkTopic       =   "Form13"
   MDIChild        =   -1  'True
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   5550
   ScaleWidth      =   8820
   ShowInTaskbar   =   0   'False
   Visible         =   0   'False
   Begin MSComctlLib.ListView ListView1
      Height          =   2535
      Left            =   210
      TabIndex        =   1
      Top             =   315
      Width           =   3165
      _ExtentX        =   5583
      _ExtentY        =   4471
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
Attribute VB_Name = "Form13"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub Form_Load()
    HelpContextID = Open_Tables_Report

    Dim fieldNames As New Collection
    fieldNames.Add "PATH"
    fieldNames.Add "TABLE"
    fieldNames.Add "FILETYPE"
    fieldNames.Add "RECCOUNT"
    fieldNames.Add "NUMFIELDS"
    fieldNames.Add "REC_WIDTH"
    fieldNames.Add "READ_ONLY"
    fieldNames.Add "ACCESSMODE"

    ReportFormLoad2 ListView1, "TABLETAG", fieldNames
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

