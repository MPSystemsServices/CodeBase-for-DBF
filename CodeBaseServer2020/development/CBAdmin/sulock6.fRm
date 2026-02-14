VERSION 5.00
Object = "{831FDD16-0C5C-11D2-A9FC-0000F8754DA1}#2.0#0"; "MSCOMCTL.OCX"
Begin VB.Form frmLockLCT
   Caption         =   "Locks - Lock Type, Connection, Table"
   ClientHeight    =   4950
   ClientLeft      =   975
   ClientTop       =   2445
   ClientWidth     =   8880
   Icon            =   "sulock6.frx":0000
   LinkTopic       =   "Form7"
   MDIChild        =   -1  'True
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   4950
   ScaleWidth      =   8880
   ShowInTaskbar   =   0   'False
   Begin MSComctlLib.ListView ListView1
      Height          =   1170
      Left            =   630
      TabIndex        =   1
      Top             =   735
      Width           =   2115
      _ExtentX        =   3731
      _ExtentY        =   2064
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
Attribute VB_Name = "frmLockLCT"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub Form_Load()
    HelpContextID = Lock_Reports

    Dim fieldNames As New Collection
    fieldNames.Add "LockType"
    fieldNames.Add "Account_ID"
    fieldNames.Add "TCPAddress"
    fieldNames.Add "PATH"
    fieldNames.Add "Table"

    ReportFormLoad2 ListView1, "LOCK_LCT", fieldNames
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

