VERSION 5.00
Object = "{831FDD16-0C5C-11D2-A9FC-0000F8754DA1}#2.0#0"; "MSCOMCTL.OCX"
Begin VB.Form frmLockCTL
   Caption         =   "Locks - Connection, Table, Lock Type"
   ClientHeight    =   5370
   ClientLeft      =   1050
   ClientTop       =   2475
   ClientWidth     =   8880
   Icon            =   "sulock4.frx":0000
   LinkTopic       =   "Form7"
   MDIChild        =   -1  'True
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   5370
   ScaleWidth      =   8880
   ShowInTaskbar   =   0   'False
   Begin MSComctlLib.ListView ListView1
      Height          =   1275
      Left            =   525
      TabIndex        =   1
      Top             =   840
      Width           =   1905
      _ExtentX        =   3360
      _ExtentY        =   2249
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
Attribute VB_Name = "frmLockCTL"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub Form_Load()
    HelpContextID = Lock_Reports

    Dim fieldNames As New Collection
    fieldNames.Add "Account_ID"
    fieldNames.Add "TCPAddress"
    fieldNames.Add "PATH"
    fieldNames.Add "Table"
    fieldNames.Add "LockType"

    ReportFormLoad2 ListView1, "LOCK_CTL", fieldNames
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

