VERSION 5.00
Object = "{831FDD16-0C5C-11D2-A9FC-0000F8754DA1}#2.0#0"; "MSCOMCTL.OCX"
Begin VB.Form Form21
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Disconnect Specific Connections"
   ClientHeight    =   4590
   ClientLeft      =   1755
   ClientTop       =   2220
   ClientWidth     =   5670
   Icon            =   "susever1.frx":0000
   LinkTopic       =   "Form21"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   4590
   ScaleWidth      =   5670
   ShowInTaskbar   =   0   'False
   Begin MSComctlLib.ListView ListView1
      Height          =   2430
      Left            =   525
      TabIndex        =   0
      Top             =   945
      Width           =   3165
      _ExtentX        =   5583
      _ExtentY        =   4286
      MultiSelect     =   -1  'True
      LabelWrap       =   -1  'True
      HideSelection   =   -1  'True
      _Version        =   393217
      ForeColor       =   -2147483640
      BackColor       =   -2147483643
      BorderStyle     =   1
      Appearance      =   1
      NumItems        =   0
   End
   Begin VB.CommandButton btnClose
      Cancel          =   -1  'True
      Caption         =   "&Close"
      Height          =   390
      Left            =   4320
      TabIndex        =   3
      Top             =   1650
      Width           =   1140
   End
   Begin VB.CommandButton btnDisconnect
      Caption         =   "&Disconnect"
      Default         =   -1  'True
      Height          =   390
      Left            =   4305
      TabIndex        =   1
      Top             =   375
      Width           =   1140
   End
   Begin VB.CommandButton btnSelectAll
      Caption         =   "Select &All"
      Height          =   390
      Left            =   4320
      TabIndex        =   2
      Top             =   855
      Width           =   1140
   End
   Begin VB.Label Label1
      AutoSize        =   -1  'True
      Caption         =   "Select client applications to disconnect:"
      Height          =   195
      Left            =   0
      TabIndex        =   4
      Top             =   60
      Width           =   2805
   End
End
Attribute VB_Name = "Form21"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub btnDisconnect_Click()
    Dim client As ListItem, count%
    For Each client In ListView1.ListItems
        If client.Selected Then
            count = count + 1
        End If
    Next client

    If count = 0 Then
        MsgBox "No clients have been selected to disconnect.", vbInformation + vbOKOnly, "Disconnect Specific Clients"
    Else
        Dim mbrc As VbMsgBoxResult, rc%
        If count = 1 Then
            If Len(Trim(ListView1.SelectedItem.Text)) > 0 Then
                mbrc = MsgBox("WARNING: Are you sure you want to disconnect " & ListView1.SelectedItem.Text & "?", vbExclamation + vbYesNo, "Disconnect Specific Clients")
            Else
                mbrc = MsgBox("WARNING: Are you sure you want to disconnect this client?", vbExclamation + vbYesNo, "Disconnect Specific Clients")
            End If
        Else
            mbrc = MsgBox("WARNING: Are you sure you want to disconnect these " & count & " clients?", vbExclamation + vbYesNo, "Disconnect Specific Clients")
        End If
        If mbrc = vbYes Then
            For Each client In ListView1.ListItems
                If client.Selected Then
                    Call code4errOff(gCode4, 1)
                    rc = code4connectCut(gCode4, client.Tag)
                    Call code4errOff(gCode4, 0)
                    If rc <> r4success Then
                        Dim errno%
                        errno = code4errorCode(gCode4, 0)
                        Select Case errno
                            Case -1399 To -1300
                                ' connection error; probably lost connection to server
                                Call MsgBox("Connection to host lost.", vbExclamation, App.title)
                                Call frmMain2.mnuDisconnect_Click
                                Unload Me
                                Exit Sub
                            Case e4name  'can't disconnect yourself
                                rc = MsgBox("You cannot disconnect yourself.", vbExclamation, "Disconnect Specific Clients")
                            Case e4authorize
                                Call MsgBox("You do not have permission to disconnect other clients.", vbInformation + vbOKOnly, "Immediate Shutdown")
                            Case Else
                                Call MsgBox("Unable to disconnect clients. CodeBase error " & errno & ".", vbExclamation, App.title)
                        End Select
                    End If  ' if rc
                End If  ' if client.Selected
            Next client
            Unload Me
        End If  ' if vbYes
    End If  ' if count
End Sub
Private Sub btnSelectAll_Click()
    Dim client
    For Each client In ListView1.ListItems
        client.Selected = True
    Next client
End Sub

Private Sub btnClose_Click()
    Unload Me
End Sub

Private Sub Form_Load()
    HelpContextID = Disconnect_menu

    Left = (Screen.Width - Width) / 2   ' Center form horizontally.
    Top = (Screen.Height - Height) / 2  ' Center form vertically.

    Dim fieldNames As New Collection
    fieldNames.Add "Account_ID"
    fieldNames.Add "TCPAddress"

    ReportFormLoad2 ListView1, "CONNECT", fieldNames
End Sub

Private Sub Form_Resize()
    With ListView1
        .Left = Me.ScaleLeft
        .Width = Me.ScaleWidth - (2 * (Me.ScaleWidth - btnClose.Left - btnClose.Width)) - btnClose.Width
        .Top = Me.ScaleTop + ((Label1.Top - Me.ScaleTop) * 2) + Label1.Height
        .Height = Me.ScaleHeight - .Top
    End With
End Sub

