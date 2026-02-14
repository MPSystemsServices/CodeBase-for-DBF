VERSION 5.00
Object = "{831FDD16-0C5C-11D2-A9FC-0000F8754DA1}#2.0#0"; "MSCOMCTL.OCX"
Begin VB.Form frm32Status
   BackColor       =   &H80000004&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Utility Status"
   ClientHeight    =   1185
   ClientLeft      =   30
   ClientTop       =   315
   ClientWidth     =   3915
   ControlBox      =   0   'False
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   1185
   ScaleWidth      =   3915
   StartUpPosition =   3  'Windows Default
   Begin MSComctlLib.ProgressBar ProgressBar1
      Height          =   315
      Left            =   210
      TabIndex        =   2
      Top             =   420
      Width           =   3495
      _ExtentX        =   6165
      _ExtentY        =   556
      _Version        =   393216
      Appearance      =   1
      Scrolling       =   1
   End
   Begin VB.Timer Timer1
      Enabled         =   0   'False
      Interval        =   100
      Left            =   420
      Top             =   600
   End
   Begin VB.Shape ShapeGrow
      BackColor       =   &H00FF0000&
      BorderStyle     =   0  'Transparent
      FillColor       =   &H000000C0&
      Height          =   495
      Left            =   240
      Top             =   720
      Width           =   15
   End
   Begin VB.Label lblInfo
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H80000004&
      Caption         =   "Processing..."
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   1320
      TabIndex        =   1
      Top             =   840
      Width           =   975
   End
   Begin VB.Label Label1
      Alignment       =   2  'Center
      BackStyle       =   0  'Transparent
      Caption         =   "0%"
      Height          =   255
      Left            =   210
      TabIndex        =   0
      Top             =   180
      Width           =   3495
   End
End
Attribute VB_Name = "frm32Status"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
'------------------------------------------------------------
'Status Form Constants
'------------------------------------------------------------
Const HIDEFRM = 1


'------------------------------------------------------------
'Status Form Variables
'------------------------------------------------------------
Dim shapeSize As Double
'initial form settings
Dim iShapeSize, ifrmStatus_ShapeGrow_Width As Integer
Dim ifrmStatus_Label1_Caption As String * 2

Private Sub runUtility(runUtil As Integer)
    DoEvents
    Select Case runUtil
        Case LOG4BACK
            Call mainLog4Back(vnumDataParams, vDllData(0), hWnd)
        Case LOG4FIX
            Call mainLog4Fix(vnumDataParams, vDllData(0), hWnd)
        Case LOG4RES
            Call mainLog4Res(vnumDataParams, vDllData(0), hWnd)
            'Call testFunction(vnumDataParams, vDllData(0))
        Case LOG4DBF
            Call mainLog4Dbf(vnumDataParams, vDllData(0), hWnd)
            'Call testFunction(vnumDataParams, vDllData(0))
        Case LOG4RECOVER_BACKUP
            Call log4recoverBackup(Trim(preLog), Trim(logName), path, SaveChanges, silent, hWnd)
    End Select

    Dim i%
    For i = 0 To vnumDataParams - 1
        Call v4Cstringfree(vDllData(i))
    Next i

    Unload Me
End Sub

Private Sub SaveInitialSettings()
    iShapeSize = shapeSize
    ifrmStatus_ShapeGrow_Width = frm32Status.ShapeGrow.Width
    ifrmStatus_Label1_Caption = "0%"
End Sub

Private Sub SetInitialSettings()
    shapeSize = iShapeSize
    frm32Status.ShapeGrow.Width = ifrmStatus_ShapeGrow_Width
    frm32Status.Label1.caption = ifrmStatus_Label1_Caption
End Sub

Private Sub Form_Load()
    shapeSize = 15

    gHW = Me.hWnd
    Hook
    Call SaveInitialSettings
    Call SetDeviceIndependentWindow(frm32Status)
    Timer1.Enabled = True
End Sub

Private Sub Form_Unload(Cancel As Integer)
    Call SetInitialSettings
    Unhook
End Sub

Private Sub Timer1_Timer()
    Timer1.Enabled = False
    Call runUtility(currentUtility)
End Sub
