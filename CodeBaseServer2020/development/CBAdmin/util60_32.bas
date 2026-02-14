Attribute VB_Name = "UTILITYS"
Option Explicit

'===================================================================================
'
'     UTILITY Global Constants
'
'===================================================================================
Public Const cProgram = 0  'standard utility name array index value


'Utility Application identification
Global Const LOG4BACK = 1
Global Const LOG4DBF = 2
Global Const LOG4FIX = 3
Global Const LOG4RES = 4
Global Const LOG4RECOVER_BACKUP = 5

' Command Button types
Global Const cCmdOk = 0
Global Const cCmdCancel = 1

' Option Button return types
Global Const OnOffOption = 0
Global Const TrueFalseOption = 1
Global Const NumOption = 2

' Custom Message for Status Window
Global Const WM_UTIL_STATUS = &H401  'Decimal val= WM_USER+1
Global Const WM_UTIL_CLOSE = &H402
Global Const WM_LBUTTONDBLCLK = &H203
Global Const EATMESSAGE = 0

Global Const cFilesSelected = " Files Selected"
Global Const cFileSelected = " File Selected"

'DF File Open Dialog
Global Const OFN_PATHMUSTEXIST = &H800&
Global Const OFN_FILEMUSTEXIST = &H1000&
Global Const OFN_NOREADONLYRETURN = &H8000&

Global Const cActionOpen = 1

Global Const cDlgOpenLog = 0
Global Const cDlgOutDir = 1
Global Const cDlgOpenCfg = 2

'Error Handling
Global Const cErrSameFile = 0
Global Const cErrFileExist = 1
Global Const cErrNullName = 2
Global Const cUtilErr = "CodeUtil Error"

'Windows Constants
Global Const OF_READWRITE = 2
Global Const OF_READ = 0
Global Const OF_SHARE_EXCLUSIVE = &H10

'===================================================================================
'
'     UTILITY Global variables
'
'===================================================================================
Global currentUtility As Integer
Global numParams As Integer     'parameters used for the specific utility
Global vDllData() As Long       ' 'C' command line simulated data
Global vnumDataParams As Integer  'total parameter switches set by user
Global vsavePref, vloadPref As Integer
Global vOutDir As String          ' Output Directory for Analysis
' LY 00/11/08 : for frm32status message intercept
Global lpPrevWndProc As Long
Global gHW As Long

'===================================================================================
'        Form: frmSelect - Global Settings
'===================================================================================
Global Const cNoItems = "(none)"
Global Const cTarget = 0         'array index for vDataRestore
Global Const cDest = 1           'array index for vDataRestore
Global Const cTemplate = 2       'array index for vDataRestore

Global vlogFileName As String    'used by frmSelect to open LogFile
Global vDataRestore() As String
Global vDataExclude() As String
Global vRestoreItems As Integer
Global vExcludeItems As Integer

'===================================================================================
'        Form: frmRecover - Global Settings
'===================================================================================
Global preLog As String
Global logName As String
Global path As String
Global SaveChanges As Boolean
Global silent As Boolean


'===================================================================================
'
'     UTILITY Access function prototypes
'
'===================================================================================
Declare Function ReturnCPtr& Lib "u4dll.dll" (ByVal vbstr As String)
Declare Function mainLog4Back Lib "u4dll.dll" (ByVal argc As Long, _
   argv As Long, ByVal hWndStatus As Long) As Long
Declare Function mainLog4Fix Lib "u4dll.dll" (ByVal argc As Long, _
   argv As Long, ByVal hWndStatus As Long) As Long
Declare Function mainLog4Res Lib "u4dll.dll" (ByVal argc As Long, _
   argv As Long, ByVal hWndStatus As Long) As Long
Declare Function mainLog4Dbf Lib "u4dll.dll" (ByVal argc As Long, _
   argv As Long, ByVal hWndStatus As Long) As Long
Declare Function getLogDatabases Lib "u4dll.dll" (ByVal logFileName$, _
   ByVal hWndListBox As Long) As Long

'===================================================================================
'
'     WINDOWS API function prototypes
'
'===================================================================================
Public Declare Function CreateFile Lib "kernel32" Alias "CreateFileA" (ByVal lpFileName As String, _
    ByVal dwDesiredAccess As Long, ByVal dwShareMode As Long, lpSecurityAttributes As SECURITY_ATTRIBUTES, _
    ByVal dwCreationDisposition As Long, ByVal dwFlagsAndAttributes As Long, ByVal hTemplateFile As Long) As Long
Public Type SECURITY_ATTRIBUTES
        nLength As Long
        lpSecurityDescriptor As Long
        bInheritHandle As Long
End Type
Public Declare Function CloseHandle Lib "kernel32" (ByVal hObject As Long) As Long
Public Const OPEN_EXISTING = 3
Public Const INVALID_HANDLE_VALUE = -1


' LY 00/11/08 : for frm32status message intercept
Declare Function CallWindowProc Lib "user32" Alias "CallWindowProcA" _
   (ByVal lpPrevWndFunc As Long, ByVal hWnd As Long, ByVal msg As Long, _
   ByVal wParam As Long, ByVal lParam As Long) As Long
Declare Function SetWindowLong Lib "user32" Alias "SetWindowLongA" _
   (ByVal hWnd As Long, ByVal nIndex As Long, ByVal dwNewLong As Long) As Long

Sub clearFrmSelectArrays()
    vRestoreItems = 0
    vExcludeItems = 0
End Sub

Function ConvertOptionButData(optionButton As Control, numButtons As Integer, returntype As Integer) As String
'This function takes a group of indexed Option Buttons and
'returns the appropriate return string which represents what
'is being returned.
'NOTE:
' ON/OFF Option Button: ON must be indexed as 0
' TRUE/FALSE Option Button: TRUE must be indexed as 0


Dim i As Integer

For i = 0 To (numButtons - 1)
   If (optionButton(i).value = True) Then
      Select Case returntype
         Case OnOffOption
            If (i = 0) Then
               ConvertOptionButData = "ON"
            Else
               ConvertOptionButData = "OFF"
            End If
         Case TrueFalseOption
            If (i = 0) Then
               ConvertOptionButData = "TRUE"
            Else
               ConvertOptionButData = "FALSE"
            End If
         Case NumOption
            ConvertOptionButData = CStr(i)
      End Select
   End If
Next i
MsgBox "Option box not set!"

End Function

Function DisplayForm(frmCaption As String, showType As Integer) As Integer
    Dim i As Integer, formExists As Integer

    formExists = False

    For i = 0 To Forms.count - 1
       If (Trim$(frmCaption) = Trim$(Forms(i).caption)) Then
          formExists = True
          Exit For
       End If
    Next i

    If formExists Then
       Forms(i).Show showType
       DisplayForm = True
    Else
       DisplayForm = False
    End If
End Function

Sub errMsg(eMsg As Integer, xtraInfo As String)
    Dim msg$

    Select Case eMsg
    Case cErrSameFile
        msg = "The Backup and Active log file names are identical." + vbCrLf + "These file names must be unique."
    Case cErrFileExist
        msg = "The specified log file '" + xtraInfo + "' cannot be opened. The file might be in use by another application such as the CodeBase database server, or the file does not exist."
    Case cErrNullName
        msg = "The '" + xtraInfo + "'' entry cannot be blank. Please correct."
    End Select

    MsgBox msg, vbCritical, cUtilErr
End Sub

Function Exist(fName As String) As Boolean
    Dim fHandle As Long
    Dim sec As SECURITY_ATTRIBUTES
    fHandle = CreateFile(fName, 0, 0, sec, OPEN_EXISTING, 0, 0)
    If fHandle = INVALID_HANDLE_VALUE Then
        'errMsg cErrFileExist, fName
        Exist = False
    Else
        Call CloseHandle(fHandle)
        Exist = True
    End If
End Function

Function ExtractPath$(pathName As String, fileName As String)
    'DF Use the fully qualified file name along with the
    'file name only, to extract the file path
    ExtractPath = Left$(pathName, Len(Trim(pathName)) - Len(Trim(fileName)))
End Function

Function getLogFileName() As String
    If (Len(Trim(vlogFileName)) > 0) Then
        getLogFileName = Trim(vlogFileName)
    Else
        MsgBox "Error in log file name"
    End If
End Function

Sub InitFileDialog(dlg As Control, initName As String, fileMustExist As Boolean)
    'Initalizes the common dialog control
    dlg.DefaultExt = "LOG"
    dlg.DialogTitle = "Select Log File"
    dlg.fileName = initName
    dlg.filter = "Log Files (*.log)|*.log"
    If fileMustExist Then
        dlg.Flags = cdlOFNPathMustExist _
                    Or cdlOFNFileMustExist _
                    Or cdlOFNHideReadOnly
    Else
        dlg.Flags = cdlOFNPathMustExist _
                    Or cdlOFNHideReadOnly
    End If
End Sub

Sub SetDeviceIndependentWindow(ThisForm As Form)
    'Centers form and changes size and position of controls depending
    'on resolution and small/large font settings -- Dan F.
    '
    Dim DesignX%              ' Screen.TwipsPerPixelX value on design system
    Dim DesignY%              ' Screen.TwipsPerPixelY value on design system
    Dim XFactor As Single     ' Horizontal resize ratio
    Dim YFactor As Single     ' Vertical resize ratio
    Dim x As Integer          ' For/Next loop variable
    Dim fLeft%, fTop%, fWidth%, fHeight%    'Form size/location vars

    ' Set the Design so you avoid resizing forms on systems that use the
    ' resolution of your design monitor.
    DesignX% = 15
    DesignY% = 15

    ' Calculate the ratio of the current screen size
    ' to the size of the design screen
    XFactor = DesignX% / Screen.TwipsPerPixelX
    YFactor = DesignY% / Screen.TwipsPerPixelY

    'If using Large fonts, ignore x/yfactor, otherwise
    'forms are displayed incorrectly
    If Screen.TwipsPerPixelX = 12 Then
        XFactor = 1
        YFactor = 1
    End If

    fWidth = ThisForm.Width * XFactor
    fHeight = ThisForm.Height * YFactor
    fLeft = (Screen.Width - fWidth) / 2   ' Center form horizontally.
    fTop = (Screen.Height - fHeight) / 2  ' Center form vertically.

    'Adjust the form
    ThisForm.Move fLeft, fTop, fWidth, fHeight

    'Modify each control on the form so that it is proportionally spaced and sized.
    For x = 0 To ThisForm.Controls.count - 1

       ' In our sample project we'll treat the Drive List Box and Simple
       ' Combo Box differently from other controls because
       ' these two types of controls produce dropdown lists.
      If TypeOf ThisForm.Controls(x) Is DriveListBox Then
        ThisForm.Controls(x).Move ThisForm.Controls(x).Left * XFactor, ThisForm.Controls(x).Top, ThisForm.Controls(x).Width * XFactor
      ElseIf TypeOf ThisForm.Controls(x) Is ComboBox Then ' If Not a Simple
                                                          ' Combo box
        If ThisForm.Controls(x).Style <> 1 Then
          ThisForm.Controls(x).Move ThisForm.Controls(x).Left * XFactor, ThisForm.Controls(x).Top * YFactor, ThisForm.Controls(x).Width * XFactor
        End If
      Else     ' Move and size all other controls

        ' Some controls cannot be processed as they do not contain
        ' some of the properties used in this section -- Dan F.
        If TypeOf ThisForm.Controls(x) Is Menu Then
        ElseIf TypeOf ThisForm.Controls(x) Is Timer Then
        ElseIf ThisForm.Controls(x).Tag = "MsgBlaster" Then
        ElseIf ThisForm.Controls(x).Tag = "CommonDialog" Then
        Else
    ' LY 00/11/10 : VB error - .Left cannot be read at run-time
    '        ThisForm.Controls(X).Move ThisForm.Controls(X).Left * XFactor, ThisForm.Controls(X).Top * YFactor, ThisForm.Controls(X).Width * XFactor, ThisForm.Controls(X).Height * YFactor
        End If
               ' Adjust the font size of text box and label controls
        If TypeOf ThisForm.Controls(x) Is TextBox Then
          ThisForm.Controls(x).FontSize = ThisForm.Controls(x).FontSize * XFactor
        ElseIf TypeOf ThisForm.Controls(x) Is Label Then
          ThisForm.Controls(x).FontSize = ThisForm.Controls(x).FontSize * XFactor
        End If
      End If
    Next x
End Sub

Function setLogFileName(logFileName As String) As Integer
    'This function sets the global var (vLogFileName)
    'This variable is accessed by frmSelect

    If (Len(Trim(logFileName)) > 0) Then
        vlogFileName = Trim(logFileName)
        setLogFileName = True
    Else
        vlogFileName = ""
        setLogFileName = False
    End If
End Function

Public Sub Hook()
    lpPrevWndProc = SetWindowLong(gHW, GWL_WNDPROC, AddressOf WindowProc)
End Sub

Public Sub Unhook()
   Dim temp As Long
   temp = SetWindowLong(gHW, GWL_WNDPROC, lpPrevWndProc)
End Sub

Function WindowProc(ByVal hw As Long, ByVal uMsg As Long, _
        ByVal wParam As Long, ByVal lParam As Long) As Long
    Select Case uMsg
        Case WM_UTIL_STATUS
            If (wParam > 98) Then
                wParam = 100
            End If
            frm32Status.Label1.caption = str$(CInt(wParam)) + "%"
            frm32Status.ProgressBar1.value = wParam
            frm32Status.Refresh
        Case WM_UTIL_CLOSE
            'MsgBox "Utility Completed"
    End Select
    WindowProc = CallWindowProc(lpPrevWndProc, hw, uMsg, wParam, lParam)
'    DoEvents
End Function
