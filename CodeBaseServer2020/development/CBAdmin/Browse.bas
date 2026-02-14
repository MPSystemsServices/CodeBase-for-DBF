Attribute VB_Name = "BrowseForFolder"
Option Explicit

Private Const BIF_BROWSEFORCOMPUTER = &H1000   ' only return computers
Private Const BIF_BROWSEFORPRINTER = &H2000    ' only return printers
Private Const BIF_BROWSEINCLUDEFILES = &H4000  ' display files
Private Const BIF_DONTGOBELOWDOMAIN = &H2
Private Const BIF_EDITBOX = &H10               ' include text box for edit
Private Const BIF_RETURNFSANCESTORS = &H8      ' only return file system ancestors
Private Const BIF_RETURNONLYFSDIRS = &H1       ' only return file system directories
Private Const BIF_STATUSTEXT = &H4             ' include status bar
Private Const BIF_VALIDATE = &H20              ' value in edit must be valid

Private Const MAX_PATH = 260

Private Declare Function SHBrowseForFolder Lib "shell32" (bi As BrowseInfo) As Long
Private Declare Function SHGetPathFromIDList Lib "shell32" (ByVal itemID As Long, ByVal path As String) As Long
Private Declare Function lstrcat Lib "kernel32" Alias "lstrcatA" (ByVal dest As String, ByVal src As String) As Long

Private Type BrowseInfo
    hWndOwner As Long
    Root As Long
    folderNamePtr As Long
    title As Long
    Flags As Long
    callbackPtr As Long
    callbackParam As Long
    iImage As Long
End Type

Public bComputer As Boolean
Public bPrinter As Boolean
Public bFiles As Boolean
Public bNotBelowDomain As Boolean
Public bEdit As Boolean
Public bAncestors As Boolean
Public bDirectories As Boolean
Public validate As Boolean

Public folderName As String
Public successful As Boolean

Public Sub Display(hWnd As Long, windowTitle As String)
    successful = False

    Dim bi As BrowseInfo
    With bi
       .hWndOwner = hWnd
       .title = lstrcat(windowTitle, "")
       .Flags = GetFlags()
    End With

    Dim IDList As Long
    IDList = SHBrowseForFolder(bi)

    If IDList <> 0 Then
       folderName = Space(MAX_PATH)
       Call SHGetPathFromIDList(IDList, folderName)
       folderName = Left(folderName, InStr(folderName, vbNullChar) - 1)
       successful = True
    End If
End Sub

Private Function GetFlags() As Long
    Dim retVal&
    If bComputer Then retVal = retVal + BIF_BROWSEFORCOMPUTER
    If bPrinter Then retVal = retVal + BIF_BROWSEFORPRINTER
    If bFiles Then retVal = retVal + BIF_BROWSEINCLUDEFILES
    If bNotBelowDomain Then retVal = retVal + BIF_DONTGOBELOWDOMAIN
    If bEdit Then retVal = retVal + BIF_EDITBOX
    If bAncestors Then retVal = retVal + BIF_RETURNFSANCESTORS
    If bDirectories Then retVal = retVal + BIF_RETURNONLYFSDIRS
    If validate Then retVal = retVal + BIF_VALIDATE
    GetFlags = retVal
End Function

Public Sub SetDefaults()
    bComputer = False
    bPrinter = False
    bFiles = False
    bNotBelowDomain = True
    bEdit = False
    bAncestors = False
    bDirectories = True
    validate = True
End Sub


