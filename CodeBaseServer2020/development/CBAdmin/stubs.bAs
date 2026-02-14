Attribute VB_Name = "Stubs"
Option Explicit

Public Function xcode4serverConfigName(code As Long) As String
    xcode4serverConfigName = "S4SERVER"
End Function
Public Function xcode4tables(code As Long, path As String) As String
    xcode4tables = "y:\users\cam_s\TABLES5"
End Function
Public Function xcode4infoRetrieve(code As Long, ByRef memAlloc As Long, ByRef nClients As Integer, ByRef elapsedSeconds As Long, ByRef nOpenFiles As Integer) As Integer
    memAlloc = 2000
    nClients = 10
    elapsedSeconds = 3000002
    nOpenFiles = 10
    xcode4infoRetrieve = 0
End Function
Public Function xcode4connectionStatus(code As Long) As Long
    xcode4connectionStatus = d4open(code, "TEST2")
End Function
Public Function xcode4shutdown(code As Long) As Integer
    xcode4shutdown = 0
End Function
Public Function xcode4connectAcceptNew(code As Long, acceptConn As Boolean) As Integer
    xcode4connectAcceptNew = 0
End Function
Public Function xcode4connectCutAll(code As Long) As Integer
    xcode4connectCutAll = 0
End Function
Public Function xcode4connectCut%(code As Long, connectionId As Integer)
    xcode4connectCut = 0
End Function
Public Function xcode4closeFiles(code As Long) As Integer
    xcode4closeFiles = 0
End Function
