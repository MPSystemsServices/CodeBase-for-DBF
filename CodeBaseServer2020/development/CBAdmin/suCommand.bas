Attribute VB_Name = "cmdProcessor"
Option Explicit

' Cam Stobbe  April 4, 2001
' This module processes a command file.
' The command file is a text file in the following format:
'    command = command_name
'    parameter_name = parameter_value
'    parameter_name = parameter_value
'    command = command_name
'    parameter_name = parameter_value
'    parameter_name = parameter_value
' A line is a comment if its first non-whitespace character is a
' punctuation mark.

Private parameterName$, parameterValue$
Private commandFileName$
Private commandFile ' As TextStream

Public Function ProcessCommand() As Boolean
    ' process instructions given on the command line
    ' currently only supports commands in a command file
    ' Returns true if application should exit after processing.
    Dim temp$
    temp = Command()
    If Left(temp, 1) = "@" Then
        commandFileName = Trim(Mid(temp, 2))
        Dim fs ' As FileSystemObject
        Set fs = CreateObject("Scripting.FileSystemObject")
        On Error Resume Next
        Set commandFile = fs.OpenTextFile(commandFileName, 1, 0)
        If Err.number <> 0 Then
            MsgBox "Unable to open " & commandFileName & "." & vbCrLf & Err.Description, vbCritical, "Command File"
            Exit Function
        End If
        On Error GoTo 0

        Do While ReadNextLine = True
            If StrComp(parameterName, "COMMAND", vbTextCompare) = 0 Then
                Exit Do
            Else
                ' parameter was not a command
                Call fileError("Command file must start with a command.")
            End If
        Loop

        Dim moreCommands As Boolean
        Do
            If StrComp(parameterName, "COMMAND", vbTextCompare) = 0 Then
                ' All functions called in this loop must return
                ' a Boolean. true means there is another command to process.
                Select Case UCase(parameterValue)
                    Case "CONNECT"
                        moreCommands = cmdConnect()
                    Case "UPDATE BACKUP"
                        moreCommands = cmdUpdateBackup()
                    Case "EXIT"
                        moreCommands = False  ' ignore any remaining commands
                        ProcessCommand = True  ' application should exit
                    Case Else
                        Call fileError("Unrecognized command.")
                        moreCommands = ReadNextLine
                End Select
            Else
                moreCommands = ReadNextLine
            End If
        Loop While moreCommands = True
    End If
End Function

Private Function ReadNextLine() As Boolean
    ' returns true if next line successfully read
    Dim lineStr$, pos&

    Do While Not commandFile.atEndOfStream
        lineStr = Trim(commandFile.readLine)
        If Not LineIsComment(lineStr) Then
            pos = InStr(lineStr, "=")
            If pos <> 0 Then
                parameterName = Trim(Left(lineStr, pos - 1))
                parameterValue = Trim(Mid(lineStr, pos + 1))
                ReadNextLine = True
                Exit Function
            Else
                Call fileError("Invalid format. Use " & vbCrLf & "parameter_name = parameter_value")
            End If
        End If
    Loop
    ReadNextLine = False
End Function

Private Function LineIsComment(line$) As Boolean
    ' A comment is a line that starts with any punctuation
    ' or is blank
    If Len(Trim(line)) > 0 Then
        Select Case Left(line, 1)
            Case "!", """", "#", "$", "%", "&", "'", "(", ")", "*", "+"
            Case ",", "-", ".", "/", "&", ":", ";", "<", "=", ">", "?"
            Case "@", "[", "\", "]", "^", "_", "`", "{", "|", "}", "~"
            Case Else
                LineIsComment = False
                Exit Function
        End Select
    End If
    LineIsComment = True
End Function

Private Sub fileError(Optional extra)
    Dim msg$
    msg = commandFileName & " line " & (commandFile.line - 1)
    If Not IsMissing(extra) Then
        msg = msg & vbCrLf & extra
    End If

    MsgBox msg, vbCritical, "Command File"
End Sub

Private Function cmdConnect() As Boolean
    ' return true if another command is after this one
    Dim host$, Port$, User$, pw$, silent As Boolean
    Do While ReadNextLine
        Select Case UCase(parameterName)
            Case "COMMAND"  ' no more parameters for this command
                cmdConnect = True
                Exit Do
            Case "HOST"
                host = parameterValue
            Case "PORT"
                Port = parameterValue
            Case "USER"
                User = parameterValue
            Case "PASSWORD"
                pw = parameterValue
            Case "SILENT"
                Select Case UCase(parameterValue)
                    Case "Y", "YES", "T", "TRUE"
                        silent = True
                    Case Else
                        silent = False
                End Select
            Case Else
                Call fileError("Invalid parameter for CONNECT.")
        End Select
    Loop

    Screen.MousePointer = vbHourglass
    Call CBAConnect(CStr(host), CStr(Port), CStr(User), CStr(pw), silent)
    Screen.MousePointer = vbDefault
End Function

Private Function cmdUpdateBackup() As Boolean
    Dim error As Boolean
    Dim OnlyIfConnected As Boolean
    Dim blog1, blog2, preLog, path$
    Dim scheduleInterval, scheduleUnits
    Dim silent As Boolean   ' LY Jun 30/04 : allow for silent backup

    Do While ReadNextLine And Not error
        Select Case UCase(parameterName)
            Case "COMMAND"
                cmdUpdateBackup = True
                Exit Do
            Case "ONLY IF CONNECTED"
                Select Case UCase(parameterValue)
                    Case "Y", "YES", "T", "TRUE"
                        OnlyIfConnected = True
                    Case Else
                        OnlyIfConnected = False
                End Select
            Case "BACKLOG1"
                If Len(parameterValue) > 0 Then
                    blog1 = parameterValue
                End If
            Case "BACKLOG2"
                If Len(parameterValue) > 0 Then
                    blog2 = parameterValue
                End If
            Case "PENDINGTRAN"
                If Len(parameterValue) > 0 Then
                    preLog = parameterValue
                End If
            Case "PATH"
                path = parameterValue
            Case "SCHEDULE INTERVAL"
                If bLoggedIn Then
                    scheduleInterval = CLng(parameterValue)
                    If scheduleInterval <= 0 Then
                        Call fileError("Invalid schedule interval.")
                        error = True
                    End If
                End If
            Case "SCHEDULE UNITS"
                If bLoggedIn Then
                    Select Case UCase(parameterValue)
                        Case "S", "SEC", "SECOND", "SECONDS"
                            scheduleUnits = schSeconds
                        Case "M", "MIN", "MINUTE", "MINUTES"
                            scheduleUnits = schMinutes
                        Case "H", "HOUR", "HOURS"
                            scheduleUnits = schHours
                        Case "D", "DAY", "DAYS"
                            scheduleUnits = schDays
                        Case Else
                            Call fileError("Invalid schedule unit.")
                            error = True
                    End Select
                End If
            Case "SILENT"   ' LY Jun 30/04
                Select Case UCase(parameterValue)
                    Case "Y", "YES", "T", "TRUE"
                        silent = True
                    Case Else
                        silent = False
                End Select
            Case Else
                Call fileError("Invalid parameter for UPDATE BACKUP.")
        End Select
    Loop

    If Not error Then
        If OnlyIfConnected And Not bLoggedIn Then
            Exit Function
        End If

        If IsEmpty(preLog) Then
            MsgBox "Insufficient parameters to update backup.", vbCritical, "Command File"
            Exit Function
        End If

        If Not bLoggedIn And (IsEmpty(blog1) Or IsEmpty(blog2)) Then
            MsgBox "Update Backup: When not connected to the server, command file must include the name of both backup log files.", vbCritical, "Command File"
            Exit Function
        End If

        If (IsEmpty(scheduleInterval) Xor IsEmpty(scheduleUnits)) = True Then
            MsgBox "Must provide both schedule parameters.", vbCritical, "Command File"
            Exit Function
        End If

        backlog1 = blog1
        backlog2 = blog2

        If IsEmpty(silent) Then ' LY Jun 30/04
            silent = False
        End If

        If IsEmpty(scheduleInterval) Then
            Call doBackup(CStr(preLog), CStr(path), , silent)
        Else
            Call setSchedule(CInt(scheduleInterval), CInt(scheduleUnits))
        End If
    End If
End Function

