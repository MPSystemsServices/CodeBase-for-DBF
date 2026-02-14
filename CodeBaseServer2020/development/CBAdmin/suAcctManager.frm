VERSION 5.00
Object = "{831FDD16-0C5C-11D2-A9FC-0000F8754DA1}#2.0#0"; "MSCOMCTL.OCX"
Begin VB.Form frmAcctManager
   Caption         =   "Account Manager"
   ClientHeight    =   6000
   ClientLeft      =   165
   ClientTop       =   450
   ClientWidth     =   7095
   LinkTopic       =   "Form1"
   MinButton       =   0   'False
   ScaleHeight     =   6000
   ScaleWidth      =   7095
   StartUpPosition =   2  'CenterScreen
   Begin VB.PictureBox cButtons
      BorderStyle     =   0  'None
      Height          =   350
      Left            =   2730
      ScaleHeight     =   345
      ScaleWidth      =   4320
      TabIndex        =   34
      Top             =   5520
      Width           =   4320
      Begin VB.CommandButton cmdCancel
         Cancel          =   -1  'True
         Caption         =   "Cancel"
         Height          =   350
         Left            =   3060
         TabIndex        =   36
         Top             =   0
         Width           =   1130
      End
      Begin VB.CommandButton cmdOK
         Caption         =   "OK"
         Default         =   -1  'True
         Height          =   350
         Left            =   1800
         TabIndex        =   35
         Top             =   0
         Width           =   1130
      End
   End
   Begin MSComctlLib.ImageList ImageList1
      Left            =   0
      Top             =   2520
      _ExtentX        =   1005
      _ExtentY        =   1005
      BackColor       =   -2147483643
      ImageWidth      =   16
      ImageHeight     =   16
      MaskColor       =   12632256
      _Version        =   393216
      BeginProperty Images {2C247F25-8591-11D1-B16A-00C0F0283628}
         NumListImages   =   2
         BeginProperty ListImage1 {2C247F27-8591-11D1-B16A-00C0F0283628}
            Picture         =   "suAcctManager.frx":0000
            Key             =   "File"
         EndProperty
         BeginProperty ListImage2 {2C247F27-8591-11D1-B16A-00C0F0283628}
            Picture         =   "suAcctManager.frx":0C54
            Key             =   "Head"
         EndProperty
      EndProperty
   End
   Begin MSComctlLib.TreeView TreeView1
      Height          =   4845
      Left            =   0
      TabIndex        =   0
      Top             =   240
      Width           =   2565
      _ExtentX        =   4524
      _ExtentY        =   8546
      _Version        =   393217
      HideSelection   =   0   'False
      Indentation     =   141
      LineStyle       =   1
      Sorted          =   -1  'True
      Style           =   7
      ImageList       =   "ImageList1"
      Appearance      =   1
      OLEDragMode     =   1
      OLEDropMode     =   1
   End
   Begin VB.PictureBox cAccount
      BorderStyle     =   0  'None
      Height          =   5340
      Left            =   2730
      ScaleHeight     =   5340
      ScaleWidth      =   4320
      TabIndex        =   26
      Top             =   105
      Width           =   4320
      Begin VB.Frame fraAdmin
         Caption         =   "Administrator Privileges"
         Height          =   750
         Left            =   0
         TabIndex        =   30
         Top             =   2625
         Width           =   4215
         Begin VB.CheckBox chkCBAdmin
            Caption         =   "Run CBA&dmin"
            Height          =   255
            Left            =   2310
            TabIndex        =   39
            ToolTipText     =   "Run CodeBase Administrator"
            Top             =   315
            Width           =   1695
         End
         Begin VB.CheckBox chkDisconnect
            Caption         =   "Disco&nnect"
            Height          =   225
            Left            =   210
            TabIndex        =   10
            Top             =   315
            Width           =   1380
         End
      End
      Begin VB.Frame fraNet
         Caption         =   "Networking Parameters"
         Height          =   1800
         Left            =   0
         TabIndex        =   29
         Top             =   3465
         Width           =   4215
         Begin VB.CommandButton cmdPassword
            Caption         =   "Change &Password"
            Height          =   435
            Left            =   630
            TabIndex        =   15
            Top             =   1155
            Width           =   1590
         End
         Begin VB.TextBox txtTCPBegin
            Height          =   300
            Left            =   1680
            MaxLength       =   15
            TabIndex        =   12
            Text            =   "Text1"
            Top             =   315
            Width           =   2430
         End
         Begin VB.TextBox txtTCPEnd
            Height          =   300
            Left            =   1680
            MaxLength       =   15
            TabIndex        =   14
            Text            =   "Text2"
            Top             =   735
            Width           =   2430
         End
         Begin VB.Label lblTCP1
            AutoSize        =   -1  'True
            Caption         =   "TCP &Begin"
            Height          =   195
            Left            =   315
            TabIndex        =   11
            Top             =   355
            Width           =   765
         End
         Begin VB.Label lblTCP2
            AutoSize        =   -1  'True
            Caption         =   "TCP &End"
            Height          =   195
            Left            =   315
            TabIndex        =   13
            Top             =   775
            Width           =   645
         End
      End
      Begin VB.Frame fraATable
         Caption         =   "Table Privileges"
         Height          =   1380
         Left            =   0
         TabIndex        =   28
         Top             =   1155
         Width           =   4215
         Begin VB.CheckBox chkCreate
            Caption         =   "&Create Permanent"
            Height          =   225
            Left            =   2310
            TabIndex        =   7
            Top             =   315
            Width           =   1695
         End
         Begin VB.CheckBox chkCreate_Del
            Caption         =   "Remove/&Overwrite"
            Height          =   225
            Left            =   2310
            TabIndex        =   9
            Top             =   945
            Width           =   1695
         End
         Begin VB.CheckBox chkCreateTemp
            Caption         =   "Create &Temporary"
            Height          =   225
            Left            =   2310
            TabIndex        =   8
            Top             =   630
            Width           =   1695
         End
         Begin VB.CheckBox chkAIndex
            Caption         =   "Create/Modify &Indexes"
            Height          =   225
            Left            =   210
            TabIndex        =   5
            Top             =   315
            Width           =   1905
         End
         Begin VB.CheckBox chkACompress
            Caption         =   "Co&mpress/Reindex"
            Height          =   225
            Left            =   210
            TabIndex        =   6
            Top             =   630
            Width           =   1695
         End
      End
      Begin VB.Frame fraARecord
         Caption         =   "Record Privileges"
         Height          =   1065
         Left            =   0
         TabIndex        =   27
         Top             =   0
         Width           =   4215
         Begin VB.CheckBox chkAUpdate
            Caption         =   "&Update"
            Height          =   225
            Left            =   2310
            TabIndex        =   3
            Top             =   315
            Width           =   1065
         End
         Begin VB.CheckBox chkARead
            Caption         =   "&Read"
            Height          =   225
            Left            =   210
            TabIndex        =   1
            Top             =   315
            Width           =   960
         End
         Begin VB.CheckBox chkAAppend
            Caption         =   "&Append"
            Height          =   225
            Left            =   210
            TabIndex        =   2
            Top             =   630
            Width           =   1065
         End
         Begin VB.CheckBox chkADelete
            Caption         =   "&Delete"
            Height          =   225
            Left            =   2310
            TabIndex        =   4
            Top             =   630
            Width           =   1065
         End
      End
   End
   Begin VB.PictureBox cFile
      BorderStyle     =   0  'None
      Height          =   5340
      Left            =   2730
      ScaleHeight     =   5340
      ScaleWidth      =   4320
      TabIndex        =   31
      Top             =   105
      Width           =   4320
      Begin VB.TextBox txtTable
         Height          =   300
         Left            =   840
         MaxLength       =   36
         TabIndex        =   25
         Text            =   "Text2"
         Top             =   2835
         Width           =   3270
      End
      Begin VB.TextBox txtPath
         Height          =   300
         Left            =   840
         MaxLength       =   250
         TabIndex        =   23
         Text            =   "Text1"
         Top             =   2415
         Width           =   3270
      End
      Begin VB.Frame Frame6
         Caption         =   "Record Privileges"
         Height          =   1065
         Left            =   0
         TabIndex        =   33
         Top             =   0
         Width           =   4215
         Begin VB.CheckBox chkPDelete
            Caption         =   "&Delete"
            Height          =   225
            Left            =   2310
            TabIndex        =   19
            Top             =   630
            Width           =   1065
         End
         Begin VB.CheckBox chkPAppend
            Caption         =   "&Append"
            Height          =   225
            Left            =   210
            TabIndex        =   17
            Top             =   630
            Width           =   1065
         End
         Begin VB.CheckBox chkPRead
            Caption         =   "&Read"
            Height          =   225
            Left            =   210
            TabIndex        =   16
            Top             =   315
            Width           =   960
         End
         Begin VB.CheckBox chkPUpdate
            Caption         =   "&Update"
            Height          =   225
            Left            =   2310
            TabIndex        =   18
            Top             =   315
            Width           =   1065
         End
      End
      Begin VB.Frame Frame5
         Caption         =   "Table Privileges"
         Height          =   1065
         Left            =   0
         TabIndex        =   32
         Top             =   1155
         Width           =   4215
         Begin VB.CheckBox chkPCompress
            Caption         =   "Co&mpress/Reindex"
            Height          =   225
            Left            =   210
            TabIndex        =   21
            Top             =   630
            Width           =   1695
         End
         Begin VB.CheckBox chkPIndex
            Caption         =   "Create/Modify &Indexes"
            Height          =   225
            Left            =   210
            TabIndex        =   20
            Top             =   315
            Width           =   1905
         End
      End
      Begin VB.Label Label1
         Caption         =   "Leave the table field blank to apply privileges to the entire directory."
         Height          =   495
         Left            =   840
         TabIndex        =   37
         Top             =   3160
         Width           =   3255
      End
      Begin VB.Label Label4
         Caption         =   "&Table:"
         Height          =   225
         Left            =   105
         TabIndex        =   24
         Top             =   2875
         Width           =   540
      End
      Begin VB.Label Label3
         Caption         =   "&Path:"
         Height          =   225
         Left            =   105
         TabIndex        =   22
         Top             =   2455
         Width           =   540
      End
   End
   Begin VB.Label lblMenuHint
      Caption         =   "Right-click below for context menu."
      Height          =   255
      Left            =   0
      TabIndex        =   38
      Top             =   0
      Width           =   2535
   End
   Begin VB.Menu mnuAccount
      Caption         =   "Account"
      Visible         =   0   'False
      Begin VB.Menu mnuNewAccount
         Caption         =   "&New Account..."
      End
      Begin VB.Menu mnuRename
         Caption         =   "&Rename Account..."
      End
      Begin VB.Menu mnuDelete
         Caption         =   "&Delete Account..."
      End
      Begin VB.Menu mnuSep1
         Caption         =   "-"
      End
      Begin VB.Menu mnuAddFile
         Caption         =   "&Add File/Directory"
      End
      Begin VB.Menu mnuDelPriv
         Caption         =   "Delete &File/Directory..."
      End
   End
End
Attribute VB_Name = "frmAcctManager"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

' account fields
Private accountdb&, privdb&
Private faAccountid&, faPassword&, faTCPBegin&, faTCPEnd&
Private faCreate&, faCreateTemp&, faCreateDel&
Private faRead&, faAppend&, faDelete&, faUpdate&
Private faIndex&, faCompress&, faDisconnect&, faAdmin&

' priv fields
Private fpAccountid&
Private fpRead&, fpAppend&, fpDelete&, fpUpdate&
Private fpIndex&, fpCompress&
Private fpPath&, fpTable&

Private AcctChanged As Boolean
Private PrivChanged As Boolean

Private bFullPath As Boolean

Private Sub chkFull_Click()
    Call SaveChanges
    Dim recno&
    recno = d4recNo(privdb)

    Dim n As Node
    For Each n In TreeView1.Nodes
        If Right(n.Key, 1) = "p" Then
            Call d4go(privdb, val(n.Key))
            n.Text = MakeLabel(f4str(fpPath), f4str(fpTable))
        End If
    Next n

    If recno <= d4recCount(privdb) Then
        Call d4go(privdb, recno)
    End If
End Sub

Private Sub chkCBAdmin_Click()
    AcctChanged = True
End Sub

Private Sub chkDisconnect_Click()
    AcctChanged = True
End Sub

Private Sub chkPAppend_Click()
    With chkPAppend
        If .value = vbUnchecked And f4true(faAppend) Then
            If .Tag = str(vbGrayed) Then
                .value = vbChecked
            Else
                .value = vbGrayed
            End If
        End If
        .Tag = str(.value)
    End With
    PrivChanged = True
End Sub

Private Sub chkPCompress_Click()
    With chkPCompress
        If .value = vbUnchecked And f4true(faCompress) Then
            If .Tag = str(vbGrayed) Then
                .value = vbChecked
            Else
                .value = vbGrayed
            End If
        End If
        .Tag = str(.value)
    End With
    PrivChanged = True
End Sub

Private Sub chkPDelete_Click()
    With chkPDelete
        If .value = vbUnchecked And f4true(faDelete) Then
            If .Tag = str(vbGrayed) Then
                .value = vbChecked
            Else
                .value = vbGrayed
            End If
        End If
        .Tag = str(.value)
    End With
    PrivChanged = True
End Sub

Private Sub chkPIndex_Click()
    With chkPIndex
        If .value = vbUnchecked And f4true(faIndex) Then
            If .Tag = str(vbGrayed) Then
                .value = vbChecked
            Else
                .value = vbGrayed
            End If
        End If
        .Tag = str(.value)
    End With
    PrivChanged = True
End Sub

Private Sub chkPRead_Click()
    With chkPRead
        If .value = vbUnchecked And f4true(faRead) Then
            If .Tag = str(vbGrayed) Then
                .value = vbChecked
            Else
                .value = vbGrayed
            End If
        End If
        .Tag = str(.value)
    End With
    PrivChanged = True
End Sub

Private Sub NewPassword()
    frmPassword.Show 1, Me
    If frmPassword.newPasswordOK Then
        Call f4assign(faPassword, frmPassword.txtPW1.Text)
        AcctChanged = True
    End If
    Unload frmPassword
End Sub

Private Sub chkPUpdate_Click()
    With chkPUpdate
        If .value = vbUnchecked And f4true(faUpdate) Then
            If .Tag = str(vbGrayed) Then
                .value = vbChecked
            Else
                .value = vbGrayed
            End If
        End If
        .Tag = str(.value)
    End With
    PrivChanged = True
End Sub

Private Sub cmdCancel_Click()
    Unload Me
End Sub

Private Sub cmdOK_Click()
    Call code4flush(gCode4)
    Call SaveChanges
    Call code4tranCommit(gCode4)
    Call code4unlock(gCode4)
    Unload Me
End Sub

Private Sub cmdPassword_Click()
    Call NewPassword
End Sub

Private Sub chkAAppend_Click()
    AcctChanged = True
End Sub

Private Sub chkACompress_Click()
    AcctChanged = True
End Sub

Private Sub chkADelete_Click()
    AcctChanged = True
End Sub

Private Sub chkAIndex_Click()
    AcctChanged = True
End Sub

Private Sub chkARead_Click()
    AcctChanged = True
End Sub

Private Sub chkAUpdate_Click()
    AcctChanged = True
End Sub

Private Sub chkCreate_Click()
    If chkCreate.value = vbChecked Then
        chkCreateTemp.value = vbChecked
    End If
    AcctChanged = True
End Sub

Private Sub chkCreate_Del_Click()
    AcctChanged = True
End Sub

Private Sub chkCreateTemp_Click()
    If chkCreateTemp.value = vbUnchecked Then
        If chkCreate.value = vbChecked Then
            chkCreateTemp.value = vbChecked
            MsgBox "If the Create privilege is granted, the Create Temporary privilege is automatically granted as well.", vbInformation, "Create Temporary"
        End If
    End If
    AcctChanged = True
End Sub

Private Sub AddAccount()
    Dim rc%
    With frmNewAccount.cmbCopyFrom
        .Clear
        .AddItem "(None)"
        .itemdata(.NewIndex) = -77
        rc = d4top(accountdb)
        If rc = r4success Then
            .Enabled = True
            Do
                .AddItem Trim(f4str(faAccountid))
                .itemdata(.NewIndex) = d4recNo(accountdb)
                rc = d4skip(accountdb, 1)
            Loop While rc = r4success
        Else
            .Enabled = False
        End If
        .ListIndex = 0
    End With

    frmNewAccount.Show 1, Me
    If frmNewAccount.success Then
        Call SaveChanges
        With frmNewAccount.cmbCopyFrom
            If .itemdata(.ListIndex) <> -77 Then
                Call d4go(accountdb, .itemdata(.ListIndex))
            End If
        End With
        Call d4appendStart(accountdb, 0)
        With frmNewAccount.cmbCopyFrom
            If .itemdata(.ListIndex) = -77 Then
                Call d4blank(accountdb)
            End If
        End With
        Call f4assign(faAccountid, frmNewAccount.txtAcctName.Text)
        rc = d4append(accountdb)
        If rc <> r4success Then
            If rc = r4unique Then
                MsgBox "The account " & frmNewAccount.txtAcctName.Text & _
                    " already exists.", vbCritical, "New Account"
            Else
                Dim errno%
                errno = code4errorCode(gCode4, 0)
                If errno <> r4success Then
                        ' could not open the file list
                    Select Case errno
                        Case -1399 To -1300
                            ' connection error; probably lost connection to server
                            Call MsgBox("Connection to host lost.", vbExclamation, App.title)
                            Call frmMain2.mnuDisconnect_Click
                            Call CloseWindow(Me.hWnd)
                        Case Else
                            Call MsgBox("CodeBase error" & errno & ".", vbExclamation, App.title)
                    End Select
                End If
            End If
        Else
            Dim keepRecno&
            Dim newAcctNode As Node
            keepRecno = d4recNo(accountdb)
            Set newAcctNode = TreeView1.Nodes.Add(, tvwLast, str(d4recNo(accountdb)) & "a", Trim(frmNewAccount.txtAcctName.Text), "Head")
            newAcctNode.Sorted = True
            TreeView1.SelectedItem = TreeView1.Nodes(str(keepRecno) & "a")
            Call TreeView1_NodeClick(TreeView1.SelectedItem)

            If frmNewAccount.chkPassword.value = vbChecked Then
                NewPassword
            End If

            ' copy files from other account
            With frmNewAccount.cmbCopyFrom
                If .itemdata(.ListIndex) <> -77 Then
                    If frmNewAccount.chkFiles.value = vbChecked Then
                        Dim acctForSeek$
                        acctForSeek = UCase(.List(.ListIndex)) + Space(f4len(faAccountid) - Len(.List(.ListIndex)))
                        rc = d4seek(privdb, acctForSeek)
                        Do While rc = r4success
                            keepRecno = d4recNo(privdb)
                            Call d4appendStart(privdb, 1)
                            Call f4assign(fpAccountid, frmNewAccount.txtAcctName.Text)
                            Call d4append(privdb)
                            TreeView1.Nodes.Add str(d4recNo(accountdb)) & "a", tvwChild, str(d4recNo(privdb)) & "p", MakeLabel(f4str(fpPath), f4str(fpTable)), "File"
                            Call d4go(privdb, keepRecno)
                            rc = d4seekNext(privdb, acctForSeek)
                        Loop
                        Call d4flush(privdb)
                    End If
                End If
            End With
            Call d4flush(accountdb)
        End If
    End If

    Unload frmNewAccount
End Sub

Private Sub cmdNewUser_Click()
    Call AddAccount
End Sub

Private Sub Form_Load()
    PrivChanged = False
    AcctChanged = False

    bFullPath = GetSetting(App.title, "AccountManager", "FullPath", False)

    Call code4errOff(gCode4, 1)  ' no error messages
    Call code4tranStart(gCode4)
    Call code4errDefaultUnique(gCode4, r4unique)

    If Len(SystemPath) = 0 Then
        accountdb = d4open(gCode4, "ACCOUNT4")
    Else
        accountdb = d4open(gCode4, SystemPath & SepChar & "ACCOUNT4")
    End If
    If accountdb <> r4success Then
        faAccountid = d4field(accountdb, "ACCOUNTID")
        faPassword = d4field(accountdb, "PASSWORD")
        faTCPBegin = d4field(accountdb, "TCP_BEGIN")
        faTCPEnd = d4field(accountdb, "TCP_END")
        faCreate = d4field(accountdb, "CREATE")
        faCreateTemp = d4field(accountdb, "CREATE_TMP")
        faCreateDel = d4field(accountdb, "CREATE_DEL")
        faRead = d4field(accountdb, "READ")
        faAppend = d4field(accountdb, "APPEND")
        faDelete = d4field(accountdb, "DELETE")
        faUpdate = d4field(accountdb, "UPDATE")
        faIndex = d4field(accountdb, "INDEX")
        faCompress = d4field(accountdb, "COMPRESS")
        faDisconnect = d4field(accountdb, "DISCONNECT")

        Call code4errFieldName(gCode4, 0)
        faAdmin = d4field(accountdb, "ADMIN")
        Call code4errFieldName(gCode4, 1)
        If faAdmin = 0 Then
            chkCBAdmin.Visible = False
        End If
    End If

    If Len(SystemPath) = 0 Then
        privdb = d4open(gCode4, "PRIV4.dbf")
    Else
        privdb = d4open(gCode4, SystemPath & SepChar & "PRIV4.dbf")
    End If
    If privdb <> 0 Then
        Call d4tagSelect(privdb, d4tag(privdb, "ACCOUNT"))
        fpAccountid = d4field(privdb, "ACCOUNTID")
        fpRead = d4field(privdb, "READ")
        fpAppend = d4field(privdb, "APPEND")
        fpUpdate = d4field(privdb, "UPDATE")
        fpDelete = d4field(privdb, "DELETE")
        fpIndex = d4field(privdb, "INDEX")
        fpCompress = d4field(privdb, "COMPRESS")
        fpPath = d4field(privdb, "PATH")
        fpTable = d4field(privdb, "TABLE")
    End If

    Dim errno%
    errno = code4errorCode(gCode4, 0)
    If errno <> r4success Then
            ' could not open the file list
        Select Case errno
            Case -1399 To -1300
                ' connection error; probably lost connection to server
                Call MsgBox("Connection to host lost.", vbExclamation, App.title)
                Call frmMain2.mnuDisconnect_Click
            Case Else
                Call MsgBox("Unable to access the file list. CodeBase error " & errno & ".", vbExclamation, App.title)
        End Select
        Call Me.Hide
        Call CloseWindow(Me.hWnd)
        Exit Sub
    End If

    #If de_bug <> 1 Then
        ' store the address of the existing Message Handler
        OldWindowProc = GetWindowLong(Me.hWnd, GWL_WNDPROC)
        ' Tell windows to forward all messages to out own Message Handler
        Call SetWindowLong(Me.hWnd, GWL_WNDPROC, AddressOf AcctManagerResize)
    #End If

    Call loadTree
End Sub

Private Sub loadTree()
    Dim rel&, s1&
    Call d4tagSelect(accountdb, d4tag(accountdb, "NAME"))
    rel = relate4init(accountdb)
    s1 = relate4createSlave(rel, privdb, "UPPER(ACCOUNTID)", d4tag(privdb, "ACCOUNT"))
    Call relate4type(s1, relate4scan)

    TreeView1.Nodes.Clear

    Dim rc%, prevrecno%, prevAcct$, thisAcct$
    Dim newAcctNode As Node
    prevrecno = -99
    rc = relate4top(rel)
    Do While rc = r4success
        If prevrecno <> d4recNo(accountdb) Then
            ' new master record
            thisAcct = Trim(f4str(faAccountid))
            Set newAcctNode = TreeView1.Nodes.Add(, tvwLast, str(d4recNo(accountdb)) & "a", thisAcct, "Head")
            newAcctNode.Sorted = True
            prevrecno = d4recNo(accountdb)
            prevAcct = thisAcct
        End If
        If d4recNo(privdb) > 0 And d4recNo(privdb) <= d4recCount(privdb) Then
            ' slave not blank
            TreeView1.Nodes.Add str(d4recNo(accountdb)) & "a", tvwChild, str(d4recNo(privdb)) & "p", Trim(f4str(fpTable)), "File"
        End If

        rc = relate4skip(rel, 1)
    Loop

    Call relate4free(rel, 0)

    If TreeView1.Nodes.count = 0 Then
        ShowAccount
    Else
        TreeView1.SelectedItem = TreeView1.Nodes(1)
        Call TreeView1_NodeClick(TreeView1.SelectedItem)
    End If
End Sub

Private Sub AddFile()
    Call SaveChanges

    Call d4appendStart(privdb, 0)
    Call d4blank(privdb)
    Call f4assign(fpAccountid, f4str(faAccountid))

'    TreeView1.Nodes(str(d4recNo(accountdb)) & "a").Text = Trim(f4str(faAccountid))
    TreeView1.Nodes.Add str(d4recNo(accountdb)) & "a", tvwChild, str(d4recNo(privdb)) & "p", "New File", "File"
    TreeView1.SelectedItem = TreeView1.Nodes(str(d4recNo(privdb)) & "p")
    Call ShowPriv
    TreeView1.SelectedItem.Text = "New File"

    PrivChanged = True

    Dim errno%
    errno = code4errorCode(gCode4, 0)
    If errno <> r4success Then
        Select Case errno
            Case -1399 To -1300
                ' connection error; probably lost connection to server
                Call MsgBox("Connection to host lost.", vbExclamation, App.title)
                Call frmMain2.mnuDisconnect_Click
                Call CloseWindow(Me.hWnd)
        End Select
    End If
End Sub

Private Sub Form_Resize()
    With TreeView1
        .Left = ScaleLeft
        .Top = ScaleTop + lblMenuHint.Height
        .Height = ScaleHeight - .Top
        If ScaleWidth - cAccount.Width - 330 > 0 Then
            .Width = ScaleWidth - cAccount.Width - 330
        End If
    End With

    cAccount.Left = TreeView1.Width + 165
    cFile.Left = cAccount.Left
    cButtons.Left = cAccount.Left
    cButtons.Top = ScaleHeight - 480
End Sub

Private Sub Form_Unload(Cancel As Integer)
    If gCode4 <> 0 Then
        If code4tranStatus(gCode4) = r4active Then
            Call code4tranRollback(gCode4)
        End If
        Call d4close(accountdb)
        Call d4close(privdb)
    End If

    #If de_bug <> 1 Then
        'Return control of messages back to windows
        Call SetWindowLong(Me.hWnd, GWL_WNDPROC, OldWindowProc)
    #End If
End Sub

Private Sub mnuAddFile_Click()
    Call AddFile
End Sub

Private Sub mnuDelete_Click()
    Call DelAccount
End Sub

Private Sub mnuDelPriv_Click()
    Call DelPriv
End Sub

Private Sub mnuNewAccount_Click()
    Call AddAccount
End Sub

Private Sub mnuRename_Click()
    TreeView1.StartLabelEdit
End Sub

Private Sub TreeView1_AfterLabelEdit(Cancel As Integer, NewString As String)
    Dim rc%, oldAcctId$
    oldAcctId = f4str(faAccountid)
    Call f4assign(faAccountid, NewString)
    rc = d4flush(accountdb)
    If rc <> r4success Then
        If rc = r4unique Then
            MsgBox "The account " & NewString & _
                " already exists.", vbCritical, "New Account"
        Else
            MsgBox "CodeBase error " & code4errorCode(gCode4, r4check)
        End If
        Cancel = 88
    Else
        TreeView1.Sorted = False
        TreeView1.Sorted = True

        ' update all slaves too
        rc = d4seek(privdb, UCase(oldAcctId))
        Do While rc = r4success
            Call f4assign(fpAccountid, NewString)
            rc = d4seekNext(privdb, UCase(oldAcctId))
        Loop
        Call d4flush(privdb)
    End If
End Sub

Private Sub TreeView1_BeforeLabelEdit(Cancel As Integer)
    If Right(TreeView1.SelectedItem.Key, 1) = "p" Then
        Cancel = 348
    End If
End Sub

Private Sub TreeView1_KeyUp(KeyCode As Integer, Shift As Integer)
    If KeyCode = 46 Then
        'Delete

        If TreeView1.Nodes.count > 0 Then
            If Right(TreeView1.SelectedItem.Key, 1) = "a" Then
                Call DelAccount
            ElseIf Right(TreeView1.SelectedItem.Key, 1) = "p" Then
                Call DelPriv
            End If
        End If
    End If
End Sub

Private Sub TreeView1_MouseMove(Button As Integer, Shift As Integer, x As Single, y As Single)
    'If (Button And vbLeftButton) And TreeView1.SelectedItem Then
'    If (Button And vbLeftButton) Then
 '       TreeView1.OLEDrag
 '   End If
End Sub

Private Sub TreeView1_MouseUp(Button As Integer, Shift As Integer, x As Single, y As Single)
    If Button = 2 Then
        If TreeView1.Nodes.count = 0 Then
            mnuDelPriv.Visible = False
            mnuSep1.Visible = False
            mnuAddFile.Visible = False
            mnuDelete.Enabled = False
            mnuRename.Enabled = False
        Else
            If Right(TreeView1.SelectedItem.Key, 1) = "a" Then
                mnuDelPriv.Visible = False
            Else
                mnuDelPriv.Visible = True
            End If
            mnuSep1.Visible = True
            mnuAddFile.Visible = True
            mnuDelete.Enabled = True
            mnuRename.Enabled = True
        End If
        PopupMenu mnuAccount
    End If
End Sub

Private Sub ShowAccount()
    If TreeView1.Nodes.count = 0 Then
        chkARead.value = vbUnchecked
        chkARead.Enabled = False
        chkAAppend.value = vbUnchecked
        chkAAppend.Enabled = False
        chkAUpdate.value = vbUnchecked
        chkAUpdate.Enabled = False
        chkADelete.value = vbUnchecked
        chkADelete.Enabled = False
        chkAIndex.value = vbUnchecked
        chkAIndex.Enabled = False
        chkACompress.value = vbUnchecked
        chkACompress.Enabled = False
        chkCreate.value = vbUnchecked
        chkCreate.Enabled = False
        chkCreateTemp.value = vbUnchecked
        chkCreateTemp.Enabled = False
        chkCreate_Del.value = vbUnchecked
        chkCreate_Del.Enabled = False
        chkDisconnect.value = vbUnchecked
        chkDisconnect.Enabled = False
        chkCBAdmin.value = vbUnchecked
        chkCBAdmin.Enabled = False
        txtTCPBegin.Text = ""
        txtTCPBegin.BackColor = vbButtonFace
        txtTCPBegin.Enabled = False
        txtTCPEnd.Text = ""
        txtTCPEnd.BackColor = vbButtonFace
        txtTCPEnd.Enabled = False
        cmdPassword.Enabled = False

        lblTCP1.Enabled = False
        lblTCP2.Enabled = False
        fraARecord.Enabled = False
        fraATable.Enabled = False
        fraAdmin.Enabled = False
        fraNet.Enabled = False
    Else
        chkARead.value = AccountToCheck(f4true(faRead))
        chkARead.Enabled = True
        chkAAppend.value = AccountToCheck(f4true(faAppend))
        chkAAppend.Enabled = True
        chkAUpdate.value = AccountToCheck(f4true(faUpdate))
        chkAUpdate.Enabled = True
        chkADelete.value = AccountToCheck(f4true(faDelete))
        chkADelete.Enabled = True
        chkAIndex.value = AccountToCheck(f4true(faIndex))
        chkAIndex.Enabled = True
        chkACompress.value = AccountToCheck(f4true(faCompress))
        chkACompress.Enabled = True
        chkCreate.value = AccountToCheck(f4true(faCreate))
        chkCreate.Enabled = True
        chkCreateTemp.value = AccountToCheck(f4true(faCreateTemp))
        chkCreateTemp.Enabled = True
        chkCreate_Del.value = AccountToCheck(f4true(faCreateDel))
        chkCreate_Del.Enabled = True
        chkDisconnect.value = AccountToCheck(f4true(faDisconnect))
        chkDisconnect.Enabled = True
        If faAdmin <> 0 Then
            chkCBAdmin.value = AccountToCheck(f4true(faAdmin))
            chkCBAdmin.Enabled = True
        End If
        txtTCPBegin.Text = Trim(f4str(faTCPBegin))
        txtTCPBegin.BackColor = vbWindowBackground
        txtTCPBegin.Enabled = True
        txtTCPEnd.Text = Trim(f4str(faTCPEnd))
        txtTCPEnd.BackColor = vbWindowBackground
        txtTCPEnd.Enabled = True
        cmdPassword.Enabled = True

        lblTCP1.Enabled = True
        lblTCP2.Enabled = True
        fraARecord.Enabled = True
        fraATable.Enabled = True
        fraAdmin.Enabled = True
        fraNet.Enabled = True
    End If

    cAccount.Visible = True
    cFile.Visible = False
End Sub

Private Sub ShowPriv()
    chkPRead.value = PrivToCheck(f4true(fpRead), f4true(faRead))
    chkPAppend.value = PrivToCheck(f4true(fpAppend), f4true(faAppend))
    chkPUpdate.value = PrivToCheck(f4true(fpUpdate), f4true(faUpdate))
    chkPDelete.value = PrivToCheck(f4true(fpDelete), f4true(faDelete))
    chkPIndex.value = PrivToCheck(f4true(fpIndex), f4true(faIndex))
    chkPCompress.value = PrivToCheck(f4true(fpCompress), f4true(faCompress))
    txtPath.Text = Trim(f4str(fpPath))
    txtTable.Text = Trim(f4str(fpTable))

    cFile.Visible = True
    cAccount.Visible = False
End Sub

Private Sub TreeView1_NodeClick(ByVal Node As MSComctlLib.Node)
    Call SaveChanges

    If Right(Node.Key, 1) = "a" Then
        'account
        Call d4go(accountdb, val(Node.Key))
        Call ShowAccount
    End If

    If Right(Node.Key, 1) = "p" Then
        'file privilege
        Call d4go(privdb, val(Node.Key))
        Call d4go(accountdb, val(Node.Parent.Key))
        Call ShowPriv
    End If

    Dim errno%
    errno = code4errorCode(gCode4, 0)
    If errno <> r4success Then
            ' could not open the file list
        Select Case errno
            Case -1399 To -1300
                ' connection error; probably lost connection to server
                Call MsgBox("Connection to host lost.", vbExclamation, App.title)
                Call frmMain2.mnuDisconnect_Click
            Case Else
                Call MsgBox("CodeBase error" & errno & ".", vbExclamation, App.title)
        End Select
        Call CloseWindow(Me.hWnd)
        Exit Sub
    End If

    AcctChanged = False
    PrivChanged = False
End Sub

Private Sub TreeView1_OLEDragDrop(data As MSComctlLib.DataObject, Effect As Long, Button As Integer, Shift As Integer, x As Single, y As Single)
    MsgBox data.GetData(vbCFText), , "Drop"
End Sub

Private Sub TreeView1_OLEStartDrag(data As MSComctlLib.DataObject, AllowedEffects As Long)
'    Data.Clear
'    Data.SetData "abc", vbCFText
'    Data.SetData TreeView1.SelectedItem.Key, vbCFText
'    Data.SetData TreeView1.SelectedItem.Text, vbCFText
End Sub

Private Sub txtPath_Change()
    PrivChanged = True
End Sub

Private Sub txtPath_GotFocus()
    TxtGotFocus Me
End Sub

Private Sub txtTable_Change()
    TreeView1.SelectedItem.Text = MakeLabel(txtPath.Text, txtTable.Text)
    PrivChanged = True
End Sub

Private Sub txtTable_GotFocus()
    TxtGotFocus Me
End Sub

Private Sub txtTCPBegin_Change()
    AcctChanged = True
End Sub

Private Sub txtTCPBegin_GotFocus()
    TxtGotFocus Me
End Sub

Private Sub txtTCPEnd_Change()
    AcctChanged = True
End Sub

Private Sub SaveChanges()
    If AcctChanged Then
        Call f4assign(faTCPBegin, txtTCPBegin.Text)
        Call f4assign(faTCPEnd, txtTCPEnd.Text)
        Call f4assign(faCreate, ValToLog(chkCreate.value))
        Call f4assign(faCreateTemp, ValToLog(chkCreateTemp.value))
        Call f4assign(faCreateDel, ValToLog(chkCreate_Del.value))
        Call f4assign(faRead, ValToLog(chkARead.value))
        Call f4assign(faAppend, ValToLog(chkAAppend.value))
        Call f4assign(faDelete, ValToLog(chkADelete.value))
        Call f4assign(faUpdate, ValToLog(chkAUpdate.value))
        Call f4assign(faIndex, ValToLog(chkAIndex.value))
        Call f4assign(faCompress, ValToLog(chkACompress.value))
        Call f4assign(faDisconnect, ValToLog(chkDisconnect.value))
        If faAdmin <> 0 Then
            Call f4assign(faAdmin, ValToLog(chkCBAdmin.value))
        End If

        Call d4flush(accountdb)

        AcctChanged = False
    End If

    If PrivChanged Then
        Call f4assign(fpRead, ValToLog(chkPRead.value))
        Call f4assign(fpAppend, ValToLog(chkPAppend.value))
        Call f4assign(fpDelete, ValToLog(chkPDelete.value))
        Call f4assign(fpUpdate, ValToLog(chkPUpdate.value))
        Call f4assign(fpIndex, ValToLog(chkPIndex.value))
        Call f4assign(fpCompress, ValToLog(chkPCompress.value))
        ' LY 2002/01/29 : requires slash at end of path to be consistent with server
        If Right(txtPath.Text, 1) <> "\" Or Right(txtPath.Text, 1) <> "/" Then
            txtPath.Text = txtPath.Text + "\"
        End If
        Call f4assign(fpPath, txtPath.Text)
        Call f4assign(fpTable, txtTable.Text)

        TreeView1.Nodes(str(d4recNo(privdb)) & "p").Text = Trim(txtTable.Text)
        TreeView1.Nodes(str(d4recNo(privdb)) & "p").Parent.Sorted = True

        If d4recNo(privdb) = 0 Then
            Call d4append(privdb)
            TreeView1.Nodes(str(0) & "p").Key = str(d4recNo(privdb)) & "p"
        Else
            Call d4flush(privdb)
        End If

        PrivChanged = False
    End If
End Sub

Private Function ValToLog(val As VBRUN.CheckBoxConstants) As String
    ' convert CheckBox value (e.g. vbChecked)
    ' to xBase logical (e.g. "T")
    ' This function returns "F" when val = vbGrayed
    ' because that means that the privilege is granted
    ' at the account level but not at the file level.

    If val = vbChecked Then
        ValToLog = "T"
    Else
        ValToLog = "F"
    End If
End Function

Private Function AccountToCheck(setting As Boolean) As VBRUN.CheckBoxConstants
    ' given the setting for a privilege in the Account
    ' table, return the setting for the Checkbox
    If setting Then
        AccountToCheck = vbChecked
    Else
        AccountToCheck = vbUnchecked
    End If
End Function

Private Function PrivToCheck(PrivSetting As Boolean, Optional AcctSetting As Boolean = False) As VBRUN.CheckBoxConstants
    ' given the setting for a privilege in the Account
    ' table, return the setting for the Checkbox
    If PrivSetting Then
        PrivToCheck = vbChecked
    ElseIf AcctSetting Then
        PrivToCheck = vbGrayed
    Else
        PrivToCheck = vbUnchecked
    End If
End Function

Private Sub DelAccount()
    Dim privCount%, acctForSeek$, rc%
    acctForSeek = UCase(f4str(faAccountid))
    rc = d4seek(privdb, acctForSeek)
    Do While rc = r4success
        privCount = privCount + 1
        rc = d4seekNext(privdb, acctForSeek)
    Loop

    Dim delPrompt$
    delPrompt = "Are you sure you want to delete the account "
    delPrompt = delPrompt & "'" & Trim(TreeView1.SelectedItem.Text) & "'"
    If privCount > 0 Then
        If privCount = 1 Then
            delPrompt = delPrompt & " and the associated file privilege"
        Else
            delPrompt = delPrompt & " and the " & privCount & " associated file privileges"
        End If
    End If
    delPrompt = delPrompt & "?"

    If MsgBox(delPrompt, vbQuestion + vbYesNo, "Delete Account") = vbYes Then
        ' delete account
        Call d4delete(accountdb)

        ' delete all file privilages
        rc = d4seek(privdb, acctForSeek)
        Do While rc = r4success
            Call d4delete(privdb)
            rc = d4seek(privdb, acctForSeek)
        Loop

        With TreeView1
            Call .Nodes.Remove(.SelectedItem.Index)
            If .Nodes.count = 0 Then
                ShowAccount
            End If
        End With
    End If
End Sub

Private Sub DelPriv()
    If MsgBox("Are you sure you want to delete the file privilege for this account?", vbQuestion + vbYesNo, "Delete File Privilege") = vbYes Then
        ' delete account
        Call d4delete(privdb)

        With TreeView1
            Call .Nodes.Remove(.SelectedItem.Index)
        End With
    End If
End Sub

Private Function MakeLabel(ByVal path$, ByVal File$) As String
    Do While Right(path, 1) = SepChar Or Right(path, 1) = " "
        path = Left(path, Len(path) - 1)
    Loop
    Dim pathLen%, fileLen%
    pathLen = Len(path)
    File = Trim(File)
    fileLen = Len(File)

    If bFullPath Then
        If pathLen = 0 Then
            If fileLen = 0 Then
                MakeLabel = defPath
            Else
                MakeLabel = defPath & SepChar & File
            End If
        Else
            If fileLen = 0 Then
                MakeLabel = path
            Else
                MakeLabel = path & SepChar & File
            End If
        End If
    Else
        If fileLen = 0 Then
            If pathLen = 0 Then
                MakeLabel = GetLastInPath(defPath)
            Else
                MakeLabel = GetLastInPath(path)
            End If
        Else
            MakeLabel = File
        End If
    End If
End Function

Private Function GetLastInPath(path$) As String
    Dim i%
    For i = Len(path) To 1
        If Mid(path, i, 1) = SepChar Then
            GetLastInPath = Right(path, Len(path) - i)
            Exit For
        End If
    Next i
End Function

Private Sub txtTCPEnd_GotFocus()
    TxtGotFocus Me
End Sub
