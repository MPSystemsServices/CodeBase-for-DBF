Option Strict Off
Option Explicit On
Friend Class Form1
	Inherits System.Windows.Forms.Form
#Region "Windows Form Designer generated code "
	Public Sub New()
		MyBase.New()
		If m_vb6FormDefInstance Is Nothing Then
			If m_InitializingDefInstance Then
				m_vb6FormDefInstance = Me
			Else
				Try 
					'For the start-up form, the first instance created is the default instance.
					If System.Reflection.Assembly.GetExecutingAssembly.EntryPoint.DeclaringType Is Me.GetType Then
						m_vb6FormDefInstance = Me
					End If
				Catch
				End Try
			End If
		End If
		'This call is required by the Windows Form Designer.
		InitializeComponent()
	End Sub
	'Form overrides dispose to clean up the component list.
	Protected Overloads Overrides Sub Dispose(ByVal Disposing As Boolean)
		If Disposing Then
			If Not components Is Nothing Then
				components.Dispose()
			End If
		End If
		MyBase.Dispose(Disposing)
	End Sub
	'Required by the Windows Form Designer
	Private components As System.ComponentModel.IContainer
	Public ToolTip1 As System.Windows.Forms.ToolTip
	Public WithEvents Command1 As System.Windows.Forms.Button
	Public WithEvents Label1 As System.Windows.Forms.Label
	'NOTE: The following procedure is required by the Windows Form Designer
	'It can be modified using the Windows Form Designer.
	'Do not modify it using the code editor.
   Public WithEvents ListBox1 As System.Windows.Forms.ListBox
   <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
      Me.components = New System.ComponentModel.Container()
      Me.ToolTip1 = New System.Windows.Forms.ToolTip(Me.components)
      Me.Command1 = New System.Windows.Forms.Button()
      Me.Label1 = New System.Windows.Forms.Label()
      Me.ListBox1 = New System.Windows.Forms.ListBox()
      Me.SuspendLayout()
      '
      'Command1
      '
      Me.Command1.BackColor = System.Drawing.SystemColors.Control
      Me.Command1.Cursor = System.Windows.Forms.Cursors.Default
      Me.Command1.Font = New System.Drawing.Font("Arial", 8.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
      Me.Command1.ForeColor = System.Drawing.SystemColors.ControlText
      Me.Command1.Location = New System.Drawing.Point(170, 300)
      Me.Command1.Name = "Command1"
      Me.Command1.RightToLeft = System.Windows.Forms.RightToLeft.No
      Me.Command1.Size = New System.Drawing.Size(111, 31)
      Me.Command1.TabIndex = 0
      Me.Command1.Text = "CLICK"
      '
      'Label1
      '
      Me.Label1.BackColor = System.Drawing.SystemColors.Window
      Me.Label1.Cursor = System.Windows.Forms.Cursors.Default
      Me.Label1.Font = New System.Drawing.Font("Arial", 8.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
      Me.Label1.ForeColor = System.Drawing.SystemColors.WindowText
      Me.Label1.Location = New System.Drawing.Point(60, 340)
      Me.Label1.Name = "Label1"
      Me.Label1.RightToLeft = System.Windows.Forms.RightToLeft.No
      Me.Label1.Size = New System.Drawing.Size(342, 22)
      Me.Label1.TabIndex = 1
      Me.Label1.Text = "Click the button to execute the example code"
      '
      'ListBox1
      '
      Me.ListBox1.ItemHeight = 16
      Me.ListBox1.Location = New System.Drawing.Point(8, 8)
      Me.ListBox1.Name = "ListBox1"
      Me.ListBox1.Size = New System.Drawing.Size(432, 276)
      Me.ListBox1.TabIndex = 2
      '
      'Form1
      '
      Me.AutoScaleBaseSize = New System.Drawing.Size(7, 15)
      Me.BackColor = System.Drawing.SystemColors.Window
      Me.ClientSize = New System.Drawing.Size(451, 398)
      Me.Controls.AddRange(New System.Windows.Forms.Control() {Me.ListBox1, Me.Command1, Me.Label1})
      Me.Font = New System.Drawing.Font("Arial", 7.8!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
      Me.ForeColor = System.Drawing.SystemColors.WindowText
      Me.Location = New System.Drawing.Point(73, 128)
      Me.Name = "Form1"
      Me.StartPosition = System.Windows.Forms.FormStartPosition.Manual
      Me.Text = "Form1"
      Me.ResumeLayout(False)

   End Sub
#End Region 
#Region "Upgrade Support "
	Private Shared m_vb6FormDefInstance As Form1
	Private Shared m_InitializingDefInstance As Boolean
	Public Shared Property DefInstance() As Form1
		Get
			If m_vb6FormDefInstance Is Nothing OrElse m_vb6FormDefInstance.IsDisposed Then
				m_InitializingDefInstance = True
				m_vb6FormDefInstance = New Form1()
				m_InitializingDefInstance = False
			End If
			DefInstance = m_vb6FormDefInstance
		End Get
		Set
			m_vb6FormDefInstance = Value
		End Set
	End Property
#End Region 
	
	Private Sub Command1_Click(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles Command1.Click
      ExCode(Me)
	End Sub
End Class