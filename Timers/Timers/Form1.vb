Public Class Form1
    Dim time As Integer = 10
    Dim now As Date = Date.Now
    Private Sub Label1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Label1.Click
        Timer1.Enabled = True
        Timer1.Interval = 1000
    End Sub

    Private Sub Timer1_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer1.Tick
        time = time - 1
        Label1.Text = time
        If time = 0 Then
            Timer1.Enabled = False
            Label1.Text = "BOOM!"
        End If
        now = DateTime.Now
        Label2.Text = now
    End Sub

    Private Sub Form1_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

    End Sub
End Class
