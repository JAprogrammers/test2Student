Public Class Form1
    Dim horizontal, vertical, time, hsize, vsize, maxTime As Integer

    Private Sub Form1_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        hsize = 500
        vsize = 500
        Me.Width = 600
        Me.Height = 600
        maxTime = 225
    End Sub
    Private Sub Timer1_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer1.Tick
        Label1.Left += horizontal
        time += 1
        Label1.Text = time
        If Label1.Left > hsize Then
            Timer1.Enabled = False
            Timer2.Enabled = True
            horizontal = 0
            Label4.Text = horizontal
        End If
        If time > maxTime Then
            Timer1.Enabled = False
            Timer2.Enabled = False
            Timer3.Enabled = False
            Timer4.Enabled = False
            Label6.Visible = True
            Label6.Text = "You lost!"
        End If
    End Sub

    Private Sub Label2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Label2.Click
        horizontal += 1
        Label4.Text = horizontal
    End Sub

    Private Sub Label1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Label1.Click
        Timer1.Enabled = True
    End Sub

    Private Sub Timer2_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer2.Tick
        Label1.Top += vertical
        time += 1
        Label1.Text = time
        If Label1.Top > vsize Then
            Timer2.Enabled = False
            Timer3.Enabled = True
            vertical = 0
            Label5.Text = vertical
        End If
        If time > maxTime Then
            Timer1.Enabled = False
            Timer2.Enabled = False
            Timer3.Enabled = False
            Timer4.Enabled = False
            Label6.Visible = True
            Label6.Text = "You lost!"
        End If
    End Sub

    Private Sub Label3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Label3.Click
        vertical += 1
        Label5.Text = vertical
    End Sub

  

    Private Sub Timer3_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer3.Tick
        Label1.Left -= horizontal
        time += 1
        Label1.Text = time
        If Label1.Left < 50 Then
            Timer3.Enabled = False
            Timer4.Enabled = True
            horizontal = 0
            Label4.Text = horizontal
        End If
        If time > maxTime Then
            Timer1.Enabled = False
            Timer2.Enabled = False
            Timer3.Enabled = False
            Timer4.Enabled = False
            Label6.Visible = True
            Label6.Text = "You lost!"
        End If
    End Sub

    Private Sub Timer4_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer4.Tick
        Label1.Top -= vertical
        time += 1
        Label1.Text = time
        If Label1.Top < 50 Then
            Timer4.Enabled = False
            vertical = 0
            Label5.Text = vertical
            Label6.Text = "You win!!!"
            Label6.Visible = True
        End If
        If time > maxTime Then
            Timer1.Enabled = False
            Timer2.Enabled = False
            Timer3.Enabled = False
            Timer4.Enabled = False
            Label6.Visible = True
            Label6.Text = "You lost!"
        End If
    End Sub
End Class
