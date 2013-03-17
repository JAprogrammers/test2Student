Public Class Form1
    Dim time As Integer = 0
    Dim l, t As Integer
    Dim clicked As Integer

    Private Sub Timer1_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer1.Tick
        time += 1
        Label1.Text = time
        If time >= 20 Then
            If clicked > 18 Then
                Label3.Text = "You win"
            Else
                Label3.Text = "You lose"
            End If
            Timer1.Enabled = False
        End If
    End Sub

    Private Sub Label1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Label1.Click
       
    End Sub

    Private Sub Form1_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        Me.Width = 501
        Me.Height = 501
    End Sub

    Private Sub PictureBox1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox1.Click
        Randomize()
        l = Int(Rnd() * 500 + 1)
        t = Int(Rnd() * 500 + 1)
        PictureBox1.Left = l
        PictureBox1.Top = t
        clicked += 1
        Label2.Text = clicked
    End Sub
End Class
