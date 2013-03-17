Public Class Form1
    Dim likes(3) As Integer
    Dim dislikes(3) As Integer
    Dim quotes(3) As String

    'fafa
    Private Sub PictureBox1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox1.Click
        likes(0) += 1
        Label4.Text = likes(0)
        Label6.Text = likes(0) - dislikes(0)
    End Sub


    Private Sub PictureBox3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox3.Click
        likes(1) += 1
    End Sub

    Private Sub PictureBox5_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox5.Click
        likes(2) += 1
    End Sub

    Private Sub PictureBox2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox2.Click
        dislikes(0) += 1
        Label5.Text = dislikes(0)
        Label6.Text = likes(0) - dislikes(0)
    End Sub

    Private Sub Form1_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        quotes(0) = "misyl 1"
        quotes(1) = "misyl 2"
        quotes(2) = "misyl 3"

        Label1.Text = quotes(0)
        Label2.Text = quotes(1)
        Label3.Text = quotes(2)
    End Sub
End Class
