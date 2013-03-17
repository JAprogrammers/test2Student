Public Class Form1
    Dim value As Integer = 0
    Dim totalScore As Integer = 0

    Public Sub MoveHero(ByVal cnt As Object, ByVal a As Object, ByVal speed As Integer)
        If Asc(a.keychar) = 97 Then
            cnt.left -= speed
        ElseIf Asc(a.keychar) = 100 Then
            cnt.left += speed
        End If
    End Sub

    Public Sub Shoot(ByVal cnt As Object, ByVal startPos As Integer, ByVal speed As Integer)
        cnt.left = startPos
        cnt.top -= speed
    End Sub

    Public Sub LoadGun(ByVal cnt As Object, ByVal x As Integer, ByVal y As Integer, ByVal a As Object, ByVal t1 As Object)
        If Asc(a.keychar) = 119 Then
            cnt.top = y
            cnt.left = x
            t1.enabled = True
        End If
    End Sub

    Public Sub HasHit(ByVal posGun As Integer, ByRef value As Integer, ByVal cnt As Object)
        If posGun < 0 Then
            value += 1
            cnt.text = value
            Timer1.Enabled = False
        End If
    End Sub

    Private Sub Form1_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) Handles Me.KeyPress
        LoadGun(PictureBox3, PictureBox2.Left, PictureBox2.Top, e, Timer1)
        MoveHero(PictureBox2, e, 5)
    End Sub

    Private Sub Timer1_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer1.Tick
        Shoot(PictureBox3, PictureBox2.Left, 7)
        HasHit(PictureBox3.Top, value, Label1)
    End Sub

    Private Sub Label2_DragEnter(ByVal sender As Object, ByVal e As System.Windows.Forms.DragEventArgs) Handles Label2.DragEnter
        totalScore += value
        value = 0
        Label2.Text = totalScore
        Label1.Text = value
    End Sub

    Private Sub Label1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Label1.Click

    End Sub

    Private Sub Label1_MouseDown(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles Label1.MouseDown
        Label1.DoDragDrop(Label1, DragDropEffects.All)
    End Sub

    Private Sub Form1_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        Label1.Text = totalScore
        Label2.Text = value
        TextBox1.AllowDrop = True
    End Sub

    Private Sub TextBox1_DragEnter(ByVal sender As Object, ByVal e As System.Windows.Forms.DragEventArgs) Handles TextBox1.DragEnter
        TextBox1.Text = Label1.Text
    End Sub

    Private Sub TextBox1_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TextBox1.TextChanged

    End Sub
End Class
