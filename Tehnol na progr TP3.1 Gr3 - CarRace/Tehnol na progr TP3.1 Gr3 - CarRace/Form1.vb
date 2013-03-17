Public Class Form1
    Dim speed As Integer = 1
    Dim time As Integer = 0

    Public Sub MoveCar(ByVal car As Object, ByVal speed As Integer, ByRef time As Integer, ByVal cnt As Object)
        car.left += speed
        time -= 1
        cnt.text = time
    End Sub

    Public Sub SpeedUp(ByRef speed As Integer, ByVal a As Object, ByVal cnt As Object)
        If Asc(a.keychar) = 97 Then
            speed += 1
            cnt.text = speed
        End If
    End Sub

    Public Sub HasWon(ByVal carPos As Integer, ByVal finishPos As Integer, ByVal time As Integer, ByVal maxTime As Integer, ByVal cnt As Object)
        If time > maxTime Then
            cnt.visible = True
            If carPos > finishPos Then
                cnt.Text = "Won!!!"
            Else
                cnt.Text = "You Lost!"
            End If
        End If
        If carPos > finishPos Then
            cnt.Text = "Won!!!"
            cnt.visible = True
        End If
    End Sub

    Private Sub Timer1_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer1.Tick
        MoveCar(Label1, speed, time, Label4)
        HasWon(Label1.Left, PictureBox1.Left, time, 20, Label3)
    End Sub

    Private Sub Label2_MouseDown(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles Label2.MouseDown
        Label1.DoDragDrop(Label1, DragDropEffects.All)
    End Sub



    Private Sub Label1_DragEnter(ByVal sender As Object, ByVal e As System.Windows.Forms.DragEventArgs) Handles Label1.DragEnter
        Timer1.Enabled = True
    End Sub

    Private Sub Form1_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) Handles Me.KeyPress
        SpeedUp(speed, e, Label3)
    End Sub

    Private Sub Form1_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        Label3.Text = speed
    End Sub
End Class
