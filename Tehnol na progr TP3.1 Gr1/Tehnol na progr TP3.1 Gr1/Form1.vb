Public Class Form1
    Dim value As Integer = 0
    Dim totalPoints As Integer = 0

    Public Sub MovePolice(ByVal cnt As Object, ByVal t1 As Object, ByVal t2 As Object, ByRef speed As Integer, ByVal size As Integer)
        cnt.left += speed
        If speed > 0 Then
            If cnt.left > size Then
                t1.enabled = False
                t2.enabled = True
            End If
        Else
            If cnt.left < 0 Then
                t1.enabled = False
                t2.enabled = True
            End If
        End If
    End Sub

    Public Sub MoveHero(ByVal cnt As Object, ByVal a As Object, ByVal speed As Integer)
        If Asc(a.KeyChar) = 97 Then
            cnt.Left -= speed
        ElseIf Asc(a.keychar) = 100 Then
            cnt.left += speed
        ElseIf Asc(a.keychar) = 119 Then
            cnt.top -= speed
        ElseIf Asc(a.keychar) = 115 Then
            cnt.top += speed
        End If
    End Sub


    Private Sub Timer1_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer1.Tick
        MovePolice(PictureBox1, Timer1, Timer2, 5, Me.Width)
    End Sub

    Public Sub GetPoint(ByVal border As Integer, ByVal hero As Integer, ByVal heroOld As Integer, ByRef points As Integer, ByVal cnt As Object)
        If heroOld < border And hero > border Then
            points += 1
            cnt.text = points
        End If
    End Sub

    Private Sub Form1_KeyPress(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyPressEventArgs) Handles Me.KeyPress
        MoveHero(Label2, e, 5)
        GetPoint(PictureBox1.Top, Label2.Top, Label2.Top - 5, value, Label2)
    End Sub

    Private Sub Timer2_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer2.Tick
        MovePolice(PictureBox1, Timer2, Timer1, -5, Me.Width)
    End Sub

    Private Sub Label1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Label1.Click
        Timer1.Enabled = True
    End Sub

    Private Sub Label2_MouseDown(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles Label2.MouseDown
        Label2.DoDragDrop(Label2, DragDropEffects.All)
    End Sub

    Private Sub Label3_DragEnter(ByVal sender As Object, ByVal e As System.Windows.Forms.DragEventArgs) Handles Label3.DragEnter
        totalPoints += value
        value = 0
        Label4.Text = totalPoints
        Label2.Text = value
    End Sub

    Private Sub Form1_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        Label4.Text = ""
    End Sub
End Class
