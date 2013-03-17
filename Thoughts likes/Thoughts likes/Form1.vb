Public Class Form1
    Dim labs As New List(Of Label)
    Private Sub Form1_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        labs.Clear()
        For Each cnt As Object In Me.Controls
            If cnt.Name.Contains("Label") Then
                labs.Add(cnt)
            End If
        Next
    End Sub

    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click
        Dim i As Integer = 1
        For Each l In labs
            l.Text = i
            i += 1
        Next
    End Sub
End Class
