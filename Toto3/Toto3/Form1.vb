Public Class Form1
    Dim numbers(6) As Integer
    Public Sub GenerateNumbers(ByVal size As Integer, ByVal final As Integer, ByVal cnt As Object)
        Dim num As Integer
        Randomize()
        cnt.Text = ""
        For i = 0 To (size - 1) Step 1
            num = Rnd() * final + 1
            Do While (CheckDuplicate(num, numbers))
                num = Rnd() * final + 1
            loop
                numbers(i) = num
                cnt.Text &= numbers(i) & "  "
        Next
    End Sub

    Public Function CheckDuplicate(ByVal num As Integer, ByVal numbers() As Integer) As Boolean
        Dim result As Boolean = False
        For i = 0 To numbers.Length - 1 Step 1
            If num = numbers(i) Then
                result = True
            End If
        Next
        Return result
    End Function

    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click
        GenerateNumbers(6, 26, Label2)
    End Sub
End Class
