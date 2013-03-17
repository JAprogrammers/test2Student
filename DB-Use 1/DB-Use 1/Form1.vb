Imports System.Data.Odbc
Public Class Form1
    Dim cn As New OdbcConnection
    Dim cmd As New OdbcCommand
    Dim rdr As OdbcDataReader

    Private Sub Label2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Label2.Click
       

    End Sub

    Private Sub Label1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Label1.Click
        cn.ConnectionString = "Provider=Microsoft.Jet.OLEDB.4.0; Data Source=C:\Users\emiliyank\Documents\GitHub\test2Student\DB-Use 1\DB-Use 1\DB1-Bulgaria.accdb" ' & System.Environment.CurrentDirectory.ToString() & "DB1-Bulgaria.accdb"
        cn.Open()
        cmd.Connection = cn

        cmd.CommandText = "SELECT * FROM Counties"
        rdr = cmd.ExecuteReader()
        While (rdr.Read())
            Label1.Text &= rdr("country") & vbCrLf
        End While
    End Sub
End Class
