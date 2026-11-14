Attribute VB_Name = "modValidation"
Option Explicit

' Read the Validation sheet and drive the Dashboard status light.
' Severity is column 5 (run_id, checked_at, series_id, check_name, severity, message).
Public Function CheckStatus() As Boolean
    Dim ws As Worksheet
    Dim lastRow As Long
    Dim i As Long
    Dim fails As Long

    On Error Resume Next
    Set ws = ThisWorkbook.Worksheets("Validation")
    On Error GoTo 0

    If ws Is Nothing Then
        CheckStatus = True
        Exit Function
    End If

    lastRow = ws.Cells(ws.Rows.Count, 1).End(xlUp).Row
    For i = 2 To lastRow
        If LCase$(Trim$(CStr(ws.Cells(i, 5).Value))) = "fail" Then
            fails = fails + 1
        End If
    Next i

    If fails = 0 Then
        SetStatusLight True, "PASS"
        CheckStatus = True
    Else
        SetStatusLight False, "FAIL (" & fails & ")"
        CheckStatus = False
    End If
End Function
