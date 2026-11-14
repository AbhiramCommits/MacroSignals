Attribute VB_Name = "modRefresh"
Option Explicit

' One-click refresh for the PM:
'  1. refuse to run if the current Validation sheet has FAIL rows;
'  2. shell Rscript scripts\run_research.R and wait for it;
'  3. reload the CSV outputs and timestamp the Dashboard.
Public Sub RefreshSignals()
    Dim root As String
    Dim cmd As String
    Dim rc As Long
    Dim sh As Object
    Dim summary As Object

    If Not CheckStatus() Then
        MsgBox "Refresh blocked: the Validation sheet contains FAIL rows." & vbCrLf & _
               "Fix or re-ingest the data and rebuild the workbook before refreshing.", _
               vbCritical, "MacroSignals"
        Exit Sub
    End If

    root = ProjectRoot()
    cmd = "Rscript """ & root & "\scripts\run_research.R"""
    Set sh = CreateObject("WScript.Shell")
    sh.CurrentDirectory = root
    rc = sh.Run(cmd, 1, True)

    If rc <> 0 Then
        MsgBox "The R research run failed with exit code " & rc & "." & vbCrLf & _
               "See the R console for the validation or runtime error.", _
               vbCritical, "MacroSignals"
        SetStatusLight False, "R FAILED"
        Exit Sub
    End If

    Application.ScreenUpdating = False
    LoadCsvToSheet root & "\results\ranked_latest.csv", "Ranked_Signals"
    LoadCsvToSheet root & "\results\metrics.csv", "Metrics"
    LoadCsvToSheet root & "\results\equity_curve.csv", "EquityCurve"
    LoadCsvToSheet root & "\results\validation_log.csv", "Validation"

    Set summary = GetRunSummary()
    With ThisWorkbook.Worksheets("Dashboard")
        .Range("B3").Value = summary("run_id")
        .Range("B4").Value = summary("as_of")
        .Range("B5").Value = Format$(Now, "yyyy-mm-dd hh:mm:ss")
        .Range("B6").Value = summary("params_hash")
    End With
    Application.ScreenUpdating = True

    If CheckStatus() Then
        MsgBox "Signals refreshed. Run id: " & summary("run_id"), _
               vbInformation, "MacroSignals"
    End If
End Sub

' Paint the named Dashboard status cell green/red.
Public Sub SetStatusLight(ByVal ok As Boolean, ByVal label As String)
    Dim cell As Range
    On Error Resume Next
    Set cell = ThisWorkbook.Worksheets("Dashboard").Range("rngStatus")
    On Error GoTo 0
    If cell Is Nothing Then Exit Sub

    cell.Value = label
    If ok Then
        cell.Interior.Color = RGB(198, 239, 206)
        cell.Font.Color = RGB(0, 97, 0)
    Else
        cell.Interior.Color = RGB(255, 199, 206)
        cell.Font.Color = RGB(156, 0, 6)
    End If
End Sub
