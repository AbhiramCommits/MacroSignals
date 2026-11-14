Attribute VB_Name = "modTests"
Option Explicit

' Run the shared golden ranking cases against the VBA implementation.
' The same file is exercised by tests/testthat/test-rank-parity.R.
Public Sub RunVbaTests()
    Dim path As String
    Dim fnum As Integer
    Dim lineText As String
    Dim cases As Long
    Dim assertions As Long
    Dim failures As Long
    Dim ws As Worksheet
    Dim fields() As String
    Dim vals() As Double
    Dim expectedRanks() As Double
    Dim expectedTop() As Double
    Dim gotRanks As Variant
    Dim gotTop As Variant
    Dim i As Long
    Dim n As Long
    Dim rng As Range
    Dim caseFail As Boolean

    path = ProjectRoot() & "\tests\vba\ranking_cases.csv"
    If Dir$(path) = "" Then
        MsgBox "Missing golden file: " & path, vbCritical, "MacroSignals"
        Exit Sub
    End If

    Application.ScreenUpdating = False
    Set ws = ThisWorkbook.Worksheets.Add
    ws.Name = "VBA_Test_Scratch"

    fnum = FreeFile
    Open path For Input As #fnum
    Line Input #fnum, lineText ' header

    Do While Not EOF(fnum)
        Line Input #fnum, lineText
        If Len(Trim$(lineText)) > 0 Then
            cases = cases + 1
            fields = Split(lineText, ",")
            vals = PipeToDoubles(fields(1))
            expectedRanks = PipeToDoubles(fields(2))
            n = CLng(fields(3))
            expectedTop = PipeToDoubles(fields(4))

            ws.Cells.Clear
            For i = LBound(vals) To UBound(vals)
                ws.Cells(i - LBound(vals) + 1, 1).Value = vals(i)
            Next i
            Set rng = ws.Range(ws.Cells(1, 1), _
                               ws.Cells(UBound(vals) - LBound(vals) + 1, 1))

            ' --- RankSignals
            assertions = assertions + 1
            gotRanks = RankSignals(rng)
            caseFail = False
            For i = LBound(expectedRanks) To UBound(expectedRanks)
                If Abs(CDbl(gotRanks(i - LBound(expectedRanks) + 1, 1)) - _
                       expectedRanks(i)) > 0.000000001 Then
                    caseFail = True
                End If
            Next i
            If caseFail Then failures = failures + 1

            ' --- TopN
            assertions = assertions + 1
            gotTop = TopN(rng, n)
            caseFail = False
            For i = LBound(expectedTop) To UBound(expectedTop)
                If Abs(CDbl(gotTop(i - LBound(expectedTop) + 1, 1)) - _
                       expectedTop(i)) > 0.000000001 Then
                    caseFail = True
                End If
            Next i
            If caseFail Then failures = failures + 1
        End If
    Loop
    Close #fnum

    Application.DisplayAlerts = False
    ws.Delete
    Application.DisplayAlerts = True
    Application.ScreenUpdating = True

    If failures = 0 Then
        MsgBox "VBA ranking tests passed: " & assertions & " assertions across " & _
               cases & " cases.", vbInformation, "MacroSignals"
    Else
        MsgBox "VBA ranking tests FAILED: " & failures & " of " & assertions & _
               " assertions.", vbCritical, "MacroSignals"
    End If
End Sub

Private Function PipeToDoubles(ByVal s As String) As Double()
    Dim parts() As String
    Dim out() As Double
    Dim i As Long

    parts = Split(s, "|")
    ReDim out(0 To UBound(parts))
    For i = 0 To UBound(parts)
        out(i) = CDbl(parts(i))
    Next i
    PipeToDoubles = out
End Function
