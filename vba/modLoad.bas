Attribute VB_Name = "modLoad"
Option Explicit

' ---------------------------------------------------------------------------
' Path and CSV helpers shared by the MacroSignals desk workbook.
' The workbook lives in excel\; the project root is its parent folder.
' ---------------------------------------------------------------------------
Public Function ProjectRoot() As String
    Dim p As String
    p = ThisWorkbook.Path
    If LCase$(Right$(p, 6)) = "\excel" Then
        p = Left$(p, Len(p) - 6)
    End If
    ProjectRoot = p
End Function

' Load a CSV file into a worksheet, replacing its contents.
Public Function LoadCsvToSheet(ByVal csvPath As String, ByVal sheetName As String) As Boolean
    Dim ws As Worksheet
    Dim fnum As Integer
    Dim lineText As String
    Dim fields() As String
    Dim rowIdx As Long
    Dim j As Long

    On Error GoTo Failed

    If Dir$(csvPath) = "" Then
        MsgBox "CSV not found: " & csvPath, vbExclamation, "MacroSignals"
        Exit Function
    End If

    Set ws = ThisWorkbook.Worksheets(sheetName)
    ws.Cells.Clear
    fnum = FreeFile
    Open csvPath For Input As #fnum
    rowIdx = 1
    Do While Not EOF(fnum)
        Line Input #fnum, lineText
        If Len(Trim$(lineText)) > 0 Then
            fields = SplitCsvLine(lineText)
            For j = LBound(fields) To UBound(fields)
                ws.Cells(rowIdx, j - LBound(fields) + 1).Value = ParseCsvValue(fields(j))
            Next j
            rowIdx = rowIdx + 1
        End If
    Loop
    Close #fnum
    LoadCsvToSheet = True
    Exit Function

Failed:
    On Error Resume Next
    Close #fnum
    MsgBox "Failed to load " & csvPath & ": " & Err.Description, vbCritical, "MacroSignals"
    LoadCsvToSheet = False
End Function

' Minimal RFC-4180-compatible parser for the CSVs this workbook writes.
Public Function SplitCsvLine(ByVal lineText As String) As String()
    Dim out() As String
    Dim i As Long
    Dim ch As String
    Dim cur As String
    Dim inQuotes As Boolean

    ReDim out(0 To 0)
    inQuotes = False
    cur = ""
    For i = 1 To Len(lineText)
        ch = Mid$(lineText, i, 1)
        If ch = """" Then
            If inQuotes And i < Len(lineText) Then
                If Mid$(lineText, i + 1, 1) = """" Then
                    cur = cur & """"
                    i = i + 1
                Else
                    inQuotes = False
                End If
            Else
                inQuotes = Not inQuotes
            End If
        ElseIf ch = "," And Not inQuotes Then
            out(UBound(out)) = cur
            cur = ""
            ReDim Preserve out(0 To UBound(out) + 1)
        Else
            cur = cur & ch
        End If
    Next i
    out(UBound(out)) = cur
    SplitCsvLine = out
End Function

Public Function ParseCsvValue(ByVal s As String) As Variant
    Dim t As String
    t = Trim$(s)
    If Len(t) = 0 Then
        ParseCsvValue = ""
    ElseIf IsNumeric(t) Then
        ParseCsvValue = CDbl(t)
    ElseIf IsDate(t) Then
        ParseCsvValue = CDate(t)
    Else
        ParseCsvValue = s
    End If
End Function

' Read run_id / as_of / params_hash out of results/run_summary.json.
Public Function GetRunSummary() As Object
    Dim dict As Object
    Dim path As String
    Dim fnum As Integer
    Dim lineText As String
    Dim txt As String

    Set dict = CreateObject("Scripting.Dictionary")
    path = ProjectRoot() & "\results\run_summary.json"
    If Dir$(path) = "" Then
        Set GetRunSummary = dict
        Exit Function
    End If

    fnum = FreeFile
    Open path For Input As #fnum
    Do While Not EOF(fnum)
        Line Input #fnum, lineText
        txt = txt & lineText
    Loop
    Close #fnum

    dict("run_id") = JsonString(txt, "run_id")
    dict("as_of") = JsonString(txt, "as_of")
    dict("params_hash") = JsonString(txt, "params_hash")
    Set GetRunSummary = dict
End Function

Private Function JsonString(ByVal txt As String, ByVal key As String) As String
    Dim p As Long
    Dim q As Long
    p = InStr(1, txt, """" & key & """", vbTextCompare)
    If p = 0 Then Exit Function
    p = InStr(p, txt, ":")
    p = InStr(p, txt, """")
    If p = 0 Then Exit Function
    q = InStr(p + 1, txt, """")
    If q = 0 Then Exit Function
    JsonString = Mid$(txt, p + 1, q - p - 1)
End Function
