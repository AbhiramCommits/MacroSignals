Attribute VB_Name = "modRank"
Option Explicit

' Desk rank/filter logic. Rank 1 = largest value; ties receive the average rank.
Public Function RankSignals(ByVal rng As Range) As Variant
    Dim vals() As Double
    Dim i As Long
    Dim n As Long

    n = rng.Cells.Count
    If n = 0 Then Exit Function
    ReDim vals(1 To n)
    For i = 1 To n
        vals(i) = CDbl(rng.Cells(i).Value)
    Next i
    RankSignals = RankArray(vals)
End Function

Public Function RankArray(ByRef vals() As Double) As Variant
    Dim out() As Variant
    Dim i As Long
    Dim j As Long
    Dim n As Long
    Dim greater As Long
    Dim equal As Long

    n = UBound(vals) - LBound(vals) + 1
    ReDim out(1 To n, 1 To 1)
    For i = 1 To n
        greater = 0
        equal = 0
        For j = 1 To n
            If vals(j) > vals(i) Then greater = greater + 1
            If vals(j) = vals(i) Then equal = equal + 1
        Next j
        out(i, 1) = greater + 1 + (equal - 1) / 2
    Next i
    RankArray = out
End Function

' Return the largest n values in descending order as an n x 1 array.
Public Function TopN(ByVal rng As Range, ByVal n As Long) As Variant
    Dim vals() As Double
    Dim i As Long
    Dim count As Long

    count = rng.Cells.Count
    If count = 0 Then Exit Function
    ReDim vals(1 To count)
    For i = 1 To count
        vals(i) = CDbl(rng.Cells(i).Value)
    Next i
    TopN = TopNArray(vals, n)
End Function

Public Function TopNArray(ByRef vals() As Double, ByVal n As Long) As Variant
    Dim tmp() As Double
    Dim out() As Variant
    Dim i As Long
    Dim j As Long
    Dim count As Long
    Dim take As Long
    Dim swap As Double

    count = UBound(vals) - LBound(vals) + 1
    ReDim tmp(1 To count)
    For i = 1 To count
        tmp(i) = vals(i)
    Next i
    For i = 1 To count - 1
        For j = i + 1 To count
            If tmp(j) > tmp(i) Then
                swap = tmp(i)
                tmp(i) = tmp(j)
                tmp(j) = swap
            End If
        Next j
    Next i

    take = n
    If take > count Then take = count
    If take < 1 Then Exit Function
    ReDim out(1 To take, 1 To 1)
    For i = 1 To take
        out(i, 1) = tmp(i)
    Next i
    TopNArray = out
End Function
