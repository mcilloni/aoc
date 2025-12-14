' Solution for Advent of Code 2025 Day 4

Imports System.Collections.Generic
Imports System.CommandLine
Imports System.IO
Imports System.Runtime.CompilerServices

Module p04

    Enum Cell
        Empty
        Roll
    End Enum

    <Extension()>
    Function ToChar(cl As Cell) As Char
        Select Case cl
            Case Cell.Empty
                Return "."c
            Case Cell.Roll
                Return "@"c
            Case Else
                Throw New ArgumentException($"invalid cell type: {cl}")
        End Select
    End Function

    <Extension()>
    Function ToCell(chr As Char) As Cell
        Select Case chr
            Case "."c
                Return Cell.Empty
            Case "@"c
                Return Cell.Roll
            Case Else
                Throw New ArgumentException($"invalid cell character: {chr}")
        End Select
    End Function

    Structure Point2d
        Public X As Integer
        Public Y As Integer

        Public Shared Function NewAt(x As Integer, y As Integer) As Point2d
            Return New Point2d With {.X = x, .Y = y}
        End Function

        Public Shared Operator +(lhs As Point2d, rhs As Point2d) As Point2d
            Return Point2d.NewAt(lhs.X + rhs.X, lhs.Y + rhs.Y)
        End Operator

        Public Shared Operator +(lhs As Point2d, rhs As (Integer, Integer)) As Point2d
            Return Point2d.NewAt(lhs.X + rhs.Item1, lhs.Y + rhs.Item2)
        End Operator


        Public Overrides Function Equals(obj As Object) As Boolean
            If Not TypeOf obj Is Point2d Then
                Return False
            End If

            Dim other = CType(obj, Point2d)
            Return Me.X = other.X AndAlso Me.Y = other.Y
        End Function

        Public Overrides Function GetHashCode() As Integer
            Return HashCode.Combine(X, Y)
        End Function

        Public Function Neighbours() As IEnumerable(Of Point2d)
            Dim this = Me
            Return From offset In {(0, 1), (1, 1), (1, 0), (1, -1), (0, -1), (-1, -1), (-1, 0), (-1, 1)} Select this + offset
        End Function

        Public Overrides Function ToString() As String
            Return $"({Me.X}, {Me.Y})"
        End Function
    End Structure

    Public Class Grid
        Private _dim As Integer
        Private _rolls As HashSet(Of Point2d)

        Public Shared Function FromChars(jaggedData As Char()()) As Grid
            Dim dimension = jaggedData.GetUpperBound(0)

            Dim data = New HashSet(Of Point2d)
            For i = 0 To dimension
                Dim row = jaggedData(i)
                If row.GetUpperBound(0) <> dimension Then
                    Throw New ArgumentException("input has stray data or is not squared")
                End If

                For j = 0 To dimension
                    If row(j).ToCell() = Cell.Roll Then
                        data.Add(Point2d.NewAt(i, j))
                    End If
                Next
            Next

            Return New Grid With {
                ._dim = dimension + 1,
                ._rolls = data
            }
        End Function

        Public Shared Function FromFile(path As String) As Grid
            Dim lines = (From line In File.ReadAllLines(path) Select line.ToCharArray()).ToArray()

            Return FromChars(lines)
        End Function

        Public Function Contains(x As Integer, y As Integer) As Boolean
            Return x < Me.Dimension AndAlso y < Me.Dimension AndAlso x >= 0 AndAlso y >= 0
        End Function

        Public Function Contains(p As Point2d) As Boolean
            Return Me.Contains(p.X, p.Y)
        End Function

        Public Sub Dump()
            If Me.Dimension = 0 Then
                Console.WriteLine("(empty grid)")
                Return
            End If

            Dim upperBound = Me.Dimension - 1

            For i = 0 To upperBound
                For j = 0 To upperBound
                    Console.Write(Me(i, j).ToChar())
                Next

                Console.WriteLine()
            Next
        End Sub

        Public Sub DropRollAt(p As Point2d)
            If Not Me._rolls.Remove(p) Then
                Throw New InvalidOperationException($"no roll at position {p} to drop")
            End If
        End Sub

        Public Sub DropRollsAt(positions As IEnumerable(Of Point2d))
            For Each p In positions
                Me.DropRollAt(p)
            Next
        End Sub

        Public Function IsRollAt(x As Integer, y As Integer) As Boolean
            Return Me.IsRollAt(Point2d.NewAt(x, y))
        End Function

        Public Function IsRollAt(p As Point2d) As Boolean
            Return Me._rolls.Contains(p)
        End Function

        Public Function NeighbourhoodOf(x As Integer, y As Integer) As IEnumerable(Of Point2d)
            Return Me.NeighbourhoodOf(Point2d.NewAt(x, y))
        End Function

        Public Function NeighbourhoodOf(p As Point2d) As IEnumerable(Of Point2d)
            Return From nbr In p.Neighbours() Where Me.Contains(nbr) Select nbr
        End Function

        Public ReadOnly Property Dimension As Integer
            Get
                Return Me._dim
            End Get
        End Property

        Default Public ReadOnly Property Item(x As Integer, y As Integer) As Cell
            Get
                If Not Me.Contains(x, y) Then
                    Throw New IndexOutOfRangeException($"index ({x}, {y}) is out of bounds for dimension {Me.Dimension}")
                End If

                Return Me.GetUnchecked(x, y)
            End Get
        End Property

        Public ReadOnly Property Rolls As IEnumerable(Of Point2d)
            Get
                Return Me._rolls
            End Get
        End Property

        Private Function GetUnchecked(x As Integer, y As Integer) As Cell
            Return If(Me.IsRollAt(x, y), Cell.Roll, Cell.Empty)
        End Function
    End Class

    Function ComputeFreeRolls(g As Grid) As IEnumerable(Of Point2d)
        Dim countRolls = Function(points As IEnumerable(Of Point2d)) As Integer
                             Return (From p In points Where g.IsRollAt(p)).Count()
                         End Function

        Return From roll In g.Rolls
               Let nbrs = g.NeighbourhoodOf(roll)
               Where countRolls(nbrs) < 4
               Select roll
    End Function

    Function Part1(g As Grid) As Integer
        Return ComputeFreeRolls(g).Count()
    End Function

    Function Part2(g As Grid) As Integer
        Dim nRemoved = 0

        Do
            Dim removable = ComputeFreeRolls(g).ToArray()

            If removable.Length > 0 Then
                g.DropRollsAt(removable)
                nRemoved += removable.Length
            Else
                Exit Do
            End If
        Loop

        Return nRemoved
    End Function

    Sub SolveInput(input As FileInfo)
        Dim g = Grid.FromFile(input.FullName)

        Console.WriteLine($"Part1: {Part1(g)}")
        Console.WriteLine($"Part2: {Part2(g)}")
    End Sub

    Sub Main(args As String())
        Dim fileArgument = New Argument(Of FileInfo)("INPUT") With {.Description = "input file"}
        Dim rootCommand = New RootCommand("04") With {.Description = "AoC 2025 Day 4"}

        rootCommand.Arguments.Add(fileArgument)

        rootCommand.SetAction(Sub(result As ParseResult) SolveInput(result.GetValue(Of FileInfo)("INPUT")))

        rootCommand.Parse(args).Invoke()
    End Sub
End Module
