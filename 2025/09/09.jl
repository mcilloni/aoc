#!/usr/bin/env julia

# AoC 2025 Day 09

import Base: +, -, <, ∈, parse
import Combinatorics: combinations

Point = Tuple{Int,Int}
Points = Vector{Point}
Segment = Tuple{Point,Point}
Segments = Vector{Segment}
SquareCandidate = NamedTuple{(:good, :tocheck), Tuple{Vector{Point}, Vector{Point}}}
Polygon = Vector{Segment}

@enum Direction horizontal vertical

struct Line
    offset::Int
    direction::Direction
end

struct BinnedSegments
    h::Segments
    v::Segments
end

+(a::Point, b::Point) = (a[1] + b[1], a[2] + b[2])
-(a::Point, b::Point) = (a[1] - b[1], a[2] - b[2])
<(a::Point, b::Point) = a[1] < b[1] || (a[1] == b[1] && a[2] < b[2])

# grid area, i.e. cell count
function areaof(p1::Point, p2::Point)
    ((x1, y1), (x2, y2)) = (p1, p2)
    (abs(x2 - x1) + 1) * (abs(y2 - y1) + 1)
end

function areas(tiles::Points)
    combinations(tiles, 2) .|> ((p1, p2)::Vector) -> (areaof(p1, p2), (p1, p2))
end

function directionof(edge::Segment)
    ((x1, y1), (x2, y2)) = edge

    if x1 == x2
        vertical
    elseif y1 == y2
        horizontal
    else
        throw(ArgumentError("Segment $edge is not axis-aligned"))
    end
end

function edgesof(points::Points)::Segments
    if length(points) < 2
        throw(ArgumentError("At least 2 points are required"))
    end

    foldl(points, init=(Nothing, [])) do (prev, acc), cur
        push!(acc, (prev, cur))
        (cur, acc)
        # drop the first edge, which is (Nothing, first(points)), and add the closing edge (last(points), first(points))
    end |> last |> (edges) -> vcat(edges[2:end], [(last(points), first(points))])
end

function binedges(edges::Segments)::BinnedSegments
    foldl(edges, init=(h = Segments(), v = Segments())) do acc, edge
        dir = directionof(edge)

        if dir == horizontal
            push!(acc.h, edge)
        else
            push!(acc.v, edge)
        end

        acc
    end |> (acc) -> BinnedSegments(acc.h, acc.v)
end

function ∈(point::Point, edge::Segment)
    (px, py) = point

    if directionof(edge) == horizontal
        ((x1, y), (x2, _)) = edge
        px ∈ min(x1, x2):max(x1, x2) && py == y
    else
        ((x, y1), (_, y2)) = edge
        py ∈ min(y1, y2):max(y1, y2) && px == x
    end
end

function lineintersects(line::Line, segment::Segment)
    sdir = directionof(segment)

    if (sdir, line.direction) == (horizontal, vertical)
        ((x1, _), (x2, _)) = segment
        min(x1, x2) <= line.offset < max(x1, x2)
    elseif (sdir, line.direction) == (vertical, horizontal)
        ((_, y1), (_, y2)) = segment
        min(y1, y2) <= line.offset < max(y1, y2)
    else
        throw(ArgumentError("Line and segment are parallel"))
    end
end

function lineinside(line::Line, segment::Segment)
    sdir = directionof(segment)

    if (sdir, line.direction) == (horizontal, vertical)
        ((x1, _), (x2, _)) = segment
        min(x1, x2) < line.offset < max(x1, x2)
    elseif (sdir, line.direction) == (vertical, horizontal)
        ((_, y1), (_, y2)) = segment
        min(y1, y2) < line.offset < max(y1, y2)
    else
        throw(ArgumentError("Line and segment are parallel"))
    end
end

function intersectsany(segment::Segment, edges::BinnedSegments)
    sdir = directionof(segment)

    ((x1, y1), (x2, y2)) = segment

    if sdir == horizontal
        any(edges.v) do edge
            ((x, _), _) = edge

            min(x1,x2) < x < max(x1,x2) && lineinside(Line(y1, horizontal), edge)
        end
    else
        any(edges.h) do edge
            ((_, y), _) = edge

            min(y1,y2) < y < max(y1,y2) && lineinside(Line(x1, vertical), edge)
        end
    end
end

function isinside(point::Point, edges::BinnedSegments)
    # raycast horizontally to the right and down

    (px, py) = point

    # raycast horizontally to the right and count intersections with vertical edges
    count = 0
    for edge ∈ edges.v
        if point ∈ edge
            # println("$point is on edge $edge, assuming inside")

            return true
        else
            ((x, _), _) = edge
            res = x >= px && lineintersects(Line(py, horizontal), edge)

            # if res
            #     println("line y = $py intersects $edge")
            # end

            count += res
        end
    end

    isodd(count)
end

function noedgecrossing(square::SquareCandidate, edges::BinnedSegments)
    ((x1y1, x2y2), (x2y1, x1y2)) = square

    segments = [
        (x1y1, x2y1),
        (x2y1, x2y2),
        (x2y2, x1y2),
        (x1y2, x1y1),
    ]

    !any(segments) do segment
        intersectsany(segment, edges)
    end
end

function parse(_::Type{Point}, str::String)
    (split(str, ',') .|> strip .|> x -> parse(Int, x)) |> Point
end

function loadfile(filename::String)
    (readlines(filename) .|> line -> parse(Point, line)) |> Points
end

function squares(tiles::Points)
    combinations(tiles, 2) .|> (((x1, y1), (x2, y2))::Vector) -> (
        good = [(x1, y1), (x2, y2)],
        tocheck = [(x2, y1), (x1, y2)],
    )
end

function part1(tiles::Points)
    sort(areas(tiles), by = x -> x[1], rev = true) |> first
end

function part2(tiles::Points)
    polygon = edgesof(tiles)

    bins = binedges(polygon)

    squares(tiles) |> filter() do square
        all(square.tocheck) do vertex
            isinside(vertex, bins) && noedgecrossing(square, bins)
        end
    end .|> (square -> (areaof(square.good...), square.good)) |> asizes -> sort(asizes, by = x -> x[1], rev = true) |> first
end

function (@main)(args)
    if (length(args) ≠ 1)
        println(stderr, "Usage: $PROGRAM_FILE INPUT")
        exit(2)
    end

    tiles = loadfile(args[1])
    
    (part1area, (part1p1, part1p2)) = part1(tiles)
    println("Part 1: $part1area (between $part1p1 and $part1p2)")

    (part2area, (part2p1, part2p2)) = part2(tiles)
    println("Part 2: $part2area (between $part2p1 and $part2p2)")
end
