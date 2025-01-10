#!/usr/bin/env julia

#=
This code is absolutely bonkers. There's most definitely a better and perhaps faster way to do this, but this was the 
only thing I came up with.

Basically this code
1. reads a grid of characters and maps them in a 2D array
2. finds regions via flood fill
3. computes the area (length), and then the perimeter of each region in the grid by adding up the number of external 
   neighbours of each point in the region
4. for part 2, it
    a. converts the points into 4 segments (N, E, S, W) of length 1
    b. filters out the ones that don't lie on the edges of the region
    c. merges them into the longest possible segment by attempting to extend them one by one
    d. splits the segments that intersect with each other in + or T junctions
    e. counts the number of segments in the region and multiplies it by the area of the region to get the final answer

=#

import Base: +, -, <, ∈

Grid = Matrix{Char}
Point = Tuple{Int,Int}
Segment = Tuple{Point,Point}

+(a::Point, b::Point) = (a[1] + b[1], a[2] + b[2])
-(a::Point, b::Point) = (a[1] - b[1], a[2] - b[2])
<(a::Point, b::Point) = a[1] < b[1] || (a[1] == b[1] && a[2] < b[2])

+(s::Segment, p::Point) = (s[1] + p, s[2] + p)

function (+)(s1::Segment, s2::Segment)
    (a1, b1) = s1
    (a2, b2) = s2

    if a1 == b2
        (a2, b1)
    elseif b1 == a2
        (a1, b2)
    else
        throw(ArgumentError("Segments are not adjacent"))
    end
end

function ∈(point::Point, segment::Segment)
    (a, b) = segment

    if a[1] == b[1]
        a[1] == point[1] && a[2] ≤ point[2] ≤ b[2]
    else
        a[2] == point[2] && a[1] ≤ point[1] ≤ b[1]
    end
end

function ∈(point::Point, grid::Grid)
    (m, n) = size(grid)
    1 ≤ point[1] ≤ m && 1 ≤ point[2] ≤ n
end

const directions = Dict(:e => (0, 1), :n => (-1, 0), :s => (1, 0), :w => (0, -1))

# already sorted N->S and W->E
# the direction indicates that a given segment is the boundary of the square on that specific side
const segments = Dict(
    :n => ((0, 0), (0, 1)),
    :e => ((0, 1), (1, 1)),
    :s => ((1, 0), (1, 1)),
    :w => ((0, 0), (1, 0)),
)

function crossintersect(s1::Segment, s2::Segment)
    (a, b) = s1
    (α, β) = s2

    local candidate = nothing
    if a[1] == b[1] && α[2] == β[2]
        candidate = (a[1], α[2])
    elseif a[2] == b[2] && α[1] == β[1]
        candidate = (α[1], a[2])
    end

    !isnothing(candidate) && candidate ∈ s1 && candidate ∈ s2 ? candidate : nothing
end

function extend(segment::Segment)
    (a, b) = segment

    (i, j) = a[1] == b[1] ? ((0, -1), (0, 1)) : ((-1, 0), (1, 0))

    α = a + i
    β = b + j

    ((α, a), (b, β))
end

function filtermap(f, iterable)
    result = []

    for item ∈ iterable
        res = f(item)

        if !isnothing(res)
            push!(result, res)
        end
    end

    result
end

function findregions(grid::Grid)
    points = Set((i, j) for i in axes(grid, 1), j in axes(grid, 2))
    regions = []

    while !isempty(points)
        region = regionof(grid, pop!(points))

        push!(regions, region)

        for point ∈ region.points
            delete!(points, point)
        end
    end

    regions
end

function loadfile(filename::String)
    lines = readlines(filename)

    # Vector{String} -> Vector{Vector{Char}} -> Vector{Matrix{Char}} -> Matrix{Char}
    reduce(vcat, permutedims.(collect.(lines)))
end

function measure_points(points::Set{Point})
    area = length(points)

    perimeter = 0

    for point ∈ points
        # - every point is a square
        # - every point has 4 neighbours
        # - a lone point has area 1 and perimeter 4 (no neighbours inside the region)
        # - for every neighbour inside the region, the perimeter decreases by 1
        perimeter += 4 - sum((p -> p ∈ points).(values(neighbours(point))))
    end

    (area=area, perimeter=perimeter)
end

function neighbours(point::Point)
    result = Dict{Symbol,Point}()

    for (sym, direction) ∈ directions
        neigh = point + direction

        push!(result, sym => neigh)
    end

    result
end

function neighbours(grid::Grid, point::Point)
    filter(pair -> pair[2] ∈ grid, neighbours(point))
end

struct Region
    value::Char
    points::Set{Point}
    area::Int
    perimeter::Int
end

function priceof(region::Region)
    region.area * region.perimeter
end

function regionof(grid::Grid, point::Point)
    value = grid[point...]
    points = [point]
    visited = Set{Point}()

    while !isempty(points)
        point = pop!(points)

        neigh = collect(values(neighbours(grid, point)))

        nlist = filter(p -> grid[p...] == value, neigh)

        append!(points, filter(p -> p ∉ visited, nlist))

        push!(visited, point)
    end

    Region(value, visited, measure_points(visited)...)
end

function edgechunks(region::Region)
    points = region.points
    isoutside(p::Point) = p ∉ points
    isedge(point::Point) = any(p -> isoutside(p[2]), neighbours(point))
    borders(point::Point) = filter(p -> isoutside(p[2]), neighbours(point))

    edgepoints = filter(isedge, points)

    # find all external segments of the shape. at first these will be short segments of length 1
    chunks = Set{Segment}()

    for point ∈ edgepoints
        for (direction, _) ∈ borders(point)
            push!(chunks, segments[direction] + point)
        end
    end

    chunks
end

function mergesegments(chunks::Set{Segment})
    result = Set{Segment}()

    while !isempty(chunks)
        segment = pop!(chunks)

        modified = true

        while modified
            modified = false

            # try getting the two one-length extension segments of this segment and merge them if they are in the set
            for ext ∈ extend(segment)
                if ext ∈ chunks
                    delete!(chunks, ext)
                    segment += ext

                    modified = true
                end
            end
        end

        push!(result, segment)
    end

    # finally, split any intersecting segments
    splitintersects(result)
end

function split(segment::Segment, point::Point)
    if point ∈ segment
        (start, finish) = segment

        if start == point
            (nothing, finish)
        elseif finish == point
            (start, nothing)
        else
            (start, point), (point, finish)
        end
    end
end

function splitintersects(segments::Set{Segment})
    function split_if_intersects(s1::Segment, s2::Segment)
        intersection = crossintersect(s1, s2)

        if !isnothing(intersection)
            (α, β) = split(s1, intersection)
            (γ, δ) = split(s2, intersection)

            # if 2 or more segments are empty, this is a corner, which is irrelevant to our calculations
            if length(filter(isnothing, (α, β, γ, δ))) < 2
                (α, β, γ, δ)
            end
        end
    end

    result = Set{Segment}()

    while !isempty(segments)
        segment = pop!(segments)

        other = nothing
        intersect = nothing

        for s ∈ segments
            res = split_if_intersects(segment, s)

            if !isnothing(res)
                intersect = res
                other = s
                break
            end
        end

        if isnothing(intersect)
            # the segment survived the round, no intersections
            push!(result, segment)
        else
            # two segments were split into 4 segments, push the non-empty ones back into the set
            delete!(segments, other)

            union!(segments, filter(s -> !isnothing(s), intersect))
        end
    end

    result
end

function (@main)(args)
    if (length(args) ≠ 1)
        println(stderr, "Usage: $PROGRAM_FILE INPUT")
        exit(2)
    end

    grid = loadfile(args[1])

    display(grid)

    regions = findregions(grid)

    display(regions)

    println("part1: ", sum(priceof.(regions)))

    part2 = 0

    for (i, region) ∈ enumerate(regions)
        sides = edgechunks(region) |> mergesegments
        len = length(sides)

        println("#$i ($(region.value)): $len sides. Sides: $sides")

        part2 += len * region.area
    end

    println("part2: ", part2)
end
