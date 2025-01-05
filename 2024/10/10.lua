#!/usr/bin/env lua

-- AoC 2024 Day 10

local table = require("table")

--- the matrix module. Useful to test the map in the repl
local M = {}

--- @class Point2d
--- @field x number
--- @field y number
M.Point2d = {}
setmetatable(M.Point2d, {__call =

--- @param x number | Point2d | number[]
--- @param y number
--- @return Point2d
function(self, x, y)
        local obj
        
        if getmetatable(x) == self then
            obj = {x = x.x, y = x.y}
        elseif type(x) == "table" and not y then
            obj = { x = x[1], y = x[2] } -- support for { x, y } syntax
        else
            obj = { x = x, y = y }
        end

        setmetatable(obj, self)
        self.__index = self

        return obj
    end
})

--- @param a Point2d
--- @param b Point2d
--- @return Point2d
function M.Point2d.__add(a, b)
    return M.Point2d(a.x + b.x, a.y + b.y)
end

function M.Point2d:__tostring()
    return string.format("(%d, %d)", self.x, self.y)
end

--- @class Map
M.Map = {}

--- @param fname string
--- @return Map
function M.Map:from_file(fname)
    local f, err = io.open(fname)

    if not f then
        error("cannot open file '" .. fname .. "': " .. err)
    end

    local lines = {}
    local nline = 1

    for line in f:lines() do
        local row = {}
        local ncol = 1
        for c in line:gmatch(".") do
            local n = tonumber(c)
            if n then
                table.insert(row, n)
            else
                error(string.format("%s:%d:%d: invalid character '%s'", fname, nline, ncol, c))
            end
        end

        table.insert(lines, row)
    end

    f:close()

    return self:new(lines)
end

--- @param matrix number[][]
--- @return Map
function M.Map:new(matrix)
    if not matrix then
        error("missing matrix")
    end

    setmetatable(matrix, self)

    return matrix
end

--- @return Point2d
function M.Map:dim()
    if #self == 0 then
        return M.Point2d(0, 0)
    end

    return M.Point2d(#self, #self[1])
end

function M.Map:dump()
    for _, row in ipairs(self) do
        print(table.concat(row))
    end
end

--- @param value number
--- @return Point2d[]
function M.Map:find(value)
   local res = {}
   
    for coord, v in self:iter() do
         if v == value then
              table.insert(res, coord)
         end
    end

    return res
end

--- @param coord Point2d | number[]
function M.Map:inside(coord)
    coord = M.Point2d(coord)

    local dim = self:dim()

    return coord.x >= 1 and coord.x <= dim.x and coord.y >= 1 and coord.y <= dim.y
end

--- @param map Map
--- @param coord Point2d
local function iter_map(map, coord)
    local dim = map:dim()

    coord = coord + M.Point2d(0, 1)

    if coord.y > dim.y then
        coord = M.Point2d(coord.x + 1, 1)
    end

    local v = map[coord]

    if v then
        return coord, v
    end
end

function M.Map:iter()
    return iter_map, self, M.Point2d(1, 0)
end

--- @type table<string, Point2d>
local directions = {
    N = M.Point2d(-1, 0),
    E = M.Point2d(0, 1),
    S = M.Point2d(1, 0),
    W = M.Point2d(0, -1),
}

--- @param coord Point2d | number[]
--- @param filter? function
--- @return table<Point2d, number>?
function M.Map:neighbours(coord, filter)
    coord = M.Point2d(coord)

    if not self:inside(coord) then
        return nil
    end

    local nlist = {}

    for dir, offset in pairs(directions) do
        local ncoord = coord + offset

        local v = self[ncoord]

        if v and (not filter or filter(v)) then
            nlist[ncoord] = v
        end
    end

    return nlist
end

--- @param coord Point2d | number[]
--- @return table<Point2d, number>?
function M.Map:neighbours_above(coord)
    local current = self[coord]
    if not current then
        return nil
    end

    return self:neighbours(coord, function(v) return v == current + 1 end)
end

function M.Map:__index(k)
    if getmetatable(k) == M.Point2d then
        local row = self[k.x]
        
        if row then
            return row[k.y]
        end
    elseif type(k) == "table" then
        local row = self[k[1]]
        
        if row then
            return row[k[2]]
        end
    elseif type(k) == "number" then
        return rawget(self, k)
    else
        return getmetatable(self)[k]
    end
end

--- @param list any[]
--- @return any[]
local function clone_list(list)
    local new_list = {}

    for _, v in ipairs(list) do
        table.insert(new_list, v)
    end

    return new_list
end

--- @param list any[]
--- @param value any
--- @return any[]
local function append_clone(list, value)
    local new_list = clone_list(list)
    table.insert(new_list, value)

    return new_list
end

--- @alias Trail { start: Point2d, path: Point2d[] }

--- @param trail Trail
--- @param point Point2d
--- @return Trail
local function append_trail(trail, point)
    local new_list = clone_list(trail.path)
    table.insert(new_list, point)

    return { start = trail.start, path = new_list }
end

--- @param map Map
--- @return Trail[]
local function find_starts(map)
    -- find all zeroes
    local trails = {}

    for _, zero in ipairs(map:find(0)) do
        table.insert(trails, { start = zero, path = { zero } })
    end

    return trails
end

--- @param trail Trail
local function format_trail(trail)
    local path = {}

    for _, p in ipairs(trail.path) do
        table.insert(path, tostring(p))
    end

    return string.format("start: %s, path: %s", trail.start, table.concat(path, " -> "))
end

--- @param trails Trail[]
local function dump_trails(trails)
    for i, trail in ipairs(trails) do
        print(string.format("Trail #%d: %s", i, format_trail(trail)))
    end
end

--- @param t table
--- @return number
local function table_length(t)
    local count = 0

    for _ in pairs(t) do
        count = count + 1
    end

    return count
end

--- @param trails Trail[]
--- @return number
local function rank_trails(trails)
    local reachable_ends = {}
    
    for _, trail in ipairs(trails) do
        -- must use strings as keys
        local starts = tostring(trail.start)
        local ends = reachable_ends[starts] or {}

        local endv = tostring(trail.path[#trail.path])

        ends[endv] = true

        reachable_ends[starts] = ends
    end

    local score = 0

    for _, ends in pairs(reachable_ends) do
        local nends = table_length(ends)

        -- print(string.format("Start: %s has rank %d", start, nends))

        score = score + nends
    end

    return score
end

--- @param map Map
local function do_problem(map)
    local done = {}
    local trails = find_starts(map)

    while #trails > 0 do
        -- dump_trails(trails)

        local new_trails = {}

        for _, trail in ipairs(trails) do
            local last = trail.path[#trail.path]
            local neighbours = map:neighbours_above(last)

            if neighbours and table_length(neighbours) > 0 then
                -- create a new trail for each neighbour
                for neigh, nval in pairs(neighbours) do
                    local updated_trail = append_trail(trail, neigh)

                    if nval == 9 then
                        assert(#updated_trail.path == 10)

                        table.insert(done, updated_trail)
                    else
                        table.insert(new_trails, updated_trail)
                    end
                end
            end
        end
        
        trails = new_trails
    end

    -- dump_trails(done)

    return {
        part1 = rank_trails(done),
        part2 = #done,
    }
end

function M.main()
    if #arg ~= 1 then
        io.stderr:write("error: wrong number of arguments\nusage: 10.lua FNAME\n")
        return 2
    end

    local fname = arg[1]

    local map = M.Map:from_file(fname)

    -- map:dump()

    local result = do_problem(map)
    print("part1: ", result.part1)
    print("part2: ", result.part2)

    return 0
end

-- note: this may broke someday if lua or luajit change the way they do things
if pcall(debug.getlocal, 4, 1) then
    return M
else
    local success, result, err = pcall(M.main)
    if not success then
        io.stderr:write("error: " .. result .. "\n")
        os.exit(1)
    end
end
