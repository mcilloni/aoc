package main

// AoC 2024 Day 20

import (
	"bufio"
	"fmt"
	"os"
	"sort"
)

type Direction int

const (
	Up Direction = iota
	Right
	Down
	Left

	CheatRadius    int64  = 20
	Part1Threshold uint64 = 100
)

func (d Direction) Versor() Point {
	switch d {
	case Up:
		return Point{-1, 0}

	case Right:
		return Point{0, 1}

	case Down:
		return Point{1, 0}

	case Left:
		return Point{0, -1}

	default:
		panic("invalid direction")
	}
}

type Set[T comparable] map[T]struct{}

func (s Set[T]) Add(val T) {
	s[val] = struct{}{}
}

func (s Set[T]) Contains(val T) bool {
	_, ok := s[val]

	return ok
}

type Dimensions struct {
	rows, cols int64
}

type Point struct {
	x, y int64
}

func (p Point) Add(other Point) Point {
	return Point{p.x + other.x, p.y + other.y}
}

func (p Point) CirclePoints(grid *Grid, radius int64) Set[Point] {
	// manhattan circle of radius r with center p
	circle := make(Set[Point])

	var coeffs = [...]Point{{1, 1}, {1, -1}, {-1, 1}, {-1, -1}}

	for r := int64(1); r <= radius; r++ {
		for _, coeff := range coeffs {
			for i := int64(0); i <= r; i++ {
				p := Point{
					x: p.x + i*coeff.x,
					y: p.y + (r-i)*coeff.y,
				}

				if grid.IsEmpty(p) {
					circle.Add(p)
				}
			}
		}
	}

	return circle
}

func (p Point) Equals(other Point) bool {
	return p.x == other.x && p.y == other.y
}

func (p Point) Move(d Direction) Point {
	return p.Add(d.Versor())
}

func (p Point) Neighbours(grid *Grid) []Point {
	var neighbors []Point

	for _, d := range [...]Direction{Up, Right, Down, Left} {
		neighbor := p.Move(d)
		if grid.IsEmpty(neighbor) {
			neighbors = append(neighbors, neighbor)
		}
	}

	return neighbors
}

func Abs[N int | int8 | int16 | int32 | int64 | float32 | float64](n N) N {
	if n < N(0) {
		return -n
	}

	return n
}

func Manhattan(p, q Point) uint64 {
	return uint64(Abs(p.x-q.x) + Abs(p.y-q.y))
}

type Cheat struct {
	from, landing Point
	savings       uint64
}

func (c Cheat) Distance() uint64 {
	return Manhattan(c.from, c.landing)
}

func (c Cheat) Savings() uint64 {
	return c.savings
}

type Grid struct {
	Start, End Point

	dims  Dimensions
	walls Set[Point]
}

func (g *Grid) Contains(p Point) bool {
	return p.x >= 0 && p.x < g.dims.rows && p.y >= 0 && p.y < g.dims.cols
}

func (g *Grid) Dump() {
	g.DumpWithPath([]Point{})
}

func (g *Grid) DumpWithPath(path []Point) {
	pathPoints := make(Set[Point])

	for _, p := range path {
		pathPoints.Add(p)
	}

	for i := int64(0); i < g.dims.rows; i++ {
		for j := int64(0); j < g.dims.cols; j++ {
			p := Point{i, j}

			if g.Start == p {
				fmt.Print("S")
			} else if g.End == p {
				fmt.Print("E")
			} else if pathPoints.Contains(p) {
				fmt.Print("O")
			} else if g.walls.Contains(p) {
				fmt.Print("#")
			} else {
				fmt.Print(".")
			}
		}

		fmt.Println()
	}
}

func (g *Grid) IsEmpty(p Point) bool {
	return !g.walls.Contains(p) && g.Contains(p)
}

func (g *Grid) IsWall(p Point) bool {
	return g.walls.Contains(p)
}

type CheatsReport map[uint64][]Cheat

func (r CheatsReport) Enumerate() {
	keys := make([]uint64, 0, len(r))

	for k := range r {
		keys = append(keys, k)
	}

	sort.Slice(keys, func(i, j int) bool {
		return keys[i] < keys[j]
	})

	for _, savings := range keys {
		cheats := r[savings]
		s1 := ""
		s2 := "s"
		verb := "is"

		if len(cheats) > 1 {
			s1, s2 = "s", ""
			verb = "are"
		}

		fmt.Printf("There %s %d cheat%s that save%s %d picoseconds.\n", verb, len(cheats), s1, s2, savings)
	}
}

// idea: for each point in the path, compute the points inside a circle of radius 20 in taxicab geometry
// the cheat saves pathDistance - manhattanDistance picoseconds
// this algorithm is not super fast, but it's somewhat linear in complexity so it will finish in a reasonable time
// (~500 ms on my machine).
func DetectCheats(grid *Grid) CheatsReport {
	path := ExtractPath(grid)

	distanceMap := make(map[Point]uint64)

	for i, p := range path {
		distanceMap[p] = uint64(i)
	}

	cheatStats := make(map[uint64][]Cheat)

	pathDistance := func(p, q Point) uint64 {
		ip, ok := distanceMap[p]
		if !ok {
			panic("p not in path")
		}

		iq, ok := distanceMap[q]
		if !ok {
			panic("q not in path")
		}

		if ip >= iq {
			panic("p must be before q in path")
		}

		return uint64(iq - ip)
	}

	visited := make(Set[Point])

	for _, p := range path {
		for reachable := range p.CirclePoints(grid, CheatRadius) {
			if !visited.Contains(reachable) {
				stepsDistance := pathDistance(p, reachable)
				manhattan := Manhattan(p, reachable)

				// if the distance is greater than the manhattan distance, then the cheat makes sense
				if stepsDistance > manhattan {
					savings := stepsDistance - manhattan
					cheat := Cheat{p, reachable, savings}

					cheatStats[savings] = append(cheatStats[savings], cheat)
				}
			}
		}

		visited.Add(p)
	}

	return cheatStats
}

func ExtractPath(grid *Grid) []Point {
	pathMap := make(map[Point]Point)

	next := func(p Point) Point {
		neigh := p.Neighbours(grid)

		for _, n := range neigh {
			if _, ok := pathMap[n]; !ok {
				return n
			}
		}

		panic("no next point, grid is malformed")
	}

	current := grid.Start

	for current != grid.End {
		nextPoint := next(current)
		pathMap[current] = nextPoint

		current = nextPoint
	}

	path := []Point{}

	current = grid.Start

	for {
		path = append(path, current)

		if current == grid.End {
			break
		}

		current = pathMap[current]
	}

	return path
}

func Part2(grid *Grid) int {
	cheats := DetectCheats(grid)

	cheats.Enumerate()

	total := 0

	for saving, cheatList := range cheats {
		if saving >= Part1Threshold {
			total += len(cheatList)
		}
	}

	return total
}

func ReadGrid(fpath string) (g Grid, err error) {
	f, err := os.Open(fpath)
	if err != nil {
		return Grid{}, err
	}

	defer f.Close()

	s := bufio.NewScanner(f)
	s.Split(bufio.ScanRunes)

	rows := int64(-1)
	cols := int64(0)
	lineStart := true

	g.walls = make(Set[Point])

	for s.Scan() {
		val := s.Text()
		if val == "\n" {
			lineStart = true
		} else {
			if lineStart {
				rows++
				cols = 0
				lineStart = false
			}

			switch val {
			case "#":
				g.walls.Add(Point{rows, cols})

			case ".":
				// do nothing

			case "E":
				g.End = Point{rows, cols}

			case "S":
				g.Start = Point{rows, cols}

			default:
				return Grid{}, fmt.Errorf(`error: invalid character "%s"`, val)
			}

			cols++
		}
	}

	if err := s.Err(); err != nil {
		return Grid{}, err
	}

	g.dims = Dimensions{rows + 1, cols}

	return g, nil
}

func main() {
	if len(os.Args) != 2 {
		fmt.Fprintf(os.Stderr, "error: wrong number of arguments\nusage: %s INPUT\n", os.Args[0])

		os.Exit(2)
	}

	input := os.Args[1]

	grid, err := ReadGrid(input)
	if err != nil {
		fmt.Fprintf(os.Stderr, "error: %v\n", err)

		os.Exit(1)
	}

	fmt.Println("Part 2: ", Part2(&grid))
}
