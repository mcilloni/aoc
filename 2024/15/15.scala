#!/usr/bin/env -S scala shebang

// AoC 2024 Day 15

import scala.language.strictEquality
import scala.io.Source

enum Direction derives CanEqual:
    case Up, Down, Left, Right

    def vertical = this match
        case Up | Down => true
        case _ => false

object Direction:
    def from(t: Token): Direction = t match
        case Token.Up => Up
        case Token.Down => Down
        case Token.Left => Left
        case Token.Right => Right
        case _ => throw new IllegalArgumentException(s"Invalid direction: $t")

given Conversion[Direction, Point] with
    def apply(d: Direction): Point = d match
        case Direction.Up => (-1, 0)
        case Direction.Down => (1, 0)
        case Direction.Left => (0, -1)
        case Direction.Right => (0, 1)

enum Entity derives CanEqual:
    case Box, Robot, Wall
    
    override def toString(): String = this match
        case Entity.Robot => "@"
        case Entity.Wall => "##"
        case Entity.Box => "[]"

    def width(): Int = this match
        case Entity.Robot => 1
        case _ => 2

object Entity:
    def from(t: Token): Entity = t match
        case Token.At => Robot
        case Token.Hash => Wall
        case Token.O => Box
        case _ => throw new IllegalArgumentException(s"Invalid entity: $t")


class Grid(val dims: (Int, Int), val entities: Map[Point, Entity]) extends Iterable[(Point, Entity)]:
    def at(p: Point): Option[(Point, Entity)] =
        this.entities.get(p).map(p -> _).orElse {
            val neigh = p + (0, -1)
            // important: discard single entities
            this.entities.get(neigh).filter(e => e.width() > 1).map(neigh -> _)
        }

    def contains(p: Point): Boolean = this.entities.contains(p)

    def dump(): Unit =
        enum State derives CanEqual:
            case Free, Taken

        import State.*

        val (rows, cols) = this.dims

        (0 until rows).foreach { i =>
            (0 until cols).foldLeft(Free) {
                case (Free, j) => 
                    val entity = entities.get((i, j))
                    val (s, width) = entity match
                        case Some(entity) => (entity.toString, entity.width())
                        case None => (".", 1)
                    
                    print(s)

                    if width > 1 then Taken else Free
                case (Taken, _) => Free
            }

            println()
        }

    def entityAt(p: Point): Option[(Vector[Point], Entity)] = 
        this.at(p).map { case ((base, entity)) =>
            entity.width() match
                case 1 => (Vector(p), entity)
                case 2 => 
                    val vec = if base == p then
                        Vector(p, p + (0, 1))
                    else 
                        Vector(base, p)
                    (vec, entity)
                case _ => throw new UnsupportedOperationException(s"Invalid entity width: $entity")

        }

    def inBounds(p: Point): Boolean = 
        val (i, j) = p
        val (rows, cols) = this.dims
        i >= 0 && i < rows && j >= 0 && j < cols

    def iterator: Iterator[(Point, Entity)] = this.entities.iterator

    def move(from: Point, to: Point) : Grid = 
        this.contains(from) && !this.contains(to) match
            case true => 
                val kind = entities(from)
                new Grid(dims, entities.removed(from).updated(to, kind))
            case false => this

    def push(point: Point, dir: Direction) : (Grid, Point) = 
        this.nudge(point, dir) match
            case Some((_, grid)) => (grid, point + dir)
            case None => (this, point)

    private def nudge(point: Point, dir: Direction): Option[(Space, Grid)] =
        this.inBounds(point) match
            case false => None
            case true => this.entityAt(point) match
                case Some((v, Entity.Wall)) => None
                case Some((points, entity)) =>
                    val head = points.head

                    import Direction.*
                    dir match
                        case Up | Down =>
                            // we need to check all points, we're wide and short
                            points.foldLeft[Option[Grid]](Some(this)) {
                                case (Some(grid), p) => grid.nudge(p + dir, dir) match
                                    case Some((Space, newGrid)) => Some(newGrid)
                                    case _ => None
                                case _ => None
                            }.map(grid => (Space, grid.move(head, head + dir)))
                        case Left =>
                            // do part 1 nudging instead
                            this.nudge(head + dir, dir) match
                                case Some((_, grid)) => Some(Space, grid.move(head, head + dir))
                                case None => None
                        case Right =>
                            val last = points.last

                            // do part 1 nudging instead, but taking last into account
                            this.nudge(last + dir, dir) match
                                case Some((_, grid)) => Some(Space, grid.move(head, head + dir))
                                case None => None
                case None => Some((Space, this))

object Grid:
    def apply(dims: (Int, Int), entities: Vector[(Point, Entity)]): Grid = 
        val (rows, cols) = dims
        new Grid((rows, cols * 2), entities.map {
            case ((i, j), entity) => (i, j * 2) -> entity
        }.toMap)

enum ParseState derives CanEqual:
    case Start
    case ReadingGrid(p: Point, bot: Option[Point], entities: Vector[(Point, Entity)])
    case CarriageReturn(p: Point, bot: Option[Point], entities: Vector[(Point, Entity)])
    case ReadingInstructions(entities: Grid, bot: Point, instructions: Vector[Direction])

    def unwrap() = this match
        case ReadingInstructions(grid, bot, instructions) => (grid, bot, instructions)
        case _ => throw new IllegalArgumentException(s"Invalid state: $this")

type Point = (Int, Int)

extension (p: Point)
    def +(other: Point): Point = (p._1 + other._1, p._2 + other._2)
    def <(other: Point): Boolean = p._1 < other._1 || (p._1 == other._1 && p._2 < other._2)

object Space
type Space = Space.type


enum Token derives CanEqual:
    case At, Dot, Down, Hash, O, Left, Newline, Right, Up

object Token:
    def fromChar(c: Char): Token = c match
        case '@' => At
        case '.' => Dot
        case 'v' => Down
        case '#' => Hash
        case 'O' => O
        case '<' => Left
        case '\n' => Newline
        case '>' => Right
        case '^' => Up
        // not a fan, must say, but when in rome...
        case _ => throw new IllegalArgumentException(s"Invalid token: $c") 

def parseInput(path: String) =
    import Entity.*, ParseState.*, Token.*
    Source
        .fromFile(path)
        .map(Token.fromChar)
        .foldLeft(Start) { 
            case (Start, Hash) => ReadingGrid((0, 1), None, Vector((0, 0) -> Wall))
            case (ReadingGrid((i, j), bot, entities), t @ (Hash | O)) => ReadingGrid((i, j + 1), bot, entities :+ (i, j) -> Entity.from(t))
            case (ReadingGrid((i, j), None, entities), At) => ReadingGrid((i, j + 1), Some((i, j)), entities :+ (i, j) -> Robot)
            case (ReadingGrid((i, j), bot, entities), Dot) => ReadingGrid((i, j + 1), bot, entities)
            case (ReadingGrid((i, j), bot, entities), Newline) => CarriageReturn((i, j), bot, entities)
            case (CarriageReturn((i, j), bot, entities), t @ (Hash | O)) => ReadingGrid((i + 1, 1), bot, entities :+ (i + 1, 0) -> Entity.from(t))
            case (CarriageReturn((i, j), None, entities), At) => ReadingGrid((i + 1, 1), Some((i + 1, 0)), entities :+ (i + 1, 0) -> Robot)
            case (CarriageReturn((i, j), bot, entities), Dot) => ReadingGrid((i + 1, 0), bot, entities)
            case (CarriageReturn((i, j), Some((b_i, b_j)), entities), Newline) => ReadingInstructions(Grid((i + 1, j), entities), (b_i, b_j * 2), Vector.empty)
            case (ReadingInstructions(grid, bot, instructions), t @ (Up | Down | Left | Right)) => ReadingInstructions(grid, bot, instructions :+ Direction.from(t))
            case (ReadingInstructions(grid, bot, instructions), Newline) => ReadingInstructions(grid, bot, instructions)

            case (ReadingGrid((i, j), Some(bot), _), At) => throw new IllegalArgumentException(s"Duplicate robot at: ${(i, j)}, existing: $bot")
            case (CarriageReturn((i, j), Some(bot), _), At) => throw new IllegalArgumentException(s"Duplicate robot at: ${(i, j)}, existing: $bot")
            case (CarriageReturn((i, j), None, entities), Newline) => throw new IllegalArgumentException(s"No robot found in input")
            case (state, token) => throw new IllegalArgumentException(s"Invalid token: $token in state: $state")
        }.unwrap()

def part2(grid: Grid, bot: Point, instructions: Vector[Direction]) =
    val (finalGrid, _) = instructions.foldLeft((grid, bot)) {
        case ((grid, bot), dir) => grid.push(bot, dir)
    }

    finalGrid.dump()

    finalGrid.foldLeft(0) {
        case (acc, (p@(x, y), Entity.Box)) => 
            println(s"box at $p")
            acc + x * 100 + y
        case (acc, _) => acc
    }

@main def p15(file: String) =
    var (grid, bot, instructions) = parseInput(file)
    
    println(s"Bot at: ${grid.entityAt(bot)}")
    
    grid.dump()

    val result = part2(grid, bot, instructions)

    println(s"part2: $result")
