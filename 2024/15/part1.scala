#!/usr/bin/env -S scala shebang

import scala.language.strictEquality
import scala.io.Source

enum Direction derives CanEqual:
    case Up, Down, Left, Right

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

object Entity:
    def from(t: Token): Entity = t match
        case Token.At => Robot
        case Token.Hash => Wall
        case Token.O => Box
        case _ => throw new IllegalArgumentException(s"Invalid entity: $t")

given Conversion[Entity, Char] with
    def apply(e: Entity): Char = e match
        case Entity.Robot => '@'
        case Entity.Wall => '#'
        case Entity.Box => 'O'

class Grid(val dims: (Int, Int), var entities: Map[Point, Entity]) extends Iterable[(Point, Entity)]:
    def at(p: Point): Option[Entity] = this.entities.get(p)

    def contains(p: Point): Boolean = this.entities.contains(p)

    def dump(): Unit = 
        val (rows, cols) = this.dims
        for 
            i <- 0 until rows
            j <- 0 until cols
        do
            print(entities.get((i, j)).map(_.toChar).getOrElse('.'))
            if j == cols - 1 then println()

    def inBounds(p: Point): Boolean = 
        val (i, j) = p
        val (rows, cols) = this.dims
        i >= 0 && i < rows && j >= 0 && j < cols

    def iterator: Iterator[(Point, Entity)] = this.entities.iterator

    def move(from: Point, to: Point) : Boolean = 
        this.contains(from) && !this.contains(to) match
            case true => 
                val kind = entities(from)
                entities = entities.removed(from).updated(to, kind)
                true
            case false => false

    def push(point: Point, dir: Direction) : Option[Point] = 
        this.nudge(point, dir) match
            case Some(_) => Some(point + dir)
            case None => None

    private def nudge(point: Point, dir: Direction): Option[Space] =
        this.inBounds(point) match
            case false => None
            case true => this.at(point) match
                case Some(Entity.Wall) => None
                case Some(entity) => this.nudge(point + dir, dir) match
                    case Some(_) => 
                        this.move(point, point + dir)
                        Some(Space)
                    case None => None
                
                case None => Some(Space)

object Grid:
    def apply(dims: (Int, Int), entities: Vector[(Point, Entity)]): Grid = 
        new Grid(dims, entities.toMap)

extension (p: Point)
    def +(other: Point): Point = (p._1 + other._1, p._2 + other._2)

enum ParseState derives CanEqual:
    case Start
    case ReadingGrid(p: Point, bot: Option[Point], entities: Vector[(Point, Entity)])
    case CarriageReturn(p: Point, bot: Option[Point], entities: Vector[(Point, Entity)])
    case ReadingInstructions(entities: Grid, bot: Point, instructions: Vector[Direction])

    def unwrap() = this match
        case ReadingInstructions(grid, bot, instructions) => (grid, bot, instructions)
        case _ => throw new IllegalArgumentException(s"Invalid state: $this")

type Point = (Int, Int)

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
            case (CarriageReturn((i, j), Some(bot), entities), Newline) => ReadingInstructions(Grid((i + 1, j), entities), bot, Vector.empty)
            case (ReadingInstructions(grid, bot, instructions), t @ (Up | Down | Left | Right)) => ReadingInstructions(grid, bot, instructions :+ Direction.from(t))
            case (ReadingInstructions(grid, bot, instructions), Newline) => ReadingInstructions(grid, bot, instructions)

            case (ReadingGrid((i, j), Some(bot), _), At) => throw new IllegalArgumentException(s"Duplicate robot at: ${(i, j)}, existing: $bot")
            case (CarriageReturn((i, j), Some(bot), _), At) => throw new IllegalArgumentException(s"Duplicate robot at: ${(i, j)}, existing: $bot")
            case (CarriageReturn((i, j), None, entities), Newline) => throw new IllegalArgumentException(s"No robot found in input")
            case (state, token) => throw new IllegalArgumentException(s"Invalid token: $token in state: $state")
        }.unwrap()

def part1(grid: Grid, bot: Point, instructions: Vector[Direction]) =
    instructions.foldLeft((grid, bot)) { case ((grid, bot), dir) =>
        grid.push(bot, dir) match
            case Some(newPoint) => (grid, newPoint)
            case None => (grid, bot)
    }

    grid.foldLeft(0) {
        case (acc, (p@(x, y), Entity.Box)) => 
            println(p)
            acc + x * 100 + y
        case (acc, _) => acc
    }

@main def p15(file: String) =
    var (grid, bot, instructions) = parseInput(file)
    grid.dump()

    println(s"Bot at: $bot")

    println(s"$instructions\n") 

    val result = part1(grid, bot, instructions)

    grid.dump()

    println(s"part1: $result")
