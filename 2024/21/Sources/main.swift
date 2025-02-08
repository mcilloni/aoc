// AoC 2024 Day 21

import Foundation

struct Point {
    let x: Int
    let y: Int

    init(_ x: Int, _ y: Int) {
        self.x = x
        self.y = y
    }

    static func - (lhs: Point, rhs: Point) -> Point {
        return Point(lhs.x - rhs.x, lhs.y - rhs.y)
    }
}

/*
 * +---+---+---+
 * | 7 | 8 | 9 |
 * +---+---+---+
 * | 4 | 5 | 6 |
 * +---+---+---+
 * | 1 | 2 | 3 |
 * +---+---+---+
 *     | 0 | A |
 *     +---+---+
 */
enum Key: Character, Equatable {
    case Zero = "0"
    case One = "1"
    case Two = "2"
    case Three = "3"
    case Four = "4"
    case Five = "5"
    case Six = "6"
    case Seven = "7"
    case Eight = "8"
    case Nine = "9"

    case A = "A"

    func coord() -> Point {
        switch self {
        case .Zero:
            return Point(3, 1)
        case .One:
            return Point(2, 0)
        case .Two:
            return Point(2, 1)
        case .Three:
            return Point(2, 2)
        case .Four:
            return Point(1, 0)
        case .Five:
            return Point(1, 1)
        case .Six:
            return Point(1, 2)
        case .Seven:
            return Point(0, 0)
        case .Eight:
            return Point(0, 1)
        case .Nine:
            return Point(0, 2)
        case .A:
            return Point(3, 2)
        }
    }

    func goto(_ key: Key) -> Moves {
        return self.gotoDispatch(key) + [.A]
    }

    // The reasoning here is that in the dpad left is more expensive than up/down, which are more expensive than right.
    // This is due to left being further away from A, which causes every move that requires going left to baloon in cost
    // at every layer. This is why we always try to go left first, then up/down, then right, except when avoiding the hole
    private func gotoDispatch(_ key: Key) -> Moves {
        switch self {
        case .A:
            switch key {
            case .Zero:
                return [.Left]
            case .One:
                return [.Up, .Left, .Left]
            case .Two:
                return [.Left, .Up]
            case .Three:
                return [.Up]
            case .Four:
                return [.Up, .Up, .Left, .Left]
            case .Five:
                return [.Left, .Up, .Up]
            case .Six:
                return [.Up, .Up]
            case .Seven:
                return [.Up, .Up, .Up, .Left, .Left]
            case .Eight:
                return [.Left, .Up, .Up, .Up]
            case .Nine:
                return [.Up, .Up, .Up]
            case .A:
                return []
            }
        case .Zero:
            switch key {
            case .Zero:
                return []
            case .One:
                return [.Up, .Left]
            case .Two:
                return [.Up]
            case .Three:
                return [.Up, .Right]
            case .Four:
                return [.Up, .Up, .Left]
            case .Five:
                return [.Up, .Up]
            case .Six:
                return [.Up, .Up, .Right]
            case .Seven:
                return [.Up, .Up, .Up, .Left]
            case .Eight:
                return [.Up, .Up, .Up]
            case .Nine:
                return [.Up, .Up, .Up, .Right]
            case .A:
                return [.Right]
            }
        case .One:
            switch key {
            case .Zero:
                return [.Right, .Down]
            case .A:
                return [.Right, .Right, .Down]
            default:
                return gotoTrivial(from: self, to: key)
            }
        case .Two:
            switch key {
            case .Zero:
                return [.Down]
            case .A:
                return [.Down, .Right]
            default:
                return gotoTrivial(from: self, to: key)
            }
        case .Three:
            switch key {
            case .Zero:
                return [.Left, .Down]
            case .A:
                return [.Down]
            default:
                return gotoTrivial(from: self, to: key)
            }
        case .Four:
            switch key {
            case .Zero:
                return [.Right, .Down, .Down]
            case .A:
                return [.Right, .Right, .Down, .Down]
            default:
                return gotoTrivial(from: self, to: key)
            }
        case .Five:
            switch key {
            case .Zero:
                return [.Down, .Down]
            case .A:
                return [.Down, .Down, .Right]
            default:
                return gotoTrivial(from: self, to: key)
            }
        case .Six:
            switch key {
            case .Zero:
                return [.Left, .Down, .Down]
            case .A:
                return [.Down, .Down]
            default:
                return gotoTrivial(from: self, to: key)
            }
        case .Seven:
            switch key {
            case .Zero:
                return [.Right, .Down, .Down, .Down]
            case .A:
                return [.Right, .Right, .Down, .Down, .Down]
            default:
                return gotoTrivial(from: self, to: key)
            }
        case .Eight:
            switch key {
            case .Zero:
                return [.Down, .Down, .Down]
            case .A:
                return [.Down, .Down, .Down, .Right]
            default:
                return gotoTrivial(from: self, to: key)
            }
        case .Nine:
            switch key {
            case .Zero:
                return [.Left, .Down, .Down, .Down]
            case .A:
                return [.Down, .Down, .Down]
            default:
                return gotoTrivial(from: self, to: key)
            }
        }
    }
}

// trivial moves, i.e. those in the 3x3 grid that don't involve either 0 or A
// in those the logic is simple: go left first, then up or down, then right
// this is because left is more expensive than up/down, and up/down are more expensive than right, so it makes
// sense to go there first and then slowly move back towards A in the dpad
func gotoTrivial(from: Key, to: Key) -> Moves {
    let delta = (to.coord() - from.coord())

    let (dirX, dirY): (Move, Move) = (delta.x > 0 ? .Down : .Up, delta.y > 0 ? .Right : .Left)

    var moves = [Move]()

    if dirY == .Left {
        moves += Array(repeating: dirY, count: abs(delta.y))
    }

    moves += Array(repeating: dirX, count: abs(delta.x))

    if dirY == .Right {
        moves += Array(repeating: dirY, count: abs(delta.y))
    }

    return moves
}

/*
 *     +---+---+
 *     | ^ | A |
 * +---+---+---+
 * | < | v | > |
 * +---+---+---+
 */
enum Move: Character, Equatable, Hashable {
    case Up = "^"
    case Down = "v"
    case Left = "<"
    case Right = ">"
    case A = "A"

    func coord() -> Point {
        switch self {
        case .Up:
            return Point(0, 1)
        case .Down:
            return Point(1, 1)
        case .Left:
            return Point(1, 0)
        case .Right:
            return Point(1, 2)
        case .A:
            return Point(0, 2)
        }
    }

    func goto(_ move: Move) -> Moves {
        return self.gotoDispatch(move) + [.A]
    }

    private func gotoDispatch(_ move: Move) -> Moves {
        switch self {
        case .Up:
            switch move {
            case .Up:
                return []
            case .Down:
                return [.Down]
            case .Left:
                return [.Down, .Left]
            case .Right:
                return [.Down, .Right]
            case .A:
                return [.Right]
            }
        case .Down:
            switch move {
            case .Up:
                return [.Up]
            case .Down:
                return []
            case .Left:
                return [.Left]
            case .Right:
                return [.Right]
            case .A:
                return [.Up, .Right]
            }
        case .Left:
            switch move {
            case .Up:
                return [.Right, .Up]
            case .Down:
                return [.Right]
            case .Left:
                return []
            case .Right:
                return [.Right, .Right]
            case .A:
                return [.Right, .Right, .Up]
            }
        case .Right:
            switch move {
            case .Up:
                return [.Left, .Up]
            case .Down:
                return [.Left]
            case .Left:
                return [.Left, .Left]
            case .Right:
                return []
            case .A:
                return [.Up]
            }
        case .A:
            switch move {
            case .Up:
                return [.Left]
            case .Down:
                return [.Left, .Down]
            case .Left:
                return [.Down, .Left, .Left]
            case .Right:
                return [.Down]
            case .A:
                return []
            }
        }
    }
}

typealias Moves = [Move]

extension Moves {
    func asString() -> String {
        return self.map { String($0.rawValue) }.joined()
    }

    func compile() -> Moves {
        typealias State = (moves: Moves, last: Move)

        return self.reduce(State(Moves(), .A)) { (state, nextMove) in
            let (moves, lastMove) = state

            return (moves + lastMove.goto(nextMove), nextMove)
        }.moves
    }
}

enum ParseError: Error {
    case invalidCharacter(char: Character)
}

typealias Pin = [Key]

extension Pin {
    func asString() -> String {
        return self.map { String($0.rawValue) }.joined()
    }

    func compile() -> Moves {
        typealias State = (moves: Moves, last: Key)

        return self.reduce(State(Moves(), .A)) { (state, nextKey) in
            let (moves, lastKey) = state

            return (moves + lastKey.goto(nextKey), nextKey)
        }.moves
    }

    func asInt() -> Int {
        return Int(self.filter { $0 != .A }.map { String($0.rawValue) }.joined()) ?? 0
    }
}

func parseInput(fname: String) throws -> [Pin] {
    let input = try String(contentsOfFile: fname, encoding: .utf8)

    var pins = [Pin]()
    var pin = Pin()

    for chr in input {
        if chr == "\n" {
            pins.append(pin)

            pin = Pin()
        } else {
            guard let key = Key(rawValue: chr) else {
                throw ParseError.invalidCharacter(char: chr)
            }

            pin.append(key)
        }
    }

    if pin.count > 0 {
        pins.append(pin)
    }

    return pins
}

func part1(_ pins: [Pin]) -> Int {
    let pins = pins.map { ($0, $0.compile().compile().compile()) }

    for (pin, moves) in pins {
        print("\(pin.asString()): \(moves.asString())")
    }

    return pins.reduce(0) { (acc, item) in
        let (pin, moves) = item

        return acc + pin.asInt() * moves.count
    }
}

struct Entry: Hashable {
    let from, to: Move
    let depth: Int
}

func countMoves(pin: Pin, cache: inout [Entry: Int], maxDepth: Int) -> Int {
    typealias State = (moves: Int, last: Move)

    func countRecursive(_ entry: Entry) -> Int {
        if let cached = cache[entry] {
            return cached
        }

        let (from, to, depth) = (entry.from, entry.to, entry.depth)

        let transOps = from.goto(to)

        let result =
            if depth == 1 {
                transOps.count
            } else {
                transOps.reduce(State(0, .A)) { (state, next) in
                    let (moves, last) = state

                    return State(
                        moves + countRecursive(Entry(from: last, to: next, depth: depth - 1)), next)
                }.moves
            }

        cache[entry] = result

        return result
    }

    let moves = pin.compile()

    return moves.reduce(State(0, .A)) { (state, next) in
        let (moves, last) = state

        return State(moves + countRecursive(Entry(from: last, to: next, depth: maxDepth)), next)
    }.moves
}

func computeScore(pins: [Pin], maxDepth: Int = 25) -> Int {
    var cache = [Entry: Int]()
    return pins.reduce(0) { (acc, pin) in
        return acc + pin.asInt() * countMoves(pin: pin, cache: &cache, maxDepth: maxDepth)
    }
}

func part2(_ pins: [Pin]) -> Int {
    return computeScore(pins: pins)
}

if CommandLine.arguments.count != 2 {
    print("Usage: \(CommandLine.arguments[0]) INPUT")

    exit(2)
}

do {
    let pins = try parseInput(fname: CommandLine.arguments[1])

    print("Part 1: \(part1(pins))")
    print("Part 1 (part2 logic): \(computeScore(pins: pins, maxDepth: 2))")
    print("Part 2: \(part2(pins))")
} catch {
    print("error: \(error)")

    exit(1)
}
