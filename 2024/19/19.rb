#!/usr/bin/env ruby

# AoC 2024 Day 19

class Designs
    def initialize(instr)
        @instr = instr
    end

    def filtered_for(test)
        Designs.new @instr.filter { |chunk| test.include? chunk }
    end

    def matches(test)
        # get all instructions that start with the test and sort them from longest to shortest
        @instr.filter_map do |instr|
            if test.start_with? instr
                [instr, test[instr.length..]]
            end 
        end.sort { |a, b| b <=> a }
    end

    def to_s
        @instr.to_s
    end
end

def read_file(fname)
    instr, tests = File.read(fname).split(/\n\n+/)

    instr = Designs.new instr.chomp.split /,\s*/

    tests = tests.lines.each(&:chomp!)

    return instr, tests
end

def test(designs, pattern)
    return true if pattern.empty?
   
    # try to match the pattern with the designs
    # then, for every match, try to match the rest of the pattern again. We go for the longest possible patterns first
    # the algorithm backtracks to the next match if the current one fails
    designs.matches(pattern).any? { |_, rest| test(designs, rest) }
end

# algorithm: given a set of designs and a pattern, we compute 
# m = matches(pattern)
# where a match is an array of tuples (d, rest)
# it's easy to demonstrate that count(pattern) = Σ count(rest) ∀ (d, rest) ∈ m - we basically are creating a tree of all
# possible solutions, and the number of nodes under a given node is the sum of the number of nodes under its children
# note that memoisation is basically mandatory here; there's a large number of overlapping subproblems where we would 
# otherwise end up recomputing the same thing over and over again
def count_all(designs, pattern, cache)
    # if the pattern is empty, we have found a terminal
    return 1 if pattern.empty?

    cached = cache[pattern]
    return cached if cached

    result = designs.matches(pattern).map { |_, rest| count_all(designs, rest, cache) }.sum

    cache[pattern] = result

    result
end

# unused, finds all possible solutions for a pattern - unusable for the input, useful for debugging
def find_all(designs, pattern, found = [], cache = {})
    return [found] if pattern.empty?

    cached = cache[pattern]
    return cached if cached

    result = designs.matches(pattern).inject([]) do |solutions, (instr, rest)|
        solutions + find_all(designs, rest, found + [instr], cache)
    end

    cache[pattern] = result

    result
end

def part1(designs, tests)
    tests.filter { |test| test(designs, test) }
end

def part2(designs, tests)
    tcache = {}

    tests.map do |test|
        # filtered_for removes uses trival string matching to remove all instructions that are not in the test at all
        # this literally halves the runtime (from 356ms to 180ms on my machine)
        count_all(designs.filtered_for(test), test, tcache)
    end.sum
end

def main(args)
    if args.length != 1
        STDERR.puts "error: too few or too many arguments\nusage: #{$0} INPUT"
        exit 2
    end
    
    designs, tests = read_file(args[0])

    good = part1(designs, tests)
    puts "part 1: #{good.count}"
    puts "part 2: #{part2(designs, good)}"
end

if __FILE__ == $0
    main(ARGV)
end
