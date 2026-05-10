// AoC 2025 Day 10
// run with java -cp /path/to/com.microsoft.z3.jar --enable-native-access=ALL-UNNAMED P10.java input

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.microsoft.z3.*;

public class P10 {

    enum Light { OFF, ON }
    enum LogPolicy { NONE, ALL }

    record Args(String fname, LogPolicy logPolicy) {
        public static Args parse(final String[] args) {
            String fname = null;
            LogPolicy logPolicy = LogPolicy.NONE;

            for (final var arg : args) {
                if (arg.equals("--verbose") || arg.equals("-v")) {
                    logPolicy = LogPolicy.ALL;
                } else if (fname == null && !arg.startsWith("-")) {
                    fname = arg;
                } else {
                    throw new IllegalArgumentException("unexpected argument: " + arg);
                }
            }

            if (fname == null) {
                throw new IllegalArgumentException("missing input file argument");
            }

            return new Args(fname, logPolicy);
        }
    }
    
    record Button(Set<Integer> indices) {
        public boolean touches(final int idx) {
            return indices.contains(idx);
        }
    }

    record Machine(List<Light> goal, List<Button> buttons, List<Integer> requirements) {}

    public record Pair<K, V>(K key, V value) {
        public static <J, U> Pair<J, U> of(final J key, final U value) {
            return new Pair<>(key, value);
        }
    }

    sealed interface Parse<T> permits Parse.Hit, Parse.Miss {
        record Hit<T>(T value, String rest) implements Parse<T> {}
        record Miss<T>(String rest) implements Parse<T> {}
    }

    public static class MalformedInputException extends RuntimeException {
        // note: a runtime exception because a. streams don't like checked exceptions and
        // b. checked exceptions are arguably stupid
        public MalformedInputException(final String message) {
            super(message);
        }

        public MalformedInputException(final String message, final Throwable cause) {
            super(message, cause);
        }
    }

    // Helpers

    static void log(final LogPolicy policy, final String message, final Object... args) {
        if (policy == LogPolicy.ALL) {
            System.out.printf(message, args);
            System.out.println();
        }
    }

    static List<Light> click(final List<Light> lights, final Button button) {
        final var result = new ArrayList<>(lights);

        for (final int idx : button.indices()) {
            result.set(idx, result.get(idx) == Light.ON ? Light.OFF : Light.ON);
        }

        return List.copyOf(result);
    }

    // Parser combinators

    static Parse<Character> maybeExpect(final String input, final char... expected) {
        if (!input.isEmpty()) {
            final char c = input.charAt(0);
            for (final char e : expected) {
                if (c == e) return new Parse.Hit<>(c, input.substring(1));
            }
        }

        return new Parse.Miss<>(input);
    }

    static Parse.Hit<Character> expect(final String input, final char... expected) {
        return switch (maybeExpect(input, expected)) {
            case Parse.Hit<Character> hit -> hit;
            case Parse.Miss<Character> _ -> {
                final var got = input.isEmpty() ? ">>EOF<<" : String.valueOf(input.charAt(0));
                if (expected.length == 1) {
                    throw new MalformedInputException(
                        "expected '%c', got '%s'".formatted(expected[0], got));
                }
                throw new MalformedInputException(
                    "expected one of [%s], got '%s'".formatted(Arrays.toString(expected), got));
            }
        };
    }

    static Parse<Integer> maybeNumber(final String input) {
        int i = 0;
        while (i < input.length() && Character.isDigit(input.charAt(i))) {
            ++i;
        }

        if (i == 0) {
            return new Parse.Miss<>(input);
        }

        final int n = Integer.parseInt(input, 0, i, 10);

        if (n > 0xFFFF) {
            throw new MalformedInputException("number out of range: " + n);
        }

        return new Parse.Hit<>(n, input.substring(i));
    }

    static String skipWhitespace(final String input) {
        int i = 0; 
        for (; i < input.length() && Character.isWhitespace(input.charAt(i)); ++i);

        return input.substring(i);
    }

    static Parse<Light> tryParseLight(final String input) {
        return switch (maybeExpect(input, '#', '.')) {
            case Parse.Hit<Character>(final var c, final var rest) ->
                new Parse.Hit<>(c == '#' ? Light.ON : Light.OFF, rest);
            case Parse.Miss<Character> _ -> new Parse.Miss<>(input);
        };
    }

    static Parse.Hit<List<Light>> parseLights(final String input) {
        var rest = expect(input, '[').rest();
        final var lights = new ArrayList<Light>();

        while (tryParseLight(rest) instanceof Parse.Hit<Light>(final var light, final var remainder)) {
            lights.add(light);
            rest = remainder;
        }

        rest = expect(rest, ']').rest();
        
        return new Parse.Hit<>(List.copyOf(lights), rest);
    }

    static Parse<Button> tryParseButton(final String input) {
        if (!(maybeExpect(input, '(') instanceof Parse.Hit<Character>(final var _, final var afterParen))) {
            return new Parse.Miss<>(input);
        }

        final var indices = new ArrayList<Integer>();
        var rest = afterParen;

        while (maybeNumber(rest) instanceof Parse.Hit<Integer>(final var n, final var remainder)) {
            indices.add(n);
            final var term = expect(remainder, ')', ',');
            rest = term.rest();
            if (term.value() == ')') {
                return new Parse.Hit<>(new Button(Set.copyOf(indices)), rest);
            }
        }

        throw new MalformedInputException("empty or malformed button");
    }

    static Parse.Hit<List<Button>> parseButtons(final String input) {
        final var buttons = new ArrayList<Button>();
        var rest = input;

        while (tryParseButton(skipWhitespace(rest)) instanceof Parse.Hit<Button> hit) {
            buttons.add(hit.value());
            rest = hit.rest();
        }

        return new Parse.Hit<>(List.copyOf(buttons), rest);
    }

    static Parse.Hit<List<Integer>> parseRequirements(final String input) {
        var rest = expect(input, '{').rest();
        final var reqs = new ArrayList<Integer>();

        for (;;) {
            if (!(maybeNumber(rest) instanceof Parse.Hit<Integer>(final var n, final var remainder))) {
                break;
            }

            reqs.add(n);
            final var term = expect(remainder, '}', ',');
            rest = term.rest();
            if (term.value() == '}') {
                return new Parse.Hit<>(List.copyOf(reqs), rest);
            }
        }

        throw new MalformedInputException("unterminated requirements block");
    }

    static Machine parseMachine(final String line) {
        final var lights = parseLights(line);
        final var buttons = parseButtons(skipWhitespace(lights.rest()));
        final var reqs = parseRequirements(skipWhitespace(buttons.rest()));

        final var trailing = skipWhitespace(reqs.rest());
        if (!trailing.isEmpty()) {
            throw new IllegalArgumentException(
                "unexpected input after requirements: '%s'".formatted(trailing));
        }

        return new Machine(lights.value(), buttons.value(), reqs.value());
    }

    static List<Machine> parseInput(final String fname) throws IOException {
        try (final var lines = Files.lines(Path.of(fname))) {
            return lines.map(P10::parseMachine).toList();
        }
    }

    // Part 1 solution (BFS + memoisation)

    static int solveMachine1(final Machine machine) {
        // BFS: we explore the entire solution space, going from button press to button press
        // I'm trying BFS instead of DFS because we don't want A solution but the *best* solution
        // I also do memoisation by keeping track of a. shortest path found yet b. what states we have already 
        // traversed, because all trees would be identical to one already traversed otherwise
        // I could have probably used Z3 for this too but it would have been less fun

        record Visited(List<Light> lights, int button) {}
        record State(List<Light> lights, int button, int depth) {}

        final var goal = machine.goal();
        final var buttons = machine.buttons();

        var best = Integer.MAX_VALUE;

        final var initial = Collections.nCopies(goal.size(), Light.OFF);
        final var queue = new ArrayDeque<State>(List.of(new State(initial, -1, 0)));
        
        final var seen = new HashSet<Visited>();

        while (!queue.isEmpty()) {
            var node = queue.poll();
            seen.add(new Visited(node.lights(), node.button()));

            if (node.lights().equals(goal)) {
                best = Math.min(best, node.depth());
                continue;
            }

            if (node.depth() >= best) {
                continue;
            }

            for (int i = 0; i < buttons.size(); ++i) {
                var newLights = click(node.lights(), buttons.get(i));
                if (!seen.contains(new Visited(newLights, i))) {
                    queue.add(new State(newLights, i, node.depth() + 1));
                }
            }
        }

        return best;
    }

    @SuppressWarnings("unchecked")
    static long solveMachine2(final Context ctx, final Machine machine) {
        final var opt = ctx.mkOptimize();

        final var buttons = machine.buttons();
        final var requirements = machine.requirements();

        if (buttons.size() <= 0 || requirements.size() <= 0) {
            throw new IllegalArgumentException("machine must have at least one button and one requirement");
        }

        final var vars = new ArrayList<Pair<Button, IntExpr>>(buttons.size());
        for (int i = 0; i < buttons.size(); ++i) {
            var var = ctx.mkIntConst("n" + i);
            vars.add(Pair.of(buttons.get(i), var));

            // note: this triggers warnings because Add() takes a vararg with generic types, which is unsafe
            // but there's no other way to do it, so that's what it is. That's what the @SuppressWarnings("unchecked")
            // is for
            opt.Add(ctx.mkGe(var, ctx.mkInt(0)));
        }
        
        // for every joltage, generate an expression where j = sum of all button presses that affect j, where the button
        // presses are the variables we defined above
        
        for (int req_no = 0; req_no < requirements.size(); ++req_no) {
            final var req = requirements.get(req_no);
            final var affectingButtons = new ArrayList<IntExpr>();

            for (final var pair : vars) {
                if (pair.key().touches(req_no)) {
                    affectingButtons.add(pair.value());
                }
            }

            opt.Add(ctx.mkEq(ctx.mkInt(req), ctx.mkAdd(affectingButtons.toArray(IntExpr[]::new))));
        }

        // finally, we want to minimise the total number of button presses, which is the sum of all variables
        final var min = opt.MkMinimize(ctx.mkAdd(vars.stream().map(Pair::value).toArray(IntExpr[]::new)));

        if (opt.Check() != Status.SATISFIABLE) {
            throw new IllegalStateException("machine %s is unsolvable".formatted(machine));
        }

        final var asInt = (IntNum) min.getValue();

        return asInt.getInt64();
    }

    static int part1(final List<Machine> machines, final LogPolicy logPolicy) {
        var total = 0;

        for (int i = 0; i < machines.size(); ++i) {
            var best = solveMachine1(machines.get(i));
            
            log(logPolicy, "machine #%d: best solution is %d", i, best);
            
            if (best == Integer.MAX_VALUE) {
                throw new IllegalStateException("no solution found for machine #" + i);
            }

            total += best;
        }

        return total;
    }

    static long part2(final List<Machine> machines, final LogPolicy logPolicy) {
        long total = 0;
        final var ctx = new Context();

        for (int i = 0; i < machines.size(); ++i) {
            final var machine = machines.get(i);
            final var best = solveMachine2(ctx, machine);
            
            log(logPolicy, "machine #%d: best solution is %d", i, best);

            total += best;
        }

        return total;
    }

    public static void main(String[] args) throws IOException {
        final Args parsedArgs;
        
        try {
            parsedArgs = Args.parse(args);
        } catch (IllegalArgumentException e) {
            System.err.println("error: " + e.getMessage());
            System.exit(2);
            return;
        }

        final var machines = parseInput(parsedArgs.fname());

        final var logPolicy = parsedArgs.logPolicy();
        
        System.out.println("part 1: " + part1(machines, logPolicy));
        System.out.println("part 2: " + part2(machines, logPolicy));
    }
}
