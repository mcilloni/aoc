% AoC 2025 day 05

-module(main).
-export([main/1]).

main([File]) -> 
    {ok, Bytes} = file:read_file(File),
    Content = binary_to_list(Bytes),
    {Ranges, IDs} = parse(Content),
    io:format("Part1: ~p~n", [part1(Ranges, IDs)]),
    io:format("Part2: ~p~n", [part2(Ranges)]);

main(_) -> exit(invalid).

% Parses a list of IDs from the content, separated by whitespace (newlines in the input)
ids(Content) -> ids_rec([], Content).

% recursive helper to accumulate IDs
ids_rec(List, Content) ->
    case int(skip_ws(Content)) of
        badint -> {lists:reverse(List), Content}; % badint means we are out of ints in the input
        {I, Rest} -> ids_rec([I | List], Rest) % appending to the right is very bad in erlang, so we add at start and reverse at the end
    end.

% returns true if ID is in the given range [Start, End] inclusive
in_range(ID, [Start, End]) -> ID >= Start andalso ID =< End.

% returns true if ID is in any of the given ranges
in_ranges(ID, [Range | Ranges]) ->
    case in_range(ID, Range) of
        true -> true;
        false -> in_ranges(ID, Ranges)
    end;
        
in_ranges(_, []) -> false.

% parses an integer from the start of Content, returns {Int, Rest} or badint
int([C | _] = Content) when C >= $0, C =< $9 -> int_rec(0, Content);
int(_) -> badint.

% internal recursive int parsing that returns 0 if it's not an number
int_rec(N, [C | Rest]) when C >= $0, C =< $9 -> int_rec(N * 10 + C - $0, Rest);
int_rec(N, Rest) -> {N, Rest}.

% merges two ranges if they overlap, otherwise returns nonoverlapping
% Note: this function assumes the two ranges are strictly sorted
merge([S1, E1], [S2, E2]) when E1 >= S2 -> [S1, max(E1, E2)];
merge(_, _) -> nonoverlapping.

% merges all overlapping ranges in a sorted list of ranges recursively
merge_all([R1, R2 | Rest]) ->
    case merge(R1, R2) of
        nonoverlapping -> [R1 | merge_all([R2 | Rest])]; % keep R1 and continue
        Merged -> merge_all([Merged | Rest])             % discard R1 and R2, continue with their merged result
    end;

% special terminal cases for one element and empty lists
merge_all([R]) -> [R];
merge_all([]) -> [].

% parses the input file
parse(Content) -> 
    maybe
        {Ranges, AfterRanges} ?= ranges(Content),  % first, read all ranges
        {IDs, Rest} ?= ids(AfterRanges),   % then read all IDs
        [] ?= skip_ws(Rest),                        % assert that there's nothing left in the input but whitespace        
        SortedRanges = lists:sort(fun ([S1|_], [S2,_]) -> S1 =< S2 end, Ranges), % sort the ranges by start value
        {merge_all(SortedRanges), IDs} % merge all ranges and return them along with the IDs
    else
        badrange -> invalid_input; % malformed range found
        _ -> invalid_input % stray input that doesn't parse as ranges or ids
    end.

% part1: counts how many IDs are contained in at least one of the ranges
part1(Ranges, IDs) ->
    lists:sum([1 || ID <- IDs, in_ranges(ID, Ranges)]).

% part2: counts how many integers are covered by all ranges. By merging them, we can avoid double counting
part2(MergedRanges) ->
    lists:sum([End - Start + 1 || [Start, End] <- MergedRanges]).

% parses a range of the form "Start-End" from the start of Content, returns {[Start, End], Rest} or badrange
range(Content) ->
    maybe
        {Start, [$- | AfterDash]} ?= int(Content),
        {End, Rest} ?= int(AfterDash),
        {[Start, End], Rest}
    else
        badint -> badrange;
        {_, _} -> badrange  % we matched something that started with an int but was not a range
    end.

ranges(Content) -> ranges_rec([], Content).

% recursive helper to accumulate ranges
ranges_rec(List, Content) ->
    case range(skip_ws(Content)) of
        badrange -> {lists:reverse(List), Content}; % badrange means we are out of ranges in the input
        {Range, Rest} -> ranges_rec([Range | List], Rest) % appending to the right is very bad in erlang, so we add at start and reverse at the end
    end.

% skips some whitespace characters at the start of Content
skip_ws([C | Rest]) when C == $ ; C == $\n; C == $\t -> skip_ws(Rest);
skip_ws(Rest) -> Rest.
