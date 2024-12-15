% part2 of AoC 2024 day 3
% this is my first time writing erlang, so please be kind.
% also I've used `maybe`, so you need at least OTP 27 for this to work

-module(main).
-export([main/1]).

main([File]) -> 
    {ok, Bytes} = file:read_file(File),
    Content = binary_to_list(Bytes),
    erlang:display(parse(Content));

main(_) -> exit(invalid).

% a way simpler but _way_ less fun version of this 
% would have been to simply s/don't\(\).*?do\(\)//g the input string and the run part1 again... but where's the fun in that?
parse(Content) -> parse_and_mul(0, Content).

parse_and_mul(N, Content) ->
    case to_next(Content) of
        [] -> {N, []};
        MulStart -> 
            {R, Rest} = do_mul(MulStart),
            parse_and_mul(N + R, Rest)
    end.

do_mul(Content) ->
    maybe
        {I, [$, | AfterComma]} ?= int(Content),
        {J, [$) | Rest]} ?= int(AfterComma),
        {I * J, Rest}
    else
        badint -> {0, Content}; % skip this sequence, go ahead
        {_, _} -> {0, Content} % we matched a random int that was malformed, go ahead
    end.

% check first digit, else fail. The recursive call will be able to fail though
int([$-, C | Content]) when C >= $0, C =< $9 -> 
    {N, Rest} = int_rec(0, [C | Content]),
    {-N, Rest};

int([C | _] = Content) when C >= $0, C =< $9 -> int_rec(0, Content);
int(_) -> badint.

% internal recursive int parsing that returns 0 if it's not an number
int_rec(N, [C | Rest]) when C >= $0, C =< $9 -> int_rec(N * 10 + C - $0, Rest);
int_rec(N, Rest) -> {N, Rest}.

to_next([]) -> [];
to_next(Content) ->
    NextMul = next_mul(Content),
    NextDont = next_dont(Content),
    case {NextMul, NextDont} of
        {[], _} -> [];     % no more mul(), quit
        {_, []} -> NextMul; % no more don't(), continue with mul()
        
        % if don't() comes before the next mul, skip everything until the next do()
        _ when length(NextMul) < length(NextDont) -> to_next(next_do(NextDont));

        _ -> NextMul % otherwise, continue with mul()
    end.

next_mul(Content) ->
    case string:find(Content, "mul(") of 
        nomatch -> [];
        Start -> string:slice(Start, 4)
    end.

next_do(Content) ->
    case string:find(Content, "do()") of
        nomatch -> [];
        Start -> string:slice(Start, 4)
    end.

next_dont(Content) ->
    case string:find(Content, "don't()") of
        nomatch -> [];
        Start -> string:slice(Start, 7)
    end.