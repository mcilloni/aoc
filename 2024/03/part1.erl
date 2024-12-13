%hello world program
-module(main).
-export([main/1]).

main([File]) -> 
    {ok, Bytes} = file:read_file(File),
    Content = binary_to_list(Bytes),
    erlang:display(parse(Content));

main(_) -> exit(invalid).

parse(Content) -> 
    case re:run(Content, "mul\\((?<op1>\\d+),(?<op2>\\d+)\\)", [global, {capture, ["op1", "op2"], list}]) of
        {_, Matches} -> lists:sum(lists:map(fun([Op1, Op2]) -> int(Op1) * int(Op2) end, Matches));
        nomatch -> 0
    end.

int(String) -> 
    case string:to_integer(String) of
        {N, []} -> N;
        _ -> exit("Invalid integer: " ++ String)
    end.