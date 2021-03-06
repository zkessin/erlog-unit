-module(erlog_unit).
-compile({parse_transform, seqbind}).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-type test_name() ::string().
-type test_result() :: {test_name(), boolean()}.




find_prolog_files(Dir) ->
    Path = Dir ++ "/*.pl",
    Files = filelib:wildcard(Path),
    {ok, Files}.

load_file_and_assertions(PL, File, Dir, TestDir) ->
    FullFile = filename:append(Dir, File),
    {ok, PL@} = erlog:consult(PL, FullFile),
    TestFile  = filename:basename(File, ".pl") ++ "_tests.pl",

    FullTestFile =  filename:append(TestDir, TestFile),
    case filelib:is_file(FullTestFile) of
	true ->
	    {ok, PL@} = erlog:consult(PL, FullTestFile),
	    {ok, PL@};
	false ->
	    {ok, PL@}
    end.

find_tests(PL) ->
    {{succeed, R}, PL@} = erlog:prove(PL, {findall, {'X'},{clause, {test, {'X'}}, {'Clause'}},{'Tests'}}),
    {ok,proplists:get_value('Tests',R) , PL@}.

execute_one(PL, TestName,Pid) ->
    Status = case erlog:prove(PL, {test, TestName}) of
		 {{succeed,_} , _} ->
		     {TestName, true};
		 {fail, _} ->
		     ?debugVal({TestName, false}),
		     {TestName, false}
	     end,
    Pid !{result, Status}.

execute(PL,TestName) ->
    {TestPID,Ref} = spawn_monitor(?MODULE, execute_one, [PL, TestName, self()]),
    receive
	{result, {TestName,Status}} ->
	    {TestName,Status};
	{'DOWN', Ref, _, TestPID, ExitCode} ->
	    ?debugFmt("Test Failed ~p ~p", [TestName, ExitCode]),
	    {TestName, false}
	    
    after 500 ->
	    {TestName, false}
    end.

execute_tests(PL) ->
    {ok,Tests, PL@ } = find_tests(PL),
    Result = lists:map(fun(TestName) ->
			       execute(PL, TestName)
		       end, Tests),
    {ok , Result}.

-spec(get_failing_tests([test_result()]) -> [test_name()]).
get_failing_tests(Tests) ->
    [TestName|| {TestName, false} <- Tests].


save_failing_tests(Dir,FailingTests) ->
    Failing = io_lib:format("%-*-Erlang-*-~n {failing_tests, ~p}.",[FailingTests]),
    ok = file:write_file(make_failing_tests_file(Dir), Failing),
    ok.

make_failing_tests_file(Dir) -> Dir ++ "/.eunit_failing_tests".


load_failing_tests(Dir) ->
    case file:consult(make_failing_tests_file(Dir)) of
	{ok, Data}  ->
	    FailingTests = proplists:get_value(failing_tests, Data),
	    {ok, FailingTests};
	{error, enoent} ->
	    {ok, []}
    end.
