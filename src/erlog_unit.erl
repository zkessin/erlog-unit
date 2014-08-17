-module(erlog_unit).
-compile({parse_transform, seqbind}).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).




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
