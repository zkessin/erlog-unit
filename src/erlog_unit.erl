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

execute_tests(PL) ->
    {ok,Tests, PL@ } = find_tests(PL),
    Result = lists:map(fun(TestName) -> 
			       case erlog:prove(PL, {test, TestName}) of
				   {{succeed,_} , _} ->
				       {TestName, true};
				   {fail, _} ->
				       {TestName, false}
			       end
		       end, Tests),
    {ok , Result}.
