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
