-module(erlog_unit_tests).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("erlog_test.hrl").
-compile(export_all).
-compile({parse_transform, seqbind}).
-define(DIR,      "../test/test_prolog_files").
-define(TEST_DIR, "../test/test_prolog_files").
-define(DIR(X),   "../test/test_prolog_files/" ++ X).



find_prolog_files_test() ->
    Dir			= "../test/test_prolog_files",
    {ok, Files}		= erlog_unit:find_prolog_files(Dir),
    ?assert(lists:member("../test/test_prolog_files/test.pl", Files)),
    true.
    
load_file_and_assertions_test() ->
    {ok, PL@}		= erlog:new(),
    File		= "example.pl",
    {ok, PL@}		= erlog_unit:load_file_and_assertions(PL@, File, ?DIR, ?TEST_DIR),
    {ok, PL@}           = erlog:consult(PL@,"../erlog/assert.pl"),
    {{succeed,[]}, _}	= erlog:prove(PL@, {test, "TEST"}),
    true.

load_file_with_no_assertions_test() ->
    {ok, PL}		= erlog:new(),
    File		= "test.pl",
    {ok, _PL1}		= erlog_unit:load_file_and_assertions(PL, File, ?DIR, ?TEST_DIR),
    true.

find_tests_test() ->
    {ok, PL@}		= erlog:new(),
    File		= "example.pl",
    {ok, PL@}		= erlog_unit:load_file_and_assertions(PL@, File, ?DIR, ?TEST_DIR),

    ?assertMatch({ok, ["TEST","FAIL"], _}, erlog_unit:find_tests(PL@)),    
    true.

spawn_and_execute_tests_test() ->
    {ok, PL@}		= erlog:new(),
    File		= "example.pl",
    {ok, PL@}           = erlog:consult(PL@,"../erlog/assert.pl"),
    {ok, PL@}		= erlog_unit:load_file_and_assertions(PL@, File, ?DIR, ?TEST_DIR),
    {ok, Result}        = erlog_unit:execute_tests(PL@),
    ?assertEqual([{"TEST", true},
		  {"FAIL", false}
		 ], Result),
    true.


assertFailure(PL@, Clause, Expect) ->
    {Pid,Ref}  = spawn_monitor(fun() -> erlog:prove(PL@, Clause) end),
    receive
	{'DOWN',Ref, _, Pid, Value} ->
	    ?assertEqual(Expect, Value)
    after 50 ->
	    exit("Process Not Exiting")
    end.


assert_true_test() ->
    {ok, PL@}		= erlog:new(),
    {ok, PL@}           = erlog:consult(PL@,"../erlog/assert.pl"),
    {{succeed,_},PL@}   = erlog:prove(PL@, {assertTrue,true, "X"}),
    Clause = {assertTrue, false, "X"},
    Expect = "X",
    assertFailure(PL@, Clause, Expect),
    true.


assert_not_true_test() ->
    {ok, PL@}		= erlog:new(),
    {ok, PL@}           = erlog:consult(PL@,"../erlog/assert.pl"),
    {{succeed,_},PL@}   = erlog:prove(PL@, {assertNot, false, "X"}),
    assertFailure(PL@, {assertNot, true, "X"}, "X"),

    true.

assert_equals_test() ->
    {ok, PL@}		= erlog:new(),
    {ok, PL@}           = erlog:consult(PL@,"../erlog/assert.pl"),
    {{succeed,_},PL@}   = erlog:prove(PL@, {assertEqual, true,true, "X"}),
    assertFailure(PL@, {assertEqual, false, true, "X"}, "X"),
    true.

assert_not_equals_test() ->
    {ok, PL@}		= erlog:new(),
    {ok, PL@}           = erlog:consult(PL@,"../erlog/assert.pl"),
    {{succeed,_},PL@}   = erlog:prove(PL@, {assertNotEqual, true,false, "X"}),
    assertFailure(PL@, {assertNotEqual, false, false, "X"}, "X"),
    true.

prop_load_last_failing_tests() ->
    ?FORALL(Tests,
	    list(non_empty(list(choose(65,90)))),
	    begin
		erlog_unit:save_failing_tests("/tmp", Tests),
		{ok, Tests}=:= erlog_unit:load_failing_tests("/tmp")
		
	    end).

failing_test_file_not_created_test() ->
    file:delete(erlog_unit:make_failing_tests_file("/tmp")),
    ?assertEqual({ok, []}, erlog_unit:load_failing_tests("/tmp")),
    true.


assert_foreach_test() ->
    nyi.

assert_generate_int_test() ->
    nyi.

assert_shrink_test() ->
    nyi.


