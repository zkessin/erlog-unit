-module(erlog_unit_tests).
-include_lib("eunit/include/eunit.hrl").
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
    {ok, PL}		= erlog:new(),
    File		= "example.pl",
    {ok, PL1}		= erlog_unit:load_file_and_assertions(PL, File, ?DIR, ?TEST_DIR),
    {{succeed,[]}, _}	= erlog:prove(PL1, {test, "TEST"}),
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

    ?assertMatch({ok, ["TEST"], PL@}, erlog_unit:find_tests(PL@)),    
    true.

spawn_and_execute_tests_test() ->
    nyi.

assert_true_test() ->
    nyi.

assert_not_true_test() ->
    nyi.

assert_equals_test() ->
    nyi.

assert_not_equals_test() ->
    nyi.

assert_foreach_test() ->
    nyi.

assert_generate_int_test() ->
    nyi.

assert_shrink_test() ->
    nyi.


