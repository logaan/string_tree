#!/opt/local/lib/erlang/bin/escript

main(["test"]) ->
  make:all([report_errors]),
  string_tree:test();
main(_) ->
  make:all([report_errors]).

