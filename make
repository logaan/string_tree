#!/opt/local/lib/erlang/bin/escript

main(["test"]) ->
  io:format(string:chars($-, 80, "\n")),
  make:all([report_errors]),
  string_tree:test();
main(_) ->
  make:all([report_errors]).

