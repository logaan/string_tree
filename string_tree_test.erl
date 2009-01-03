#!/usr/bin/env escript

main(_) ->
  compile:file(string_tree, [report_errors]),
  string_tree:test().

