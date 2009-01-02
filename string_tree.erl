-module(string_tree).
-export([format/1, test/0]).

format(StringTree) -> format(StringTree, 0).

% Leaves are in the format {String, []}
% Regular nodes are in the format {String, [nodes, or leaves]}
format({String, Children}, Indentation) when is_list(Children) ->
  indented_print(String, Indentation),
  format(Children, Indentation + 1);
format([H|T], Indentation) ->
  format(H, Indentation),
  format(T, Indentation);
format([], Indentation) ->
  ok;
format({String, []}, Indentation) ->
  indented_print(String, Indentation).

indented_print(String, Indentation) ->
  IndentationString = string:chars($ , Indentation * 2),
  io:format("~s- ~s~n", [IndentationString, String]).

test() ->
  io:format("Single Leaf Test~n"),
  single_leaf_test().

single_leaf_test() ->
  Foo = {"Foo", []},
  format(Foo).

