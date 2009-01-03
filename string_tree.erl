-module(string_tree).
-export([format/1]).

format(StringTree) -> format(StringTree, 0).

% Leaves are in the format {String, []}
% Regular nodes are in the format {String, [Node1, Leaf1, ... NodeN, LeafN]}
format({String, []}, Indentation) ->
  indented_print(String, Indentation);

format({String, Children}, Indentation) ->
  indented_print(String, Indentation),
  format(Children, Indentation + 1);

format([H|T], Indentation) ->
  format(H, Indentation),
  format(T, Indentation);

format([], Indentation) ->
  ok.

indented_print(String, Indentation) ->
  IndentationString = string:chars($ , Indentation * 2),
  io:format("~s- ~s~n", [IndentationString, String]).

