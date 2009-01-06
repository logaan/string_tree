-module(string_tree).
-export([format/1]).
-include_lib("eunit/include/eunit.hrl").

% TODO:
% - If we made leafs to be just strings then the same method that prints the
%   string in a non-leaf node could be used to handle leaf nodes.
% - Output shouldn't be passed to every level, the lowest levels should just
%   return how they're to be represented and higher levels should join them.
% - There might be a way to cut down on code by directly working on the contents
%   of the head of a list like [{String, Children} | Tail]

format(StringTree) -> format(StringTree, 0, "").

% Leaves are in the format {String, []}
% Regular nodes are in the format {String, [Node1, Leaf1, ... NodeN, LeafN]}
format({String, Children}, Indentation, Output) ->
  NewOutput = format(String, Indentation, Output),
  format(Children, Indentation + 1, NewOutput);

format([], Indentation, Output) ->
  Output;

format([H|T], Indentation, Output) when is_list(H); is_tuple(H) ->
  HeadOutput = format(H, Indentation, Output),
  format(T, Indentation, HeadOutput);

format(String, Indentation, Output) ->
  IndentationString = string:chars($ , Indentation * 2),
  string:join([Output, IndentationString, "- ", String, "\n"], "").

%
% FORMAT TESTS
%

single_leaf_test() ->
  SingleLeaf = "Foo",
  ExpectedOutput = "- Foo\n",
  ?assertEqual(ExpectedOutput, format(SingleLeaf)).

parent_child_test() ->
  Foo = {"Foo", ["Bar"]},
  ExpectedOutput = "- Foo\n  - Bar\n",
  ?assertEqual(ExpectedOutput, format(Foo)).

siblings_test() ->
  Foo = {"Foo", ["Bar", "Baz"]},
  ExpectedOutput = "- Foo\n  - Bar\n  - Baz\n",
  ?assertEqual(ExpectedOutput, format(Foo)).

family_tree_test() ->
  Mother = {"Mother", ["Grandmother", "Grandfather"]},
  Father = {"Father", ["Nonna", "Pops"]},
  Me = {"Me", [Mother, Father]},
  ExpectedOutput = "- Me
  - Mother
    - Grandmother
    - Grandfather
  - Father
    - Nonna
    - Pops
",
  ?assertEqual(ExpectedOutput, format(Me)).

