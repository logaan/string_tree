-module(string_tree).
-export([format/1]).
-include_lib("eunit/include/eunit.hrl").

% If we made leafs to be just strings then the same method that prints the
% string in a non-leaf node could be used to handle leaf nodes.

format(StringTree) -> format(StringTree, 0, "").

% Leaves are in the format {String, []}
% Regular nodes are in the format {String, [Node1, Leaf1, ... NodeN, LeafN]}
format({String, []}, Indentation, Output) ->
  indented_print(String, Indentation, Output);

format({String, Children}, Indentation, Output) ->
  NewOutput = indented_print(String, Indentation, Output),
  format(Children, Indentation + 1, NewOutput);

format([H|T], Indentation, Output) ->
  HeadOutput = format(H, Indentation, Output),
  format(T, Indentation, HeadOutput);

format([], Indentation, Output) ->
  Output.

indented_print(String, Indentation, Output) ->
  IndentationString = string:chars($ , Indentation * 2),
  string:join([Output, IndentationString, "- ", String, "\n"], "").

%
% FORMAT TESTS
%

single_leaf_test() ->
  SingleLeaf = {"Foo", []},
  ExpectedOutput = "- Foo\n",
  ?assert(format(SingleLeaf) == ExpectedOutput).

parent_child_test() ->
  Bar = {"Bar", []},
  Foo = {"Foo", [Bar]},
  ExpectedOutput = "- Foo\n  - Bar\n",
  ?assert(format(Foo) == ExpectedOutput).

siblings_test() ->
  Bar = {"Bar", []},
  Baz = {"Baz", []},
  Foo = {"Foo", [Bar, Baz]},
  ExpectedOutput = "- Foo\n  - Bar\n  - Baz\n",
  ?assert(format(Foo) == ExpectedOutput).

family_tree_test() ->
  FamilyTree = {
    "Me",
    [
      {
        "Mother",
        [
          { "Grandmother", [] },
          { "Grandfather", [] }
        ]
      },
      {
        "Father",
        [
          { "Nonna", [] },
          { "Pops", [] }
        ]
      }
    ]
  },
  ExpectedOutput = "- Me
  - Mother
    - Grandmother
    - Grandfather
  - Father
    - Nonna
    - Pops
",
  ?assert(format(FamilyTree) == ExpectedOutput).
