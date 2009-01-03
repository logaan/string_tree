#!/usr/bin/env escript

main(_) ->
  io:format("== Single Leaf Test~n"),
  single_leaf_test(),
  io:format("== Parent Child Test~n"),
  parent_child_test(),
  io:format("== Sibling Test~n"),
  siblings_test(),
  io:format("== Family Tree Test~n"),
  family_tree_test().

single_leaf_test() ->
  Foo = {"Foo", []},
  string_tree:format(Foo).

parent_child_test() ->
  Bar = {"Bar", []},
  Foo = {"Foo", [Bar]},
  string_tree:format(Foo).

siblings_test() ->
  Bar = {"Bar", []},
  Baz = {"Baz", []},
  Foo = {"Foo", [Bar, Baz]},
  string_tree:format(Foo).

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
  string_tree:format(FamilyTree).
