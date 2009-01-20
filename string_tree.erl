-module( string_tree ).
-export( [format/1, parse/1, add/2] ).
-include_lib( "eunit/include/eunit.hrl" ).

% TODO:
% - Output shouldn't be passed to every level, the lowest levels should just
%   return how they're to be represented and higher levels should join them.

format( StringTree) -> format(StringTree, 0, "" ).

% Regular nodes are in the format {String, [Node1, Leaf1, ... NodeN, LeafN]}
format( {String, Children}, Indentation, Output ) ->
  NewOutput = format( String, Indentation, Output ),
  format( Children, Indentation + 1, NewOutput );

% Empty list of children
format( [], _, Output ) ->
  Output;

% List of children
format( [Head|Tail], Indentation, Output) when is_list( Head ); is_tuple(Head ) ->
  HeadOutput = format( Head, Indentation, Output ),
  format( Tail, Indentation, HeadOutput );

% Leaf node or branch node text
format( String, Indentation, Output ) ->
  IndentationString = string:chars( $ , Indentation * 2 ),
  string:join( [Output, IndentationString, "- ", String, "\n"], "" ).



parse( String ) ->
  parse( [], string:tokens( String, ".?!" ) ).

parse( [], [CurrentSentence | Sentences] ) ->
  StrippedSentance = string:strip( CurrentSentence ),
  parse( [StrippedSentance], Sentences );

parse( Tree, [CurrentSentence | Sentences] ) ->
  StrippedSentance = string:strip( CurrentSentence ),
  parse( [StrippedSentance|Tree], Sentences );

parse( Tree, [] ) ->
  Tree.



% Entry point
add( String, Tree ) ->
  add( [], string:tokens( String, " " ), Tree ).

% Words are equal
add( Result, [Sh|St], [Th|Tt] ) when Sh == Th ->
  add( [Th|Result], St, Tt );

% Words are not equal and we're at the end of both
add( Result, [Sh|[]], [Th|[]] ) when Sh /= Th ->
  Fork = [Sh, Th],
  lists:reverse( [Fork|Result] );

% Words are not equal and we're mid way through both
add( Result, [Sh|St], [Th|Tt] ) when Sh /= Th ->
  Fork = [[Sh|St], [Th|Tt]],
  lists:reverse( [Fork|Result] );

% Ran out of string
add( Result, [], Tree ) ->
  lists:append( lists:reverse( Result ), Tree );

% Ran out of tree
add( Result, String, [] ) ->
  lists:append( lists:reverse( Result ), String ).

  

%
% FORMAT TESTS
%

single_leaf_test() ->
  SingleLeaf = "Foo",
  ExpectedOutput = "- Foo\n",
  ?assertEqual( ExpectedOutput, format( SingleLeaf ) ).

parent_child_test() ->
  Foo = {"Foo", ["Bar"]},
  ExpectedOutput = "- Foo\n  - Bar\n",
  ?assertEqual( ExpectedOutput, format( Foo ) ).

siblings_test() ->
  Foo = {"Foo", ["Bar", "Baz"]},
  ExpectedOutput = "- Foo\n  - Bar\n  - Baz\n",
  ?assertEqual( ExpectedOutput, format( Foo ) ).

no_common_base_test() ->
  Foo = ["Foo", "Bar"],
  ExpectedOutput = "- Foo\n- Bar\n",
  ?assertEqual( ExpectedOutput, format( Foo ) ).

family_tree_test() ->
  Mother = {"Mother", ["Grand Mother", "Grand Father"]},
  Father = {"Father", ["Nonna", "Pops"]},
  Me = {"Me", [Mother, Father]},
  ExpectedOutput = "- Me
  - Mother
    - Grand Mother
    - Grand Father
  - Father
    - Nonna
    - Pops
",
  ?assertEqual( ExpectedOutput, format( Me ) ).


single_sentence_test() ->
  Quote = "If you prick us do we not bleed?",
  ExpectedOutput = ["If you prick us do we not bleed"],
  ?assertEqual( ExpectedOutput, parse( Quote ) ).

unmatching_two_sentence_test() ->
  Quote = "If you prick us do we not bleed? Goats love grass.",
  ExpectedOutput = ["Goats love grass", "If you prick us do we not bleed"],
  ?assertEqual( ExpectedOutput, parse( Quote ) ).


equal_words_test() ->
  String = "a b c",
  Tree   = ["a", "b", "c"],
  ?assertEqual( Tree, add( String, Tree ) ).

string_longer_test() ->
  String = "a b c d",
  Tree   = ["a", "b", "c"],
  ExpectedOutput = ["a", "b", "c", "d"],
  ?assertEqual( ExpectedOutput, add( String, Tree ) ).

tree_longer_test() ->
  String = "a b c",
  Tree = ["a", "b", "c", "d"],
  ExpectedOutput = ["a", "b", "c", "d"],
  ?assertEqual( ExpectedOutput, add( String, Tree ) ).

single_difference_test() ->
  String = "a b c",
  Tree = ["a", "b", "d"],
  ExpectedOutput = ["a", "b", ["c", "d"]],
  ?assertEqual( ExpectedOutput, add( String, Tree ) ).

string_multiple_difference_test() ->
  String = "a b c d",
  Tree = ["a", "b", "e"],
  ExpectedOutput = ["a", "b", [["c", "d"], ["e"]]],
  ?assertEqual( ExpectedOutput, add( String, Tree ) ).

