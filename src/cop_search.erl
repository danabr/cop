% encoding: utf-8
% @doc Defines search algorithms provided by cop.
% A search function is a function taking two parameters:
% a list of spaces to search and an arbitrary parameter needed to continue
% a previously started search.
% The search function should stop at the first successful leaf it encounters.
% All search functions must return the following tuple:
% {Result, Leaves, NextSpaces, NextParams}.
% Result should be either of the atoms solved or failed.
% Leaves should contain all the leaves needed for building up the search tree.
% NextSpaces should be a list of spaces to search next to find the next solution.
% NextParams should contain the parameters needed for continuing the search.
-module(cop_search).

-export([dfs/2]).

-include("include/cop.hrl").

% Performs depth first search for a solution.
dfs([], Leaves) ->
  {failed, Leaves, [], []};

dfs([Space|Ss], Leaves) ->
  Space1 = cop_core:propagate(Space),
  case cop_core:evaluate(Space1) of
    unsolved ->
      Subspaces = cop_branch:branch_on(Space1),
      dfs(Subspaces ++ Ss, Leaves);
    failed ->
      dfs(Ss, [Space1#space{status=failed}|Leaves]);
    solved ->
      NewLeaves = [Space1#space{status=solved}|Leaves],
      {solved, NewLeaves, Ss, NewLeaves}
  end.
