% encoding: utf-8
% @doc Cop - a constraint programming engine implemented in Erlang.
-module(cop).
-include("include/cop.hrl").

-export([model/4, search_all/1, search_all/2, search_next/1, search_next/2, branch/3]).

% Returns a new space describing the given model.
% Vars should be a list of variables in the model.
% Propagators should be a list of propagators defining the constraints of the model.
% Branchers should be a list of branchers describing how to create the search tree.
% SearchMethod should be either the atom dfs, or a tuple {SearchFun, SearchParams}.
model(Vars, Propagators, Branchers, SearchMethod) ->
  #space{vars=Vars, propagators=Propagators, branchers=Branchers, search_method=SearchMethod, status=open}.

% Finds all solutions in the given space's subtree.
% Returns a tuple {Solutions, Leaves}, where Solutions is a list of spaces
% representing all solutions to the problem, and Leaves is a list of all
% leaves in the search tree.
search_all(Space=#space{search_method=SearchMethod}) ->
  {_, StartParams} = search_data_for(SearchMethod),
  search_all([Space], StartParams).

% Finds all solutions for the given spaces.
% StartParams should contain parameters needed for continuing any  previous search.
search_all(Spaces=[S|_], StartParams) ->
  #space{search_method=SearchMethod} = S,
  {SearchFun, _} = search_data_for(SearchMethod),
  search_all(Spaces, SearchFun, StartParams, [], []).

search_all([], _, _, Solutions, LeafNodes) ->
  {Solutions, LeafNodes};

search_all(Spaces, SearchFun, Params, Solutions, _LeafNodes) ->
  {Res, NewLeafNodes, NextSpaces, NextParams} = search_next(Spaces, SearchFun, Params),
  case Res of
    solved ->
      [Solution|_] = NewLeafNodes,
      search_all(NextSpaces, SearchFun, NextParams, [Solution|Solutions], NewLeafNodes);
    failed ->
      {Solutions, NewLeafNodes}
  end.

% Searches for the next solution in the given space.
search_next(Space=#space{search_method=SearchMethod}) ->
  {SearchFun, StartParams} = search_data_for(SearchMethod),
  search_next([Space], SearchFun, StartParams).

% Searches for the next solution for the given list of spaces.
search_next(Spaces=[S|_], StartParams) ->
  #space{search_method=SearchMethod} = S,
  {SearchFun, _} = search_data_for(SearchMethod),
  search_next(Spaces, SearchFun, StartParams).

search_next(Spaces, SearchFun, Params) ->
  SearchFun(Spaces, Params).

% Returns a tuple {SearchFun, StartParams}. SearchFun can be used to
% search a space, and takes two parameters: The spaces to search, and
% any parameters to pass on to the search engine in the form of a list.
% StartParams is a list of parameters to pass into the search function
% at the first invocation of the search.
search_data_for(dfs) ->
  { fun cops_search:dfs/2, [] };

search_data_for({Fun, _StartParams}=ToUse) when is_function(Fun) ->
  ToUse.

% Returns a brancher for branching on the given list of variables,
% using the given variable and value selection strategies.
branch(Vars=[_|_], VarStrategy, ValStrategy) ->
  VarIds = [Id || #var{id=Id} <- Vars],
  #brancher{vars=VarIds, var_strategy=VarStrategy, val_strategy=ValStrategy}.
