%% Experimenting with implementing Gecode in Erlang.

-module(gecode).
-export([problem1/0, model/4, search_all/1, search_all/2, search_next/1, search_next/2]).

-record(space, {vars=[], propagators=[], branchers=[], search_method=dfs, status=unsolved, subspaces, parent}).
-record(propagator, {implementation, vars}).
-record(var, {type, id, domain}).
-record(brancher, {vars, var_strategy, val_strategy}).


% Returns a new space describing the given model.
% Vars should be a list of variables in the model.
% Propagators should be a list of propagators defining the constraints of the model.
% Branchers should be a list of branchers describing how to create the search tree.
% SearchMethod should be either the atom dfs, or a tuple {SearchFun, SearchParams}.
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
  { fun dfs/2, [] };

search_data_for({Fun, _StartParams}=ToUse) when is_function(Fun) ->
  ToUse.

% Performs depth first search for a solution.
dfs([], Leaves) ->
  {failed, Leaves, [], []};

dfs([Space|Ss], Leaves) ->
  Space1 = propagate(Space),
  case evaluate(Space1) of
    unsolved ->
      Subspaces = branch_on(Space1),
      dfs(Subspaces ++ Ss, Leaves);
    failed ->
      dfs(Ss, [Space1#space{status=failed}|Leaves]);
    solved ->
      NewLeaves = [Space1#space{status=solved}|Leaves],
      {solved, NewLeaves, Ss, NewLeaves}
  end.

propagate(Space = #space{vars=Vars, propagators=Propagators}) ->
  {Vars1, Propagators1} = propagate1([], Vars, Propagators),
  Space#space{vars = Vars1, propagators=Propagators1}.

propagate1(Vars, Vars, Propagators) ->
  {Vars, Propagators}; % We have reached a fix point

propagate1(_LastVars, Vars, Propagators) ->
  {NewVars, PropsLeft} = lists:foldl(fun(P, {V1, PToKeep}) ->
    case prop_single(V1, P) of
      {subsumed, V2 } -> {V2, PToKeep};
      {_, V2} -> {V2, [P|PToKeep]}
    end
    end,
    {Vars, []},
    Propagators),

  propagate1(Vars, NewVars, PropsLeft).

prop_single(AllVars, #propagator{implementation=PropImpl, vars=PropVars}) ->
  Vars = lists:filter(fun({_, _, Id, _}) -> lists:member(Id, PropVars) end, AllVars),
  {Status, NewDomains} = PropImpl(Vars),
  {Status, update_domains(AllVars, NewDomains) }.

update_domains(Vars, NewDomains) ->
  MapImpl = fun(Var=#var{id=Id}) ->
    case proplists:get_value(Id, NewDomains) of
      undefined -> Var;
      NewDomain -> Var#var{domain=NewDomain}
    end
  end,
  lists:map(MapImpl, Vars).

evaluate(#space{vars=Vars}) ->
  evaluate(Vars, solved).

evaluate([], Status) -> Status;
evaluate([#var{domain=D}|Vs], Status) ->
  case length(D) of
    0 -> failed;
    1 -> evaluate(Vs, Status);
    _N -> evaluate(Vs, unsolved)
  end.

% Uses the next available brancher to create a set of subspaces for further
% exploration of the search tree.
branch_on(Space=#space{branchers=Branchers, vars=Vars}) ->
  #brancher{var_strategy=VarS, val_strategy=ValS} = select_brancher(Branchers, Vars),
  BranchVar = select_branch_variable(Vars, VarS),
  RestOfVars = lists:delete(BranchVar, Vars),
  branch_on(Space, BranchVar, RestOfVars, ValS).

% Selects the next brancher to use.
% Selects the first brancher that can branch on any of the given variables.
select_brancher(Branchers, Vars) ->
  Unassigned = lists:filter(fun(#var{domain=D}) -> length(D) > 1 end, Vars),
  UnassignedIds = [Id || #var{id=Id} <- Unassigned],
  select_brancher1(Branchers, UnassignedIds).

select_brancher1([B|Bs], VarIds) ->
  #brancher{vars=BranchVarIds}= B,
  case lists:any(fun(X) -> lists:member(X, VarIds) end, BranchVarIds) of
    true -> B;
    false ->  select_brancher1(Bs, VarIds)
  end.

% Selects which variable to branch on, according to the given strategy.
% VarStrategy should either be a fun or a symbol identifying one of the
% built in strategies.
% This function is guaranteed to be called with a list of variables that has atleast
% one unassigned variable, and no failed variables.
select_branch_variable(Vars, VarStrategy) when is_function(VarStrategy) ->
  VarStrategy(Vars);

% Selects the unassigned variable with the smallest domain
select_branch_variable(Vars, int_var_size_min) ->
  Unassigned = lists:filter(fun(#var{domain=D}) -> length(D) > 1 end, Vars),
  ByDomainSize = lists:sort(fun(#var{domain=D1}, #var{domain=D2}) -> length(D1) =< length(D2) end, Unassigned),
  hd(ByDomainSize).

% Branches on the smallest value in the domain of the selected variable.
branch_on(Space, Var=#var{domain=[Min|Rest]} , RestOfVars, int_val_min) ->
  MinSpace = Space#space{vars=[Var#var{domain=[Min]}|RestOfVars], parent=Space},
  RestSpace = Space#space{vars=[Var#var{domain=Rest}|RestOfVars], parent=Space},
  [MinSpace, RestSpace];

branch_on(Space, Var, RestOfVars, Fun) when is_function(Fun) ->
  Fun(Space, Var, RestOfVars).

% Implementation of not equal propagator.
propagate_ne([#var{type=integer, id=Id, domain=Domain}], Constant) ->
  Domain1 = lists:delete(Constant, Domain),
  {subsumed, [{Id, Domain1}]}.

% Registers a custom propagator for the given list of variables.
% Fun should be a function taking a list of variables, and
% return a tuple {Status, NewDomains}.
propagate_custom(Vars=[_|_], Fun) when is_function(Fun) ->
  #propagator{implementation=Fun, vars=Vars}.

% Registers a constraint that the given variable must not be equal to the given constant.
ne(#var{type=integer, id=Id}, Constant) when is_integer(Constant) ->
  Impl = fun(Vars) -> propagate_ne(Vars, Constant) end,
  #propagator{implementation=Impl, vars=[Id]}.

% Returns a brancher for branching on the given list of variables,
% using the given variable and value selection strategies.
branch(Vars=[_|_], VarStrategy, ValStrategy) ->
  VarIds = [Id || #var{id=Id} <- Vars],
  #brancher{vars=VarIds, var_strategy=VarStrategy, val_strategy=ValStrategy}.

% Registers a new integer variable with the given min and max values.
int_var(Id, Min, Max) when is_atom(Id), is_integer(Min), is_integer(Max), Max >= Min ->
  {var, integer, Id, lists:seq(Min, Max)}.

problem1() ->
  Vars = [S=int_var(s, 0, 9)],

  Propagators = [ne(S, 0)],

  Branchers = [branch(Vars, int_var_size_min, int_val_min)],

  SearchMethod = dfs,

  model(Vars, Propagators, Branchers, SearchMethod).
