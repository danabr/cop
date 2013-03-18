% encoding: utf-8
% @doc Contains brancher definitions.
-module(cop_branch).

-export([branch_on/1]).

-include("include/cop.hrl").

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

