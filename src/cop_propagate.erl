% encoding: utf-8
% @doc Exports functions for specifying propagators.
-module(cop_propagate).

-export([custom/2, ne/2]).

-include("include/cop.hrl").

% Implementation of not equal propagator.
propagate_ne([#var{type=integer, id=Id, domain=Domain}], Constant) ->
  Domain1 = lists:delete(Constant, Domain),
  {subsumed, [{Id, Domain1}]}.

% @spec custom([var()], PropagatorFun) -> propagator()
%  PropagatorFun = fun([var()]) -> {Status, [domain()]}
%  Status = subsumed | fix| nofix
% @doc Registers a custom propagator for the given list of variables.
custom(Vars=[_|_], Fun) when is_function(Fun) ->
  #propagator{implementation=Fun, vars=Vars}.

% @spec ne(var::var(), Constant::integer()) -> propagator()
% @doc Registers a constraint that the given variable must not be equal to the given constant.
ne(#var{type=integer, id=Id}, Constant) when is_integer(Constant) ->
  Impl = fun(Vars) -> propagate_ne(Vars, Constant) end,
  #propagator{implementation=Impl, vars=[Id]}.
