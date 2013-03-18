% encoding: utf-8
% @doc Contains functions used internally by the cop engine.
-module(cop_core).

-export([propagate/1, evaluate/1]).

-include("include/cop.hrl").

% Performs propagation on the given space.
% Propagation will continue until a fix point is reached.
% Returns a new space with updated variables and propagators.
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

% @doc Evaluates the given space and determines if it is solved, failed, or open,
% meaning that further branching and/or propagation is needed to assign all variables.
evaluate(#space{vars=Vars}) ->
  evaluate(Vars, solved).

evaluate([], Status) -> Status;
evaluate([#var{domain=D}|Vs], Status) ->
  case length(D) of
    0 -> failed;
    1 -> evaluate(Vs, Status);
    _N -> evaluate(Vs, unsolved)
  end.

