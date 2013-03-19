% encoding: utf-8
% @doc Interface for creating and modifying variables.
% Use this interface instead of relying on how variable domains are implemented.
-module(cop_var).

% Functions for defining variables.
-export([int_var/3]).

% Functions for inspecting variable domains.
-export([status/1, min/1, max/1]).

% Functions for modifying variable domains. Intented for internal use.
-export([nq/2, le/2, lq/2, gr/2, gq/2]).

-include("include/cop.hrl").

% @spec int_var(Id::atom(), Min::integer(), Max::integer()) -> var()
% @doc Registers a new integer variable with the given min and max values.
int_var(Id, Min, Max) when is_atom(Id), is_integer(Min), is_integer(Max), Max >= Min ->
  #var{type=integer, id=Id, domain=lists:seq(Min, Max)}.

% @spec status(var()) -> assigned | failed | open
% Returns whether the variable is assigned (having only one value in its domain),
% failed (having no values in its domain) or open (having multiple values in its domain).
status(#var{domain=[]}) ->
  failed;
status(#var{domain=[_]}) ->
  assigned;
status(#var{}) ->
  open.

% @spec min(var()) -> integer()
% Returns the minimum value of the domain.
% Will throw a badmatch error if the variable has an empty domain.
min(#var{domain=[Min|_]}) -> Min.

% @spec max(var()) -> integer()
% Returns the maximum value of the domain.
% Will throw a badmatch error if the variable has an empty domain.
max(#var{domain=D}) -> lists:last(D).

% @spec nq(var(), Val::integer()) -> var()
% Returns a new variable with Val removed from the domain.
nq(Var=#var{domain=D}, Val) ->
  D1 = lists:delete(Val, D),
  Var#var{domain=D1}.

% @spec le(var(), Val::integer()) -> var()
% Returns a new variable with all values greater or equal to Val removed from the domain.
le(Var=#var{type=integer, domain=D}, Val) ->
  D1 = lists:filter(fun(X) -> X < Val end, D),
  Var#var{domain=D1}.

% @spec lq(var(), Val::integer()) -> var()
% Returns a new variable with all values greater than Val removed from the domain.
lq(Var=#var{domain=D}, Val) ->
  D1 = lists:filter(fun(X) -> X =< Val end, D),
  Var#var{domain=D1}.

% @spec gr(var(), Val::integer()) -> var()
% Returns a new variable with all values less or equal to Val removed from the domain.
gr(Var=#var{domain=D}, Val) ->
  D1 = lists:filter(fun(X) -> X > Val end, D),
  Var#var{domain=D1}.

% @spec gq(var(), Val::integer()) -> var()
% Returns a new variable with all values less than Val removed from the domain.
gq(Var=#var{domain=D}, Val) ->
  D1 = lists:filter(fun(X) -> X >= Val end, D),
  Var#var{domain=D1}.
