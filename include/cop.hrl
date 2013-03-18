% encoding: utf-8
-header(cop).

% This file contains the record definitions used internally by cop.
% You should never use these directly, but instead use the interface
% methods provided by cop.

-record(space, {vars=[], propagators=[], branchers=[], search_method=dfs, status=unsolved, parent}).

-record(propagator, {implementation, vars}).

-record(var, {type, id, domain}).

-record(brancher, {vars, var_strategy, val_strategy}).
