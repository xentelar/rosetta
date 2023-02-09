
%%%-------------------------------------------------------------------
%% @doc workflow model
%% @end
%%%-------------------------------------------------------------------
-record(workflow, {
    process_name        :: binary(),
    template            :: binary(),
    module              :: binary(),
    space               :: binary(),
    events              :: map(),
    states              :: map()}
).

%%%-------------------------------------------------------------------
%% @doc state model
%% @end
%%%-------------------------------------------------------------------
-record(state, {
    state               :: binary(),
    action              :: tuple(),
    transitions         :: list()}
).

%%%-------------------------------------------------------------------
%% @doc action model
%% @end
%%%-------------------------------------------------------------------
-record(action, {
    module              :: binary() | undefined,
    space          :: binary() | undefined}
).

%%%-------------------------------------------------------------------
%% @doc transition model
%% @end
%%%-------------------------------------------------------------------
-record(transition, {
    state               :: binary(),
    next_state          :: binary(),
    event               :: binary()}
).