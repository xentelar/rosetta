
-record(workflow, {
    process_name        :: binary(),
    template            :: binary(),
    module              :: binary(),
    space               :: binary(),
    events              :: map(),
    states              :: map()}
).

-record(state, {
    state               :: binary(),
    action              :: tuple(),
    transitions         :: list()}
).

-record(action, {
    module              :: binary() | undefined,
    space          :: binary() | undefined}
).

-record(transition, {
    state               :: binary(),
    next_state          :: binary(),
    event               :: binary()}
).