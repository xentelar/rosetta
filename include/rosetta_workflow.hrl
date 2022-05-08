
-record(workflow, {
    process_name        :: binary(),
    type                :: binary() | undefined,
    nodule              :: binary() | undefined,
    class_name          :: binary() | undefined,
    nodes               :: map(),
    transitions         :: map()}
).

-record(action, {
    nodule              :: binary() | undefined,
    class_name          :: binary() | undefined}
).

-record(transition, {
    action              :: binary() | undefined,
    nodule              :: binary() | undefined,
    class_name          :: binary() | undefined,
    state               :: binary(),
    next_state          :: binary(),
    event               :: binary()}
).