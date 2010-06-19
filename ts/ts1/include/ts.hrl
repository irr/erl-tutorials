-define(APP, ts).

-record(ts_mod, {app}).

-record(ts_state, {tout, mod, att}).

-record(ts_process, {lbin = <<>>}).
