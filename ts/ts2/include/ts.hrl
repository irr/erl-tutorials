-define(APP, ts).

-record(ts_mod, {app}).

-record(ts_state, {lsock, aref, tout, mod, att}).

-record(ts_process, {lbin = <<>>}).
