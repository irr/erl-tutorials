{application, ts,
 [{description, "echo server application"},
  {vsn, "1.0"},
  {modules, [ts_app, ts_sup, ts, echo]},
  {registered, [ts]},
  {applications, [kernel, stdlib]},
  {mod, {ts_app, []}},
  {env, [{module, echo}, {port, 1972}, {timeout, infinity}]}
 ]}.

