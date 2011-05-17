{application, etrader,
 [{description, "etrader server application"},
  {vsn, "1.0"},
  {modules, [etrader_app, etrader_sup, etrader, etrader_stats, etrader_csv]},
  {registered, [etrader]},
  {applications, [kernel, stdlib]},
  {mod, {etrader_app, []}},
  {env, [{csv, "../priv/etrader.csv"}, {timeout, 10000}]}
 ]}.

