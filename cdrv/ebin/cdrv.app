{application, cdrv,
 [{description, "cdrv application"},
  {vsn, "1.0"},
  {modules, [cdrv_app, cdrv_sup, cdrv, test]},
  {registered, [cdrv]},
  {applications, [kernel, stdlib]},
  {mod, {cdrv_app, []}},
  {env, []}
 ]}.

