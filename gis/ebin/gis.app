{application, gis,
 [{description, "gen server application"},
  {vsn, "1.0"},
  {modules, [gis_app, gis_sup, gis]},
  {registered, [gis]},
  {applications, [kernel, stdlib]},
  {mod, {gis_app, []}},
  {env, []}
 ]}.

