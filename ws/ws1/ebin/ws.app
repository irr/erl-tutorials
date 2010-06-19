{application, ws,
 [{description, "web server application"},
  {vsn, "1.0"},
  {modules, [ws_app, ws_sup, ws]},
  {registered, [ws]},
  {applications, [kernel, stdlib, inets]},
  {mod, {ws_app, []}},
  {env, [{modules, [ws]}, {port, 1972}, {timeout, infinity}, 
         {bind, {127,0,0,1}}, {name, "ws1"},
         {server_root, "/tmp"}, {document_root, "/tmp"}]}
 ]}.

