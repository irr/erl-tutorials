{application, sql,
 [{description, "MySQL client gen server application"},
  {vsn, "1.0"},
  {modules, [sql_app, sql_sup, sql, 
             mysql_auth, mysql_conn, mysql_recv, mysql]},
  {registered, [sql]},
  {applications, [kernel, stdlib]},
  {mod, {sql_app, []}},
  {env, [{host, "localhost"}, {port, 3306}, {size, 5},
         {user, "irr"}, {password, "mysql"}, 
         {database, "test"}, {encoding, latin1},
         {timeout, 10000}]}
 ]}.

