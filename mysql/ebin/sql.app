{application, sql,
 [{description, "MySQL client gen server application"},
  {vsn, "1.0"},
  {modules, [sql_app, sql_sup, sql, crypt64,
             mysql_auth, mysql_conn, mysql_recv, mysql]},
  {registered, [sql]},
  {applications, [kernel, stdlib]},
  {mod, {sql_app, []}},
  {env, [{host, "localhost"}, {port, 3306}, {size, 5},
         {user, "irr"}, 
		 {password, "hstDm8gkoVelffvnzuy8l0nllutL1IOacFE6iNPbfnAX94C2pWMt9g1bIabFTiIpLRmwqKW4RfUvEuHuafR82Q=="}, 
         {database, "test"}, {encoding, latin1},
         {timeout, 10000}]}
 ]}.

