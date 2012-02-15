===============
Rebar skeleton
===============

About
-----
* A basic rebar skeleton including inets

Dependencies
------------
- Erlang/OTP

Author
------
Ivan Ribeiro Rocha <ivan.ribeiro@gmail.com> 

=====
Basic
=====

Creating project::

 ./rebar create-app appid=rbt

Creating node::

 mkdir rel
 cd rel
 ../rebar create-node nodeid=rbt

Patching **rel/reltool.config**::

 {sys, [
       {lib_dirs, ["../../"]}, <<<<< changed from []!
       {erts, [{mod_cond, derived}, {app_file, strip}]},
       {app_file, strip},
       {rel, "rbt", "1",
        [
         kernel,
         stdlib,
         sasl,
         inets, <<<<< added!
         rbt
        ]},
       {rel, "start_clean", "",
        [
         kernel,
         stdlib
        ]},
       {boot_rel, "rbt"},
       {profile, embedded},
       {incl_cond, exclude},
       {excl_archive_filters, [".*"]}, %% Do not archive built libs
       {excl_sys_filters, ["^bin/.*", "^erts.*/bin/(dialyzer|typer)",
                           "^erts.*/(doc|info|include|lib|man|src)"]},
       {excl_app_filters, ["\.gitignore"]},
       {app, inets,   [{incl_cond, include}]}, <<<<< added!
       {app, sasl,   [{incl_cond, include}]},
       {app, stdlib, [{incl_cond, include}]},
       {app, kernel, [{incl_cond, include}]},
       {app, rbt, [{incl_cond, include}]}
      ]}.

 {target_dir, "rbt"}.

 {overlay, [
           {mkdir, "log/sasl"},
           {copy, "files/erl", "\{\{erts_vsn\}\}/bin/erl"},
           {copy, "files/nodetool", "\{\{erts_vsn\}\}/bin/nodetool"},
           {copy, "files/rbt", "bin/rbt"},
           {copy, "files/sys.config", "releases/\{\{rel_vsn\}\}/sys.config"},
           {copy, "files/rbt.cmd", "bin/rbt.cmd"},
           {copy, "files/start_erl.cmd", "bin/start_erl.cmd"},
           {copy, "files/vm.args", "releases/\{\{rel_vsn\}\}/vm.args"}
          ]}.

Verifying **erts** parameters (**rel/files/vm.args**)::

 ## Name of the node
 -name rbt@127.0.0.1

 ## Cookie for distributed erlang
 -setcookie rbt

 ## Heartbeat management; auto-restarts VM if it dies or becomes unresponsive
 ## (Disabled by default..use with caution!)
 ##-heart

 ## Enable kernel poll and a few async threads
 +K true
 +A 42

 ## Increase number of concurrent ports/sockets
 -env ERL_MAX_PORTS 4096

 ## Tweak GC to run more often
 -env ERL_FULLSWEEP_AFTER 10

=======
Testing
=======

From inside **rbt** root directory, type::

 [irocha@york rbt (master)]$ ./rebar clean && ./rebar compile && ./rebar generate
 ==> rel (clean)
 ==> rbt (clean)
 ==> rel (compile)
 ==> rbt (compile)
 Compiled src/rbt_app.erl
 Compiled src/rbt_sup.erl
 Compiled src/rbt_server.erl
 ==> rel (generate)

Executing **rbt**::

 [irocha@york rbt (master)]$ rel/rbt/bin/rbt console
 Exec: /home/irocha/erl-tutorials/rbt/rel/rbt/erts-5.8.5/bin/erlexec -boot /home/irocha/erl-tutorials/rbt/rel/rbt/releases/1/rbt -mode embedded -config /home/irocha/erl-tutorials/rbt/rel/rbt/releases/1/sys.config -args_file /home/irocha/erl-tutorials/rbt/rel/rbt/releases/1/vm.args -- console
 Root: /home/irocha/erl-tutorials/rbt/rel/rbt
 Erlang R14B04 (erts-5.8.5) [source] [64-bit] [smp:4:4] [rq:4] [async-threads:42] [hipe] [kernel-poll:true]

 RBT started [{port,1972},
              {server_root,"/tmp"},
              {document_root,"/tmp"},
              {bind_address,{127,0,0,1}},
              {server_name,"rbt"},
              {modules,[rbt_server]}]...
 Eshell V5.8.5  (abort with ^G)
 (rbt@127.0.0.1)1> 

 [irocha@york rbt (master)]$ curl -v http://localhost:1972/ -d "data=ale%20&%20ivan";echo
 * About to connect() to localhost port 1972 (#0)
 *   Trying 127.0.0.1... connected
 * Connected to localhost (127.0.0.1) port 1972 (#0)
 > POST / HTTP/1.1
 > User-Agent: curl/7.21.7 (x86_64-redhat-linux-gnu) libcurl/7.21.7 NSS/3.13.1.0 zlib/1.2.5 libidn/1.22 libssh2/1.2.7
 > Host: localhost:1972
 > Accept: */*
 > Content-Length: 19
 > Content-Type: application/x-www-form-urlencoded
 > 
 < HTTP/1.1 200 OK
 < Server: inets/5.7.1
 < Date: Wed, 15 Feb 2012 16:26:44 GMT
 < Content-Length: 686
 < Content-Type: plain/text; charset=ISO-8859-1
 < 
 RBT (data received):
 {{mod,{init_data,{49072,"127.0.0.1"},"york"},
      [],ip_comm,#Port<0.1018>,httpd_conf__127_0_0_1__1972,"POST",
      "localhost:1972/","/","HTTP/1.1","POST / HTTP/1.1",
      [{"content-type","application/x-www-form-urlencoded"},
       {"content-length","19"},
       {"accept","*/*"},
       {"host","localhost:1972"},
       {"user-agent",
        "curl/7.21.7 (x86_64-redhat-linux-gnu) libcurl/7.21.7 NSS/3.13.1.0 zlib/1.2.5 libidn/1.22 libssh2/1.2.7"}],
      "data=ale%20&%20ivan",true},
 "data=ale%20&%20ivan",
 [{port,1972},
  {server_root,"/tmp"},
  {document_root,"/tmp"},
  {bind_address,{127,0,0,1}},
  {server_name,"rbt"},
  {modules,[rbt_server]}]}
 * Connection #0 to host localhost left intact
 * Closing connection #0

Manual start with **shell**::

 [irocha@york rbt (master)]$ erl -pa ebin +K true +A 42 +B -s inets start -s rbt_app start
 Erlang R14B04 (erts-5.8.5) [source] [64-bit] [smp:4:4] [rq:4] [async-threads:42] [hipe] [kernel-poll:true]

 Eshell V5.8.5  (abort with ^G)
 1> RBT started [{port,1972},
                 {server_root,"/tmp"},
                 {document_root,"/tmp"},
                 {bind_address,{127,0,0,1}},
                 {server_name,"rbt"},
                 {modules,[rbt_server]}]...

