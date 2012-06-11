======================
Rebar + Cowboy + Redis
======================

About
-----
* A basic rebar skeleton using cowboy and redo as dependencies...

Dependencies
------------
- Erlang/OTP
- cowboy (https://github.com/extend/cowboy)
- redo (https://github.com/JacobVorreuter/redo)

Author
------
Ivan Ribeiro Rocha <ivan.ribeiro@gmail.com> 

=====
Basic
=====

Creating project::

 ./rebar create-app appid=rbtc

Creating node::

 mkdir rel
 cd rel
 ../rebar create-node nodeid=rbtc

Patching **rel/reltool.config**:

**patch**::

 ...
       {lib_dirs, ["../../", "../deps"]}, 
 ...
       {rel, "rbt", "1",
        [
         kernel,
         stdlib,
         sasl,
         inets,
         cowboy,
         redo,
         rbt
        ]},
  ...
       {app, redo,   [{incl_cond, include}]},
       {app, cowboy,   [{incl_cond, include}]},       
       {app, inets,   [{incl_cond, include}]},
  ...

**final**::

 {sys, [
       {lib_dirs, ["../../", "../deps"]},
       {erts, [{mod_cond, derived}, {app_file, strip}]},
       {app_file, strip},
       {rel, "rbtc", "1",
        [
         kernel,
         stdlib,
         sasl,
         inets,
         cowboy,
         redo,
         rbtc
        ]},
       {rel, "start_clean", "",
        [
         kernel,
         stdlib
        ]},
       {boot_rel, "rbtc"},
       {profile, embedded},
       {incl_cond, exclude},
       {excl_archive_filters, [".*"]}, %% Do not archive built libs
       {excl_sys_filters, ["^bin/.*", "^erts.*/bin/(dialyzer|typer)",
                           "^erts.*/(doc|info|include|lib|man|src)"]},
       {excl_app_filters, ["\.gitignore"]},
       {app, redo,   [{incl_cond, include}]},
       {app, cowboy,   [{incl_cond, include}]},              
       {app, inets,   [{incl_cond, include}]},
       {app, sasl,   [{incl_cond, include}]},
       {app, stdlib, [{incl_cond, include}]},
       {app, kernel, [{incl_cond, include}]},
       {app, rbtc, [{incl_cond, include}]}
      ]}.

 {target_dir, "rbtc"}.

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
 -name rbtc@127.0.0.1

 ## Cookie for distributed erlang
 -setcookie rbtc

 ## Heartbeat management; auto-restarts VM if it dies or becomes unresponsive
 ## (Disabled by default..use with caution!)
 ##-heart

 ## Enable kernel poll and a few async threads
 +K true
 +A 42
 +B

 ## Increase number of concurrent ports/sockets
 -env ERL_MAX_PORTS 4096

 ## Tweak GC to run more often
 -env ERL_FULLSWEEP_AFTER 10

========
Building
========

From inside **rbtc** root directory, type::

 [irocha@york rbtc (master)]$ ./rebar clean && ./rebar get-deps && ./rebar check-deps && ./rebar compile && ./rebar generate
 ==> redo (clean)
 ==> cowboy (clean)
 ==> rel (clean)
 ==> rbtc (clean)
 ==> redo (get-deps)
 ==> cowboy (get-deps)
 ==> rel (get-deps)
 ==> rbtc (get-deps)
 ==> redo (check-deps)
 ==> cowboy (check-deps)
 ==> rel (check-deps)
 ==> rbtc (check-deps)
 ==> redo (compile)
 Compiled src/bench.erl
 Compiled src/redo_redis_proto.erl
 Compiled src/redo_concurrency_test.erl
 Compiled src/redo_uri.erl
 Compiled src/redo.erl
 ==> cowboy (compile)
 Compiled src/cowboy_protocol.erl
 Compiled src/cowboy_http_websocket_handler.erl
 Compiled src/cowboy_http_handler.erl
 Compiled src/cowboy_bstr.erl
 Compiled src/cowboy_dispatcher.erl
 Compiled src/cowboy_clock.erl
 Compiled src/cowboy_multipart.erl
 Compiled src/cowboy_listener.erl
 Compiled src/cowboy_requests_sup.erl
 Compiled src/cowboy_http_req.erl
 Compiled src/cowboy_listener_sup.erl
 Compiled src/cowboy_tcp_transport.erl
 Compiled src/cowboy_acceptors_sup.erl
 Compiled src/cowboy_http_protocol.erl
 Compiled src/cowboy_ssl_transport.erl
 Compiled src/cowboy_acceptor.erl
 Compiled src/cowboy.erl
 Compiled src/cowboy_sup.erl
 Compiled src/cowboy_app.erl
 Compiled src/cowboy_http_static.erl
 Compiled src/cowboy_cookies.erl
 Compiled src/cowboy_http_rest.erl
 Compiled src/cowboy_http_websocket.erl
 Compiled src/cowboy_http.erl
 ==> rel (compile)
 ==> rbtc (compile)
 Compiled src/rbtc_sup.erl
 Compiled src/rbtc_app.erl
 Compiled src/rbtc_handler.erl
 ==> rel (generate) 


Executing **rbtc**::

 [irocha@york rbtc (master)]$ rel/rbtc/bin/rbtc console
 Exec: /home/irocha/git/erl-tutorials/rbtc/rel/rbtc/erts-5.9.1/bin/erlexec -boot /home/irocha/git/erl-tutorials/rbtc/rel/rbtc/releases/1/rbtc -mode embedded -config /home/irocha/git/erl-tutorials/rbtc/rel/rbtc/releases/1/sys.config -args_file /home/irocha/git/erl-tutorials/rbtc/rel/rbtc/releases/1/vm.args -- console
 Root: /home/irocha/git/erl-tutorials/rbtc/rel/rbtc
 Erlang R15B01 (erts-5.9.1) [source] [64-bit] [smp:4:4] [async-threads:42] [hipe] [kernel-poll:true]

 Eshell V5.9.1  (abort with ^G)
 (rbtc@127.0.0.1)1>
 ...

 [irocha@york rbtc (master)]$ curl -v http://localhost:8080/
 [irocha@york rbtc (master)]$ curl -v http://localhost:8080/
 * About to connect() to localhost port 8080 (#0)
 *   Trying 127.0.0.1... connected
 > GET / HTTP/1.1
 > User-Agent: curl/7.22.0 (x86_64-pc-linux-gnu) libcurl/7.22.0 OpenSSL/1.0.1 zlib/1.2.3.4 libidn/1.23 librtmp/2.3
 > Host: localhost:8080
 > Accept: */*
 > 
 < HTTP/1.1 500 Internal Server Error
 < Connection: close
 < Content-Length: 0
 < Date: Mon, 23 Apr 2012 14:54:49 GMT
 < Server: Cowboy
 < 
 * Closing connection #0

Manual start with **shell**::

 [irocha@york rbtc (master)]$ erl -boot start_sasl -pa ebin -pa deps/*/ebin +K true +A 42 +B -s rbtc_app
 Erlang R15B01 (erts-5.9.1) [source] [64-bit] [smp:4:4] [async-threads:42] [hipe] [kernel-poll:true]

 Eshell V5.9.1  (abort with ^G)
 1> 
 ...
