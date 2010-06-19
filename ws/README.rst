===============
WS (web server)
===============

About
-----
* A basic web server using inets service API

Dependencies
------------
- Erlang/OTP

Author
------
Ivan Ribeiro Rocha <ivan.ribeiro@gmail.com> 

=======
Testing
=======

From inside **ws1/src** directory, type::

 erl -pa ../ebin +K true +A 42 +B -run ws_app start

To change parameters (*module, timeout or port*), use (**-ws <parameter> <value>**)::

 erl -pa ../ebin +K true +A 42 +B -run ws_app start -ws <parameter> <value>

Valid parameters can be checked inside **ws1/ebin/ws.app**, see bellow::

 {application, ws,
  [{description, "web server application"},
   {vsn, "1.0"},
   {modules, [ws_app, ws_sup, ws]},
   {registered, [ws]},
   {applications, [kernel, stdlib]},
   {mod, {ws_app, []}},
   {env, [{modules, [ws]}, {port, 1972}, {timeout, infinity}, 
          {bind, {127,0,0,1}}, {name, "ws1"},
          {server_root, "/tmp"}, {document_root, "/tmp"}]}
  ]}.

========
Releases
========

Generating *boot scripts* from inside **ws1/src** directory::

 [irocha@napoleon src (master)]$ erl -pa ../ebin +K true +A 42 +B
 Erlang R14A (erts-5.8) [source] [64-bit] [smp:2:2] [rq:2] [async-threads:42] [hipe] [kernel-poll:true]

 Eshell V5.8  (abort with ^G)
 1> systools:make_script("ws_rel-1.0", [local]).
 ok

* **local** is an option that means that the directories where the applications are found are used in the *boot script*, instead of $ROOT/lib
* **$ROOT** is the root directory of the installed release

If you want to make a **release package**, type::

 [irocha@napoleon src (master)]$ erl -pa ../ebin +K true +A 42 +B
 Erlang R14A (erts-5.8) [source] [64-bit] [smp:2:2] [rq:2] [async-threads:42] [hipe] [kernel-poll:true]

 Eshell V5.8  (abort with ^G)
 1> systools:make_script("ws_rel-1.0").
 ok
 2> systools:make_tar("ws_rel-1.0").   
 ok

Generated files::
 
 ws_rel-1.0.boot
 ws_rel-1.0.script
 ws_rel-1.0.tar.gz (make_tar)

Executing **ws1** *boot script*::

 [irocha@napoleon src (master)]$ erl +K true +A 42 +B -boot ws_rel-1.0
 Erlang R14A (erts-5.8) [source] [64-bit] [smp:2:2] [rq:2] [async-threads:42] [hipe] [kernel-poll:true]

 WS started [{port,1972},
             {server_root,"/tmp"},
             {document_root,"/tmp"},
             {bind_address,{127,0,0,1}},
             {server_name,"ws1"},
             {modules,[ws]}]...

 Eshell V5.8  (abort with ^G)
 1> 
 

 

 



