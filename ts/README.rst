=========
TS Server
=========

About
-----
* A basic tcp server (with an *echo* server sample module) using two aproaches:
 - ts1: basic erlang (gen_server) 
 - ts2: using async acceptors using undocumented module (prim_inet)

Dependencies
------------
- Erlang/OTP

Author
------
Ivan Ribeiro Rocha <ivan.ribeiro@gmail.com> 

=======
Testing
=======

From inside **ts1/src** or **ts2/src** directory, type::

 erl -pa ../ebin +K true +A 42 +B -run ts_app start

To change parameters (*module, timeout or port*), use (**-ts <parameter> <value>**)::

 erl -pa ../ebin +K true +A 42 +B -run ts_app start -ts <parameter> <value>

Valid parameters can be checked inside **ts?/ebin/ts.app**, see bellow::

 {application, ts,
   [{description, "echo server application"},
    {vsn, "1.0"},
    {modules, [ts_app, ts_sup, ts, echo]},
    {registered, [ts]},
    {applications, [kernel, stdlib]},
    {mod, {ts_app, []}},
    {env, [{module, echo}, {port, 1972}, {timeout, infinity}]}
   ]}.

========
Releases
========

Generating *boot scripts* from inside **ts1/src** or **ts2/src** directory::

 [irocha@napoleon src (master)]$ erl -pa ../ebin +K true +A 42 +B
 Erlang R14A (erts-5.8) [source] [64-bit] [smp:2:2] [rq:2] [async-threads:42] [hipe] [kernel-poll:true]

 Eshell V5.8  (abort with ^G)
 1> systools:make_script("ts_rel-1.0", [local]).
 ok

* **local** is an option that means that the directories where the applications are found are used in the *boot script*, instead of $ROOT/lib.
* **$ROOT** is the root directory of the installed release.)

If you want to make a **release package**, type::

 [irocha@napoleon src (master)]$ erl -pa ../ebin +K true +A 42 +B
 Erlang R14A (erts-5.8) [source] [64-bit] [smp:2:2] [rq:2] [async-threads:42] [hipe] [kernel-poll:true]

 Eshell V5.8  (abort with ^G)
 1> systools:make_script("ts_rel-1.0").
 ok
 2> systools:make_tar("ts_rel-1.0").   
 ok

Generated files::
 
 ts_rel-1.0.boot
 ts_rel-1.0.script
 ts_rel-1.0.tar.gz (make_tar)

Executing **ts** *boot script*::

 [irocha@napoleon src (master)]$ erl +K true +A 42 +B -boot ts_rel-1.0
 Erlang R14A (erts-5.8) [source] [64-bit] [smp:2:2] [rq:2] [async-threads:42] [hipe] [kernel-poll:true]

 TS server started {ts_state,infinity,echo,{}}...
 Eshell V5.8  (abort with ^G)
 1> 
 

 

 



