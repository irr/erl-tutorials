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
 Exec: /home/irocha/git/erl-tutorials/rbt/rel/rbt/erts-5.8.5/bin/erlexec -boot /home/irocha/git/erl-tutorials/rbt/rel/rbt/releases/1/rbt -mode embedded -config /home/irocha/git/erl-tutorials/rbt/rel/rbt/releases/1/sys.config -args_file /home/irocha/git/erl-tutorials/rbt/rel/rbt/releases/1/vm.args -- console
 Root: /home/irocha/git/erl-tutorials/rbt/rel/rbt
 Erlang R14B04 (erts-5.8.5) [source] [64-bit] [smp:4:4] [rq:4] [async-threads:0] [hipe] [kernel-poll:false]

 Eshell V5.8.5  (abort with ^G)
 (rbt@127.0.0.1)1> rbt_server:test().
 Test ok!
 ok
