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

 

 

 



