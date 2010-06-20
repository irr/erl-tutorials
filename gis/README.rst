=========
GIS Server
=========

About
-----
* Creating dynamic **gen_servers** and *gen_fsms* adding and removing childs

Dependencies
------------
- Erlang/OTP

Author
------
Ivan Ribeiro Rocha <ivan.ribeiro@gmail.com> 

=======
Testing
=======

From inside **gis/src** directory, type::

 [irocha@napoleon src (master)]$ erl -pa ../ebin +K true +A 42 +B -run gis_app start
 Erlang R14A (erts-5.8) [source] [64-bit] [smp:2:2] [rq:2] [async-threads:42] [hipe] [kernel-poll:true]

 Eshell V5.8  (abort with ^G)
 GIS started gis_fsm ({gen,<0.38.0>,fsm,<0.39.0>})...


Verifying *status* using **regs()**::

 1> regs().
 
 ** Registered procs on node nonode@nohost **
 Name                  Pid          Initial Call                      Reds Msgs
 ...
 gis                   <0.38.0>     gis:init/1                          95    0
 gis_fsm               <0.39.0>     gis_fsm:init/1                      19    0
 gis_sup               <0.37.0>     supervisor:gis_sup/1               109    0
 ...

Starting another *child* managed by **gis_sup** (*supervisor*)::

 2> gis_sup:add_child(gis1).
 GIS started gis1_fsm ({gen,<0.43.0>,fsm,<0.44.0>})...
 {ok,<0.43.0>}
 3> regs().
 
 ** Registered procs on node nonode@nohost **
 Name                  Pid          Initial Call                      Reds Msgs
 ...
 gis                   <0.38.0>     gis:init/1                          95    0
 gis1                  <0.43.0>     gis:init/1                          68    0
 gis1_fsm              <0.44.0>     gis_fsm:init/1                      19    0
 gis_fsm               <0.39.0>     gis_fsm:init/1                      19    0
 gis_sup               <0.37.0>     supervisor:gis_sup/1               172    0
 ...

Calling **gis** and **gis1**::

 4> gis:test(gis, 1).
 test called: {{gen,<0.38.0>,fsm,<0.39.0>}}
 {ok,1}
 5> gis:test(gis1, 1).
 test called: {{gen,<0.43.0>,fsm,<0.44.0>}}
 {ok,1}

Testing *supervisor* monitoring **gis1** (supervisor **must** create a replacement)::

 6> exit(whereis(gis1), kill).
 true
 GIS started gis1_fsm ({gen,<0.49.0>,fsm,<0.51.0>})...
 7> regs().

 ** Registered procs on node nonode@nohost **
 Name                  Pid          Initial Call                      Reds Msgs
 ...
 gis                   <0.38.0>     gis:init/1                         118    0
 gis1                  <0.49.0>     gis:init/1                          68    0
 gis1_fsm              <0.51.0>     gis_fsm:init/1                      19    0
 gis_fsm               <0.39.0>     gis_fsm:init/1                      19    0
 gis_sup               <0.37.0>     supervisor:gis_sup/1               245    0
 ...

Shutting down the whole **application** with all childs::

 8> application:stop(gis).

 =INFO REPORT==== 20-Jun-2010::09:14:13 ===
     application: gis
     exited: stopped
     type: temporary
 ok


