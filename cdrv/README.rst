=======================
Port Driver (fail-safe)
=======================

About
-----
* Basic supervisioned **test** *port driver*

Dependencies
------------
- Erlang/OTP
- GCC

Author
------
Ivan Ribeiro Rocha <ivan.ribeiro@gmail.com> 

=====
Setup
=====

Verify *erlang include directory* inside **priv/Makefile**::

 [irocha@napoleon cdrv (master)]$ cat priv/Makefile 
 GCC=gcc
 LDFLAGS=-shared -fpic
 SRCS=`ls *.c`
 OUTPUT=test_drv.so
 CFLAGS=-o ${OUTPUT}
 INCLUDE=-I /usr/local/lib/erlang/erts-5.8/include/

 all:
	${GCC} ${SRCS} ${LDFLAGS} ${INCLUDE} ${CFLAGS}

 clean:
    rm -rf ${OUTPUT}


=========
Compiling
=========

From inside **cdrv** directory, type::

 [irocha@napoleon cdrv (master)]$ make clean all
 cd src && make clean
 make[1]: Entering directory `/home/irocha/git/erl-tutorials/cdrv/src'
 rm -rf ../ebin/*.beam  *~
 make[1]: Leaving directory `/home/irocha/git/erl-tutorials/cdrv/src'
 cd priv && make clean
 make[1]: Entering directory `/home/irocha/git/erl-tutorials/cdrv/priv'
 rm -rf test_drv.so
 make[1]: Leaving directory `/home/irocha/git/erl-tutorials/cdrv/priv'
 cd src && make
 make[1]: Entering directory `/home/irocha/git/erl-tutorials/cdrv/src'
 erlc +debug_info -o ../ebin -I ../include -W test.erl
 erlc +debug_info -o ../ebin -I ../include -W cdrv_app.erl
 erlc +debug_info -o ../ebin -I ../include -W cdrv_sup.erl
 erlc +debug_info -o ../ebin -I ../include -W cdrv.erl
 make[1]: Leaving directory `/home/irocha/git/erl-tutorials/cdrv/src'
 cd priv && make
 make[1]: Entering directory `/home/irocha/git/erl-tutorials/cdrv/priv'
 gcc `ls *.c` -shared -fpic -I /usr/local/lib/erlang/erts-5.8/include/ -o test_drv.so
 make[1]: Leaving directory `/home/irocha/git/erl-tutorials/cdrv/priv'

=======
Running
=======

From inside **cdrv/src** directory, type::

 [irocha@napoleon src (master)]$ erl -pa ../ebin +K true +A 42 +B -run cdrv_app start
 Erlang R14A (erts-5.8) [source] [smp:2:2] [rq:2] [async-threads:42] [hipe] [kernel-poll:true]

 Eshell V5.8  (abort with ^G)
 CDRV started cdrv ({gen,<0.38.0>,port,{ok,test}})...
 1> cdrv:f(1).
 f(1) = 2
 f called: {{gen,<0.38.0>,port,{ok,test}},2}
 {ok,1}
 2> regs().

 ** Registered procs on node nonode@nohost **
 Name                  Pid          Initial Call                      Reds Msgs
 ...
 cdrv                  <0.43.0>     cdrv:init/1                         91    0
 cdrv_sup              <0.37.0>     supervisor:cdrv_sup/1              185    0
 ...
 test                  <0.44.0>     erlang:apply/2                       9    0
 ...

Killing *port driver* (testing *restart*)::

 3> exit(whereis(test), kill).
 true
 CDRV started cdrv ({gen,<0.43.0>,port,{ok,test}})...
 4> cdrv:f(2).                
 f(2) = 3
 f called: {{gen,<0.43.0>,port,{ok,test}},3}
 {ok,2}


