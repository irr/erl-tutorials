=======================
MySQL client gen_server
=======================

About
-----
* Executing SQL commands through a **gen_server** application

Dependencies
------------
- Erlang/OTP
- Erlang MySQL Driver written by Fredrik Thulin, Magnus Ahltorp, Yariv Sadan and Kungliga Tekniska

Author
------
Ivan Ribeiro Rocha <ivan.ribeiro@gmail.com> 

=====
Setup
=====

MySQL configuration
-------------------

MySQL config (**my.cnf**), must include this::

 [mysqld]
 init-connect          = 'SET AUTOCOMMIT=0'
 transaction-isolation = READ-COMMITTED

Database *setup*::

 CREATE DATABASE test;
 USE test;
 CREATE TABLE test(k VARCHAR(64) NOT NULL, d VARCHAR(256) NOT NULL, PRIMARY KEY(k)) ENGINE=INNODB;

=======
Running
=======

From inside **sql/src** directory, type::

 [irocha@napoleon src (master)]$ erl -make && erl -pa ../ebin +K true +A 42 +B -run sql_app start 
 Erlang R14A (erts-5.8) [source] [64-bit] [smp:2:2] [rq:2] [async-threads:42] [hipe] [kernel-poll:true]

 Eshell V5.8  (abort with ^G)
 1> SQL started sql ({{gen,<0.38.0>},
                   {mysql,[{host,"localhost"},
                           {user,"irr"},
                           {included_applications,[]},
                           {encoding,latin1},
                           {password,"mysql"},
                           {port,3306},
                           {size,5},
                           {database,"test"}]}})...

 
Verifying MySQL connections::

 1> regs().

 ** Registered procs on node nonode@nohost **
 Name                  Pid          Initial Call                      Reds Msgs
 ...
 crypto_server         <0.45.0>     crypto_server:init/1               417    0
 crypto_sup            <0.44.0>     supervisor:crypto_sup/1            110    0
 ...
 mysql_dispatcher      <0.46.0>     mysql:init/1                       295    0
 ...
 sql                   <0.38.0>     sql:init/1                        2294    0
 sql_sup               <0.37.0>     supervisor:sql_sup/1               109    0

 ** Registered ports on node nonode@nohost **
 Name                  Id              Command                                 
 crypto_drv01          #Port<0.921>    crypto_drv elibcrypto /usr/local/lib/erl
 crypto_drv02          #Port<0.920>    crypto_drv elibcrypto /usr/local/lib/erl
 ... 
 crypto_drv<nn>        #Port<0....>    crypto_drv elibcrypto /usr/local/lib/erl

Executing SQL commands::

 2> sql:exec("select * from test;").
 {ok,[]}
 3> sql:exec("insert into test(k,d) values('irr', 'ivan ribeiro rocha');").
 {ok,1}
 4> sql:exec("insert into test(k,d) values('ale', 'alessandra santos');"). 
 {ok,1}
 5> sql:exec("select * from test;").
 {ok,[[<<"ale">>,<<"alessandra santos">>],
      [<<"irr">>,<<"ivan ribeiro rocha">>]]}
 6> sql:exec("delete from test;").
 {ok,2}
 7> sql:exec("select * from test;").
 {ok,[]}
