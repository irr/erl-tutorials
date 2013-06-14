=============
erl-tutorials
=============

About
-----
**erl-tutorials** is a set of sample codes whose main purpose is to teach and guide various aspects of *erlang programming*... 

Setup
-----

Dependencies::

  sudo yum install make gcc gcc-c++ kernel-devel m4 java-1.7.0-openjdk-devel
  sudo yum install freeglut-devel wxGTK-devel openssl-devel unixODBC-devel ncurses-devel tk fop
  ./configure --enable-threads --enable-smp-support --enable-kernel-poll --disable-hipe --with-ssl --enable-halfword-emulator


========
Projects
========

TS
--
* `ts <https://github.com/irr/erl-tutorials/tree/master/ts>`_ (tcp servers *README*)
 - `ts1 <https://github.com/irr/erl-tutorials/tree/master/ts/ts1>`_ (basic tcp server)
 - `ts2 <https://github.com/irr/erl-tutorials/tree/master/ts/ts2>`_ (basic tcp server using **prim_inet:async_accept** [*based on thrift_server.erl*])

WS
--
* `ws <https://github.com/irr/erl-tutorials/tree/master/ws>`_ (web services *README*)
 - `ws1 <https://github.com/irr/erl-tutorials/tree/master/ws/ws1>`_ (inets web service)

GIS
---
* `gis <https://github.com/irr/erl-tutorials/tree/master/gis>`_ (**gen_server** and **gen_fsm** *README*)


MySQL
-----
* `mysql <https://github.com/irr/erl-tutorials/tree/master/mysql>`_ (MySQL client **gen_server** *README*)


Port Driver
-----------
* `cdrv <https://github.com/irr/erl-tutorials/tree/master/cdrv>`_ (C Port Driver **gen_server** *README*)

Trader
------
* `et <https://github.com/irr/erl-tutorials/tree/master/et>`_ (Trader **gen_server** *README*)

RBT
------
* `rbt <https://github.com/irr/erl-tutorials/tree/master/rbt>`_ (RBT **rebar** *README*)

Dependencies
------------
- Erlang/OTP

Author
------
Ivan Ribeiro Rocha <ivan.ribeiro@gmail.com> 

Copyright and License
---------------------
Copyright 2012 Ivan Ribeiro Rocha

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.