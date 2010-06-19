=========
TS Server
=========

About
-----
* A basic tcp server using two aproaches:
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

From inside *ts1/src* or *ts2/src* directory, type::

 erl -pa ../ebin +K true +A 42 +B -run ts_app start

To change parameters (*module, timeout or port*), use (**-ts <parameter> <value>**)::

 erl -pa ../ebin +K true +A 42 +B -run ts_app start -ts <parameter> <value>


 



