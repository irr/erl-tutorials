(defmodule test
  (export (setup 0) (run 0)))

(defun show
  (((tuple 'ok Result)) (tuple 'res Result)))

(defun setup ()
    (list-comp ((<- x '(crypto asn1 public_key ssl pooler re2 semver snappy lz4 quickrand uuid cqerl)))
        (application:start x)))

(defun exec 
    (((tuple 'ok Client)) (show (cqerl:run_query Client (binary "SELECT * FROM rt_series;")))))

(defun run ()
    (let ((Conn (cqerl:get_client "127.0.0.1:9042" '(#(keyspace "irr")))))
        (exec Conn)))

