#!/usr/bin/env python
import sys

# ./ddl.py symbols.sql csv > symbols.csv
# ...|grep UOLL4| awk -F "," '{print $2","$3","$4","$5","$6","$7","$8}' > UOLL4.csv

# sqlite3 symbols.db
# CREATE TABLE symbols (S TEXT NOT NULL, D TEXT NOT NULL, O REAL NOT NULL, H REAL NOT NULL, L REAL NOT NULL, C REAL NOT NULL, V REAL NOT NULL);
# CREATE INDEX symbols_idx01 ON symbols (S);
# CREATE INDEX symbols_idx02 ON symbols (S, D);
# .separator ','
# .import symbols.csv symbols

# R commands:
#> library(quantmod)
#> getSymbols("UOLL4", src="csv", dir="/data/bovespa")
#> candleChart(UOLL4,multi.col=TRUE,theme="white")
#> chartSeries(to.weekly(UOLL4),up.col='white',dn.col='blue',theme="white")
#> addMACD()
#> addBBands()
#> addSAR()
#> addSMI()

INSERT = "INSERT INTO symbols VALUES(\"%s\", \"%s\", %s, %s, %s, %s, %s);"

CREATE_SQLITE = """
/* ./ddl.py symbols.sql |sqlite3 bovespa.db */
DROP TABLE IF EXISTS symbols;

CREATE TABLE symbols (S TEXT NOT NULL, D TEXT NOT NULL, O REAL NOT NULL, H REAL NOT NULL, L REAL NOT NULL, C REAL NOT NULL, V REAL NOT NULL);
CREATE INDEX symbols_idx01 ON symbols (S);
CREATE INDEX symbols_idx02 ON symbols (S, D);
"""

CREATE_MYSQL = """
DROP DATABASE IF EXISTS bovespa;
CREATE DATABASE bovespa;
USE bovespa;
CREATE TABLE symbols (S varchar(10) COLLATE latin1_bin NOT NULL, D varchar(10) COLLATE latin1_bin NOT NULL, O float NOT NULL, H float NOT NULL, L float NOT NULL, C float NOT NULL, V float NOT NULL) ENGINE=MyISAM DEFAULT CHARSET=latin1 COLLATE=latin1_bin;
CREATE INDEX symbols_idx01 ON symbols (S);
CREATE INDEX symbols_idx02 ON symbols (S, D);
START TRANSACTION;
"""

def check(line, fields, n):
    if len(fields) <> n:
        print "ERROR: invalid line! [%s](%s:%d)" % (line, fields, n)
        sys.exit(1)

def process(fname, db = None):
    with open(fname, 'r') as f:
        if db == None or db.lower() == "sqlite":
            print CREATE_SQLITE
        elif db.lower() == "mysql":
            print CREATE_MYSQL
        for l in f:
            if l.startswith("/*"):
                continue
            line = l.strip()
            f1 = line.split("INSERT INTO")
            check(line, f1, 2)
            f2 = f1[1].split("VALUES")
            check(line, f2, 2)
            stock = f2[0].strip().replace(" ", "_")
            f3 = f2[1].split("(")
            check(line, f3, 2)
            f4 = f3[1].split(");")[0]
            values = f4.split(",")
            values.insert(0, stock)
            values[1] = "%s-%s-%s" % (values[1][:4], values[1][4:6], values[1][6:])
            if db <> None and db.lower() == "csv":
                print "%s,%s,%s,%s,%s,%s,%s" % tuple(values)
            else:
                print INSERT % tuple(values)
    if db.lower() == "mysql":
        print "COMMIT;"

if __name__ == '__main__':
    if len(sys.argv) > 1:
        if len(sys.argv) > 2:
            process(sys.argv[1], sys.argv[2])
        else:
            process(sys.argv[1])
    else:
        print "usage: ddl.py <symbols sql file> [sqlite|mysql|csv] default=sqlite"
        sys.exit(1)


