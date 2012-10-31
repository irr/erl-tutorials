#!/bin/bash
echo "Cleaning up..."
rm -rf symbols.db symbols.sql
echo "Extracting SQL file..."
7z x symbols.sql.7z 2>&1 > /dev/null
echo "Generating csv from SQL file..."
./ddl.py symbols.sql csv > symbols.csv
echo "Creating SQLite3 database..."
cat sqlite.cmds |sqlite3 symbols.db
echo "Removing temporary files..."
rm -rf symbols.sql symbols.csv
echo "Done."
