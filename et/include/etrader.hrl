-define(APP, etrader).
-record(etrs, {csv, timeout, limit = 0, data = undefined}).
-record(ohlc, {name, date, open, high, low, close, volume}).

