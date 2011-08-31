-define(APP, etrader).

-record(etrs, {csv, timeout, limit = 0, data = undefined}).
-record(ohlc, {name, date, open, high, low, close, volume}).

-define(TEST_DATA, "../priv/test.dat").
-define(TEST_SYMBOL, "UOLL4").
-define(TEST_EMA21, "../priv/ema21.dat").
-define(TEST_SMA21, "../priv/sma21.dat").


