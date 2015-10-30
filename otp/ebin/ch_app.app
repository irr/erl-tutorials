
{application, ch_app,
    [{description, "OTP ch sample"},
     {vsn, "0.1.0"},
     {modules, [ch_app, ch_sup, ch_server]},
     {registered, [ch_sup]},
     {applications, [kernel, stdlib, sasl]},
     {mod, {ch_app, []}}
]}.
