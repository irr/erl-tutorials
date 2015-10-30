
{application, ch_app,
    [{description, "OTP ch sample"},
     {vsn, "0.1.0"},
     {modules, [ch_app, ch_sup, ch_server, eredis, eredis_client, eredis_parser, eredis_sub, eredis_sub_client]},
     {registered, [ch_sup]},
     {applications, [kernel, stdlib, sasl]},
     {mod, {ch_app, []}}
]}.
