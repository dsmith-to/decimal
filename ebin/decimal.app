%% This is the application resource file (.app file) for the decimal,
%% application.
{application, decimal, 
  [{description, "Decimal module."},
   {vsn, "0.1.0"},
   {modules, [decimal,
              decimal_raw,
              decimal_app,
              decimal_sup,
              decimal_server,
              decimal_tests,
              ct_expand]},
   {registered,[decimal_sup]},
   {applications, [kernel, stdlib]},
   {mod, {decimal_app,[]}},
   {start_phases, []}]}.

