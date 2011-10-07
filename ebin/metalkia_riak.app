{application,metalkia_riak,
             [{description,"Metalkia riak interface"},
              {vsn,"0.1"},
              {registered,[]},
              {applications,[kernel,stdlib,gproc,riakc]},
              {included_applications,[riak_kv]},
              {mod,{mtriak_app,[]}},
              {env,[]},
              {modules,[mtriak,mtriak_app,mtriak_srv,mtriak_sup]}]}.
