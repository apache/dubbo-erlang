# Provider Configurations

## Base Config
Provider config is under the dubboerl application with sys.config
```erlang
{dubboerl,[
	%% other config ...
	{provider,[
	    {module_implements,interface_module,interface_fullname,[Options]},
	    %% eg:
        {user_impl,user,<<"com.ifcoder.demo.facade.User">>,[Option]}
    ]}
]}
```

| ConfigName | Type | DefaultValue | Remarks |
| --- | --- | --- | --- |
| module_implements | atom() | - | The service implements module name|
| interface_module | atom() | - | Interface module name is transfer form java jar |
| interface_fullname | binary() | - | Interface full name is the java class name |

Option is to be added.