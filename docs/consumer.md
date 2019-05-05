# Consumer Configurations

## Base Config
Consumer config is under the dubboerl application with sys.config
```erlang
{dubboerl,[
	%% other config ...
	{consumer,[
		{<<"interface fullname">>,[Option]},
		%% eg:
		{<<"com.ifcoder.demo.facade.User">>,[]},
	]}
]}
```
Option is to be added.
