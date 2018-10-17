dubboerl
=====
A Erlang framework for dubbo.

Feature
-----

* 支持Zookeeper注册中心
* 支持Hession serialize
* 支持Consumer
* 支持Provider
* sync invoker
* async invoker

Start
-----

参考demo [dubboerl_demo](https://github.com/DLive/dubboerl_demo)

#### Step1
Transfer java facede t
o erlang lib and add to you project dir.

#### Step2

configure dubbo refernce

```
{dubboerl,[
	{zookeeper_list,[{"127.0.0.1",2181}]},
	{application,<<"testdubboerl">>},
	{consumer,[
		{<<"me.dlive.dubboservice.service.IProcessData">>,[]}
	]},
	{provider,[
		{scherdule_impl,scherdule_behaviour,<<"me.dlive.dubboservice.service.Scherdule">>,[]}
	]}
	
]}
``` 
#### Step4
Init dubboerl application context

	dubboerl:init().

#### Step5
Do your interface method invoker.

```
RequestPara = #testReq{name = <<"nameinfo">>,nick = <<"nickinfo">>,age = 10},
iProcessData:queryinfo(Info,[]).
```
	


Build
-----

    $ rebar3 compile


release
	
	$ ./rebar3 as dubboerl release -n dubboerl


	
