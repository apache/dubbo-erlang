dubboerl
=====
Apache Dubbo Erlang Implementation.

[![Build Status](https://travis-ci.org/dubboerl/dubboerl.svg?branch=master)](https://travis-ci.org/dubboerl/dubboerl)
[![codecov](https://codecov.io/gh/dubboerl/dubboerl/branch/master/graph/badge.svg)](https://codecov.io/gh/dubboerl/dubboerl)

Feature list
-----
The following features are supported.

* Zookeeper registry center (√)
* Tcp Transport
* Serialize
	* Hession serialize (√)
	* Json serialize (√)
* Erlang project as a consumer (√)
* Erlang project as a provider (√)
* Sync invoker (√)
* Async invoker (√)
* Random load balance (√)
* Network rate limit (√)
* Connection pools (√)

Start
-----

#### Import

Add dubblerl to rebar.config with your project
```erlang
{deps, [
    {dubboerl, {git, "http://github.com/dubboerl/dubboerl.git", {branch, "master"}}}
]}.
```

#### Step1

Use [erlanalysis](https://github.com/dubboerl/erlanalysis) tool transfer java interface to erlang lib. And add the lib to you project app dir.

#### Step2

configure dubbo reference.

in sys.config add dubboerl config. 
For example:
```erlang
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
Init dubboerl application context when your project start.

	dubboerl:init().

#### Step5
Call the interface method.

```erlang
RequestPara = #testReq{name = <<"nameinfo">>,nick = <<"nickinfo">>,age = 10},
iProcessData:queryinfo(Info,[]).
```

Sample
------
Reference the demo project [dubboerl_demo](https://github.com/dubboerl/dubboerl_demo)

More Documents
------
Reference [Docs](docs/index.md)

