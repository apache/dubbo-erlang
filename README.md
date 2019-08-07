dubboerl
=====
Apache Dubbo Erlang Implementation.

[![Build Status](https://travis-ci.org/apache/dubbo-erlang.svg?branch=master)](https://travis-ci.org/apache/dubbo-erlang)
[![codecov](https://codecov.io/gh/apache/dubbo-erlang/branch/master/graph/badge.svg)](https://codecov.io/gh/apache/dubbo-erlang)

Feature list
-----
The following features are supported.

* Zookeeper registry center (√)
* Dubbo Protocol (√)
* Serialize
	* Hessian serialize (√)
	* Json serialize (√)
* Erlang project as consumer or provider (√)
* Sync and Async invoker (√)
* Random loadbalance (√)
* Connection pools (√)
* Support protocol,cluster,loadbalance,registry and filter extension point.


Start
-----

#### Import

Add dubblerl to rebar.config with your project
```erlang
{deps, [
    {dubboerl, {git, "https://github.com/apache/dubbo-erlang.git", {branch, "master"}}}
]}.
```

#### Step1

Use [erlanalysis](./tools/erlanalysis) tool transfer java interface to erlang lib. And add the lib to you project app dir.

#### Step2

configure dubbo reference.

in sys.config add dubboerl config. 
For example:
```erlang
{dubboerl,[
    {application,<<"testdubboerl">>},
    {protocol, {dubbo, [{port, 20882}]}},
    {registry,zookeeper},
    {zookeeper_list,[{"127.0.0.1",2181}]},
    {serialization,hessian},
    {consumer,[
        {<<"org.apache.dubbo.erlang.sample.service.facade.UserOperator">>,[]}
    ]},
    {provider,[
        {user_impl,userOperator,<<"org.apache.dubbo.erlang.sample.service.facade.UserOperator">>,[]}
    ]}
]}
```

#### Step4
Init dubboerl application context when your project start.

	dubboerl:init().

#### Step5
Call the interface method.

```erlang
Request = #userInfoRequest{requestId = 123, username = "testname"},
userOperator:queryUserInfo(Request,#{sync=> true}).
```

Sample
------
Reference the demo project [dubboerl_demo](./samples/dubboerl_demo)

More Documents
------
Reference [Docs](http://dubbo.apache.org/en-us/docs/user/languages/erlang/start.html)

