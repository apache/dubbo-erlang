## 开发

```
    ./rebar3 eunit -m dubbo_zookeeper_tests

    dubbo_zookeeper:register_consumer(<<"com.ifcoder.demo.facade.User">>,[]).
    
    iProcessData:call_object().
    
    
    scherdule:call_object().


    ./rebar3 shell --apps "testdubboerl"
    dubboerl:init().
    
```
### eunit Test
	./rebar3 eunit -m dubbo_zookeeper_tests
	./rebar3 eunit -m de_codec_tests

## 服务注册流程

### 消费者注册



### 生产者注册



### 消费者调用

```
    Option=[
        sync,
        {timeout,5000}
        {attachments,[{<<"a">>,<<"b">>}]}
    ]
```




## Develop Log
