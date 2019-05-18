
## Start 

#### Step1

Generate Java facade

```
cd dubboservice
mvn install
```

#### Step2

Generate Erlang lib by erlanalysis tool.

```
java -cp erlanalysis-1.0.jar com.ifcoder.dubboerl.analysis.ParserStart com.ifcoder.demo dubbo-service 1.2
```

#### Step3

	cp dubbo-service erlang lib to dubboerl_demo

#### Stpe4

```
cd dubboerl_demo
rebar3 shell --apps "dubboerl_demo"
```
