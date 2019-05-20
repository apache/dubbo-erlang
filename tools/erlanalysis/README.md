# erlanalysis
parse dubbo interface to erlang lib

## Usage

### Step1 
    mvn pacakge -Dmaven.skip.test=true

### Steps2
```
java -jar target/erlanalysis-1.0.jar groupId artifactId version
## example:
## java -jar target/erlanalysis-1.0.jar org.apache.dubbo.erlang dubbo-sample-service 1.3
```