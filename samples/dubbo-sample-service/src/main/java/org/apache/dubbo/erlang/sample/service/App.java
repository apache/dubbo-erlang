/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.dubbo.erlang.sample.service;

import org.apache.dubbo.erlang.sample.service.bean.UserInfo;
import org.apache.dubbo.erlang.sample.service.facade.UserOperator;
import org.springframework.context.support.ClassPathXmlApplicationContext;

import java.io.IOException;

/**
 * Hello world!
 */
public class App {
    public static void main(String[] args) throws IOException {
        System.out.println("将要监听服务");
        ClassPathXmlApplicationContext context = new ClassPathXmlApplicationContext(
                new String[]{"applicationConsumer.xml"});
        context.start();
        UserOperator userOperator = (UserOperator) context.getBean("userInterface");
        UserInfo result = userOperator.getUserInfo("hh-bb");
        System.out.println("result:" + result.getUserName());

        System.out.println("按任意键退出");
        System.in.read();
    }
}
