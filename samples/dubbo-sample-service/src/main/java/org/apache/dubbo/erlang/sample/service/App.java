package org.apache.dubbo.erlang.sample.service;

import org.apache.dubbo.erlang.sample.service.bean.UserInfo;
import org.apache.dubbo.erlang.sample.service.facade.UserOperator;
import org.springframework.context.support.ClassPathXmlApplicationContext;

import java.io.IOException;

/**
 * Hello world!
 *
 */
public class App 
{
    public static void main( String[] args ) throws IOException {
        System.out.println("将要监听服务");
        ClassPathXmlApplicationContext context = new ClassPathXmlApplicationContext(
                new String[] { "applicationProvider.xml" });
        context.start();
        UserOperator userOperator = (UserOperator) context.getBean("userInterface");
        UserInfo result = userOperator.getUserInfo("hh-bb");
        System.out.println("result:" + result.getUserName());

        System.out.println("按任意键退出");
        System.in.read();
    }
}
