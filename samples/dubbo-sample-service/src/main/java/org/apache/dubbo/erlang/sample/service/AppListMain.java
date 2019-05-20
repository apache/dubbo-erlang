package org.apache.dubbo.erlang.sample.service;

import org.springframework.context.support.ClassPathXmlApplicationContext;

import java.io.IOException;

/**
 * Created by dlive on 2018/9/12.
 */
public class AppListMain {

    public static void main( String[] args ) throws IOException {
        System.out.println("将要监听服务");
        ClassPathXmlApplicationContext context = new ClassPathXmlApplicationContext(
                new String[] { "applicationProvider.xml" });
        context.start();
//        UserOperator user = (UserOperator) context.getBean("userInterface");
//        UserRes result = user.queryUserList("listquery");
//        System.out.println("result:" + result.getUserlist().get(0).getUserName());

        System.out.println("按任意键退出");
        System.in.read();
    }
}
