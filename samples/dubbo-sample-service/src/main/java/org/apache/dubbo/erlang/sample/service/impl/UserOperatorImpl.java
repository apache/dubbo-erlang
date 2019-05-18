package org.apache.dubbo.erlang.sample.service.impl;

import org.apache.dubbo.erlang.sample.service.bean.UserInfo;
import org.apache.dubbo.erlang.sample.service.bean.UserInfoRequest;
import org.apache.dubbo.erlang.sample.service.bean.UserRes;
import org.apache.dubbo.erlang.sample.service.facade.UserOperator;

public class UserOperatorImpl implements UserOperator {

    @Override
    public String genUserId() {
        return "userid-123";
    }

    @Override
    public UserInfo getUserInfo(String userid) {
        UserInfo info = new UserInfo();
        info.setUserAge(10);
        info.setUserId("1");
        info.setUserName("testname");
        return info;
    }

    @Override
    public UserInfo queryUserInfo(UserInfoRequest request) {

        System.out.println("request:"+request.getRequestId());
        UserInfo info = new UserInfo();
        info.setUserAge(99);
        info.setUserId("id123");
        info.setUserName("中文姓名");
        return info;
    }

    @Override
    public UserRes queryUserList(String info) {
        return null;
    }
}
