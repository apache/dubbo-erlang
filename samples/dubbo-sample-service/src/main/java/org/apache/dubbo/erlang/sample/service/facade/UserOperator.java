package org.apache.dubbo.erlang.sample.service.facade;

import org.apache.dubbo.erlang.sample.service.bean.UserInfo;
import org.apache.dubbo.erlang.sample.service.bean.UserInfoRequest;
import org.apache.dubbo.erlang.sample.service.bean.UserRes;

public interface UserOperator {
    public String genUserId();
    public UserInfo getUserInfo(String userid);
    public UserInfo queryUserInfo(UserInfoRequest request);
    public UserRes queryUserList(String info);

}
