package org.apache.dubbo.erlang.sample.service.bean;

import java.util.List;

/**
 * Created by dlive on 2018/9/12.
 */
public class UserRes {
    private String message;
    private Integer code;
    private List<UserInfo> userlist;

    public List<UserInfo> getUserlist() {
        return userlist;
    }

    public void setUserlist(List<UserInfo> userlist) {
        this.userlist = userlist;
    }
}
