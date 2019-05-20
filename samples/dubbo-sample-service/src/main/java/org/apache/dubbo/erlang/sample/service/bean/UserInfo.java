package org.apache.dubbo.erlang.sample.service.bean;

import java.io.Serializable;

public class UserInfo implements Serializable {

    private String userId;
    private String userName;
    private Integer userAge;

    public String getUserId() {
        return userId;
    }

    public void setUserId(String userId) {
        this.userId = userId;
    }

    public String getUserName() {
        return userName;
    }

    public void setUserName(String userName) {
        this.userName = userName;
    }

    public Integer getUserAge() {
        return userAge;
    }

    public void setUserAge(Integer userAge) {
        this.userAge = userAge;
    }
}
