package org.apache.dubbo.erlang.sample.service.bean;

/**
 * Created by dlive on 2018/9/12.
 */
public class UserInfoRequest {
    public String requestId;
    public String username;

    public void setRequestId(String requestId) {
        this.requestId = requestId;
    }

    public String getRequestId() {
        return requestId;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    public String getUsername() {
        return username;
    }
}
