package org.apache.dubbo.erlang.analysis.erltool;

import java.io.Serializable;

/**
 * Created by dlive on 16/9/29.
 */
public class UserInfo implements Serializable {
    private String username;
    private Integer age;
    private String password;

    public UserInfo(){

    }

    public String getUsername() {
        return username;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    public Integer getAge() {
        return age;
    }

    public void setAge(Integer age) {
        this.age = age;
    }

    public String getPassword() {
        return password;
    }

    public void setPassword(String password) {
        this.password = password;
    }
}
