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

        System.out.println("request:" + request.getRequestId());
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
