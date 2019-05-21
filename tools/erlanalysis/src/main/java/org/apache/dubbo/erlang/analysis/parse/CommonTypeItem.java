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

package org.apache.dubbo.erlang.analysis.parse;

import org.apache.dubbo.erlang.analysis.utils.ErlTypeTransformUtil;

import java.util.HashSet;
import java.util.Set;

public class CommonTypeItem {
    private String className;
    private Set fields = new HashSet();

    public String getClassName() {
        return className;
    }

    public void setClassName(String className) {
        this.className = className;
    }

    public Set getFields() {
        return fields;
    }

    public void setFields(Set fields) {
        this.fields = fields;
    }

    public void addField(CommonTypeFieldInfo field) {
        fields.add(field);
    }

    /**
     * 获取类型名称
     *
     * @return
     */
    public String getTypeName() {
        return ErlTypeTransformUtil.fullClassNameToLowerShortName(className);
    }

    public CommonTypeFieldInfo[] getFieldList() {
        CommonTypeFieldInfo[] ret = new CommonTypeFieldInfo[fields.size()];
        fields.toArray(ret);
        return ret;
    }
}
