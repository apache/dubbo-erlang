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
import org.objectweb.asm.Type;

import java.util.*;

public class MethodInfo {
    private String name;

    private String methodDescriptor;
    private String argsType;
    private int argsLength;
    private String returnType;
    private String[] parameterName;

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getArgsType() {
        return argsType;
    }

    public void setArgsType(String argsType) {
        this.argsType = argsType;
    }

    public String getReturnType() {
        return returnType;
    }

    public void setReturnType(String returnType) {
        this.returnType = returnType;
    }


    public String getParameterNameString() {
        return String.join(",", parameterName);
    }

    public void setParameterName(String[] parameterName) {
        for (int i = 0; i < parameterName.length; i++) {
            parameterName[i] = parameterName[i].substring(0, 1).toUpperCase() + parameterName[i].substring(1);
        }
        this.parameterName = parameterName;
    }

    public String getParameterTypeDef() {
        ArrayList<String> retList = new ArrayList<>();
        Type[] types = Type.getArgumentTypes(methodDescriptor);
        for (int i = 0; i < types.length; i++) {
            String def = ErlTypeTransformUtil.fullClassNameToTypeDef(types[i].getClassName());
            retList.add(def);
        }
        return String.join(",\n", retList);
    }

    public Map<String, String> getParameterTypeList() {
        Type[] types = Type.getArgumentTypes(methodDescriptor);
        Map<String, String> retInfo = new LinkedHashMap<>();
        for (int i = 0; i < types.length; i++) {
            String def = ErlTypeTransformUtil.fullClassNameToErlType(types[i].getClassName());
            retInfo.put(parameterName[i], def);
        }
        return retInfo;
    }

    /**
     * get -spec return type
     *
     * @return
     */
    public String getReturnErlType() {
        Type types = Type.getReturnType(methodDescriptor);
        return ErlTypeTransformUtil.fullClassNameToErlType(types.getClassName());
    }

    public String getResponseTypeDef() {
        Type types = Type.getReturnType(methodDescriptor);
        return ErlTypeTransformUtil.fullClassNameToTypeDef(types.getClassName());
    }

    public String getMethodDescriptor() {
        return methodDescriptor;
    }

    public void setMethodDescriptor(String methodDescriptor) {
        this.methodDescriptor = methodDescriptor;
    }


    public int getArgsLength() {
        return argsLength;
    }

    public void setArgsLength(int argsLength) {
        this.argsLength = argsLength;
    }
}
