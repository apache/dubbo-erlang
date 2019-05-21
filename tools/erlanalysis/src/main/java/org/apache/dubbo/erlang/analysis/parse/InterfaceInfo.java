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

import java.util.ArrayList;
import java.util.List;

public class InterfaceInfo {
    private String interfaceName;
    private List<MethodInfo> methods = new ArrayList<MethodInfo>();

    public String getInterfaceName() {
        return interfaceName;
    }

    public String getModuleName() {
        String moduleName = interfaceName.substring(interfaceName.lastIndexOf(".") + 1);
        moduleName = moduleName.substring(0, 1).toLowerCase() + moduleName.substring(1);
        moduleName = moduleName.replace('-', '_');
        return moduleName;
    }

    public void setInterfaceName(String interfaceName) {
        this.interfaceName = interfaceName;
    }


    public List<MethodInfo> getMethods() {
        return methods;
    }

    public void getMethodExportList() {

    }

    public void addMethods(MethodInfo method) {
        this.methods.add(method);
    }
}
