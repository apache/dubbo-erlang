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

package org.apache.dubbo.erlang.analysis.utils;

import java.util.List;
import java.util.Map;
import java.util.Set;

public class ErlTypeTransformUtil {

    public static String fullClassNameToTypeDef(String fullClassName) {
        String className = fullClassName.substring(fullClassName.lastIndexOf(".") + 1);
        className = className.substring(0, 1).toLowerCase() + className.substring(1);
        String fieldNames = "";
        switch (className) {
            case "string":
                fieldNames = "[]";
                break;
            default:
                fieldNames = String.format("record_info(fields,%s)", className);
        }
        return String.format("#type_def{foreign_type = <<\"%s\">>,\n" +
                "            native_type = %s,\n" +
                "            fieldnames = %s}", fullClassName, className, fieldNames);
    }

    public static String fullClassNameToLowerShortName(String fullClassName) {
        String className = fullClassName.substring(fullClassName.lastIndexOf(".") + 1);
        className = className.substring(0, 1).toLowerCase() + className.substring(1);
        return className;
    }

    public static String stringFirstToLower(String str) {
        str = str.substring(0, 1).toLowerCase() + str.substring(1);
        return str;
    }

    public static String fullClassNameToErlType(String fullClassName) {
        try {
            String type = null;
            if (fullClassName.startsWith("java.lang") || fullClassName.equals("int") || fullClassName.equals("double") || fullClassName.equals("float")) {
                switch (fullClassName) {
                    case "java.lang.String":
                        type = "list()";
                        break;
                    case "java.lang.Integer":
                        type = "integer()";
                        break;
                    case "java.lang.Boolean":
                        type = "boolean()";
                        break;
                    case "java.lang.Float":
                        type = "float()";
                        break;
                    case "int":
                        type = "integer()";
                        break;
                    case "double":
                        type = "float()";
                        break;
                    default:
                        return "unkonw";
                }

                return type;
            }
            Class<?> classInfo = Class.forName(fullClassName, false, Thread.currentThread().getContextClassLoader());

            if (classInfo.isAssignableFrom(List.class)) {
                type = "[]";
            } else if (classInfo.isAssignableFrom(Map.class)) {
                type = "Map";
            } else if (classInfo.isAssignableFrom(Set.class)) {
                type = "Set";
            } else {
                type = "#" + fullClassNameToLowerShortName(fullClassName) + "{}";
            }
            return type;
        } catch (ClassNotFoundException e) {
            e.printStackTrace();
        }
        return null;
    }
}
