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

import org.objectweb.asm.Type;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Method;
import java.lang.reflect.Parameter;
import java.util.*;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.jar.JarInputStream;

public class ParseJarInterfaceInfo {
    private final static Logger logger = LoggerFactory.getLogger(ParseJarInterfaceInfo.class);

    static final String CLAZZ = ".class";


    public List<InterfaceInfo> parseJar(String jarfile) {
        List<InterfaceInfo> list_interfaces = getInterfaceList(jarfile);
        JarFile jarFile = null;
        InputStream inputInteface = null;
        try {
            jarFile = new JarFile(jarfile);
            String interfacePathName;
            for (InterfaceInfo interfaceItem : list_interfaces) {

            }
        } catch (IOException e) {
            logger.debug("parse:", e);
        } finally {
            if (jarFile != null) {
                try {
                    jarFile.close();
                } catch (IOException e) {
                    logger.error("", e);
                }
            }
            if (inputInteface != null)
                try {
                    inputInteface.close();
                } catch (IOException e) {
                    logger.error("", e);
                }
        }
//        parseArgs(jarfile);
        return list_interfaces;
    }

    private List<InterfaceInfo> getInterfaceList(String jarfilePath) {
        JarInputStream jarFile = null;
        JarEntry jarEntry;
        String name;
        List<InterfaceInfo> list_interfaces = new ArrayList<InterfaceInfo>();
        try {
            jarFile = new JarInputStream(new FileInputStream(jarfilePath));
            jarEntry = jarFile.getNextJarEntry();
            while (jarEntry != null) {
                name = jarEntry.getName();
                if (name.endsWith(CLAZZ)) {
                    logger.debug(name);
                    if (name.indexOf("WEB-INF/classes/") > -1)
                        name = name.substring(16, name.length());
                    name = name.replace("/", ".").substring(0, name.length() - 6);
                    logger.debug("get interfaceName {}", name);
                    Class tmpClass = Class.forName(name, false, Thread.currentThread().getContextClassLoader());
                    if (tmpClass.isInterface()) {
                        InterfaceInfo newInterface = new InterfaceInfo();
                        newInterface.setInterfaceName(name);
                        parseClassMethods(tmpClass, newInterface);
                        list_interfaces.add(newInterface);
                    }
                }
                jarEntry = jarFile.getNextJarEntry();
            }
        } catch (IOException e) {
            logger.error("", e);
        } catch (Exception e) {
            logger.error("", e);
        } finally {
            if (jarFile != null) {
                try {
                    jarFile.close();
                } catch (IOException e) {
                    logger.error("", e);
                }
            }
        }
        return list_interfaces;
    }

    private void parseClassMethods(Class classObj, InterfaceInfo interfaceInfo) {
        Method[] methodsList = classObj.getDeclaredMethods();
        for (int i = 0; i < methodsList.length; i++) {
            Method method = methodsList[i];
            MethodInfo methodInfo = new MethodInfo();
            methodInfo.setName(method.getName());
            String methodDescriptor = Type.getMethodDescriptor(method);
            methodInfo.setMethodDescriptor(methodDescriptor);

            int tmpIndex = methodDescriptor.indexOf(")");
            String argsDescriptor = methodDescriptor.substring(1, tmpIndex);
            String returnDescriptor = methodDescriptor.substring(tmpIndex);
            methodInfo.setArgsType(argsDescriptor);
            methodInfo.setReturnType(returnDescriptor);

            methodInfo.setArgsLength(Type.getArgumentTypes(method).length);

            Parameter[] parameters = method.getParameters();
            String[] paramterNames = new String[parameters.length];
            for (int index = 0; index < parameters.length; index++) {
                paramterNames[index] = parameters[index].getName();
            }
            methodInfo.setParameterName(paramterNames);
//          通过asm获取存在为null的情况。
//            try {
//                String[] paramterNames = MethodParseUtil.getMethodParamNames(method);
//                methodInfo.setParameterName(paramterNames);
//            } catch (IOException e) {
//                logger.warn("无法获取参数名称",e);
//                Parameter[] parameters = method.getParameters();
//                String[] paramterNames=new String[parameters.length];
//                for(int index=0;index<parameters.length;index++){
//                    paramterNames[i]=parameters[i].getName();
//                }
//                methodInfo.setParameterName(paramterNames);
//            }


            interfaceInfo.addMethods(methodInfo);

            checkCommonType(Type.getArgumentTypes(method));
            checkCommonType(Type.getReturnType(method));
        }
    }

    private void checkCommonType(Type[] types) {
        for (int i = 0; i < types.length; i++) {
            checkCommonType(types[i]);
        }
    }

    private void checkCommonType(Type type) {
        CommonTypeInfo.add(type.getClassName());

//        switch (type.getSort()){
//            case Type.ARRAY:
//
//                break;
//            case Type.OBJECT:
//                type.getClassName();
//                if(!type.getClassName().startsWith("java.lang")){
//                    CommonTypeInfo.add(type.getClassName());
//                }
//                break;
//            default:
//        }
    }

//    private String[] generalMethodId(Method method, ClassUtils utils, String classPath){
//        String[] pathOfMethods = utils.getPathOfMethod(method);
//        String requestType = utils.getRequestType(method);
//        List<String> annotationParameters = utils.getAnnotationParameters(method,false);
//        String[] methodIds = new String[pathOfMethods.length];
//        int count = 0;
//        for(String pathOfMethod:pathOfMethods){
//            String urlPath  = utils.getUrlPath(annotationParameters, classPath, pathOfMethod);
//            if(urlPath.indexOf("/{")>0)
//                urlPath = urlPath.replace("/{","/${");
//            if(urlPath.indexOf("={")>0)
//                urlPath = urlPath.replace("={","=${");
//            String methodId = requestType+"#"+urlPath;
//            methodIds[count++]=methodId;
//        }
//
//        return  methodIds;
//    }

    private String formatData(List<String> list, String prefix, String suffix, boolean isJsonFormat) {
        StringBuilder jsonSb = new StringBuilder(prefix);
        for (String key : list) {
            if (jsonSb.length() > 1)
                jsonSb.append(",");
            if (isJsonFormat)
                jsonSb.append("\"" + key + "\":" + ",\"\"");
            else
                jsonSb.append(key);
        }
        jsonSb.append(suffix);
        if (jsonSb.length() < 2)
            jsonSb.delete(0, jsonSb.length());
        return jsonSb.toString();
    }
}
