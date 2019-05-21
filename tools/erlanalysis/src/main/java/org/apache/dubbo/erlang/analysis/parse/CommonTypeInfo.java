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

import java.beans.IntrospectionException;
import java.beans.PropertyDescriptor;
import java.lang.reflect.Field;
import java.util.*;

import org.objectweb.asm.Type;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CommonTypeInfo {

    private static CommonTypeInfo obj;
    private final static Logger logger = LoggerFactory.getLogger(CommonTypeInfo.class);

    private Map<String, CommonTypeItem> commonTypeList = new HashMap<>();

    public static CommonTypeInfo getInstances() {
        if (obj == null) {
            obj = new CommonTypeInfo();
        }
        return obj;
    }

    public static void add(String commonArgType) {
        CommonTypeInfo typeinfo = getInstances();
        typeinfo.parseTypeinfo(commonArgType);
    }


    private void addCommonTypeList(String typeName, CommonTypeItem item) {

    }

    private void parseTypeinfo(String typeName) {
        if (commonTypeList.get(typeName) != null) {
            return;
        }
        logger.info("will parse type info {}", typeName);
        Type type = Type.getObjectType(typeName);
        switch (type.getSort()) {
            case Type.ARRAY:
                //todo
                break;
            case Type.OBJECT:
                if (isTypeNeedParse(type.getClassName())) {
                    parseTypeDetailInfo(type);
                } else if (type.getClassName().startsWith("java.util.List")) {

                }
                break;
            default:
        }
    }

    private void parseTypeDetailInfo(Type type) {
        CommonTypeItem typeItem = new CommonTypeItem();
        typeItem.setClassName(type.getClassName());
        Class classObj = null;
        try {
            classObj = Class.forName(type.getClassName(), false, Thread.currentThread().getContextClassLoader());
            Field[] fields = classObj.getDeclaredFields();
            for (Field field : fields) {
                try {
                    PropertyDescriptor propertyDesc = new PropertyDescriptor(field.getName(), classObj);
                    if (propertyDesc.getReadMethod() == null && propertyDesc.getWriteMethod() == null) {
                        continue;
                    }
                } catch (IntrospectionException e) {
                    logger.warn("get field property error", e);
                    continue;
                }
                CommonTypeFieldInfo fieldInfo = new CommonTypeFieldInfo();
                fieldInfo.setFieldName(field.getName());
                fieldInfo.setFieldType(field.getType());
                typeItem.addField(fieldInfo);
                //判断该类型是否需要继续解析
                parseTypeinfo(field.getType().getName());
            }
            commonTypeList.put(type.getClassName(), typeItem);
            logger.debug("parse type detail success {}", type.getClassName());
        } catch (ClassNotFoundException e) {
            e.printStackTrace();
        }


    }

    public List<CommonTypeItem> getCommonTypeList() {
        List<CommonTypeItem> list = new ArrayList<>(commonTypeList.size());
        for (Map.Entry<String, CommonTypeItem> item : commonTypeList.entrySet()) {
            CommonTypeItem type = item.getValue();
            if (type.getTypeName().equals("list")) {
                continue;
            }
            list.add(item.getValue());
        }
        return list;
    }

    public boolean isTypeNeedParse(String typeName) {
        if (typeName.startsWith("java.lang") || typeName.equals("int") || typeName.equals("double") || typeName.equals("float")) {
            return false;
        }
        return true;
    }
}
