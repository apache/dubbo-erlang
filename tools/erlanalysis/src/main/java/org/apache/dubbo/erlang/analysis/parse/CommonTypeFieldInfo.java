package org.apache.dubbo.erlang.analysis.parse;

import org.apache.dubbo.erlang.analysis.utils.ErlTypeTransformUtil;

/**
 * Created by dlive on 26/02/2018.
 */
public class CommonTypeFieldInfo {
    private String fieldName;
    private Class<?> fieldType;

    public String getFieldName() {
        return ErlTypeTransformUtil.stringFirstToLower(fieldName);
    }

    public void setFieldName(String fieldName) {
        this.fieldName = fieldName;
    }

    public Class<?> getFieldType() {
        return fieldType;
    }

    public void setFieldType(Class<?> fieldType) {
        this.fieldType = fieldType;
    }

    public String getFieldErlType(){
        return ErlTypeTransformUtil.fullClassNameToErlType(fieldType.getName());
    }
}
