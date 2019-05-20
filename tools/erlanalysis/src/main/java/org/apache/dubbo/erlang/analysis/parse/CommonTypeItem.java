package org.apache.dubbo.erlang.analysis.parse;

import org.apache.dubbo.erlang.analysis.utils.ErlTypeTransformUtil;

import java.util.HashSet;
import java.util.Set;

/**
 * Created by dlive on 26/02/2018.
 */
public class CommonTypeItem {
    private String className;
    private Set fields=new HashSet();

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
    public void addField(CommonTypeFieldInfo field){
        fields.add(field);
    }

    /**
     * 获取类型名称
     * @return
     */
    public String getTypeName(){
        return ErlTypeTransformUtil.fullClassNameToLowerShortName(className);
    }

    public CommonTypeFieldInfo[] getFieldList(){
        CommonTypeFieldInfo[] ret=new CommonTypeFieldInfo[fields.size()];
        fields.toArray(ret);
        return ret;
    }
}
