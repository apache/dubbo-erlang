package org.apache.dubbo.erlang.analysis.parse;

import java.util.ArrayList;
import java.util.List;

public class InterfaceInfo {
    private String interfaceName;
    private List<MethodInfo> methods = new ArrayList<MethodInfo>();

    public String getInterfaceName() {
        return interfaceName;
    }

    public String getModuleName(){
        String moduleName= interfaceName.substring(interfaceName.lastIndexOf(".")+1);
        moduleName = moduleName.substring(0, 1).toLowerCase() + moduleName.substring(1);
        moduleName = moduleName.replace('-','_');
        return moduleName;
    }

    public void setInterfaceName(String interfaceName) {
        this.interfaceName = interfaceName;
    }


    public List<MethodInfo> getMethods() {
        return methods;
    }

    public void getMethodExportList(){

    }
    public void addMethods(MethodInfo method) {
        this.methods.add(method);
    }
}
