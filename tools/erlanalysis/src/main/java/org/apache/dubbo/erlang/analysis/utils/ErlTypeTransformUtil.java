package org.apache.dubbo.erlang.analysis.utils;

import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Created by dlive on 23/02/2018.
 */
public class ErlTypeTransformUtil {

    public static String fullClassNameToTypeDef(String fullClassName){
        String className = fullClassName.substring(fullClassName.lastIndexOf(".")+1);
        className = className.substring(0, 1).toLowerCase() + className.substring(1);
        String fieldNames = "";
        switch (className){
            case "string":
                fieldNames="[]";
                break;
            default:
                fieldNames = String.format("record_info(fields,%s)",className);
        }
        return String.format("#type_def{foreign_type = <<\"%s\">>,\n" +
                "            native_type = %s,\n" +
                "            fieldnames = %s}",fullClassName,className,fieldNames);
    }

    public static String fullClassNameToLowerShortName(String fullClassName){
        String className = fullClassName.substring(fullClassName.lastIndexOf(".")+1);
        className = className.substring(0, 1).toLowerCase() + className.substring(1);
        return className;
    }

    public static String stringFirstToLower(String str){
        str = str.substring(0, 1).toLowerCase() + str.substring(1);
        return str;
    }

    public static String fullClassNameToErlType(String fullClassName){
        try {
            String type=null;
            if(fullClassName.startsWith("java.lang")|| fullClassName.equals("int") || fullClassName.equals("double") || fullClassName.equals("float")){
                switch (fullClassName){
                    case "java.lang.String":
                        type="list()";
                        break;
                    case "java.lang.Integer":
                        type="integer()";
                        break;
                    case "java.lang.Boolean":
                        type="boolean()";
                        break;
                    case "java.lang.Float":
                        type="float()";
                        break;
                    case "int":
                        type="integer()";
                        break;
                    case "double":
                        type="float()";
                        break;
                    default:
                        return "unkonw";
                }

                return type;
            }
            Class<?> classInfo = Class.forName(fullClassName, false, Thread.currentThread().getContextClassLoader());

            if(classInfo.isAssignableFrom(List.class) ){
                 type="[]";
            }else if(classInfo.isAssignableFrom(Map.class) ){
                type="Map";
            }else if(classInfo.isAssignableFrom(Set.class) ){
                type="Set";
            }else{
                type = "#"+fullClassNameToLowerShortName(fullClassName)+"{}";
            }
            return type;
        } catch (ClassNotFoundException e) {
            e.printStackTrace();
        }
        return null;
    }
}
