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

package org.apache.dubbo.erlang.analysis.generater;

import org.apache.dubbo.erlang.analysis.parse.CommonTypeInfo;
import org.apache.dubbo.erlang.analysis.parse.InterfaceInfo;

import org.apache.velocity.Template;
import org.apache.velocity.VelocityContext;
import org.apache.velocity.app.VelocityEngine;
import org.apache.velocity.runtime.RuntimeConstants;
import org.apache.velocity.runtime.resource.loader.ClasspathResourceLoader;

import java.io.*;
import java.util.List;

public class ErlProjectGenerater {
    private VelocityEngine ve = new VelocityEngine();

    private ProjectInfo projectInfo;
    private String projectSaveDir;
    private String projectSrcDir;
    private String projectIncludeDir;

    public ErlProjectGenerater(ProjectInfo projectInfo) {
        this.projectInfo = projectInfo;

        ve.setProperty(RuntimeConstants.RESOURCE_LOADER, "classpath");
        ve.setProperty("classpath.resource.loader.class", ClasspathResourceLoader.class.getName());
        ve.init();
        makeProjectDir();

    }

    public void genProject(List<InterfaceInfo> list_interfaces) {
        for (InterfaceInfo interfaceItem : list_interfaces) {
            genInterface(interfaceItem);
        }
        genProjectCommonFile();
        genProjectIncludeInfo();
        genProjectTypeList();
    }

    private boolean genInterface(InterfaceInfo interfaceItem) {
        // 获取模板文件
        Template t = ve.getTemplate("templates/interface.vm");
        // 设置变量
        VelocityContext ctx = new VelocityContext();
        ctx.put("appName", projectInfo.getAppName());
        ctx.put("moduleName", interfaceItem.getModuleName());
        ctx.put("className", interfaceItem.getInterfaceName());
        ctx.put("methodList", interfaceItem.getMethods());

        // 输出
        StringWriter sw = new StringWriter();
        t.merge(ctx, sw);
        FileWriter sourceFile = null;
        try {
            sourceFile = new FileWriter(this.projectSrcDir + File.separator + interfaceItem.getModuleName() + ".erl");
            sourceFile.write(sw.toString());
        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            try {
                sourceFile.close();
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
        return true;
    }

    private boolean makeProjectDir() {
        String savedir = System.getProperty("user.dir") + File.separator + "genProjectDir";
        projectSaveDir = String.format("%s%s%s", savedir, File.separator, projectInfo.getAppName());

        File rootDir = new File(projectSaveDir);
        if (!rootDir.exists()) {
            if (!rootDir.mkdirs()) {
                return false;
            }
        }
        projectSrcDir = String.format("%s%ssrc", projectSaveDir, File.separator);
        File projectSrcFile = new File(projectSrcDir);
        if (!projectSrcFile.exists()) {
            if (!projectSrcFile.mkdirs()) {
                return false;
            }
        }

        projectIncludeDir = String.format("%s%sinclude", projectSaveDir, File.separator);
        File projectIncludeFile = new File(projectIncludeDir);
        if (!projectIncludeFile.exists()) {
            if (!projectIncludeFile.mkdirs()) {
                return false;
            }
        }
        return true;
    }

    /**
     * 生成项目公共文件
     *
     * @return
     */
    private boolean genProjectCommonFile() {
        // 获取模板文件

        VelocityContext ctx = new VelocityContext();
        ctx.put("appName", projectInfo.getAppName());
        ctx.put("appVersion", projectInfo.getAppVersion());


        FileWriter appFile = null;
        FileWriter appSrcFile = null;
        FileWriter appSupFile = null;
        try {
            // 输出
            Template appTpl = ve.getTemplate("templates/app.vm");
            StringWriter appFileWriter = new StringWriter();
            appTpl.merge(ctx, appFileWriter);

            appFile = new FileWriter(this.projectSrcDir + File.separator + projectInfo.getAppName() + "_app.erl");
            appFile.write(appFileWriter.toString());

            // 输出
            Template appSrcTpl = ve.getTemplate("templates/app_src.vm");
            StringWriter appSrcFileWriter = new StringWriter();
            appSrcTpl.merge(ctx, appSrcFileWriter);
            appSrcFile = new FileWriter(this.projectSrcDir + File.separator + projectInfo.getAppName() + ".app.src");
            appSrcFile.write(appSrcFileWriter.toString());

            Template appSupTpl = ve.getTemplate("templates/app_sup.vm");
            StringWriter appSupFileWriter = new StringWriter();
            appSupTpl.merge(ctx, appSupFileWriter);
            appSupFile = new FileWriter(this.projectSrcDir + File.separator + projectInfo.getAppName() + "_sup.erl");
            appSupFile.write(appSupFileWriter.toString());
        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            try {
                appFile.close();
            } catch (IOException e) {
                e.printStackTrace();
            }
            try {
                appSrcFile.close();
            } catch (IOException e) {
                e.printStackTrace();
            }
            try {
                appSupFile.close();
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
        return true;
    }

    private void genProjectIncludeInfo() {
        // 获取模板文件
        Template t = ve.getTemplate("templates/app_type_include.vm");
        // 设置变量
        VelocityContext ctx = new VelocityContext();
        ctx.put("typeList", CommonTypeInfo.getInstances().getCommonTypeList());
        // 输出
        StringWriter sw = new StringWriter();
        t.merge(ctx, sw);
        writeFile(this.projectIncludeDir + File.separator + projectInfo.getAppName() + ".hrl", sw.toString());
    }

    private void genProjectTypeList() {
        // 获取模板文件
        Template t = ve.getTemplate("templates/app_type_list.vm");
        // 设置变量
        VelocityContext ctx = new VelocityContext();
        ctx.put("appName", projectInfo.getAppName());
        ctx.put("typeList", CommonTypeInfo.getInstances().getCommonTypeList());
        // 输出
        StringWriter sw = new StringWriter();
        t.merge(ctx, sw);
        writeFile(this.projectSrcDir + File.separator + projectInfo.getAppName() + "_type_list.erl", sw.toString());
    }


    private boolean writeFile(String filePath, String content) {
        FileWriter sourceFile = null;
        try {
            sourceFile = new FileWriter(filePath);
            sourceFile.write(content);
        } catch (IOException e) {
            e.printStackTrace();
            return false;
        } finally {
            try {
                sourceFile.close();
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
        return true;
    }

}
