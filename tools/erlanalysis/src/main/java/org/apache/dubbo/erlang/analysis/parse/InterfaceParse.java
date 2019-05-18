package org.apache.dubbo.erlang.analysis.parse;

import org.apache.dubbo.erlang.analysis.generater.ErlProjectGenerater;
import org.apache.dubbo.erlang.analysis.generater.ProjectInfo;
import org.apache.dubbo.erlang.analysis.utils.MavenJarUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;

import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.List;

public class InterfaceParse {
    private final static Logger logger= LoggerFactory.getLogger(InterfaceParse.class);

    public void parse(String group,String artifactid,String version){


        MavenJarUtil mavenInfo = new MavenJarUtil(group, artifactid, version);
        if(!mavenInfo.copyDependence()){
            logger.error("download maven jar error");
        }
        String parserJarPath=mavenInfo.getMainJarPath();
        logger.info("parse main jar "+ parserJarPath);
        loadDependencyJar(mavenInfo.getRootDir()+File.separator+"lib");
        ParseJarInterfaceInfo parser = new ParseJarInterfaceInfo();
        List<InterfaceInfo> interfaceList = parser.parseJar(parserJarPath);

        ProjectInfo projectInfo = new ProjectInfo();
        projectInfo.setAppName(mavenInfo.getArtifactId());
        projectInfo.setAppVersion(mavenInfo.getVersion());
        projectInfo.setPrefix("test_");

        ErlProjectGenerater generater = new ErlProjectGenerater(projectInfo);
        generater.genProject(interfaceList);
    }


    private boolean loadDependencyJar(String rootPath){
        File jarPathFile = new File(rootPath);
        if(!jarPathFile.exists() || jarPathFile.isFile() ){
            logger.error("load dependency error target dir unexist {}",rootPath);
            return false;
        }
        List urlList=new ArrayList<URL>();
        File[] fileList = jarPathFile.listFiles();
        for(int i =0;i<fileList.length;i++){
            File item = fileList[i];
            if(item.isDirectory()){
                continue;
            }
            try {
                if(item.getAbsolutePath().endsWith(".jar")) {
                    urlList.add(item.toURI().toURL());
                    logger.debug("url class load add lib {}", item.getAbsolutePath());
                }
            } catch (MalformedURLException e) {
                logger.error("get jar list error ",e);
            }
        }

        URL[] urls = new URL[urlList.size()];
        urlList.toArray(urls);
        URLClassLoader cl = new URLClassLoader(urls,Thread.currentThread().getContextClassLoader());
        Thread.currentThread().setContextClassLoader(cl);
        return true;
    }


}
