package org.apache.dubbo.erlang.analysis.utils;


import org.apache.maven.shared.invoker.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Collections;

/**
 * Created by dlive on 16/5/31.
 */
public class MavenJarUtil {
    private String groupid;
    private String artifactId;
    private String version;
    private String mainJarPath;
    private String rootDir;
    private static Logger log = LoggerFactory.getLogger(MavenJarUtil.class);

    public MavenJarUtil(String groupid, String artifactId, String version){
        this.groupid = groupid;
        this.artifactId = artifactId;
        this.version = version;


    }

    public boolean copyDependence(){
        String mvn_home =  System.getenv("MAVEN_HOME");
        this.rootDir = genProjectDir(System.getProperty("project_save_dir",System.getProperty("user.dir")+File.separator+"mavenDown"));
        if(rootDir==null){
            return false;
        }

        log.info("down load lib jar dir:"+rootDir);
        String pomPathJar = genPomFile(this.rootDir,"jar");

        InvocationRequest request = new DefaultInvocationRequest();
        request.setPomFile( new File( pomPathJar ) );
        request.setGoals( Collections.singletonList( "compile" ) );

        Invoker invoker = new DefaultInvoker();
        try {
            if(mvn_home!=null){
                log.info("use MAVEN_HOME:"+mvn_home);
                invoker.setMavenHome(new File(mvn_home));
            }

            invoker.setWorkingDirectory(new File(rootDir));
            InvocationResult result = invoker.execute( request );
            if(result.getExitCode()!=0){
                String pomPathWar = genPomFile(this.rootDir,"war");
                InvocationRequest requestWar = new DefaultInvocationRequest();
                requestWar.setPomFile(new File(pomPathWar));
                requestWar.setGoals(Collections.singletonList("compile"));
                invoker.setWorkingDirectory(new File(rootDir));
                InvocationResult resultWar = invoker.execute( requestWar );
                if(resultWar.getExitCode()!=0){
                    log.error("mvn run result exception:"+resultWar.getExecutionException());
                    return false;
                }
            }

        } catch (MavenInvocationException e) {
            e.printStackTrace();
        }
        return true;
    }

    private String genPomFile(String rootDir,String pomDependenType ){

        String pomContent = String.format("<project xmlns=\"http://maven.apache.org/POM/4.0.0\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n" +
                "  xsi:schemaLocation=\"http://maven.apache.org/POM/4.0.0 http://maven.apache.org/maven-v4_0_0.xsd\">\n" +
                "  <modelVersion>4.0.0</modelVersion>\n" +
                "  <groupId>net.ifcoder.erlanalysis</groupId>\n" +
                "  <artifactId>denpendencedown</artifactId>\n" +
                "  <packaging>pom</packaging>\n" +
                "  <version>1.0-SNAPSHOT</version>\n" +
                "  <name>denpendencedown</name>\n" +
                "  <url>http://maven.apache.org</url>\n" +
                "  <dependencies>\n" +
                "\t  <dependency>\n" +
                "\t\t  <groupId>%s</groupId>\n" +
                "\t\t  <artifactId>%s</artifactId>\n" +
                "\t\t  <version>%s</version><type>%s</type>\n" +
                "       <exclusions>\n" +
                "        <exclusion>\n" +
                "          <groupId>log4j</groupId>\n" +
                "          <artifactId>log4j</artifactId>\n" +
                "        </exclusion>\n" +
                "      </exclusions>"+
                "\t  </dependency>\n" +
                "  </dependencies>\n" +
                "    <build>\n" +
                "        <plugins>\n" +
                "            <plugin>\n" +
                "                <groupId>org.apache.maven.plugins</groupId>\n" +
                "                <artifactId>maven-dependency-plugin</artifactId>\n" +
                "                <version>2.10</version>\n" +
                "<executions>\n" +
                "                    <execution>\n" +
                "                        <id>copy-dependencies</id>\n" +
                "                        <phase>compile</phase>\n" +
                "                        <goals>\n" +
                "                            <goal>copy-dependencies</goal>\n" +
                "                        </goals>\n" +
                "                       <!--  <configuration>\n" +
                "                            <outputDirectory>${basedir}/lib</outputDirectory>\n" +
                "                            <excludeScope>provided</excludeScope>\n" +
                "                        </configuration> -->\n" +
                "                    </execution>\n" +
                "                </executions>"+
                "               <configuration>\n" +
                "\t\t\t\t\t         <outputDirectory>${basedir}/lib</outputDirectory>\n" +
                "\t\t\t\t\t         <excludeScope>provided</excludeScope>\n" +
                "                  <excludeArtifactIds>commons-logging</excludeArtifactIds>\n" +
                "                </configuration>"+
                "            </plugin>\n" +
                "        </plugins>\n" +
                "    </build>\n" +
                "</project>\n",groupid,artifactId,version,pomDependenType);
        //logback-core,logback-classic,
        String pompath = rootDir+File.separator+"pom.xml";
        log.info("will create pom file:"+pompath);
        File pomfile = new File(pompath);
        try {
            //pomfile.createNewFile();
            FileOutputStream out=new FileOutputStream(pomfile);
            out.write(pomContent.getBytes());
            out.close();
        } catch (IOException e) {
            e.printStackTrace();
            return null;
        }
        return pompath;
    }

    private String  genProjectDir(String savedir){


        String tmpSavePath;
        if(savedir.endsWith(File.separator)){
            tmpSavePath=String.format("%s%s%s%s",savedir,this.artifactId,File.separator,this.version);
        }else{
            tmpSavePath=String.format("%s%s%s%s%s",savedir,File.separator,this.artifactId,File.separator,this.version);
        }
        File rootDir = new File(tmpSavePath);
        if(!rootDir.exists()){
            if(!rootDir.mkdirs()){
                return null;
            }
        }
        return tmpSavePath;
    }

    public String getMainJarPath() {
        if(mainJarPath==null){
            mainJarPath=String.format("%s%slib%s%s-%s.jar",rootDir,File.separator,File.separator,artifactId,version);
        }
        return mainJarPath;
    }

    public String getRootDir() {
        return rootDir;
    }

    public String getArtifactId(){
        return artifactId;
    }

    public String getVersion() {
        return version;
    }
}
