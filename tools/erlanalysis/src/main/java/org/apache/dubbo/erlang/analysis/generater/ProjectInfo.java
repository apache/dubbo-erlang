package org.apache.dubbo.erlang.analysis.generater;

/**
 * Created by dlive on 22/02/2018.
 */
public class ProjectInfo {
    private String appName;
    private String appVersion;
    private String prefix;

    public String getAppName() {
        return appName.replace("-","_");
    }

    public void setAppName(String appName) {
        this.appName = appName.replace("-","_");
    }

    public String getPrefix() {
        return prefix;
    }

    public void setPrefix(String prefix) {
        this.prefix = prefix;
    }

    public String getAppVersion() {
        return appVersion;
    }

    public void setAppVersion(String appVersion) {
        this.appVersion = appVersion;
    }
}
