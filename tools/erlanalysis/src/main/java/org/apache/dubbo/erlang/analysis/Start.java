package org.apache.dubbo.erlang.analysis;

import org.apache.dubbo.erlang.analysis.parse.InterfaceParse;

public class Start {


    public static void main(String[] args) {
        if (args.length < 3) {
            System.out.println("please input groupId artifactId version");
            return;
        }
        String groupId = args[0];
        String artifactId = args[1];
        String version = args[2];
        InterfaceParse parser = new InterfaceParse();
        parser.parse(groupId, artifactId, version);
    }
}
