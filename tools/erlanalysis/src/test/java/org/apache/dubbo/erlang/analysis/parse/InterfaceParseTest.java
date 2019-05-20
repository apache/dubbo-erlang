package org.apache.dubbo.erlang.analysis.parse;

import junit.framework.TestCase;

public class InterfaceParseTest extends TestCase {
    public void testParse() throws Exception {
        InterfaceParse parser = new InterfaceParse();
        parser.parse("org.apache.dubbo.erlang","dubbo-sample-service","1.3");
    }
}