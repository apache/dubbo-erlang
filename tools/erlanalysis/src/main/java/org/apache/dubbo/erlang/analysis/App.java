package org.apache.dubbo.erlang.analysis;

import com.caucho.hessian.io.HessianInput;
import com.caucho.hessian.io.HessianOutput;
import org.apache.dubbo.erlang.analysis.parse.InterfaceParse;
import org.apache.dubbo.erlang.analysis.erltool.UserInfo;

import java.io.*;

import static java.lang.System.exit;

/**
 * Hello world!
 *
 */
public class App 
{
    public static void writesome() throws IOException {
        UserInfo user=new UserInfo();
        user.setAge(10);
        user.setUsername("userabc");
        user.setPassword("password");

        ByteArrayOutputStream os = new ByteArrayOutputStream();
        HessianOutput ho = new HessianOutput(os);
        ho.writeObject(user);

        FileOutputStream out = new FileOutputStream("/tmp/hessian.data");
        out.write(os.toByteArray());
        out.flush();
        out.close();

    }

    public static void readsome() throws IOException {
        FileInputStream input = new FileInputStream("/tmp/hessianw.data");
        byte[] buffer = new byte[input.available()];
        input.read(buffer);

        ByteArrayInputStream is = new ByteArrayInputStream(buffer);
        HessianInput hi = new HessianInput(is);
        Object obj = hi.readObject();
        System.out.println("obj:"+obj);
        UserInfo user = (UserInfo)obj;
        System.out.println("user:"+user);
    }
    public static void main( String[] args ){
        if (args.length<3){
            System.out.println("please input args: group artifactid version");
            exit(1);
        }
        String group=args[0];
        String artifactid=args[1];
        String version = args[2];
        System.out.println("will parse facade "+ group+":"+artifactid+":"+version);
        InterfaceParse parser = new InterfaceParse();
        parser.parse(group,artifactid,version);
//
//        try {
////            readsome();
//            writesome();
//        } catch (IOException e) {
//            e.printStackTrace();
//        }
//        System.out.println( "Hello World!" );
    }
}
