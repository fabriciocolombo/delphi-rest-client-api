package br.com.fabriciodev.rest.domain;

import javax.xml.bind.annotation.XmlRootElement;

@XmlRootElement
public class HelloWorld {
	
	private String msg;

	public String getMsg() {
		return msg;
	}

	public void setMsg(String msg) {
		this.msg = msg;
	}
	
	public static HelloWorld create(String msg) {
		HelloWorld helloWorld = new HelloWorld();

		helloWorld.setMsg(msg);
		
		return helloWorld;
	}

}
