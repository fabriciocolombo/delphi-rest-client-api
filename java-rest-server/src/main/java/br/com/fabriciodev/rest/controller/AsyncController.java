package br.com.fabriciodev.rest.controller;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

@Path("/async")
public class AsyncController {
	@GET
	@Produces({ MediaType.TEXT_PLAIN })
	public String longRunningMethod() throws InterruptedException {
		Thread.sleep(5000);
		return "ok";
	}
}
