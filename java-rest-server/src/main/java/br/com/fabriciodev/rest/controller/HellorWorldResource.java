package br.com.fabriciodev.rest.controller;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.MultivaluedMap;
import javax.ws.rs.core.Response;

import com.sun.jersey.api.client.WebResource;

import br.com.fabriciodev.rest.domain.HelloWorld;

@Path("/helloworld")
public class HellorWorldResource {

	@GET
	@Produces({ MediaType.APPLICATION_JSON, MediaType.TEXT_XML })
	public HelloWorld sayHello() {
		return HelloWorld.create("Olá Mundo!");
	}

	@GET
	@Path("header/{key}")
	@Produces(MediaType.APPLICATION_JSON)
	public Response echoHeader(@Context HttpHeaders headers, @PathParam("key") String key) {
		Set<String> items = headers.getRequestHeaders().keySet();

		Map<String, String> match = new LinkedHashMap<String, String>();

		for (String headerKey : items) {
			if (headerKey.toLowerCase().startsWith(key.toLowerCase())) {
				match.put(headerKey, headers.getRequestHeader(headerKey).get(0));
			}
		}

		return Response.ok().entity(match).build();
	}

	@GET
	@Path("header/languages")
	@Produces(MediaType.APPLICATION_JSON)
	public Response echoHeader(@Context HttpHeaders headers) {
		List<String> languages = new ArrayList<String>();

		for (Locale locale : headers.getAcceptableLanguages()) {
			languages.add(locale.toString());
		}

		return Response.ok().entity(languages).build();
	}
	
	@GET
	@Path("header/accept")
	@Produces(MediaType.APPLICATION_JSON)
	public Response echoAccept(@Context HttpHeaders headers) {
		List<String> types = new ArrayList<String>();
		
		for (MediaType mediaType : headers.getAcceptableMediaTypes()) {
			types.add(mediaType.toString());
		}
		
		return Response.ok().entity(types).build();
	}
	
	@GET
	@Path("header/content")
	@Produces(MediaType.APPLICATION_JSON)
	public Response echoContent(@Context HttpHeaders headers) {
		return Response.ok().entity(headers.getRequestHeader("content-type").get(0)).build();
	}
}
