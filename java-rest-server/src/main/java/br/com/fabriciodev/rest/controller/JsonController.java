package br.com.fabriciodev.rest.controller;

import java.util.Calendar;
import java.util.List;

import javax.ws.rs.Consumes;
import javax.ws.rs.DELETE;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import br.com.fabriciodev.rest.domain.Person;

@Path("/json/")
public class JsonController {

	private static Person PERSON = new Person(123, "Fabricio", "fabricio.colombo.mva@gmail.com", Calendar.getInstance()
			.getTime());

	@POST
	@Path("person")
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	public Response postPerson(Person person) {
		return Response.ok(person).build();
	}
	
	@POST
	@Path("persons")
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	public Response postPerson(List<Person> people) {
		return Response.ok(people).build();
	}	

	@PUT
	@Path("person")
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	public Response putPerson(Person person) {
		return Response.ok(person).build();
	}

	@GET
	@Path("person")
	@Produces(MediaType.APPLICATION_JSON)
	public Response getPerson() {
		return Response.ok(PERSON).build();
	}

	@DELETE
	@Path("person")
	@Consumes(MediaType.APPLICATION_JSON)
	public void removePerson(Person person) {
		assert(person != null);

		Response.noContent().build();
	}

}
