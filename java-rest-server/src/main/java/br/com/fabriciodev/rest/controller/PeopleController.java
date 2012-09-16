package br.com.fabriciodev.rest.controller;

import java.net.URI;
import java.util.List;

import javax.ws.rs.Consumes;
import javax.ws.rs.DELETE;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.UriInfo;

import br.com.fabriciodev.rest.domain.Person;
import br.com.fabriciodev.rest.repository.PeopleRepository;

import com.sun.jersey.api.NotFoundException;
import com.sun.jersey.api.uri.UriBuilderImpl;

@Path("/")
@Produces({ MediaType.APPLICATION_JSON, MediaType.TEXT_XML })
public class PeopleController {

	private static PeopleRepository repository = new PeopleRepository();

	@GET
	@Path("persons/reset")
	public Response reset() {
		repository = new PeopleRepository();
		
		return Response.ok().build();
	}

	@GET
	@Path("persons")
	public Response findAll() {
		List<Person> persons = repository.findAll();
		
		return Response.ok(persons).build();
	}

	@GET
	@Path("person/{id}")	
	public Response find(@PathParam("id") Integer id) {
		Person person = repository.findById(id);
		
		if (person == null){
			throw new NotFoundException("Person not found!");
		}

		return Response.ok(person).build();
	}

	@POST
	@Path("person")
	@Consumes({ MediaType.APPLICATION_JSON, MediaType.TEXT_XML })
	public Response save(@Context UriInfo uriInfo, Person person) {
		person = repository.save(person);
		
		URI uri =  uriInfo.getAbsolutePath();
		URI location = new UriBuilderImpl().uri(uri).path(person.getId().toString()).build();
		
		return Response.created(location).entity(person).build();
	}

	@PUT
	@Path("person")
	@Consumes({ MediaType.APPLICATION_JSON, MediaType.TEXT_XML })
	public Response update(@Context UriInfo uriInfo, Person person) {
		person = repository.update(person);
		
		URI uri =  uriInfo.getAbsolutePath();
		URI location = new UriBuilderImpl().uri(uri).path(person.getId().toString()).build();
		
		return Response.created(location).entity(person).build();
	}

	@DELETE
	@Path("person/{id}")
	public Response remove(@PathParam("id") Integer id) {
		repository.remove(id);

		return Response.noContent().build();
	}
}
