package br.com.fabriciodev.rest;

import javax.ws.rs.core.MediaType;

import org.junit.Assert;
import org.junit.Test;

import br.com.fabriciodev.rest.domain.Person;

import com.sun.jersey.api.client.Client;
import com.sun.jersey.api.client.config.ClientConfig;
import com.sun.jersey.api.client.config.DefaultClientConfig;
import com.sun.jersey.api.json.JSONConfiguration;
import com.sun.jersey.json.impl.provider.entity.JacksonProviderProxy;

public class TestJson {

	@Test
	public void delete() {
		ClientConfig config = new DefaultClientConfig();
		config.getClasses().add(JacksonProviderProxy.class);

		config.getFeatures().put(JSONConfiguration.FEATURE_POJO_MAPPING, Boolean.TRUE);

		Client client = Client.create(config);

		Person entity = new Person(123, "Fabricio", "fabricio.colombo.mva@gmail.com");

		client.resource("http://localhost:8080/java-rest-server/rest/json/person").accept(MediaType.APPLICATION_JSON)
				.type(MediaType.APPLICATION_JSON).delete();

	}
}
