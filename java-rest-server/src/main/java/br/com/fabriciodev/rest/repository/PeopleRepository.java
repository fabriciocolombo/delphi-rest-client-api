package br.com.fabriciodev.rest.repository;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;

import br.com.fabriciodev.rest.domain.Person;

public class PeopleRepository {

	List<Person> peoples = new ArrayList<Person>();

	public PeopleRepository() {
		initData();
	}

	private void initData() {
		peoples.add(new Person(1, "John Doe", "john@hotmail.com", Calendar.getInstance().getTime()));
		peoples.add(new Person(2, "Mike Myers", "myers@hotmail.com", Calendar.getInstance().getTime()));
		peoples.add(new Person(3, "José Climber", "climber@hotmail.com", Calendar.getInstance().getTime()));
		peoples.add(new Person(4, "Mikaela Romanova", "romanova@hotmail.com", Calendar.getInstance().getTime()));
	}

	public List<Person> findAll() {
		return peoples;
	}

	public Person findById(Integer id) {
		for (Person person : peoples) {
			if (person.getId().equals(id)) {
				return person;
			}
		}
		return null;
	}

	public Person update(Person person) {
		return saveOrUpdate(person);
	}

	public Person save(Person person) {
		return saveOrUpdate(person);
	}

	public Person saveOrUpdate(Person person) {

		if ((person.getId() == null)||(person.getId() == 0)) {
			person.setId(peoples.size() + 1);
			peoples.add(person);
		} else {
			int index = remove(person.getId());
			peoples.add(index, person);
		}

		return person;
	}

	public int remove(Integer id) {
		for (int i = 0; i < peoples.size(); i++) {
			Person person = peoples.get(i);

			if (person.getId().equals(id)) {
				peoples.remove(i);

				return i;
			}
		}

		throw new IllegalArgumentException("Person not found for id " + id);
	}

	public void remove(Person person) {
		peoples.remove(person);
	}
}
