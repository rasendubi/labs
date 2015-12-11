package com.alexeyshmalko.javaee.lab1.entity;

import com.alexeyshmalko.javaee.lab1.lazy.LazyEntity;
import com.alexeyshmalko.javaee.lab1.dao.Entity;

import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

public class Project extends Entity {
	public String name;
	public LazyEntity<Manager> manager;
	public List<LazyEntity<Programmer>> programmers = new ArrayList<>();

	@Override
	public String toString() {
		try {
			return "{ " + super.toString() + ", name=" + name + ", manager=" + manager.get().name + ", programmers=" + programmers + " }";
		} catch (SQLException e) {
			throw new RuntimeException(e);
		}
	}
}
