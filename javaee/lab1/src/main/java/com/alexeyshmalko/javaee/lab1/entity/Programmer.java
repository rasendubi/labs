package com.alexeyshmalko.javaee.lab1.entity;

import com.alexeyshmalko.javaee.lab1.lazy.LazyEntity;
import com.alexeyshmalko.javaee.lab1.dao.Entity;

import java.sql.SQLException;

public class Programmer extends Entity {
	public String name;
	public LazyEntity<Project> project;

	@Override
	public String toString() {
		try {
			return "{ " + super.toString() + ", name=" + name + ", project=" + project.get().name + " }";
		} catch (SQLException e) {
			throw new RuntimeException(e);
		}
	}
}
