package com.alexeyshmalko.javaee.lab1.entity;

import com.alexeyshmalko.javaee.lab1.LazyEntity;
import com.alexeyshmalko.javaee.lab1.dao.Entity;

public class Programmer extends Entity {
	public String name;
	public LazyEntity<Project> project;

	@Override
	public String toString() {
		return "{ " + super.toString() + ", name=" + name + ", project=" + project.get().name + " }";
	}
}
