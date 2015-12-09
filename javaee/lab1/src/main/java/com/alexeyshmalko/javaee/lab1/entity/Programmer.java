package com.alexeyshmalko.javaee.lab1.entity;

import com.alexeyshmalko.javaee.lab1.dao.Entity;

public class Programmer extends Entity {
	public String name;
	public Project project;

	@Override
	public String toString() {
		return "{ " + super.toString() + ", name=" + name + ", project=" + project.name + " }";
	}
}
