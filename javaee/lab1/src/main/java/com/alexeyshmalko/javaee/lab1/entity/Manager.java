package com.alexeyshmalko.javaee.lab1.entity;

import com.alexeyshmalko.javaee.lab1.dao.Entity;

import java.util.HashMap;
import java.util.Map;

public class Manager extends Entity {
	public String name;
	public Map<Long, Project> projects = new HashMap<>();

	@Override
	public String toString() {
		return "{ " + super.toString() + ", name=" + name + ", projects=" + projects.values() + " }";
	}
}
