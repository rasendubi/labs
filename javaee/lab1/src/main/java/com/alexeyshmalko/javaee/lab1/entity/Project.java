package com.alexeyshmalko.javaee.lab1.entity;

import com.alexeyshmalko.javaee.lab1.LazyEntity;
import com.alexeyshmalko.javaee.lab1.dao.Entity;

import java.util.ArrayList;
import java.util.List;

public class Project extends Entity {
	public String name;
	public Manager manager;
	public List<LazyEntity<Programmer>> programmers = new ArrayList<>();

	@Override
	public String toString() {
		return "{ " + super.toString() + ", name=" + name + ", manager=" + manager.name + ", programmers=" + programmers + " }";
	}
}
