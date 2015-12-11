package com.alexeyshmalko.javaee.lab1.entity;

import com.alexeyshmalko.javaee.lab1.lazy.LazyEntity;
import com.alexeyshmalko.javaee.lab1.dao.Entity;

import java.util.ArrayList;
import java.util.List;

public class Manager extends Entity {
	public String name;
	public List<LazyEntity<Project>> projects = new ArrayList<>();

	@Override
	public String toString() {
		return "{ " + super.toString() + ", name=" + name + ", projects=" + projects + " }";
	}
}
