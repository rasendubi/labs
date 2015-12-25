package com.alexeyshmalko.javaee.lab1.entity;

import com.alexeyshmalko.javaee.lab1.lazy.LazyEntity;
import com.alexeyshmalko.javaee.lab1.dao.Entity;

import javax.persistence.*;
import java.util.ArrayList;
import java.util.List;

@javax.persistence.Entity
@Table(name = "manager")
public class Manager extends Entity {
	@Column(name = "name")
	public String name;

	@Transient
	public List<LazyEntity<Project>> projects = new ArrayList<>();

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}

	@Column(name = "name")
	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	@Override
	public String toString() {
		return "{ " + super.toString() + ", name=" + name + ", projects=" + projects + " }";
	}
}
