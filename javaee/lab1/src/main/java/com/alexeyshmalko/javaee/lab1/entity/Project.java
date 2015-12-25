package com.alexeyshmalko.javaee.lab1.entity;

import com.alexeyshmalko.javaee.lab1.lazy.LazyEntity;
import com.alexeyshmalko.javaee.lab1.dao.Entity;

import javax.persistence.*;
import java.util.ArrayList;
import java.util.List;

@javax.persistence.Entity
@Table(name = "project")
public class Project extends Entity {
	@Column(name = "name")
	public String name;

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "manager_id")
	public Manager manager;

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	@ManyToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "manager_id")
	public Manager getManager() {
		return manager;
	}

	public void setManager(Manager manager) {
		this.manager = manager;
	}

	@Transient
	public List<LazyEntity<Programmer>> programmers = new ArrayList<>();

	@Override
	public String toString() {
		return "{ " + super.toString() + ", name=" + name + ", manager=" + manager.name + ", programmers=" + programmers + " }";
	}
}
