package com.alexeyshmalko.javaee.lab1.dao;

import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;

public class Entity {
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	public Long id = null;

	@Override
	public String toString() {
		return "id=" + id;
	}
}
