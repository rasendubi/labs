package com.alexeyshmalko.javaee.lab1.dao;

public class Entity {
	// package-local. accessed from Dao
	public Long id = null;

	@Override
	public String toString() {
		return "id=" + id;
	}
}
