package com.alexeyshmalko.javaee.lab1.lazy;

import com.alexeyshmalko.javaee.lab1.dao.Dao;
import com.alexeyshmalko.javaee.lab1.dao.Entity;

import java.sql.SQLException;

public class LazyFromDao<T extends Entity> implements LazyEntity<T> {
	final Dao<T> dao;
	final long id;

	public LazyFromDao(Dao<T> dao, long id) {
		this.dao = dao;
		this.id = id;
	}

	@Override
	public long getId() {
		return id;
	}

	@Override
	public T get() throws SQLException {
		return dao.findOne(id);
	}

	@Override
	public String toString() {
		return "LazyId{" + id + "}";
	}
}
