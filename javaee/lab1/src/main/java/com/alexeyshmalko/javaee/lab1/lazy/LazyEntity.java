package com.alexeyshmalko.javaee.lab1.lazy;

import com.alexeyshmalko.javaee.lab1.dao.Entity;

import java.sql.SQLException;

public interface LazyEntity<T extends Entity> {
	long getId();

	T get() throws SQLException;
}
