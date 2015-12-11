package com.alexeyshmalko.javaee.lab1.test;

import com.alexeyshmalko.javaee.lab1.db.Database;

import java.sql.DriverManager;
import java.sql.SQLException;

public class TestDatabase {
	public static final Database db;

	static {
		try {
			db = new Database(DriverManager.getConnection("jdbc:postgresql://localhost/javaee", "javaee", "javaee"));
		} catch (SQLException e) {
			throw new RuntimeException(e);
		}
	}
}
