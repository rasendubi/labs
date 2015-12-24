package com.alexeyshmalko.javaee.lab1.db;

import java.sql.DriverManager;
import java.sql.SQLException;

public class DatabaseSingleton {
	public static final Database db;

	static {
		try {
			db = new Database(DriverManager.getConnection("jdbc:postgresql://localhost/javaee", "javaee", "javaee"));
		} catch (SQLException e) {
			throw new RuntimeException(e);
		}
	}
}
