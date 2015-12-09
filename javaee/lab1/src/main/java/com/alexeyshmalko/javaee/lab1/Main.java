package com.alexeyshmalko.javaee.lab1;

import com.alexeyshmalko.javaee.lab1.db.Database;
import com.alexeyshmalko.javaee.lab1.entity.Manager;

import java.sql.DriverManager;
import java.sql.SQLException;

public class Main {
	public static void main(String[] args) throws SQLException {
		Database db = new Database(DriverManager.getConnection("jdbc:postgresql://localhost/javaee", "javaee", "javaee"));

		System.out.println(db.managers.findAll());
		Manager noob = db.managers.findOne(2);
		System.out.println(noob);
		db.managers.delete(noob);
		System.out.println(db.managers.findAll());
	}
}
