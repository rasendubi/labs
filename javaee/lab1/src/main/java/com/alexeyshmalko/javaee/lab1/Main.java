package com.alexeyshmalko.javaee.lab1;

import com.alexeyshmalko.javaee.lab1.db.Database;
import com.alexeyshmalko.javaee.lab1.entity.Manager;
import com.alexeyshmalko.javaee.lab1.entity.Programmer;
import com.alexeyshmalko.javaee.lab1.entity.Project;
import com.alexeyshmalko.javaee.lab1.lazy.LazyEntity;

import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.List;

public class Main {
	public static void main(String[] args) throws SQLException {
		Database db = new Database(DriverManager.getConnection("jdbc:postgresql://localhost/javaee", "javaee", "javaee"));

		List<Manager> managers = db.managers.findAll();
		System.out.println(managers);
		Manager noob = managers.get(managers.size() - 1);
		System.out.println(noob);
		db.managers.delete(noob);
		System.out.println(db.managers.findAll());

		Manager iurii = db.managers.findOne(1);
		for (LazyEntity<Project> project : iurii.projects) {
			System.out.println(project.get());
		}

		Project kaa = db.projects.findOne(2);
		for (LazyEntity<Programmer> programmer : kaa.programmers) {
			System.out.println(programmer.get());
		}

		db.managers.save(noob);
	}
}
