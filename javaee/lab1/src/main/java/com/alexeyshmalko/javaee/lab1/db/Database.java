package com.alexeyshmalko.javaee.lab1.db;

import com.alexeyshmalko.javaee.lab1.dao.Dao;
import com.alexeyshmalko.javaee.lab1.db.impl.ManagerDao;
import com.alexeyshmalko.javaee.lab1.db.impl.ProgrammerDao;
import com.alexeyshmalko.javaee.lab1.db.impl.ProjectDao;
import com.alexeyshmalko.javaee.lab1.entity.Manager;
import com.alexeyshmalko.javaee.lab1.entity.Programmer;
import com.alexeyshmalko.javaee.lab1.entity.Project;

import java.sql.Connection;

public class Database {
	public final Dao<Manager> managers;
	public final Dao<Programmer> programmers;
	public final Dao<Project> projects;

	public Database(Connection connection) {
		managers = new ManagerDao(this, connection);
		programmers = new ProgrammerDao(this, connection);
		projects = new ProjectDao(this, connection);
	}
}
