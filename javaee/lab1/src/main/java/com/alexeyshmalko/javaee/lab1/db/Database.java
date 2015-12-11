package com.alexeyshmalko.javaee.lab1.db;

import com.alexeyshmalko.javaee.lab1.dao.Dao;
import com.alexeyshmalko.javaee.lab1.db.impl.ManagerDao;
import com.alexeyshmalko.javaee.lab1.db.impl.ProgrammerDao;
import com.alexeyshmalko.javaee.lab1.entity.Manager;
import com.alexeyshmalko.javaee.lab1.entity.Programmer;

import java.sql.Connection;

public class Database {
	public final Dao<Manager> managers;
	public final Dao<Programmer> programmers;

	public Database(Connection connection) {
		managers = new ManagerDao(connection);
		programmers = new ProgrammerDao(connection);
	}
}
