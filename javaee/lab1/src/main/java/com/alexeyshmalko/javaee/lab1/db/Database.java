package com.alexeyshmalko.javaee.lab1.db;

import com.alexeyshmalko.javaee.lab1.dao.Dao;
import com.alexeyshmalko.javaee.lab1.db.impl.ManagerDao;
import com.alexeyshmalko.javaee.lab1.entity.Manager;

import java.sql.Connection;

public class Database {
	public final Dao<Manager> managers;

	public Database(Connection connection) {
		managers = new ManagerDao(connection);
	}
}
