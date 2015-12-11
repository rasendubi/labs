package com.alexeyshmalko.javaee.lab1.db.impl;

import com.alexeyshmalko.javaee.lab1.db.Database;
import com.alexeyshmalko.javaee.lab1.dao.Dao;
import com.alexeyshmalko.javaee.lab1.entity.Manager;
import com.alexeyshmalko.javaee.lab1.lazy.LazyFromDao;

import java.sql.*;
import java.util.*;

public class ManagerDao extends Dao<Manager> {
	private final Database db;

	public ManagerDao(Database db, Connection connection) {
		super(connection);
		this.db = db;
	}

	@Override
	protected String tableName() {
		return "manager";
	}

	@Override
	protected List<String> getFields() {
		return Arrays.asList("name");
	}

	@Override
	protected void fillStatement(PreparedStatement statement, Manager entity) throws SQLException {
		statement.setString(1, entity.name);
	}

	@Override
	protected void saveRelations(Connection connection, Manager entity) {
	}

	@Override
	protected String getSelectConstraints() {
		return "LEFT JOIN project ON manager.id = project.manager_id";
	}

	@Override
	protected Manager updateValue(Manager value, ResultSet resultSet) throws SQLException {
		if (value == null) {
			value = new Manager();
			value.name = resultSet.getString(2);
		}

		long project_id = resultSet.getLong(3);
		if (project_id != 0) {
			value.projects.add(new LazyFromDao<>(db.projects, resultSet.getLong(3)));
		}

		return value;
	}
}
