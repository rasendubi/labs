package com.alexeyshmalko.javaee.lab1.db.impl;

import com.alexeyshmalko.javaee.lab1.LazyConst;
import com.alexeyshmalko.javaee.lab1.dao.Dao;
import com.alexeyshmalko.javaee.lab1.entity.Manager;
import com.alexeyshmalko.javaee.lab1.entity.Project;

import java.sql.*;
import java.util.*;

public class ManagerDao extends Dao<Manager> {
	public ManagerDao(Connection connection) {
		super(connection);
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

		Long project_id = resultSet.getLong(3);
		if (project_id != 0) {
			final Project project = new Project();
			project.id = project_id;
			project.name = resultSet.getString(4);
			project.manager = value;

			value.projects.add(new LazyConst<>(project));
		}

		return value;
	}
}
