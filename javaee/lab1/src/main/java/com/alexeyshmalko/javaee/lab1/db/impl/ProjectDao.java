package com.alexeyshmalko.javaee.lab1.db.impl;

import com.alexeyshmalko.javaee.lab1.dao.Dao;
import com.alexeyshmalko.javaee.lab1.db.Database;
import com.alexeyshmalko.javaee.lab1.entity.Programmer;
import com.alexeyshmalko.javaee.lab1.entity.Project;
import com.alexeyshmalko.javaee.lab1.lazy.LazyFromDao;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Arrays;
import java.util.List;

public class ProjectDao extends Dao<Project> {
	private final Database db;

	public ProjectDao(Database db, Connection connection) {
		super(connection);
		this.db = db;
	}

	@Override
	protected String tableName() {
		return "project";
	}

	@Override
	protected List<String> getFields() {
		return Arrays.asList("name", "manager_id");
	}

	@Override
	protected void fillStatement(PreparedStatement statement, Project entity) throws SQLException {
		statement.setString(1, entity.name);
		statement.setLong(2, entity.manager.getId());
	}

	@Override
	protected void saveRelations(Connection connection, Project entity) {
	}

	@Override
	protected String getSelectConstraints() {
		return "LEFT JOIN programmer ON project.id = programmer.project_id";
	}

	@Override
	protected Project updateValue(Project value, ResultSet resultSet) throws SQLException {
		if (value == null) {
			value = new Project();
			value.name = resultSet.getString(2);
			value.manager = new LazyFromDao<>(db.managers, resultSet.getLong(3));
		}

		long programmer_id = resultSet.getLong(4);
		if (programmer_id != 0) {
			value.programmers.add(new LazyFromDao<>(db.programmers, programmer_id));
		}

		return value;
	}
}
