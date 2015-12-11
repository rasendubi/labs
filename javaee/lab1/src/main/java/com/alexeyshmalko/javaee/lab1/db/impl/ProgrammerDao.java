package com.alexeyshmalko.javaee.lab1.db.impl;

import com.alexeyshmalko.javaee.lab1.dao.Dao;
import com.alexeyshmalko.javaee.lab1.db.Database;
import com.alexeyshmalko.javaee.lab1.entity.Programmer;
import com.alexeyshmalko.javaee.lab1.lazy.LazyFromDao;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Arrays;
import java.util.List;

public class ProgrammerDao extends Dao<Programmer> {
	private final Database db;

	public ProgrammerDao(Database db, Connection connection) {
		super(connection);
		this.db = db;
	}

	@Override
	protected String tableName() {
		return "programmer";
	}

	@Override
	protected List<String> getFields() {
		return Arrays.asList("name", "project_id");
	}

	@Override
	protected void fillStatement(PreparedStatement statement, Programmer entity) throws SQLException {
		statement.setString(1, entity.name);
		statement.setLong(2, entity.project.getId());
	}

	@Override
	protected void saveRelations(Connection connection, Programmer entity) {
	}

	@Override
	protected String getSelectConstraints() {
		return "";
	}

	@Override
	protected Programmer updateValue(Programmer value, ResultSet resultSet) throws SQLException {
		value = new Programmer();
		value.name = resultSet.getString("name");
		value.project = new LazyFromDao<>(db.projects, resultSet.getLong("project_id"));
		return value;
	}
}
