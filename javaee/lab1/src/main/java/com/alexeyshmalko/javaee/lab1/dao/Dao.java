package com.alexeyshmalko.javaee.lab1.dao;

import com.alexeyshmalko.javaee.lab1.Utils;

import java.sql.*;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public abstract class Dao<T extends Entity> {
	private final Connection connection;

	public Dao(Connection connection) {
		this.connection = connection;
	}

	public T saveUpdate(T entity) throws SQLException {
		return entity.id == null ? save(entity) : update(entity);
	}

	public final T save(T entity) throws SQLException {
		try (PreparedStatement statement = insertStatement(entity)) {
			fillStatement(statement, entity);

			boolean autocommit = connection.getAutoCommit();
			try {
				connection.setAutoCommit(false);
				statement.executeUpdate();
				saveRelations(entity);
				connection.commit();
			} finally {
				connection.setAutoCommit(autocommit);
			}

			try (ResultSet generatedKeys = statement.getGeneratedKeys()) {
				if (generatedKeys.next()) {
					entity.id = generatedKeys.getLong(1);
				} else {
					throw new SQLException("Inserted value, no ID obtained");
				}
			}

			return entity;
		}
	}

	public final T update(T entity) throws SQLException {
		if (entity.id == null) {
			throw new IllegalArgumentException("Entity id is null");
		}

		try (PreparedStatement statement = updateStatement(entity)) {
			fillStatement(statement, entity);
			statement.setLong(statement.getParameterMetaData().getParameterCount(), entity.id);
			statement.executeUpdate();
			return entity;
		}
	}

	public final T findOne(long id) throws SQLException {
//		try (PreparedStatement statement = selectStatement(id)) {
//			ResultSet resultSet = statement.executeQuery();
//			while (resultSet.next()) {
//
//			}
//		}
		return null;
	}

	public final void delete(T entity) throws SQLException {
		try (PreparedStatement statement = deleteStatement(entity)) {
			statement.executeUpdate();
		}
	}

	private PreparedStatement insertStatement(T entity) throws SQLException {
		String sql =
				"INSERT INTO " +
				tableName() +
				" (" +
				Utils.join(getFields(), ", ") +
				") VALUES (" +
				Utils.join(Collections.nCopies(getFields().size(), "?"), ", ") +
				")";

		PreparedStatement statement = connection.prepareStatement(sql, Statement.RETURN_GENERATED_KEYS);
		fillStatementOrClose(statement, entity);
		return statement;
	}

	private PreparedStatement updateStatement(T entity) throws SQLException {
		StringBuilder sql = new StringBuilder();
		sql.append("UPDATE ");
		sql.append(tableName());
		sql.append(" SET ");

		ArrayList<String> values = new ArrayList<>();
		for (String field : getFields()) {
			values.add(field + "=?");
		}

		sql.append(Utils.join(values, ", "));
		sql.append(" WHERE id=?");

		// Can't use try-with-resources because I'm returning from try.
		PreparedStatement statement = connection.prepareStatement(sql.toString());
		fillStatementOrClose(statement, entity);
		return statement;
	}

	private PreparedStatement selectStatement(long id) throws SQLException {
		// Can't use try-with-resources because I'm returning from try.
		PreparedStatement statement = connection.prepareStatement(getSelectQuery());
		try {
			statement.setLong(1, id);
			return statement;
		} catch (Exception e) {
			statement.close();
			throw e;
		}
	}

	private PreparedStatement deleteStatement(T entity) throws SQLException {
		String sql =
				"DELETE FROM " +
				tableName() +
				" WHERE id=?";

		// Can't use try-with-resources because I'm returning from try.
		PreparedStatement statement = connection.prepareStatement(sql);
		try {
			statement.setLong(1, entity.id);
			return statement;
		} catch (Exception e) {
			statement.close();
			throw e;
		}
	}

	private void fillStatementOrClose(PreparedStatement statement, T entity) throws SQLException {
		try {
			fillStatement(statement, entity);
		} catch (Exception e) {
			statement.close();
			throw e;
		}
	}

	protected abstract void saveRelations(T entity);

	protected abstract void fillStatement(PreparedStatement statement, T entity);

	protected abstract List<String> getFields();

	protected abstract String tableName();

	protected abstract String getSelectQuery();
}
