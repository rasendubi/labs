package com.alexeyshmalko.javaee.lab1.dao;

import static com.alexeyshmalko.javaee.lab1.Utils.join;

import java.sql.*;
import java.util.*;

public abstract class Dao<T extends Entity> {
	private final Connection connection;

	public Dao(Connection connection) {
		this.connection = connection;
	}

	public final T saveUpdate(T entity) throws SQLException {
		return entity.id == null ? save(entity) : update(entity);
	}

	public final T save(T entity) throws SQLException {
		try (PreparedStatement statement = insertStatement(entity)) {
			fillStatement(statement, entity);

			boolean autocommit = connection.getAutoCommit();
			try {
				connection.setAutoCommit(false);
				statement.executeUpdate();
				saveRelations(connection, entity);
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
		try (PreparedStatement statement = selectStatement(id)) {
			ResultSet resultSet = statement.executeQuery();
			List<T> list = parseResultSet(resultSet);
			return list.iterator().next();
		}
	}

	public final List<T> findAll() throws SQLException {
		try (PreparedStatement statement = selectAllStatement()) {
			ResultSet resultSet = statement.executeQuery();
			return parseResultSet(resultSet);
		}
	}

	public final void delete(T entity) throws SQLException {
		try (PreparedStatement statement = deleteStatement(entity.id)) {
			statement.executeUpdate();
		}
	}

	public final void deleteAll() throws SQLException {
		try (PreparedStatement statement = deleteAllStatement()) {
			statement.executeUpdate();
		}
	}

	private PreparedStatement insertStatement(T entity) throws SQLException {
		String sql =
				"INSERT INTO " +
				tableName() +
				" (" +
				join(getFields(), ", ") +
				") VALUES (" +
				join(Collections.nCopies(getFields().size(), "?"), ", ") +
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

		sql.append(join(values, ", "));
		sql.append(" WHERE id=?");

		PreparedStatement statement = connection.prepareStatement(sql.toString());
		fillStatementOrClose(statement, entity);
		return statement;
	}

	private PreparedStatement selectStatement(long id) throws SQLException {
		PreparedStatement statement = connection.prepareStatement(selectQuery() + " WHERE " + tableName() + ".id=?");
		setIdOrClose(statement, id);
		return statement;
	}

	private PreparedStatement selectAllStatement() throws SQLException {
		return connection.prepareStatement(selectQuery());
	}

	private String selectQuery() {
		return "SELECT * FROM " + tableName() + " " +
				getSelectConstraints();
	}

	private PreparedStatement deleteStatement(long id) throws SQLException {
		String sql =
				"DELETE FROM " +
				tableName() +
				" WHERE id=?";

		PreparedStatement statement = connection.prepareStatement(sql);
		setIdOrClose(statement, id);
		return statement;
	}

	private PreparedStatement deleteAllStatement() throws SQLException {
		return connection.prepareStatement("DELETE FROM " + tableName());
	}

	private void fillStatementOrClose(PreparedStatement statement, T entity) throws SQLException {
		try {
			fillStatement(statement, entity);
		} catch (Exception e) {
			statement.close();
			throw e;
		}
	}

	private static void setIdOrClose(PreparedStatement statement, long id) throws SQLException {
		try {
			statement.setLong(1, id);
		} catch (Exception e) {
			statement.close();
			throw e;
		}
	}

	private final List<T> parseResultSet(ResultSet resultSet) throws SQLException {
		Map<Long, T> values = new HashMap<>();
		while (resultSet.next()) {
			long id = resultSet.getLong("id");

			T value = values.get(id);
			value = updateValue(value, resultSet);
			value.id = id;
			values.put(id, value);
		}
		return new ArrayList<>(values.values());
	}

	protected abstract String tableName();

	protected abstract List<String> getFields();

	protected abstract void fillStatement(PreparedStatement statement, T entity) throws SQLException;

	protected abstract void saveRelations(Connection connection, T entity);

	protected abstract String getSelectConstraints();

	protected abstract T updateValue(T value, ResultSet resultSet) throws SQLException;
}
