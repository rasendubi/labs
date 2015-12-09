package com.alexeyshmalko.javaee.lab1;

import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.List;

public class Utils {
	public static StringBuilder join(List<String> values, String separator) {
		StringBuilder result = new StringBuilder();
		boolean needSeparator = false;
		for (String str : values) {
			if (needSeparator) {
				result.append(separator);
			} else {
				needSeparator = true;
			}

			result.append(str);
		}

		return result;
	}

	public static void debugResultSet(ResultSet resultSet) throws SQLException {
		ResultSetMetaData metaData = resultSet.getMetaData();
		for (int i = 1; i <= metaData.getColumnCount(); ++i) {
			String colType = metaData.getColumnTypeName(i);
			Object val =
					colType.equals("serial") ? resultSet.getLong(i) :
					colType.equals("int4") ? resultSet.getLong(i) :
					colType.equals("text") ? resultSet.getString(i) :
					"(unknown type " + colType + ")";
			System.out.println(i + "(" + metaData.getColumnName(i) + ") = " + val);
		}
		System.out.println();
	}
}
