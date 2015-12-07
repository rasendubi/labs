package com.alexeyshmalko.javaee.lab1;

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
}
