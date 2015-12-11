package com.alexeyshmalko.javaee.lab1;

import com.alexeyshmalko.javaee.lab1.dao.Entity;

public interface LazyEntity<T extends Entity> {
	long getId();
	T get();
}
