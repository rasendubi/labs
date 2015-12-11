package com.alexeyshmalko.javaee.lab1;

import com.alexeyshmalko.javaee.lab1.dao.Entity;

public class LazyConst<T extends Entity> implements LazyEntity<T> {
	private final T value;

	public LazyConst(T value) {
		this.value = value;
	}

	@Override
	public long getId() {
		return value.id;
	}

	@Override
	public T get() {
		return value;
	}

	@Override
	public String toString() {
		return "Lazy{ " + value.toString() + " }";
	}
}
