package com.alexeyshmalko.javaee.lab1.utils;

import org.hibernate.SessionFactory;
import org.hibernate.boot.MetadataSources;
import org.hibernate.boot.registry.StandardServiceRegistry;
import org.hibernate.boot.registry.StandardServiceRegistryBuilder;

public class SessionSingleton {
	public static final SessionFactory sessionFactory;

	static {
		final StandardServiceRegistry registry =
				new StandardServiceRegistryBuilder()
						.configure()
						.build();
		try {
			sessionFactory = new MetadataSources(registry).buildMetadata().buildSessionFactory();
		} catch (Exception e) {
			StandardServiceRegistryBuilder.destroy(registry);
			throw e;
		}
	}
}
