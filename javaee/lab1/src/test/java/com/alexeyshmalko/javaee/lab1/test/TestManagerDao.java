package com.alexeyshmalko.javaee.lab1.test;

import com.alexeyshmalko.javaee.lab1.dao.Dao;
import com.alexeyshmalko.javaee.lab1.entity.Manager;
import org.junit.Test;
import static org.junit.Assert.*;

import java.sql.SQLException;

public class TestManagerDao {
	private final Dao<Manager> managers = TestDatabase.db.managers;

	@Test
	public void testModify() throws SQLException {
		Manager iurii = managers.findOne(1);
		assertNotNull(iurii);
		iurii.name = "Lurii";
		managers.update(iurii);

		Manager lurii = managers.findOne(1);
		assertEquals("Lurii", lurii.name);

		iurii.name = "Iurii";
		managers.update(iurii);
	}

	@Test
	public void testAdd() throws SQLException {
		Manager manager = new Manager();
		manager.name = "False Manager";
		managers.save(manager);

		Manager managerCopy = managers.findOne(manager.id);
		assertNotNull(managerCopy);
		assertEquals("False Manager", managerCopy.name);
		assertTrue(managerCopy.projects.isEmpty());

		managers.delete(manager);
		managerCopy = managers.findOne(manager.id);
		assertNull(managerCopy);
	}
}
