package com.alexeyshmalko.javaee.lab1.test;

import com.alexeyshmalko.javaee.lab1.db.Database;
import com.alexeyshmalko.javaee.lab1.entity.Project;
import com.alexeyshmalko.javaee.lab1.lazy.LazyFromDao;
import org.junit.Test;
import static org.junit.Assert.*;

import java.sql.SQLException;

public class TestProjectDao {
	private final Database db = TestDatabase.db;

	@Test
	public void testAdd() throws SQLException {
		Project project = new Project();
		project.name = "test project";
		project.manager = new LazyFromDao<>(db.managers, 1);

		db.projects.save(project);
		assertNotNull(project.id);

		Project project1 = db.projects.findOne(project.id);
		assertNotNull(project1);
		assertEquals(project.id, project1.id);
		assertEquals("test project", project1.name);
		assertTrue(project1.programmers.isEmpty());

		db.projects.delete(project);
		assertNull(db.projects.findOne(project.id));
	}

	@Test
	public void testModify() throws SQLException {
		Project project = new Project();
		project.name = "test project";
		project.manager = new LazyFromDao<>(db.managers, 1);

		db.projects.save(project);
		assertNotNull(project.id);

		Project project1 = db.projects.findOne(project.id);
		assertNotNull(project1);
		project1.name = "foobar";
		db.projects.update(project1);

		Project project2 = db.projects.findOne(project.id);
		assertNotNull(project2);

		assertEquals(project.id, project2.id);
		assertEquals("foobar", project2.name);
		assertEquals(1, project2.manager.getId());
		assertTrue(project2.programmers.isEmpty());

		db.projects.delete(project);
		assertNull(db.projects.findOne(project.id));
	}
}
