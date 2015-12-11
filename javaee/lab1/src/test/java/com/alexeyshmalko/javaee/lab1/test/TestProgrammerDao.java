package com.alexeyshmalko.javaee.lab1.test;

import com.alexeyshmalko.javaee.lab1.db.Database;
import com.alexeyshmalko.javaee.lab1.entity.Programmer;
import com.alexeyshmalko.javaee.lab1.entity.Project;
import com.alexeyshmalko.javaee.lab1.lazy.LazyFromDao;
import org.junit.Test;
import static org.junit.Assert.*;

import java.sql.SQLException;

public class TestProgrammerDao {
	private final Database db = TestDatabase.db;

	@Test
	public void testAdd() throws SQLException {
		Project project = new Project();
		project.name = "test project";
		project.manager = new LazyFromDao<>(db.managers, 1);
		db.projects.save(project);
		assertNotNull(project.id);

		Programmer programmer = new Programmer();
		programmer.name = "test programmer";
		programmer.project = new LazyFromDao<>(db.projects, project.id);
		db.programmers.save(programmer);
		assertNotNull(programmer.id);

		Programmer programmer1 = db.programmers.findOne(programmer.id);
		assertNotNull(programmer1);
		assertEquals(programmer.id, programmer1.id);
		assertEquals("test programmer", programmer1.name);
		assertEquals((long)project.id, programmer1.project.getId());

		db.programmers.delete(programmer);
		assertNull(db.programmers.findOne(programmer.id));
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

		Programmer programmer = new Programmer();
		programmer.name = "test programmer";
		programmer.project = new LazyFromDao<>(db.projects, project.id);
		db.programmers.save(programmer);
		assertNotNull(programmer.id);

		Programmer programmer1 = db.programmers.findOne(programmer.id);
		assertNotNull(programmer1);
		programmer1.name = "not test programmer";
		db.programmers.update(programmer1);

		Programmer programmer2 = db.programmers.findOne(programmer.id);
		assertNotNull(programmer2);
		assertEquals("not test programmer", programmer2.name);
		assertEquals((long)project.id, programmer2.project.getId());

		db.programmers.delete(programmer);
		assertNull(db.programmers.findOne(programmer.id));
		db.projects.delete(project);
		assertNull(db.projects.findOne(project.id));
	}
}
