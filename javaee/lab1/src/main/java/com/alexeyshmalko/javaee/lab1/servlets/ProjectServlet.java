package com.alexeyshmalko.javaee.lab1.servlets;

import com.alexeyshmalko.javaee.lab1.db.Database;
import com.alexeyshmalko.javaee.lab1.db.DatabaseSingleton;
import com.alexeyshmalko.javaee.lab1.entity.Manager;
import com.alexeyshmalko.javaee.lab1.entity.Project;
import com.alexeyshmalko.javaee.lab1.lazy.LazyFromDao;
import com.alexeyshmalko.javaee.lab1.utils.SessionSingleton;
import org.hibernate.Session;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.PrintWriter;
import java.sql.SQLException;

@WebServlet(urlPatterns = {"/projects"})
public class ProjectServlet extends HttpServlet {
	@Override
	protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
		final PrintWriter writer = resp.getWriter();

		resp.setContentType("text/html");

		writer.write(
				"List of projects" +
				"<table border=1>" +
				"<tr>" +
				"<th>ID</th>" +
				"<th>Name</th>" +
				"<th>Manager</th>" +
				"</tr>"
		);

		try (Session session = SessionSingleton.sessionFactory.openSession()) {
			for (Object o : session.createCriteria(Project.class).list()) {
				Project project = (Project)o;

				writer.write("<tr>" +
						"<td>" + project.id + "</td>" +
						"<td>" + project.name + "</td>" +
						"<td>" + project.manager.name + " (" + project.manager.id + ")</td>" +
						"</tr>"
				);
			}
		}

		writer.write("</table>");

		writer.write("<form method=post>" +
				"<table>" +
				"<tr><td>Name</td><td><input name=name></td></tr>" +
				"<tr><td>Manager ID</td><td><input name=manager></td></tr>" +
				"<tr><td colspan=2><input type=submit /></td></tr>" +
				"</table>" +
				"</form>");
	}

	@Override
	protected void doPost(final HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {

		try (Session session = SessionSingleton.sessionFactory.openSession()) {
			final Project project = new Project();
			project.name = req.getParameter("name");
			project.manager = session.get(Manager.class, Long.parseLong(req.getParameter("manager")));

			session.getTransaction().begin();
			session.persist(project);
			session.getTransaction().commit();
		}

		resp.sendRedirect("projects");
	}
}
