package com.alexeyshmalko.javaee.lab1.servlets;

import com.alexeyshmalko.javaee.lab1.db.Database;
import com.alexeyshmalko.javaee.lab1.db.DatabaseSingleton;
import com.alexeyshmalko.javaee.lab1.entity.Project;
import com.alexeyshmalko.javaee.lab1.lazy.LazyFromDao;

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
	private final Database db = DatabaseSingleton.db;

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

		try {
			for (Project project : db.projects.findAll()) {
				writer.write("<tr>" +
						"<td>" + project.id + "</td>" +
						"<td>" + project.name + "</td>" +
						"<td>" + project.manager.get().name + "</td>" +
						"</tr>"
				);
			}
		} catch (SQLException e) {
			throw new ServletException(e);
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
		final Project project = new Project();
		project.name = req.getParameter("name");
		project.manager = new LazyFromDao<>(db.managers, Long.parseLong(req.getParameter("manager")));

		try {
			db.projects.save(project);
		} catch (SQLException e) {
			throw new ServletException(e);
		}

		resp.sendRedirect("projects");
	}
}
