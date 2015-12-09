DROP TABLE manager;
DROP TABLE project;
DROP TABLE programmer;

CREATE TABLE manager(
	id SERIAL PRIMARY KEY,
	name TEXT NOT NULL
);

CREATE TABLE project(
	id SERIAL PRIMARY KEY,
	name TEXT NOT NULL,
	manager_id INT NOT NULL
);

CREATE TABLE programmer(
	id SERIAL PRIMARY KEY,
	name TEXT NOT NULL,
	project_id INT);

INSERT INTO manager(name) VALUES ('Iurii'), ('Noob');
INSERT INTO project(name, manager_id) VALUES ('savvy', 1), ('kaa', 1);
INSERT INTO programmer(name, project_id) VALUES ('Alexey', 2), ('Max', 2);
