<!-- TOC start -->
- [SQL Note](#sql-note)
  * [1.1 PostgreSQL](#11-postgresql)
    + [1. Check the version of psql postgres](#1-check-the-version-of-psql-postgres)
    + [2. Show the list of databases](#2-show-the-list-of-databases)
    + [3. Setting up users for PostgreSQL](#3-setting-up-users-for-postgresql)
    + [4. Viewing all the users in a database](#4-viewing-all-the-users-in-a-database)
  * [1.2 Creating a Database and Tables](#12-creating-a-database-and-tables)
    + [1. Create Database](#1-create-database)
    + [2. Connect to database with user](#2-connect-to-database-with-user)
    + [3. GRANT and REVOKE permissions](#3-grant-and-revoke-permissions)
    + [4. Create a table](#4-create-a-table)
    + [5. display the table list](#5-display-the-table-list)
  * [1.3 Basic SQL commands](#13-basic-sql-commands)
    + [1. INSERT values into a Table](#1-insert-values-into-a-table)
    + [2. UPDATE existing data](#2-update-existing-data)
    + [3. SELECT from a table](#3-select-from-a-table)
    + [4. DELETE from a table](#4-delete-from-a-table)
  * [2.1 Importing Data](#21-importing-data)
    + [1. Remove an existing database/table completely](#1-remove-an-existing-databasetable-completely)
    + [2. Remove the data in the database/table](#2-remove-the-data-in-the-databasetable)
    + [3. .csv file](#3-csv-file)
    + [4. COPY the data from .csv to a table](#4-copy-the-data-from-csv-to-a-table)
  * [2.2 Creating Constraints](#22-creating-constraints)
    + [1. ALTER existing structure](#1-alter-existing-structure)
    + [2. ALTER a default constraint](#2-alter-a-default-constraint)
    + [3. Checking existing constraints and relationships](#3-checking-existing-constraints-and-relationships)
  * [2.3 SQL Functions](#23-sql-functions)
    + [1. Return the number of rows a specific query returns](#1-return-the-number-of-rows-a-specific-query-returns)
    + [2. Return the SUM of all the numeric values in the selected columns](#2-return-the-sum-of-all-the-numeric-values-in-the-selected-columns)
    + [3. See what the minimum and maximum number of days a task took would be](#3-see-what-the-minimum-and-maximum-number-of-days-a-task-took-would-be)
    + [4. The AS command renames the column in a result](#4-the-as-command-renames-the-column-in-a-result)
    + [5. Get the average number of days](#5-get-the-average-number-of-days)
    + [6. ORDER BY ASC and DESC](#6-order-by-asc-and-desc)
  * [3.1 Associative Entities](#31-associative-entities)
    + [1. Auto Increment PRIMARY KEYS](#1-auto-increment-primary-keys)
  * [3.2 Joins](#32-joins)
    + [1. INNER JOIN](#1-inner-join)
    + [2. LEFT/RIGHT JOIN](#2-leftright-join)
    + [3. FULL JOIN](#3-full-join)
<!-- TOC end -->

# SQL Note

> ↑/↓ use last command

## 1.1 PostgreSQL

### 1. Check the version of psql postgres

- Select Version();

> use ';' in the end to end this command 结尾加';'来表示结束

### 2. Show the list of databases

- \l

> 'l' for Length 是'l'不是'i'

### 3. Setting up users for PostgreSQL

- CREATE ROLE `testuser` NOINHERIT LOGIN PASSWORD '`password`'

> the use of single quotes for string literals 用单引号

### 4. Viewing all the users in a database

- \du

## 1.2 Creating a Database and Tables

### 1. Create Database

- CREATE DATABASE `todo_list`  
  WITH  
  OWNER = `testuser`  
  ENCODING = '`UTF8`'  
  CONNECTION LIMIT = `-1`;

> 'UTF8' is coding type and connection limit keep it as '-1' 结尾加';'

### 2. Connect to database with user

- \c `todo_list` `testuser`

### 3. GRANT and REVOKE permissions

- REVOKE SELECT ON `public.todo_item` FROM `testuser`;
- GRANT SELECT ON `public.todo_item` TO `testuser`;

> 撤销或者增加某些权限 REVOKE...FROM 撤销 GRANT...TO 增加

### 4. Create a table

- CREATE TABLE `public.todo_item`  
  (  
  todo_item_id integer NOT NULL,  
  description text,  
  owner_id integer,  
  priority_id integer,  
  context_id integer,  
  status_id integer,  
  project_id integer,  
  due_date date,  
  completion_date date,  
  PRIMARY KEY (todo_item_id)  
  );

> 基本信息可删可增 'SELECT * FROM `public.todo_item`;' 显示所有属性

### 5. display the table list

- \dt

## 1.3 Basic SQL commands

### 1. INSERT values into a Table

- INSERT INTO `public.todo_item` VALUES (`0, 'New Task', 0, 0, 0, 0, '2022-02-02', null`);

> 按照创建table的属性顺序  
> INSERT INTO `public.todo_item(todo_item_id, description, owner_id, priority_id, context_id, project_id, due_date, completion_date)`  
> VALUES (`0, 'New Task', 0, 0, 0, 0, '2022-02-02', null`);  
> 在前面添加属性顺序后也可以

### 2. UPDATE existing data

- UPDATE `public.todo_item` SET  
  `completion_date = now()`  
  WHERE `todo_item_id = 0`;

> WHERE 改变的目标的属性

### 3. SELECT from a table

- SELECT * FROM `public.todo_item`;
- SELECT `description` FROM `public.todo_item` WHERE `todo_item_id = 1`;

> 符合条件的枚举出来

### 4. DELETE from a table

- DELETE FROM `public.todo_item` WHERE `due_date < '2022-02-03'`;

## 2.1 Importing Data

### 1. Remove an existing database/table completely

- DROP TABLE `public.status`;

> When using the DROP command as that will completely remove whatever database structures you set

### 2. Remove the data in the database/table

- TRUNCATE TABLE `public.status`;

> DELETE performs the remove row-by-row  TRUNCATE will be much faster than DELETE

### 3. .csv file

- 0,Carl,Sagan
- 1,Neil,de Grasse Tyson
- 2,Andrea,Ghez

### 4. COPY the data from .csv to a table

- \copy `owner(owner_id, name, surname)`  
  FROM '`joubertp / Documents / owners .csv`'  
  DELIMITER '`,`';

> DELIMITER 分隔符 FROM+路径

## 2.2 Creating Constraints

### 1. ALTER existing structure

- ALTER TABLE IF EXISTS `public.todo_item`  
  ADD FOREIGN KEY `(owner_id)`  
  REFERENCES `public.owner (owner_id)` MATCH  
  SIMPLE  
  ON UPDATE CASCADE  
  ON DELETE CASCADE  
  NOT VALID;

> ALTER command can be used to change almost any part of the structure of a table

### 2. ALTER a default constraint

- ALTER TABLE `public.todo_item` ALTER COLUMN `due_date` SET DEFAULT `now() + '7days'`;

### 3. Checking existing constraints and relationships

- \d `todo_item`;

## 2.3 SQL Functions

### 1. Return the number of rows a specific query returns

- SELECT COUNT (*)  
  FROM `public.todo_item`  
  WHERE `owner_id = 1`;

### 2. Return the SUM of all the numeric values in the selected columns

- SELECT SUM(`completion_date - due_date`)  
  FROM `public.todo_item`;

### 3. See what the minimum and maximum number of days a task took would be

- SELECT  
  MAX(`completion_date - due_date`),  
  MIN(`completion_date - due_date`)  
  FROM `public.todo_item`;

### 4. The AS command renames the column in a result

- SELECT  
  MAX(`completion_date - due_date`) AS "`Max completion time`",  
  MIN(`completion_date - due_date`) AS "`Min completion time`"  
  FROM `public.todo_item`;

### 5. Get the average number of days

- SELECT  
  AVG(`completion_date - due_date`)  
  AS "`Average number of days per task`"  
  FROM `public.todo_item`;

### 6. ORDER BY ASC and DESC

- SELECT * FROM `todo_item`  
  ORDER BY `description` ASC;

## 3.1 Associative Entities

### 1. Auto Increment PRIMARY KEYS

- `todo_item_id integer` GENERATED ALWAYS AS IDENTITY NOT NULL;

## 3.2 Joins

### 1. INNER JOIN

- SELECT `todo_item.description, owner.owner_name, owner.owner_surname`  
  FROM `todo_item`  
  INNER JOIN `owner` ON `todo_item.owner_id = owner.owner_id`;

> An INNER JOIN selects rows that match in both tables

### 2. LEFT/RIGHT JOIN

- SELECT `todo_item.description, owner.owner_name, owner.owner_surname`
- FROM `todo_item`
- RIGHT JOIN `owner` ON `todo_item.owner_id = owner.owner_id`;

> Note how we now have additional owner, that is not linked to a todo_item

### 3. FULL JOIN

- SELECT `todo_item.description, owner.owner_name, owner.owner_surname`
- FROM `todo_item`
- FULL JOIN `owner` ON `todo_item.owner_id = owner.owner_id`;

> `todo_item` must have an `owner` and our FULL join will not have an additional `todo_item entry`
