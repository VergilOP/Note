-- Create role
CREATE ROLE video_shop_owner 
    NOINHERIT LOGIN PASSWORD 'password';

-- Create database
CREATE DATABASE ye_old_video_shop 
    WITH OWNER = video_shop_owner 
    ENCODING = 'UTF8' 
    CONNECTION LIMIT = -1;

-- Connect to database with user
\c ye_old_video_shop video_shop_owner 

-- Create video table

CREATE TABLE video (
    video_id integer, 
    video_name varchar(50), 
    video_description text, 
    video_genre integer,
    PRIMARY KEY (video_id)
);

-- Create actor table (note actor_image stores a URL to an image file, not the image itself)

CREATE TABLE actor (
    actor_id integer, 
    actor_name varchar(50), 
    actor_surname varchar(50), 
    actor_profile text, 
    actor_image varchar(50), 
    PRIMARY KEY (actor_id)
);

-- Create genre table

CREATE TABLE genre (
    genre_id integer, 
    genre_name varchar(50), 
    PRIMARY KEY (genre_id)
);

-- Create customer table

CREATE TABLE customer (
    customer_id integer, 
    customer_name varchar(50), 
    customer_surname varchar(50), 
    customer_credit numeric(10,2), 
    PRIMARY KEY (customer_id)
);

-- Create video_actor table
CREATE TABLE video_actor (
    video_id integer, 
    actor_id integer, 
    PRIMARY KEY(video_id, actor_id)
);

-- Create rental table
CREATE TABLE rental (
    rental_id integer, 
    video_id integer, 
    customer_id integer, 
    rental_date date, 
    rental_period integer, 
    PRIMARY KEY (rental_id)
);

-- Insert into genre table

INSERT INTO genre (genre_id, genre_name) VALUES 
    (0, 'Comedy'), 
    (1, 'Drama'), 
    (2, 'Sci-fi'), 
    (3, 'Romantic'), 
    (4, 'Action');

-- Insert into actor table

INSERT INTO actor (actor_id, actor_name, actor_surname, actor_profile, actor_image) VALUES 
    (0, 'Jason', 'Statham', 'Tough British Bloke', 'jstatham.jpeg'), 
    (1, 'Natalie', 'Portman', 'Versatile actress', 'nportman.jpeg'), 
    (2, 'Benedict', 'Cumberbatch', 'Talented Britsh Actor', 'bcumberbatch.jpeg');

-- Insert into customer table

INSERT INTO customer (customer_id, customer_name, customer_surname, customer_credit) VALUES 
    (0, 'Pieter', 'Joubert', -100.00), 
    (1, 'Jacqui', 'Chetty', 100.00), 
    (2, 'Felipe', 'Orihuela-Espina', 10000.00), 
    (3, 'Mohammed', 'Baja', 100.00);

-- Insert into video table

INSERT INTO video (video_id, video_name, video_description, video_genre) VALUES 
    (0, 'The Transporter', 'Guy drives around a lot', 4), 
    (1, 'The Imitation Game', 'Story of WWII UK code breakers', 2), 
    (2, 'The Phantom Menance', 'Star Wars but kinda bad', 2);

-- Insert into video_actor

INSERT INTO video_actor (video_id, actor_id) VALUES 
    (0, 0),
    (2, 1),
    (1, 2);

-- Insert into rental

INSERT INTO rental (rental_id, video_id, customer_id, rental_date, rental_period) VALUES 
    (0, 1, 0, '2022-03-18', 7), 
    (1, 2, 1, '2022-03-20', 7), 
    (2, 2, 1, '2022-03-20', 14);

-- Select statements (these are just some examples, please feel free to test more of your own)

SELECT * FROM video;
SELECT description FROM video;
SELECT rental_period FROM video WHERE rental_period > 7;
SELECT surname, name FROM cutomer WHERE customer_credit < 0;