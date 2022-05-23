-- Add foreign key relationships
ALTER TABLE rental 
    ADD FOREIGN KEY (video_id) 
    REFERENCES video(video_id) 
    ON UPDATE CASCADE 
    ON DELETE CASCADE 
    NOT VALID;

ALTER TABLE rental 
    ADD FOREIGN KEY (customer_id) 
    REFERENCES customer(customer_id) 
    ON UPDATE CASCADE 
    ON DELETE CASCADE 
    NOT VALID; 

-- csv code, copy to a new file
0, 'The Transporter', 'Guy drives around a lot', 4
1, 'The Imitation Game', 'Story of WWII UK code breakers', 2
2, 'The Phantom Menance', 'Star Wars but kinda bad', 2

-- copy command assume you have a file named videos.csv in your Documents folder
\copy video(video_id, video_name, video_description, video_genre)
    FROM '~joubertp\Documents\videos.csv' DELIMITER ','

-- example of some sql functions
SELECT MAX(customer_credit) from customer;
SELECT MAX(customer_credit) AS "Maxmium Credit" from customer;
SELECT AVG(rental_period) from rental;
SELECT ROUND(AVG(rental_period)) from rental;
SELECT customer_id, video_id, rental_date FROM rental ORDER BY rental_date ASC;
