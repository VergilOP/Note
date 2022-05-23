--
-- PostgreSQL database dump
--

-- Dumped from database version 14.2
-- Dumped by pg_dump version 14.2

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: context; Type: TABLE; Schema: public; Owner: testuser
--

CREATE TABLE public.context (
    context_id integer NOT NULL,
    context_name text
);


ALTER TABLE public.context OWNER TO testuser;

--
-- Name: context_context_id_seq; Type: SEQUENCE; Schema: public; Owner: testuser
--

ALTER TABLE public.context ALTER COLUMN context_id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME public.context_context_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: owner; Type: TABLE; Schema: public; Owner: testuser
--

CREATE TABLE public.owner (
    owner_id integer NOT NULL,
    owner_name character varying(50),
    owner_surname character varying(50)
);


ALTER TABLE public.owner OWNER TO testuser;

--
-- Name: owner_owner_id_seq; Type: SEQUENCE; Schema: public; Owner: testuser
--

ALTER TABLE public.owner ALTER COLUMN owner_id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME public.owner_owner_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: owner_project; Type: TABLE; Schema: public; Owner: testuser
--

CREATE TABLE public.owner_project (
    owner_id integer NOT NULL,
    project_id integer NOT NULL
);


ALTER TABLE public.owner_project OWNER TO testuser;

--
-- Name: priority; Type: TABLE; Schema: public; Owner: testuser
--

CREATE TABLE public.priority (
    priority_id integer NOT NULL,
    priority_name character varying(50)
);


ALTER TABLE public.priority OWNER TO testuser;

--
-- Name: priority_priority_id_seq; Type: SEQUENCE; Schema: public; Owner: testuser
--

ALTER TABLE public.priority ALTER COLUMN priority_id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME public.priority_priority_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: project; Type: TABLE; Schema: public; Owner: testuser
--

CREATE TABLE public.project (
    project_id integer NOT NULL,
    project_name text,
    cost_per_hour numeric(10,2) DEFAULT 0.00
);


ALTER TABLE public.project OWNER TO testuser;

--
-- Name: project_project_id_seq; Type: SEQUENCE; Schema: public; Owner: testuser
--

ALTER TABLE public.project ALTER COLUMN project_id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME public.project_project_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: status; Type: TABLE; Schema: public; Owner: testuser
--

CREATE TABLE public.status (
    status_id integer NOT NULL,
    status_name character varying(50)
);


ALTER TABLE public.status OWNER TO testuser;

--
-- Name: status_status_id_seq; Type: SEQUENCE; Schema: public; Owner: testuser
--

ALTER TABLE public.status ALTER COLUMN status_id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME public.status_status_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: todo_item; Type: TABLE; Schema: public; Owner: testuser
--

CREATE TABLE public.todo_item (
    todo_item_id integer NOT NULL,
    description text,
    owner_id integer,
    priority_id integer,
    context_id integer,
    project_id integer,
    status_id integer,
    due_date date,
    completion_date date
);


ALTER TABLE public.todo_item OWNER TO testuser;

--
-- Name: todo_item_todo_item_id_seq; Type: SEQUENCE; Schema: public; Owner: testuser
--

ALTER TABLE public.todo_item ALTER COLUMN todo_item_id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME public.todo_item_todo_item_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Data for Name: context; Type: TABLE DATA; Schema: public; Owner: testuser
--

COPY public.context (context_id, context_name) FROM stdin;
1	Email
2	Office
3	Zoom
\.


--
-- Data for Name: owner; Type: TABLE DATA; Schema: public; Owner: testuser
--

COPY public.owner (owner_id, owner_name, owner_surname) FROM stdin;
1	Carl	Sagan
2	Neil	de Grasse Tyson
3	Andrea	Ghaez
4	Thomas	Kepler
\.


--
-- Data for Name: owner_project; Type: TABLE DATA; Schema: public; Owner: testuser
--

COPY public.owner_project (owner_id, project_id) FROM stdin;
1	1
1	2
2	1
3	1
\.


--
-- Data for Name: priority; Type: TABLE DATA; Schema: public; Owner: testuser
--

COPY public.priority (priority_id, priority_name) FROM stdin;
1	HIGH
2	MEDIUM
3	LOW
\.


--
-- Data for Name: project; Type: TABLE DATA; Schema: public; Owner: testuser
--

COPY public.project (project_id, project_name, cost_per_hour) FROM stdin;
1	James Webb Telescope	1000.00
2	SETI	50.00
\.


--
-- Data for Name: status; Type: TABLE DATA; Schema: public; Owner: testuser
--

COPY public.status (status_id, status_name) FROM stdin;
1	TODO
2	WAIT
3	DONE
\.


--
-- Data for Name: todo_item; Type: TABLE DATA; Schema: public; Owner: testuser
--

COPY public.todo_item (todo_item_id, description, owner_id, priority_id, context_id, project_id, status_id, due_date, completion_date) FROM stdin;
2	Align telescope	1	3	1	1	1	2022-03-31	\N
3	Align telescope	2	1	1	1	1	2022-04-15	\N
4	Write Report	2	1	3	1	1	2022-04-15	\N
5	Send Report	2	1	1	1	1	2022-04-16	\N
6	Send Another Report	2	1	1	2	3	2022-03-16	2022-03-21
7	Check Email	2	1	1	2	3	2022-03-15	2022-03-16
8	Check Email	3	1	2	2	3	2022-03-16	\N
9	Align telescope	1	1	1	1	1	2022-04-01	\N
10	Check telescope	3	1	1	2	2	2022-04-04	\N
\.


--
-- Name: context_context_id_seq; Type: SEQUENCE SET; Schema: public; Owner: testuser
--

SELECT pg_catalog.setval('public.context_context_id_seq', 3, true);


--
-- Name: owner_owner_id_seq; Type: SEQUENCE SET; Schema: public; Owner: testuser
--

SELECT pg_catalog.setval('public.owner_owner_id_seq', 4, true);


--
-- Name: priority_priority_id_seq; Type: SEQUENCE SET; Schema: public; Owner: testuser
--

SELECT pg_catalog.setval('public.priority_priority_id_seq', 3, true);


--
-- Name: project_project_id_seq; Type: SEQUENCE SET; Schema: public; Owner: testuser
--

SELECT pg_catalog.setval('public.project_project_id_seq', 2, true);


--
-- Name: status_status_id_seq; Type: SEQUENCE SET; Schema: public; Owner: testuser
--

SELECT pg_catalog.setval('public.status_status_id_seq', 3, true);


--
-- Name: todo_item_todo_item_id_seq; Type: SEQUENCE SET; Schema: public; Owner: testuser
--

SELECT pg_catalog.setval('public.todo_item_todo_item_id_seq', 10, true);


--
-- Name: context context_pkey; Type: CONSTRAINT; Schema: public; Owner: testuser
--

ALTER TABLE ONLY public.context
    ADD CONSTRAINT context_pkey PRIMARY KEY (context_id);


--
-- Name: owner owner_pkey; Type: CONSTRAINT; Schema: public; Owner: testuser
--

ALTER TABLE ONLY public.owner
    ADD CONSTRAINT owner_pkey PRIMARY KEY (owner_id);


--
-- Name: owner_project owner_project_pkey; Type: CONSTRAINT; Schema: public; Owner: testuser
--

ALTER TABLE ONLY public.owner_project
    ADD CONSTRAINT owner_project_pkey PRIMARY KEY (owner_id, project_id);


--
-- Name: priority priority_pkey; Type: CONSTRAINT; Schema: public; Owner: testuser
--

ALTER TABLE ONLY public.priority
    ADD CONSTRAINT priority_pkey PRIMARY KEY (priority_id);


--
-- Name: project project_pkey; Type: CONSTRAINT; Schema: public; Owner: testuser
--

ALTER TABLE ONLY public.project
    ADD CONSTRAINT project_pkey PRIMARY KEY (project_id);


--
-- Name: status status_pkey; Type: CONSTRAINT; Schema: public; Owner: testuser
--

ALTER TABLE ONLY public.status
    ADD CONSTRAINT status_pkey PRIMARY KEY (status_id);


--
-- Name: todo_item todo_item_pkey; Type: CONSTRAINT; Schema: public; Owner: testuser
--

ALTER TABLE ONLY public.todo_item
    ADD CONSTRAINT todo_item_pkey PRIMARY KEY (todo_item_id);


--
-- Name: todo_item todo_item_context_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: testuser
--

ALTER TABLE ONLY public.todo_item
    ADD CONSTRAINT todo_item_context_id_fkey FOREIGN KEY (context_id) REFERENCES public.context(context_id) ON UPDATE CASCADE ON DELETE CASCADE NOT VALID;


--
-- Name: todo_item todo_item_owner_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: testuser
--

ALTER TABLE ONLY public.todo_item
    ADD CONSTRAINT todo_item_owner_id_fkey FOREIGN KEY (owner_id) REFERENCES public.owner(owner_id) ON UPDATE CASCADE ON DELETE CASCADE NOT VALID;


--
-- Name: todo_item todo_item_priority_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: testuser
--

ALTER TABLE ONLY public.todo_item
    ADD CONSTRAINT todo_item_priority_id_fkey FOREIGN KEY (priority_id) REFERENCES public.priority(priority_id) ON UPDATE CASCADE ON DELETE CASCADE NOT VALID;


--
-- Name: todo_item todo_item_project_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: testuser
--

ALTER TABLE ONLY public.todo_item
    ADD CONSTRAINT todo_item_project_id_fkey FOREIGN KEY (project_id) REFERENCES public.project(project_id) ON UPDATE CASCADE ON DELETE CASCADE NOT VALID;


--
-- Name: todo_item todo_item_status_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: testuser
--

ALTER TABLE ONLY public.todo_item
    ADD CONSTRAINT todo_item_status_id_fkey FOREIGN KEY (status_id) REFERENCES public.status(status_id) ON UPDATE CASCADE ON DELETE CASCADE NOT VALID;


--
-- PostgreSQL database dump complete
--

