--
-- PostgreSQL database dump
--

SET statement_timeout = 0;
SET lock_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

SET search_path = public, pg_catalog;

--
-- Data for Name: edgetype; Type: TABLE DATA; Schema: public; Owner: savageworlds
--

INSERT INTO edgetype (id, version, description, name) VALUES (1, 0, '425378', 'Background Edges');
INSERT INTO edgetype (id, version, description, name) VALUES (2, 0, NULL, 'Combat Edges');
INSERT INTO edgetype (id, version, description, name) VALUES (3, 0, '425379', 'Leadership Edges');
INSERT INTO edgetype (id, version, description, name) VALUES (4, 0, '425380', 'Power Edges');
INSERT INTO edgetype (id, version, description, name) VALUES (5, 0, NULL, 'Professional Edges');
INSERT INTO edgetype (id, version, description, name) VALUES (6, 0, NULL, 'Social Edges');
INSERT INTO edgetype (id, version, description, name) VALUES (7, 0, NULL, 'Weird Edges');
INSERT INTO edgetype (id, version, description, name) VALUES (8, 0, '425381', 'Wild Card Edges');
INSERT INTO edgetype (id, version, description, name) VALUES (9, 0, NULL, 'Legendary Edges');


--
-- Name: edgetype_id_seq; Type: SEQUENCE SET; Schema: public; Owner: savageworlds
--

SELECT pg_catalog.setval('edgetype_id_seq', 9, true);


--
-- PostgreSQL database dump complete
--

