--
-- PostgreSQL database dump
--

-- Dumped from database version 10.0
-- Dumped by pg_dump version 10.0

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'WIN1252';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner:
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: messages; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE IF NOT EXISTS messages (
    "Exp" text NOT NULL,
    "Contents" text NOT NULL,
    "UserId" bigint NOT NULL,
    "Spam" boolean NOT NULL,
    "PreviousMessage" boolean NOT NULL,
    "SamePerson" boolean NOT NULL,
    "Time" bigint NOT NULL,
    "Id" bigint NOT NULL GENERATED ALWAYS AS IDENTITY
);

CREATE TABLE IF NOT EXISTS users (
    "Exp" text NOT NULL,
    "Rank" integer NOT NULL,
    "UserId" bigint NOT NULL PRIMARY KEY
);