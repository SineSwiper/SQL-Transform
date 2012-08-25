set enable_seqscan=off;
-- ensure consistent test output regardless of the default bytea format
SET bytea_output TO escape;

CREATE TABLE test_timetz (
	i timetz
);
CREATE TABLE test_char (
	i "char"
);

INSERT INTO test_timetz VALUES
	( '03:55:08 GMT+2' ),
	( '04:55:08 GMT+2' ),
	( '05:55:08 GMT+2' ),
	( '08:55:08 GMT+2' ),
	( '09:55:08 GMT+2' ),
	( '10:55:08 GMT+2' )
;

CREATE INDEX idx_timetz ON test_timetz USING gin (i);

SELECT * FROM test_timetz WHERE i<'08:55:08 GMT+2'::timetz ORDER BY i;
SELECT * FROM test_char WHERE i<'d'::"char" ORDER BY i;

CREATE EXTENSION btree_gin;

CREATE FUNCTION pg_catalog.pg_file_rename(text, text, text)
RETURNS bool
AS 'MODULE_PATHNAME', 'pg_file_rename'
LANGUAGE C VOLATILE;

CREATE FUNCTION pg_catalog.pg_file_rename(text, text)
RETURNS bool
AS 'SELECT pg_catalog.pg_file_rename($1, $2, NULL::pg_catalog.text);'
LANGUAGE SQL VOLATILE STRICT;

CREATE FUNCTION pg_catalog.pg_logfile_rotate()
RETURNS int4
AS 'pg_rotate_logfile'
LANGUAGE INTERNAL VOLATILE STRICT;

CREATE FUNCTION gin_extract_query_int2(int2, internal, int2, internal, internal)
RETURNS internal
AS 'MODULE_PATHNAME'
LANGUAGE C STRICT IMMUTABLE;

CREATE OPERATOR CLASS int2_ops
DEFAULT FOR TYPE int2 USING gin
AS
    OPERATOR        1       <,
    OPERATOR        2       <=,
    OPERATOR        3       =,
    OPERATOR        4       >=,
    OPERATOR        5       >,
    FUNCTION        1       btint2cmp(int2,int2),
    FUNCTION        2       gin_extract_value_int2(int2, internal),
    FUNCTION        3       gin_extract_query_int2(int2, internal, int2, internal, internal),
    FUNCTION        4       gin_btree_consistent(internal, int2, anyelement, int4, internal, internal),
    FUNCTION        5       gin_compare_prefix_int2(int2,int2,int2, internal),
STORAGE         int2;

ALTER EXTENSION btree_gin ADD function gin_btree_consistent(internal,smallint,anyelement,integer,internal,internal);
ALTER EXTENSION btree_gin ADD function gin_extract_value_int2(smallint,internal);
ALTER EXTENSION btree_gin ADD function gin_compare_prefix_int2(smallint,smallint,smallint,internal);
ALTER EXTENSION btree_gin ADD function gin_extract_query_int2(smallint,internal,smallint,internal,internal);
ALTER EXTENSION btree_gin ADD operator family int2_ops using gin;
ALTER EXTENSION btree_gin ADD operator class int2_ops using gin;
ALTER EXTENSION btree_gin ADD function gin_extract_value_int8(bigint,internal);
ALTER EXTENSION btree_gin ADD function gin_extract_value_float4(real,internal);
ALTER EXTENSION btree_gin ADD function gin_extract_value_float8(double precision,internal);
ALTER EXTENSION btree_gin ADD function gin_extract_value_money(money,internal);
ALTER EXTENSION btree_gin ADD function gin_extract_value_oid(oid,internal);
ALTER EXTENSION btree_gin ADD function gin_extract_value_timestamp(timestamp without time zone,internal);
ALTER EXTENSION btree_gin ADD function gin_extract_value_timestamptz(timestamp with time zone,internal);
ALTER EXTENSION btree_gin ADD function gin_extract_value_time(time without time zone,internal);
ALTER EXTENSION btree_gin ADD function gin_extract_value_timetz(time with time zone,internal);
ALTER EXTENSION btree_gin ADD function gin_extract_value_date(date,internal);
ALTER EXTENSION btree_gin ADD function gin_extract_value_interval(interval,internal);
ALTER EXTENSION btree_gin ADD function gin_extract_value_macaddr(macaddr,internal);
ALTER EXTENSION btree_gin ADD function gin_extract_value_inet(inet,internal);
ALTER EXTENSION btree_gin ADD function gin_extract_value_cidr(cidr,internal);
ALTER EXTENSION btree_gin ADD function gin_extract_value_text(text,internal);
ALTER EXTENSION btree_gin ADD function gin_extract_value_char("char",internal);
ALTER EXTENSION btree_gin ADD function gin_extract_value_bytea(bytea,internal);
ALTER EXTENSION btree_gin ADD function gin_extract_value_bit(bit,internal);
ALTER EXTENSION btree_gin ADD function gin_extract_value_varbit(bit varying,internal);
ALTER EXTENSION btree_gin ADD function gin_extract_value_numeric(numeric,internal);

CREATE TYPE gbtreekey4 (
	INTERNALLENGTH = 4,
	INPUT  = gbtreekey4_in,
	OUTPUT = gbtreekey4_out
);

CREATE OPERATOR <-> (
	LEFTARG = money,
	RIGHTARG = money,
	PROCEDURE = cash_dist,
	COMMUTATOR = '<->'
);

ALTER OPERATOR FAMILY gist_cash_ops USING gist ADD
	OPERATOR	6	<> (money, money) ,
	OPERATOR	15	<-> (money, money) FOR ORDER BY pg_catalog.money_ops ,
	FUNCTION	8 (money, money) gbt_cash_distance (internal, money, int2, oid) ;

ALTER OPERATOR FAMILY gist_macaddr_ops USING gist ADD
	OPERATOR	6	<> (macaddr, macaddr) ;

CREATE OPERATOR CLASS gist_oid_ops
DEFAULT FOR TYPE oid USING gist
AS
	OPERATOR	1	<  ,
	OPERATOR	2	<= ,
	OPERATOR	3	=  ,
	OPERATOR	4	>= ,
	OPERATOR	5	>  ,
	FUNCTION	1	gbt_oid_consistent (internal, oid, int2, oid, internal),
	FUNCTION	2	gbt_oid_union (bytea, internal),
	FUNCTION	3	gbt_oid_compress (internal),
	FUNCTION	4	gbt_decompress (internal),
	FUNCTION	5	gbt_oid_penalty (internal, internal, internal),
	FUNCTION	6	gbt_oid_picksplit (internal, internal),
	FUNCTION	7	gbt_oid_same (internal, internal, internal),
	STORAGE		gbtreekey8;

-- Add operators that are new in 9.1.  We do it like this, leaving them
-- "loose" in the operator family rather than bound into the opclass, because
-- that's the only state that can be reproduced during an upgrade from 9.0.

ALTER OPERATOR FAMILY gist_oid_ops USING gist ADD
	OPERATOR	6	<> (oid, oid) ,
	OPERATOR	15	<-> (oid, oid) FOR ORDER BY pg_catalog.oid_ops ,
	FUNCTION	8 (oid, oid) gbt_oid_distance (internal, oid, int2, oid) ;

CREATE TABLE test_ne (
   a  TIMESTAMP,
   b  NUMERIC
);
CREATE INDEX test_ne_idx ON test_ne USING gist (a, b);

INSERT INTO test_ne SELECT '2009-01-01', 10.7 FROM generate_series(1,1000);
INSERT INTO test_ne VALUES('2007-02-03', -91.3);
INSERT INTO test_ne VALUES('2011-09-01', 43.7);
INSERT INTO test_ne SELECT '2009-01-01', 10.7 FROM generate_series(1,1000);

EXPLAIN (COSTS OFF) SELECT * FROM test_ne WHERE a <> '2009-01-01' AND b <> 10.7;

SELECT count(*) FROM macaddrtmp WHERE a <= '22:00:5c:e5:9b:0d'::macaddr;

CREATE OPERATOR = (
	leftarg = chkpass,
	rightarg = text,
	negator = <>,
	procedure = eq
);

CREATE OPERATOR <> (
	leftarg = chkpass,
	rightarg = text,
	negator = =,
	procedure = ne
);

COMMENT ON TYPE chkpass IS 'password type with checks';

WITH RECURSIVE typeoids(typoid) AS
  ( SELECT 'citext'::pg_catalog.regtype UNION
    SELECT oid FROM pg_catalog.pg_type, typeoids
      WHERE typelem = typoid OR typbasetype = typoid )
UPDATE pg_catalog.pg_type SET typcollation = 100
FROM typeoids
WHERE oid = typeoids.typoid;

UPDATE pg_catalog.pg_index SET indcollation[7] = 100
WHERE indclass[7] IN (
  WITH RECURSIVE typeoids(typoid) AS
    ( SELECT 'citext'::pg_catalog.regtype UNION
      SELECT oid FROM pg_catalog.pg_type, typeoids
        WHERE typelem = typoid OR typbasetype = typoid )
  SELECT oid FROM pg_catalog.pg_opclass, typeoids
  WHERE opcintype = typeoids.typoid
);

CREATE TYPE citext (
    INPUT          = citextin,
    OUTPUT         = citextout,
    RECEIVE        = citextrecv,
    SEND           = citextsend,
    INTERNALLENGTH = VARIABLE,
    STORAGE        = extended,
    -- make it a non-preferred member of string type category
    CATEGORY       = 'S',
    PREFERRED      = false,
    COLLATABLE     = true
);

CREATE CAST (citext AS text)    WITHOUT FUNCTION AS IMPLICIT;
CREATE CAST (citext AS varchar) WITHOUT FUNCTION AS IMPLICIT;
CREATE CAST (citext AS bpchar)  WITHOUT FUNCTION AS ASSIGNMENT;
CREATE CAST (text AS citext)    WITHOUT FUNCTION AS ASSIGNMENT;
CREATE CAST (varchar AS citext) WITHOUT FUNCTION AS ASSIGNMENT;
CREATE CAST (bpchar AS citext)  WITH FUNCTION citext(bpchar)  AS ASSIGNMENT;
CREATE CAST (boolean AS citext) WITH FUNCTION citext(boolean) AS ASSIGNMENT;
CREATE CAST (inet AS citext)    WITH FUNCTION citext(inet)    AS ASSIGNMENT;

CREATE OPERATOR = (
    LEFTARG    = CITEXT,
    RIGHTARG   = CITEXT,
    COMMUTATOR = =,
    NEGATOR    = <>,
    PROCEDURE  = citext_eq,
    RESTRICT   = eqsel,
    JOIN       = eqjoinsel,
    HASHES,
    MERGES
);

CREATE OPERATOR !~* (
    PROCEDURE = texticregexne,
    LEFTARG   = citext,
    RIGHTARG  = citext,
    NEGATOR   = ~*,
    RESTRICT  = icregexnesel,
    JOIN      = icregexnejoinsel
);

CREATE FUNCTION regexp_matches( citext, citext ) RETURNS TEXT[] AS $$
    SELECT pg_catalog.regexp_matches( $1::pg_catalog.text, $2::pg_catalog.text, 'i' );
$$ LANGUAGE SQL IMMUTABLE STRICT;

/* CREATE FUNCTION regexp_matches( citext, citext ) RETURNS TEXT[] AS $$
    SELECT pg_catalog.regexp_matches( $1::pg_catalog.text, $2::pg_catalog.text, 'i' );
$$ LANGUAGE SQL IMMUTABLE STRICT; */

CREATE FUNCTION regexp_split_to_array( citext, citext, text ) RETURNS TEXT[] AS $$
    SELECT pg_catalog.regexp_split_to_array( $1::pg_catalog.text, $2::pg_catalog.text, CASE WHEN pg_catalog.strpos($3, 'c') = 0 THEN  $3 || 'i' ELSE $3 END );
$$ LANGUAGE SQL IMMUTABLE STRICT;

CREATE FUNCTION replace( citext, citext, citext ) RETURNS TEXT AS $$
    SELECT pg_catalog.regexp_replace( $1::pg_catalog.text, pg_catalog.regexp_replace($2::pg_catalog.text, '([^a-zA-Z_0-9])', E'\\\\\\1', 'g'), $3::pg_catalog.text, 'gi' );
$$ LANGUAGE SQL IMMUTABLE STRICT;

CREATE FUNCTION split_part( citext, citext, int ) RETURNS TEXT AS $$
    SELECT (pg_catalog.regexp_split_to_array( $1::pg_catalog.text, pg_catalog.regexp_replace($2::pg_catalog.text, '([^a-zA-Z_0-9])', E'\\\\\\1', 'g'), 'i'))[$3];
$$ LANGUAGE SQL IMMUTABLE STRICT;

CREATE FUNCTION translate( citext, citext, text ) RETURNS TEXT AS $$
    SELECT pg_catalog.translate( pg_catalog.translate( $1::pg_catalog.text, pg_catalog.lower($2::pg_catalog.text), $3), pg_catalog.upper($2::pg_catalog.text), $3);
$$ LANGUAGE SQL IMMUTABLE STRICT;

INSERT INTO try (name)
VALUES ('a'), ('ab'), ('â'), ('aba'), ('b'), ('ba'), ('bab'), ('AZ');

SELECT name, 'a' = name AS eq_a   FROM try WHERE name <> 'â';
SELECT name, 'a' = name AS t      FROM try where name = 'a';
SELECT name, 'A' = name AS "eq_A" FROM try WHERE name <> 'â';
SELECT name, 'A' = name AS t      FROM try where name = 'A';
SELECT name, 'A' = name AS t      FROM try where name = 'A';

-- expected failures on duplicate key
INSERT INTO try (name) VALUES ('a');
INSERT INTO try (name) VALUES ('A');
INSERT INTO try (name) VALUES ('aB');

-- Make sure that citext_smaller() and citext_lager() work properly.
SELECT citext_smaller( 'aa'::citext, 'ab'::citext ) = 'aa' AS t;
SELECT citext_smaller( 'AAAA'::citext, 'bbbb'::citext ) = 'AAAA' AS t;
SELECT citext_smaller( 'aardvark'::citext, 'Aaba'::citext ) = 'Aaba' AS t;
SELECT citext_smaller( 'aardvark'::citext, 'AARDVARK'::citext ) = 'AARDVARK' AS t;

SELECT citext_larger( 'aa'::citext, 'ab'::citext ) = 'ab' AS t;
SELECT citext_larger( 'AAAA'::citext, 'bbbb'::citext ) = 'bbbb' AS t;
SELECT citext_larger( 'aardvark'::citext, 'Aaba'::citext ) = 'aardvark' AS t;

CREATE OPERATOR @> (
	LEFTARG = cube, RIGHTARG = cube, PROCEDURE = cube_contains,
	COMMUTATOR = '<@',
	RESTRICT = contsel, JOIN = contjoinsel
);

CREATE OPERATOR CLASS gist_cube_ops
   DEFAULT FOR TYPE cube USING gist AS
      OPERATOR	3	&& ,
      OPERATOR	6	= ,
      OPERATOR	7	@> ,
      OPERATOR	8	<@ ,
      OPERATOR	13	@ ,
      OPERATOR	14	~ ,
      FUNCTION	1	g_cube_consistent (internal, cube, int, oid, internal),
      FUNCTION	2	g_cube_union (internal, internal),
      FUNCTION	3	g_cube_compress (internal),
      FUNCTION	4	g_cube_decompress (internal),
      FUNCTION	5	g_cube_penalty (internal, internal, internal),
      FUNCTION	6	g_cube_picksplit (internal, internal),
      FUNCTION	7	g_cube_same (cube, cube, internal);

ALTER EXTENSION cube ADD operator <@(cube,cube);
ALTER EXTENSION cube ADD operator @>(cube,cube);
ALTER EXTENSION cube ADD operator ~(cube,cube);
ALTER EXTENSION cube ADD operator @(cube,cube);

select '(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)'::cube;

CREATE FUNCTION dblink_get_notify(
   OUT notify_name TEXT,
   OUT be_pid INT4,
   OUT extra TEXT
)
RETURNS setof record
AS 'MODULE_PATHNAME', 'dblink_get_notify'
LANGUAGE C STRICT;

CREATE DOMAIN lo AS pg_catalog.oid;

--
-- For backwards compatibility, define a function named lo_oid.
--
-- The other functions that formerly existed are not needed because
-- the implicit casts between a domain and its underlying type handle them.
--
CREATE FUNCTION lo_oid(lo) RETURNS pg_catalog.oid AS
'SELECT $1::pg_catalog.oid' LANGUAGE SQL STRICT IMMUTABLE;

-- This is used in triggers
CREATE FUNCTION lo_manage()
RETURNS pg_catalog.trigger
AS 'MODULE_PATHNAME'
LANGUAGE C;

/* @extschema@ isn't legal...
 * DROP FUNCTION @extschema@.xml_is_well_formed(text);
 */

DROP FUNCTION extschema.xml_is_well_formed(text);
 
CREATE TEXT SEARCH PARSER testparser (
   START    = testprs_start,
   GETTOKEN = testprs_getlexeme,
   END      = testprs_end,
   HEADLINE = pg_catalog.prsd_headline,
   LEXTYPES = testprs_lextype
);

CREATE TEXT SEARCH CONFIGURATION testcfg (PARSER = testparser);

ALTER TEXT SEARCH CONFIGURATION testcfg ADD MAPPING FOR word WITH simple;

SELECT "me"."id", "me"."name", "me"."group_id", "me"."user", "me"."version", "me"."is_active", "key_server_side_mia", "button_server_side_mia", "messenger_server_side_mia", "key_pro_server_side_mia", "key_status", "button_status", "messenger_status", "key_pro_status", "is_group_active", "last_checkin", "macro"
  FROM (
    SELECT "me"."id", "me"."name", "me"."group_id", "me"."user", "me"."version", "me"."is_active", "key_server_side_mia", "button_server_side_mia", "messenger_server_side_mia", "key_pro_server_side_mia", "key_status", "button_status", "messenger_status", "key_pro_status", "is_group_active", "last_checkin", "macro"
      FROM (
        SELECT "me"."id", "me"."name", "me"."group_id", "me"."user", "me"."version", "me"."is_active", (
            SELECT CASE WHEN device.last_checkin < DATEADD ( second, 4 * 60, GETUTCDATE(  ) ) THEN 1 ELSE 0 END
              FROM "Computer_Device" "computer_device_links_alias"
              JOIN "Devices" "device"
                ON "device"."id" = "computer_device_links_alias"."device_id"
              LEFT JOIN "Computer_Device" "computer_device_links"
                ON "computer_device_links"."device_id" = "device"."id"
              LEFT JOIN "Computers" "computer"
                ON "computer"."id" = "computer_device_links"."computer_id"
              LEFT JOIN "ComputerGroups" "group"
                ON "group"."id" = "computer"."group_id"
            WHERE "device"."type_id" = '2' AND "group"."is_lynx_key_pro" = '0' AND "device"."is_enabled" = '1' AND "computer_device_links_alias"."computer_id" = "me"."id"
           ) AS "key_server_side_mia", (
            SELECT CASE WHEN device.last_checkin < DATEADD ( second, 4 * 60, GETUTCDATE(  ) ) THEN 1 ELSE 0 END
              FROM "Computer_Device" "computer_device_links_alias"
              JOIN "Devices" "device"
                ON "device"."id" = "computer_device_links_alias"."device_id"
            WHERE "device"."type_id" = '1' AND "device"."is_enabled" = '1' AND "computer_device_links_alias"."computer_id" = "me"."id"
           ) AS "button_server_side_mia", (
            SELECT CASE WHEN device.last_checkin < DATEADD ( second, 4 * 60, GETUTCDATE(  ) ) THEN 1 ELSE 0 END
              FROM "Computer_Device" "computer_device_links_alias"
              JOIN "Devices" "device"
                ON "device"."id" = "computer_device_links_alias"."device_id"
            WHERE "device"."type_id" = '3' AND "device"."is_enabled" = '1' AND "computer_device_links_alias"."computer_id" = "me"."id"
           ) AS "messenger_server_side_mia", (
            SELECT CASE WHEN device.last_checkin < DATEADD ( second, 4 * 60, GETUTCDATE(  ) ) THEN 1 ELSE 0 END
              FROM "Computer_Device" "computer_device_links_alias"
              JOIN "Devices" "device"
                ON "device"."id" = "computer_device_links_alias"."device_id"
              LEFT JOIN "Computer_Device" "computer_device_links"
                ON "computer_device_links"."device_id" = "device"."id"
              LEFT JOIN "Computers" "computer"
                ON "computer"."id" = "computer_device_links"."computer_id"
              LEFT JOIN "ComputerGroups" "group"
                ON "group"."id" = "computer"."group_id"
            WHERE "device"."type_id" = '2' AND "group"."is_lynx_key_pro" = '1' AND "device"."is_enabled" = '1' AND "computer_device_links_alias"."computer_id" = "me"."id"
           ) AS "key_pro_server_side_mia", (
            SELECT "device"."status_id"
              FROM "Computer_Device" "computer_device_links_alias"
              JOIN "Devices" "device"
                ON "device"."id" = "computer_device_links_alias"."device_id"
              LEFT JOIN "Computer_Device" "computer_device_links"
                ON "computer_device_links"."device_id" = "device"."id"
              LEFT JOIN "Computers" "computer"
                ON "computer"."id" = "computer_device_links"."computer_id"
              LEFT JOIN "ComputerGroups" "group"
                ON "group"."id" = "computer"."group_id"
            WHERE "device"."type_id" = '2' AND "group"."is_lynx_key_pro" = '0' AND "device"."is_enabled" = '1' AND "computer_device_links_alias"."computer_id" = "me"."id"
           ) AS "key_status", (
            SELECT "device"."status_id"
              FROM "Computer_Device" "computer_device_links_alias"
              JOIN "Devices" "device"
                ON "device"."id" = "computer_device_links_alias"."device_id"
            WHERE "device"."type_id" = '1' AND "device"."is_enabled" = '1' AND "computer_device_links_alias"."computer_id" = "me"."id"
           ) AS "button_status", (
            SELECT "device"."status_id"
              FROM "Computer_Device" "computer_device_links_alias"
              JOIN "Devices" "device"
                ON "device"."id" = "computer_device_links_alias"."device_id"
            WHERE "device"."type_id" = '3' AND "device"."is_enabled" = '1' AND "computer_device_links_alias"."computer_id" = "me"."id"
           ) AS "messenger_status", (
            SELECT "device"."status_id"
              FROM "Computer_Device" "computer_device_links_alias"
              JOIN "Devices" "device"
                ON "device"."id" = "computer_device_links_alias"."device_id"
              LEFT JOIN "Computer_Device" "computer_device_links"
                ON "computer_device_links"."device_id" = "device"."id"
              LEFT JOIN "Computers" "computer"
                ON "computer"."id" = "computer_device_links"."computer_id"
              LEFT JOIN "ComputerGroups" "group"
                ON "group"."id" = "computer"."group_id"
            WHERE "device"."type_id" = '2' AND "group"."is_lynx_key_pro" = '1' AND "device"."is_enabled" = '1' AND "computer_device_links_alias"."computer_id" = "me"."id"
           ) AS "key_pro_status", (
            SELECT "group_alias"."is_active"
              FROM "ComputerGroups" "group_alias"
            WHERE "group_alias"."id" = "me"."group_id"
           ) AS "is_group_active", (
            SELECT MAX( "device"."last_checkin" )
              FROM "Computer_Device" "computer_device_links_alias"
              JOIN "Devices" "device"
                ON "device"."id" = "computer_device_links_alias"."device_id"
            WHERE "device"."is_enabled" = '1' AND "computer_device_links_alias"."computer_id" = "me"."id"
           ) AS "last_checkin", "macro"."to" AS "macro"
          FROM "Computers" "me"
          LEFT JOIN "Lookup" "macro"
            ON "macro"."from" = "me"."name"
          JOIN "ComputerGroups" "group"
            ON "group"."id" = "me"."group_id"
        WHERE "group"."is_active" = '1' AND "me"."is_active" = '1'
       ) "me"
   ) "me"
WHERE "rno__row__index" >= '1' AND "rno__row__index" <= '25'

SELECT "id", "start_date", "end_date", "is_active", "status", "location_tests_count", "failed_location_tests_count", "device_tests_count", "all_computers_count", "failed_computers_count", "untested_computers_count", "succeeded_computers_count"
  FROM (
    SELECT "id", "start_date", "end_date", "is_active", "status", "location_tests_count", "failed_location_tests_count", "device_tests_count", "all_computers_count", "failed_computers_count", "untested_computers_count", "succeeded_computers_count"
    FROM (
        SELECT "me"."id", "me"."start_date", "me"."end_date", "me"."is_active", "me"."status", (
            SELECT COUNT( * )
              FROM "Test_Computer" "test_computer_links_alias"
            WHERE "test_computer_links_alias"."test_id" = "me"."id"
           ) AS "location_tests_count", (
            SELECT COUNT( * )
              FROM "Test_Computer" "test_computer_links_alias"
              LEFT JOIN "Test_ComputerResults" "test_computer_result"
                ON "test_computer_result"."test_computer_id" = "test_computer_links_alias"."id"
              LEFT JOIN "Locations" "location"
                ON "location"."test_computer_result_id" = "test_computer_result"."id"
            WHERE "location"."corrected_location" IS NOT NULL AND "test_computer_links_alias"."test_id" = "me"."id"
           ) AS "failed_location_tests_count", (
            SELECT COUNT( * )
              FROM "Test_Device" "test_device_links_alias"
            WHERE "test_device_links_alias"."test_id" = "me"."id"
           ) AS "device_tests_count", (
            SELECT COUNT( * )
              FROM (
                SELECT "computer"."id", "computer"."name", "computer"."group_id", "computer"."user", "computer"."version", "computer"."is_active"
                  FROM "Test_Computer" "test_computer_links_alias"
                  JOIN "Computers" "computer"
                    ON "computer"."id" = "test_computer_links_alias"."computer_id"
                WHERE "test_computer_links_alias"."test_id" = "me"."id" UNION
                SELECT "computer"."id", "computer"."name", "computer"."group_id", "computer"."user", "computer"."version", "computer"."is_active"
                  FROM "Test_Device" "test_device_links_alias"
                  JOIN "Devices" "device"
                    ON "device"."id" = "test_device_links_alias"."device_id"
                  JOIN "Computer_Device" "computer_device_links"
                    ON "computer_device_links"."device_id" = "device"."id"
                  JOIN "Computers" "computer"
                    ON "computer"."id" = "computer_device_links"."computer_id"
                WHERE "test_device_links_alias"."test_id" = "me"."id"
               ) "computer"
           ) AS "all_computers_count", (
            SELECT COUNT( * )
              FROM (
                SELECT "computer"."id", "computer"."name", "computer"."group_id", "computer"."user", "computer"."version", "computer"."is_active"
                  FROM "Test_Computer" "test_computer_links_alias"
                  LEFT JOIN "Test_ComputerResults" "test_computer_result"
                    ON "test_computer_result"."test_computer_id" = "test_computer_links_alias"."id"
                  LEFT JOIN "Locations" "location"
                    ON "location"."test_computer_result_id" = "test_computer_result"."id"
                  JOIN "Computers" "computer"
                    ON "computer"."id" = "test_computer_links_alias"."computer_id"
                WHERE "location"."corrected_location" IS NOT NULL AND "test_computer_links_alias"."test_id" = "me"."id" UNION
                SELECT "computer"."id", "computer"."name", "computer"."group_id", "computer"."user", "computer"."version", "computer"."is_active"
                  FROM "Test_Device" "test_device_links_alias"
                  LEFT JOIN "TestResults" "test_result"
                    ON "test_result"."test_device_id" = "test_device_links_alias"."id"
                  LEFT JOIN "TestResult_TestResultType" "test_result_test_result_type_links"
                    ON "test_result_test_result_type_links"."test_result_id" = "test_result"."id"
                  JOIN "Devices" "device"
                    ON "device"."id" = "test_device_links_alias"."device_id"
                  JOIN "Computer_Device" "computer_device_links"
                    ON "computer_device_links"."device_id" = "device"."id"
                  JOIN "Computers" "computer"
                    ON "computer"."id" = "computer_device_links"."computer_id"
                WHERE "test_result_test_result_type_links"."is_success" = '0' AND "test_device_links_alias"."test_id" = "me"."id"
               ) "computer"
           ) AS "failed_computers_count", (
            SELECT COUNT( * )
              FROM (
                SELECT "computer"."id", "computer"."name", "computer"."group_id", "computer"."user", "computer"."version", "computer"."is_active"
                  FROM "Test_Computer" "test_computer_links_alias"
                  LEFT JOIN "Test_ComputerResults" "test_computer_result"
                    ON "test_computer_result"."test_computer_id" = "test_computer_links_alias"."id"
                  JOIN "Computers" "computer"
                    ON "computer"."id" = "test_computer_links_alias"."computer_id"
                WHERE "test_computer_result"."id" IS NULL AND "test_computer_links_alias"."test_id" = "me"."id" UNION
                SELECT "computer"."id", "computer"."name", "computer"."group_id", "computer"."user", "computer"."version", "computer"."is_active"
                  FROM "Test_Device" "test_device_links_alias"
                  LEFT JOIN "TestResults" "test_result"
                    ON "test_result"."test_device_id" = "test_device_links_alias"."id"
                  JOIN "Devices" "device"
                    ON "device"."id" = "test_device_links_alias"."device_id"
                  JOIN "Computer_Device" "computer_device_links"
                    ON "computer_device_links"."device_id" = "device"."id"
                  JOIN "Computers" "computer"
                    ON "computer"."id" = "computer_device_links"."computer_id"
                WHERE "test_result"."id" IS NULL AND "test_device_links_alias"."test_id" = "me"."id"
               ) "computer"
           ) AS "untested_computers_count", (
            SELECT COUNT( * )
              FROM "Computers" "me"
            WHERE "id" IN (
                SELECT "computer"."id"
                  FROM (
                    SELECT "computer"."id", "computer"."name", "computer"."group_id", "computer"."user", "computer"."version", "computer"."is_active"
                      FROM "Test_Computer" "test_computer_links_alias"
                      JOIN "Computers" "computer"
                        ON "computer"."id" = "test_computer_links_alias"."computer_id"
                    WHERE "test_computer_links_alias"."test_id" = "me"."id" UNION
                    SELECT "computer"."id", "computer"."name", "computer"."group_id", "computer"."user", "computer"."version", "computer"."is_active"
                      FROM "Test_Device" "test_device_links_alias"
                      JOIN "Devices" "device"
                        ON "device"."id" = "test_device_links_alias"."device_id"
                      JOIN "Computer_Device" "computer_device_links"
                        ON "computer_device_links"."device_id" = "device"."id"
                      JOIN "Computers" "computer"
                        ON "computer"."id" = "computer_device_links"."computer_id"
                    WHERE "test_device_links_alias"."test_id" = "me"."id"
                   ) "computer"
               ) AND "id" NOT IN (
                SELECT "computer"."id"
                  FROM (
                    SELECT "computer"."id", "computer"."name", "computer"."group_id", "computer"."user", "computer"."version", "computer"."is_active"
                      FROM "Test_Computer" "test_computer_links_alias"
                      LEFT JOIN "Test_ComputerResults" "test_computer_result"
                        ON "test_computer_result"."test_computer_id" = "test_computer_links_alias"."id" 
                      LEFT JOIN "Locations" "location"
                        ON "location"."test_computer_result_id" = "test_computer_result"."id"
                      JOIN "Computers" "computer"
                        ON "computer"."id" = "test_computer_links_alias"."computer_id"
                    WHERE "location"."corrected_location" IS NOT NULL AND "test_computer_links_alias"."test_id" = "me"."id" UNION
                    SELECT "computer"."id", "computer"."name", "computer"."group_id", "computer"."user", "computer"."version", "computer"."is_active"
                      FROM "Test_Device" "test_device_links_alias"
                      LEFT JOIN "TestResults" "test_result"
                        ON "test_result"."test_device_id" = "test_device_links_alias"."id"
                      LEFT JOIN "TestResult_TestResultType" "test_result_test_result_type_links"
                        ON "test_result_test_result_type_links"."test_result_id" = "test_result"."id"
                      JOIN "Devices" "device"
                        ON "device"."id" = "test_device_links_alias"."device_id"
                      JOIN "Computer_Device" "computer_device_links"
                        ON "computer_device_links"."device_id" = "device"."id"
                      JOIN "Computers" "computer"
                        ON "computer"."id" = "computer_device_links"."computer_id"
                    WHERE "test_result_test_result_type_links"."is_success" = '0' AND "test_device_links_alias"."test_id" = "me"."id"
                   ) "computer"
               )
           ) AS "succeeded_computers_count"
          FROM (
            SELECT "me"."id", "me"."start_date", "me"."end_date", "me"."is_active", 'completed' as status
              FROM "Tests" "me"
            WHERE "me"."end_date" < GETUTCDATE(  ) UNION
            SELECT "me"."id", "me"."start_date", "me"."end_date", "me"."is_active", 'in progress' as status
              FROM "Tests" "me"
            WHERE "me"."end_date" >= GETUTCDATE(  ) AND "me"."start_date" <= GETUTCDATE(  ) UNION
            SELECT "me"."id", "me"."start_date", "me"."end_date", "me"."is_active", 'scheduled' as status
              FROM "Tests" "me"
            WHERE "me"."start_date" > GETUTCDATE(  )
           ) "me"
       ) "me"
   ) "me"
WHERE "rno__row__index" >= '1' AND "rno__row__index" <= '25'
