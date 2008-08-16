/*

Epic.sql -- A framework for unit testing Postgres, especially PL/pgSQL.

The framework consists of functions to both help write tests and help run them.

Installing epic.sql
===================

Simply execute/import the epic.sql file (this file) into the database you'd
like to test:

    psql testdb
    testdb=# \i 'C:\\Python24\\Lib\\site-packages\\sql\\pgtest\\epic.sql'
    CREATE FUNCTION
     assert_test_schema
    --------------------
     t
    (1 row)

    DROP FUNCTION
    CREATE FUNCTION
    CREATE VIEW
    CREATE FUNCTION
    CREATE FUNCTION
    CREATE FUNCTION
    CREATE FUNCTION
    CREATE FUNCTION
    CREATE FUNCTION
    CREATE FUNCTION

Then, execute/import any tests you've written (see below):

    testdb=# \i 'C:\\Python24\\Lib\\site-packages\\sql\\pgtest\\test_users.sql'
    SET
    CREATE FUNCTION
    testdb=# \i 'C:\\Python24\\Lib\\site-packages\\sql\\pgtest\\test_transactions.sql'
    SET
    CREATE FUNCTION
    CREATE FUNCTION


Writing tests
=============

Write tests as PL/pgSQL procedures. There are only a couple of things to do
to ensure they work well with the framework:
    
    1. ALWAYS RAISE EXCEPTION at the end of test procs to rollback! Even if
        the test passes, RAISE EXCEPTION '[OK]'.
    2. Put your test in the "test" schema.
    3. Start the name of your test with "test_".
        I like "test_[schema]_[target proc]" but do what you like.
    4. Include a comment of the form "-- module: [module_name]", replacing
        [module_name] with the (arbitrary) name of the "module". This can
        then be used with test.run_module().


Example test
------------

CREATE OR REPLACE FUNCTION test.test_inner_set_user_state() RETURNS VOID AS $$
-- module: test_users
DECLARE
  user_id   integer;
  user_rec  users%ROWTYPE;
BEGIN
  <<MAIN>>
  BEGIN
    -- Create dummy records
    INSERT INTO users (login_name) VALUES ('test1') RETURNING user_id INTO user_id;
    
    -- Run the proc
    PERFORM "inner".set_user_state(user_id);
    
    -- The proc MUST set users.state to 'active';
    SELECT INTO user_rec * FROM users WHERE user_id = user_id;
    PERFORM test.assert_equal(user_rec.state, 'active');
  END MAIN;

  -- ALWAYS RAISE EXCEPTION at the end of test procs to rollback!
  RAISE EXCEPTION '[OK]';
END;
$$ LANGUAGE plpgsql;


Test helper functions
---------------------

Epic.sql includes some functions to make tests easier to write (and shorter).
The following functions all return void, raising an exception if the assertion
doesn't hold:

    * test.assert(assertion boolean, msg text): this is the 'catch-all'
        to assert anything that can be evaluated to a boolean. For example,
        PERFORM test.assert(substring(a from b), b||" not found in "||a);
    * test.assert_void(call text)
    * test.assert_equal(elem_1 anyelement, elem_2 anyelement)
    * test.assert_not_equal(elem_1 anyelement, elem_2 anyelement)
    * test.assert_greater_than(elem_1 anyelement, elem_2 anyelement)
    * test.assert_greater_than_or_equal(elem_1 anyelement, elem_2 anyelement)
    * test.assert_less_than(elem_1 anyelement, elem_2 anyelement)
    * test.assert_less_than_or_equal(elem_1 anyelement, elem_2 anyelement)
    * test.assert_values(column text, source text, expected anyarray):
        Raises an exception if SELECT column FROM source != expected.
    
    * test.assert_raises(call text, errm text, state text): Raises an
        exception if 'SELECT * FROM [call];' does not raise errm
        (if provided) or state (if provided).

Some return dynamic SQL:
    
    * test.record_asserter(varname1 text, varname2 text, colnames text):
        Returns EXECUTE-able SQL to assert equal fields for the two records.
        LOOP over its results and PERFORM each one.


Running tests
=============

Epic includes a VIEW to manage known tests:

    testdb=# SELECT * FROM test.testnames;
                         name          |      module
    -----------------------------------+-------------------
     test_inner_trans_set_active       | test_transactions
     test_inner_trans_set_create       | test_transactions
     test_inner_count_users_by_login   | test_users
    (3 rows)

Because you made the effort to RAISE EXCEPTION even if the test passes,
it's safe to run any of these tests directly:

    testdb=# SELECT * FROM test.test_inner_trans_set_create();
    ERROR:  [OK]

However, if you'd like to run multiple tests together, use the module names
you created to group them:

    testdb=# SELECT * FROM test.run_module('test_transactions');
                         name    |      module       | result | errcode | errmsg
    -----------------------------+-------------------+--------+---------+--------
     test_inner_trans_set_active | test_transactions | [OK]   |         |
     test_inner_trans_set_create | test_transactions | [OK]   |         |
    (2 rows)

As you can see, when you use the test.run_* functions, the results are stored
in a table (called 'test.results'). If you get busy doing other things, you
can always read directly from that table to see which tests passed. Note also
that the test.run_* functions trap the [OK] and other exceptions from each test.

If you want to run all tests without regard to module:

    testdb=# SELECT * FROM test.run_all();
                         name        |      module       | result | errcode |                             errmsg
    ---------------------------------+-------------------+--------+---------+---------------------------------------------------------------
     test_inner_trans_set_active     | test_transactions | [OK]   |         |
     test_inner_trans_set_create     | test_transactions | [OK]   |         |
     test_inner_count_users_by_login | test_users        | [FAIL] | 42883   | function inner.count_users_by_login("unknown") does not exist
    (3 rows)

If you want the complete CONTEXT, etc. for the [FAIL] above, run the test
directly and you'll get the normal traceback, etc. from PL/pgSQL.

Finally, you can run any test individually via:

    testdb=# SELECT * FROM test.run_test('test_inner_trans_set_create');
     run_test
    ----------
     t
    (1 row)

Using run_test, you get a T/F response instead of an exception (that running
the test directly would raise). You also get an entry in test.results,
so if you fix a single test, you can update its success without having
to run a whole set of tests you didn't modify.

*/

CREATE OR REPLACE FUNCTION assert_test_schema() RETURNS boolean AS $$
BEGIN
  SET client_min_messages = warning;
  
  BEGIN
    CREATE SCHEMA test;
  EXCEPTION WHEN duplicate_schema THEN
    NULL;
  END;
  
  BEGIN
    CREATE TABLE test.results (name text PRIMARY KEY, module text,
                               ok boolean, errcode text, errmsg text);
  EXCEPTION WHEN duplicate_table THEN
    NULL;
  END;
  
  BEGIN
    CREATE TYPE test.suite_results AS (name text, module text, result text,
                                       errcode text, errmsg text);
  EXCEPTION WHEN duplicate_table THEN
    NULL;
  END;
  
  RETURN TRUE;
END;
$$ LANGUAGE plpgsql;

SELECT * FROM assert_test_schema();
DROP FUNCTION assert_test_schema();


CREATE OR REPLACE FUNCTION test.run_test(testname text) RETURNS boolean AS $$
-- Runs the named test, stores in test.results, and returns success.
BEGIN
  DELETE FROM test.results WHERE name = testname;
  
  BEGIN
    EXECUTE 'SELECT * FROM test.' || testname || '();';
  EXCEPTION WHEN OTHERS THEN
    IF SQLERRM = '[OK]' THEN
      INSERT INTO test.results (name, ok) VALUES (testname, TRUE);
      RETURN TRUE;
    ELSE
      INSERT INTO test.results (name, ok, errcode, errmsg)
           VALUES (testname, FALSE, SQLSTATE, SQLERRM);
      RETURN FALSE;
    END IF;
  END;
  
  RAISE EXCEPTION 'Test % did not raise an exception as it should have. Exceptions must ALWAYS be raised in test procedures for rollback.', testname;
END;
$$ LANGUAGE plpgsql;


CREATE OR REPLACE VIEW test.testnames AS
  SELECT pg_proc.proname AS name,
    substring(pg_proc.prosrc from E'--\\s+module[:]\\s+(\\S+)') AS module
  FROM pg_namespace LEFT JOIN pg_proc
  ON pg_proc.pronamespace::oid = pg_namespace.oid::oid
  WHERE pg_namespace.nspname = 'test'
    AND pg_proc.proname LIKE E'test\_%';


CREATE OR REPLACE FUNCTION test.run_module(modulename text) RETURNS SETOF test.suite_results AS $$
-- Runs all tests in the given module, stores in test.results, and returns results.
DECLARE
  testname        pg_proc.proname%TYPE;
  output_record   test.suite_results%ROWTYPE;
BEGIN
  FOR testname IN SELECT name, module FROM test.testnames WHERE module = modulename
  LOOP
    PERFORM test.run_test(testname);
    UPDATE test.results SET module = modulename WHERE name = testname;
  END LOOP;
  
  FOR output_record in
    SELECT name, module, CASE WHEN ok=true THEN '[OK]' ELSE '[FAIL]' END,
           errcode, errmsg
    FROM test.results
    WHERE module = modulename
  LOOP
    RETURN NEXT output_record;
  END LOOP;
END;
$$ LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION test.run_all() RETURNS SETOF test.suite_results AS $$
-- Runs all known test functions, stores in test.results, and returns results.
DECLARE
  testname pg_proc.proname%TYPE;
  modulename text;
  output_record test.suite_results%ROWTYPE;
BEGIN
  FOR modulename in SELECT DISTINCT module FROM test.testnames ORDER BY module ASC
  LOOP
    FOR testname IN SELECT name FROM test.testnames WHERE module = modulename
    LOOP
      PERFORM test.run_test(testname);
      UPDATE test.results SET module = modulename WHERE name = testname;
    END LOOP;
  END LOOP;
  
  FOR output_record in
    SELECT name, module, CASE WHEN ok=true THEN '[OK]' ELSE '[FAIL]' END, errcode, errmsg
    FROM test.results
  LOOP
    RETURN NEXT output_record;
  END LOOP;
END;
$$ LANGUAGE plpgsql;


------------------------------ Test helpers ------------------------------


CREATE OR REPLACE FUNCTION test.assert_void(call text) RETURNS VOID AS $$
-- Raises an exception if SELECT * FROM call != void.
DECLARE
  retval    text;
BEGIN
  EXECUTE ('SELECT * FROM ' || call || ';') INTO retval;
  IF retval != '' THEN
    RAISE EXCEPTION 'Call: ''%'' did not return void. Got ''%'' instead.', call, retval;
  END IF;
END;
$$ LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION test.assert(assertion boolean, msg text) RETURNS VOID AS $$
-- Raises an exception (msg) if assertion is false.
-- 
-- assertion may not be NULL.
BEGIN
  IF assertion IS NULL THEN
    RAISE EXCEPTION 'Assertion test may not be NULL.';
  END IF;
  
  IF NOT assertion THEN
    RAISE EXCEPTION '%', msg;
  END IF;
END;
$$ LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION test.assert_equal(elem_1 anyelement, elem_2 anyelement) RETURNS VOID AS $$
-- Raises an exception if elem_1 is not equal to elem_2.
-- 
-- The two arguments must be of the same type. If they are not,
-- you will receive "ERROR:  invalid input syntax ..."
BEGIN
  IF ((elem_1 IS NULL AND NOT (elem_2 IS NULL)) OR
      (elem_2 IS NULL AND NOT (elem_1 IS NULL)) OR
      elem_1 != elem_2) THEN
    RAISE EXCEPTION '% != %', elem_1, elem_2;
  END IF;
END;
$$ LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION test.assert_not_equal(elem_1 anyelement, elem_2 anyelement) RETURNS VOID AS $$
-- Raises an exception if elem_1 is equal to elem_2
-- 
-- The two arguments must be of the same type. If they are not,
-- you will receive "ERROR:  invalid input syntax ..."
BEGIN
  IF ((elem_1 IS NULL AND elem_2 IS NULL) OR elem_1 = elem_2) THEN
    RAISE EXCEPTION '% = %', elem_1, elem_2;
  END IF;
END;
$$ LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION test.assert_less_than(elem_1 anyelement, elem_2 anyelement) RETURNS VOID AS $$
-- Raises an exception if elem_1 >= elem_2
-- 
-- The two arguments must be of the same type. If they are not,
-- you will receive "ERROR:  invalid input syntax ..."
BEGIN
  IF (elem_1 IS NULL or elem_2 IS NULL) THEN
    RAISE EXCEPTION 'Assertion arguments may not be NULL.';
  END IF;
  IF NOT (elem_1 < elem_2) THEN
    RAISE EXCEPTION '% not < %', elem_1, elem_2;
  END IF;
END;
$$ LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION test.assert_less_than_or_equal(elem_1 anyelement, elem_2 anyelement) RETURNS VOID AS $$
-- Raises an exception if elem_1 > elem_2
-- 
-- The two arguments must be of the same type. If they are not,
-- you will receive "ERROR:  invalid input syntax ..."
BEGIN
  IF (elem_1 IS NULL or elem_2 IS NULL) THEN
    RAISE EXCEPTION 'Assertion arguments may not be NULL.';
  END IF;
  IF NOT (elem_1 <= elem_2) THEN
    RAISE EXCEPTION '% not <= %', elem_1, elem_2;
  END IF;
END;
$$ LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION test.assert_greater_than(elem_1 anyelement, elem_2 anyelement) RETURNS VOID AS $$
-- Raises an exception if elem_1 <= elem_2
-- 
-- The two arguments must be of the same type. If they are not,
-- you will receive "ERROR:  invalid input syntax ..."
BEGIN
  IF (elem_1 IS NULL or elem_2 IS NULL) THEN
    RAISE EXCEPTION 'Assertion arguments may not be NULL.';
  END IF;
  IF NOT (elem_1 > elem_2) THEN
    RAISE EXCEPTION '% not > %', elem_1, elem_2;
  END IF;
END;
$$ LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION test.assert_greater_than_or_equal(elem_1 anyelement, elem_2 anyelement) RETURNS VOID AS $$
-- Raises an exception if elem_1 < elem_2
-- 
-- The two arguments must be of the same type. If they are not,
-- you will receive "ERROR:  invalid input syntax ..."
BEGIN
  IF (elem_1 IS NULL or elem_2 IS NULL) THEN
    RAISE EXCEPTION 'Assertion arguments may not be NULL.';
  END IF;
  IF NOT (elem_1 >= elem_2) THEN
    RAISE EXCEPTION '% not >= %', elem_1, elem_2;
  END IF;
END;
$$ LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION test.assert_raises(call text, errm text, state text) RETURNS VOID AS $$
-- Raises an exception if 'SELECT * FROM [call];' does not raise errm.
-- 
-- Example:
--
--    PERFORM test.assert_raises('get_transaction_by_id("a")', 'Bad argument', NULL);
--
-- If errm or state are NULL, that value will not be tested. This allows
-- you to test by message alone (since the 5-char SQLSTATE values are cryptic),
-- or trap a range of errors by SQLSTATE without regard for the exact message.
-- 
-- If you don't know the message you want to trap, call this function with
-- errm = '' and state = ''. The resultant error will tell you the
-- SQLSTATE and SQLERRM that were raised.
BEGIN
  BEGIN
    EXECUTE 'SELECT * FROM '||call||';';
  EXCEPTION
    WHEN OTHERS THEN
      IF ((state IS NOT NULL AND SQLSTATE != state) OR
          (errm IS NOT NULL AND SQLERRM != errm)) THEN
        RAISE EXCEPTION 'Call: ''%'' raised ''(%) %'' instead of ''(%) %''.', call, SQLSTATE, SQLERRM, state, errm;
      END IF;
      RETURN;
  END;
  RAISE EXCEPTION 'Call: ''%'' did not raise an error.', call;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION test.assert_raises(call text, errm text) RETURNS VOID AS $$
-- Implicit column version of assert_values
BEGIN
  PERFORM test.assert_raises(call, errm, NULL);
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION test.assert_raises(call text) RETURNS VOID AS $$
-- Implicit errm, column version of assert_values
BEGIN
  PERFORM test.assert_raises(call, NULL, NULL);
END;
$$ LANGUAGE plpgsql;



CREATE OR REPLACE FUNCTION test.record_asserter(varname1 text, varname2 text, colnames text) RETURNS SETOF text AS $$
-- Returns EXECUTE-able SQL to assert equal fields for the two records.
--
-- Pass the variable *names* (not the records themselves) as the first
-- two arguments, and a comma-delimited list of column names to compare.
--
-- Example:
-- 
--    SELECT INTO old * FROM table WHERE id = 1;
--    SELECT INTO new * FROM table WHERE id = 2;
--    FOR assertion in
--      SELECT * FROM test.record_asserter('old', 'new', 'first, last, city')
--    LOOP
--      PERFORM assertion;
--    END LOOP;
--
DECLARE
  i             integer:=1;
  colname       text;
  colnames_arr  text[];
BEGIN
  --TODO: IF colnames IS NULL grab colnames from type
  
  colnames_arr := string_to_array(colnames, ',');
  FOR i IN array_lower(colnames_arr, 1)..array_upper(colnames_arr, 1)
  LOOP
    colname := quote_ident(trim(both ' ' from colnames_arr[i]));
    RETURN NEXT 'PERFORM test.assert_equal(' ||
                 quote_ident(varname1) || '.' || colname || ', ' ||
                 quote_ident(varname2) || '.' || colname || ');';
  END LOOP;
  RETURN;
END;
$$ LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION test.assert_values(source text, expected anyarray, colname text) RETURNS VOID AS $$
-- Raises an exception if SELECT column FROM source != expected.
--
-- colname shoudl be the name of the column in source to compare.
-- If NULL, it will be taken from the first column of source's output.
--
-- source can be any table, view, or procedure that returns records.
-- expected MUST be an array of the same type as colname.
-- Neither source nor expected need to be sorted.
-- 
-- Example:
--    PERFORM test.assert_values(
--      'get_favorite_user_ids(' || user_id || ');',
--      ARRAY[24, 10074, 87321], 'user_id');
-- 
DECLARE
  i             integer;
  record        record;
  firstname     text;
BEGIN
  -- Dump the source into a temp table
  IF colname IS NULL THEN
    EXECUTE 'CREATE TEMPORARY TABLE _test_assert_values_base AS ' ||
      'SELECT * FROM ' || source || ';';
    SELECT INTO firstname a.attname
      FROM pg_class c LEFT JOIN pg_attribute a ON c.oid = a.attrelid
      WHERE c.relname = '_test_assert_values_base'
      -- "The number of the column. Ordinary columns are numbered from 1 up.
      -- System columns, such as oid, have (arbitrary) negative numbers"
      AND a.attnum >= 1
      ORDER BY a.attnum;
    EXECUTE 'ALTER TABLE _test_assert_values_base RENAME ' || firstname || ' TO result;';
  ELSE
    EXECUTE 'CREATE TEMPORARY TABLE _test_assert_values_base AS ' ||
      'SELECT ' || colname || ' AS result FROM ' || source || ';';
  END IF;
  
  -- Dump the provided array into a temp table
  -- Use EXECUTE for all statements involving this table so its query plan
  -- doesn't get cached and re-used (or subsequent calls will fail).
  EXECUTE 'CREATE TEMPORARY TABLE _test_assert_values_expected (LIKE _test_assert_values_base);';
  FOR i IN array_lower(expected, 1)..array_upper(expected, 1)
  LOOP
    EXECUTE 'INSERT INTO _test_assert_values_expected (result) VALUES (' || quote_literal(expected[i]) || ');';
  END LOOP;
  
  -- Compare the two tables in setwise fashion.
  <<TRY>>
  BEGIN
    FOR record IN EXECUTE '(SELECT * FROM _test_assert_values_base EXCEPT ALL
                     SELECT * FROM _test_assert_values_expected)'
    LOOP
      RAISE EXCEPTION 'result: % not in array: %', record.result, expected;
    END LOOP;
    
    FOR record IN EXECUTE '(SELECT * FROM _test_assert_values_expected EXCEPT ALL
                     SELECT * FROM _test_assert_values_base)'
    LOOP
      RAISE EXCEPTION 'element: % not in source: %', record.result, source;
    END LOOP;
  EXCEPTION WHEN OTHERS THEN
    DROP TABLE _test_assert_values_base;
    EXECUTE 'DROP TABLE _test_assert_values_expected';
    RAISE EXCEPTION '%', SQLERRM;
  END TRY;
  
  DROP TABLE _test_assert_values_base;
  EXECUTE 'DROP TABLE _test_assert_values_expected';
END;
$$ LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION test.assert_values(source text, expected anyarray) RETURNS VOID AS $$
-- Implicit column version of assert_values
BEGIN
  PERFORM test.assert_values(source, expected, NULL);
END;
$$ LANGUAGE plpgsql;
