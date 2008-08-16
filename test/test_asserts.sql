SET search_path = test, public, pg_catalog;


CREATE OR REPLACE FUNCTION test.test_assert_test_schema() RETURNS VOID AS $$
-- Assert the correct operation of epic.assert_test_schema
-- module: test_asserts
DECLARE
  nsoid           oid;
  objname         text;
BEGIN
  -- assert_test_schema MUST create a 'test' schema.
  -- Of course, if it didn't, this proc wouldn't compile, but what the hey.
  SELECT pg_namespace.oid INTO nsoid FROM pg_namespace WHERE nspname = 'test';
  IF nsoid IS NULL THEN
    RAISE EXCEPTION 'assert_test_schema did not create a ''test'' schema.';
  END IF;
  
  -- assert_test_schema MUST create a 'test.results' table
  SELECT tablename INTO objname FROM pg_tables 
    WHERE schemaname = 'test' AND tablename = 'results';
  IF objname IS NULL THEN
    RAISE EXCEPTION 'assert_test_schema did not create a test.results table.';
  END IF;
  
  -- assert_test_schema MUST create a 'test.suite_results' type
  SELECT typname INTO objname FROM pg_type
    WHERE typnamespace = nsoid AND typname = 'suite_results';
  IF objname IS NULL THEN
    RAISE EXCEPTION 'assert_test_schema did not create a test.suite_results type.';
  END IF;
  
  RAISE EXCEPTION '[OK]';
END;
$$ LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION test.test_assert_raises() RETURNS VOID AS $$
-- Assert the correct operation of test.assert_raises
-- module: test_asserts
DECLARE
  retval    text;
  failed    bool;
BEGIN
  -- assert_raises() MUST return VOID if an error is raised.
  SELECT INTO retval * FROM test.assert_raises('unknown', 'relation "unknown" does not exist', '42P01');
  IF retval != '' THEN
    RAISE EXCEPTION 'test.assert_raises() did not return void.';
  END IF;
  
  -- assert_raises() MUST raise an exception if no error is raised.
  failed := false;
  BEGIN
    SELECT INTO retval * FROM test.assert_raises('pg_namespace', '', '');
  EXCEPTION WHEN OTHERS THEN
    failed := true;
    IF SQLERRM = 'Call: ''pg_namespace'' did not raise an error.' THEN
      NULL;
    ELSE
      RAISE EXCEPTION 'test.assert_raises() did not raise the given message on falsehood. Raised: %', SQLERRM;
    END IF;
  END;
  IF NOT failed THEN
    RAISE EXCEPTION 'test.assert_raises() did not fail.';
  END IF;
  
  RAISE EXCEPTION '[OK]';
END;
$$ LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION test.test_assert() RETURNS VOID AS $$
-- Assert the correct operation of test.assert
-- module: test_asserts
DECLARE
  retval    text;
BEGIN
  -- assert() MUST return VOID if the given assertion holds.
  SELECT INTO retval * FROM test.assert(true, 'truth is falsehood!');
  IF retval != '' THEN
    RAISE EXCEPTION 'test.assert() did not return void.';
  END IF;
  
  -- assert() MUST raise an exception if the given assertion does not hold.
  PERFORM test.assert_raises('test.assert(false, ''falsehood is truth'')',
                             'falsehood is truth', 'P0001');
  
  -- assert() MUST raise an exception if the given assertion is NULL.
  PERFORM test.assert_raises('test.assert(null, ''null should choke'')',
                             'Assertion test may not be NULL.', 'P0001');
  
  RAISE EXCEPTION '[OK]';
END;
$$ LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION test.test_assert_equal() RETURNS VOID AS $$
-- Assert the correct operation of test.assert_equal
-- module: test_asserts
DECLARE
  retval    text;
BEGIN
  -- assert_equal() MUST return VOID if the given assertion holds.
  SELECT INTO retval * FROM test.assert_equal(1, 1);
  IF retval != '' THEN
    RAISE EXCEPTION 'test.assert_equal() did not return void for (1, 1).';
  END IF;
  SELECT INTO retval * FROM test.assert_equal('abc'::text, 'abc');
  IF retval != '' THEN
    RAISE EXCEPTION 'test.assert_equal() did not return void for (''abc'', ''abc'').';
  END IF;
  
  -- assert_equal() MUST raise an exception if the given assertion does not hold.
  PERFORM test.assert_raises('test.assert_equal(1, 2)', '1 != 2', 'P0001');
  
  PERFORM test.assert_raises('test.assert_equal(''abc''::text, ''xyz'')', 'abc != xyz', 'P0001');
  
  -- assert_equal() MUST return VOID if both args are null.
  SELECT INTO retval * FROM test.assert_equal(NULL::int, NULL);
  IF retval != '' THEN
    RAISE EXCEPTION 'test.assert_equal() did not return void for (null, null).';
  END IF;
  
  -- assert_equal() MUST raise an exception if only one arg is NULL.
  PERFORM test.assert_raises('test.assert_equal(8, NULL::int)', '8 != <NULL>', 'P0001');
  PERFORM test.assert_raises('test.assert_equal(NULL::int, 7)', '<NULL> != 7', 'P0001');
  
  -- assert_equal() will raise an undefined_function exception if the args have different types.
  -- It would be nice to find a way around this (without writing M x N overloaded funcs).
  PERFORM test.assert_raises('test.assert_equal(8, ''abc''::text)',
    'function test.assert_equal(integer, text) does not exist', '42883');
  
  RAISE EXCEPTION '[OK]';
END;
$$ LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION test.test_assert_not_equal() RETURNS VOID AS $$
-- Assert the correct operation of test.assert_not_equal
-- module: test_asserts
DECLARE
  retval    text;
BEGIN
  -- assert_not_equal() MUST return VOID if the given assertion does not hold.
  SELECT INTO retval * FROM test.assert_not_equal(1, 2);
  IF retval != '' THEN
    RAISE EXCEPTION 'test.assert_not_equal() did not return void for (1, 2).';
  END IF;
  SELECT INTO retval * FROM test.assert_not_equal('abc'::text, 'xyz');
  IF retval != '' THEN
    RAISE EXCEPTION 'test.assert_not_equal() did not return void for (''abc'', ''xyz'').';
  END IF;
  
  -- assert_not_equal() MUST raise an exception if the given assertion holds.
  PERFORM test.assert_raises('test.assert_not_equal(1, 1)', '1 = 1', 'P0001');
  PERFORM test.assert_raises('test.assert_not_equal(''abc''::text, ''abc'')', 'abc = abc', 'P0001');
  
  -- assert_not_equal() MUST return VOID if only one arg is NULL.
  SELECT INTO retval * FROM test.assert_not_equal(8, NULL);
  IF retval != '' THEN
    RAISE EXCEPTION 'test.assert_not_equal() did not return void for (8, null).';
  END IF;
  SELECT INTO retval * FROM test.assert_not_equal(NULL, 7);
  IF retval != '' THEN
    RAISE EXCEPTION 'test.assert_not_equal() did not return void for (null, 7).';
  END IF;
  
  -- assert_not_equal() MUST raise an exception if both args are NULL.
  PERFORM test.assert_raises('test.assert_not_equal(NULL::int, NULL)', '<NULL> = <NULL>', 'P0001');
  
  -- assert_not_equal() will raise an undefined_function exception if the args have different types.
  -- It would be nice to find a way around this (without writing M x N overloaded funcs).
  PERFORM test.assert_raises('test.assert_not_equal(8, ''abc''::text)', 
    'function test.assert_not_equal(integer, text) does not exist', '42883');
  
  RAISE EXCEPTION '[OK]';
END;
$$ LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION test.test_assert_less_than_or_equal() RETURNS VOID AS $$
-- Assert the correct operation of test.assert_less_than_or_equal
-- module: test_asserts
DECLARE
  retval    text;
BEGIN
  -- assert_less_than_or_equal() MUST return VOID if a <= b.
  SELECT INTO retval * FROM test.assert_less_than_or_equal(1, 2);
  IF retval != '' THEN
    RAISE EXCEPTION 'test.assert_less_than_or_equal() did not return void for (1, 2).';
  END IF;
  SELECT INTO retval * FROM test.assert_less_than_or_equal(1, 1);
  IF retval != '' THEN
    RAISE EXCEPTION 'test.assert_less_than_or_equal() did not return void for (1, 1).';
  END IF;
  SELECT INTO retval * FROM test.assert_less_than_or_equal('abc'::text, 'xyz');
  IF retval != '' THEN
    RAISE EXCEPTION 'test.assert_less_than_or_equal() did not return void for (''abc'', ''xyz'').';
  END IF;
  SELECT INTO retval * FROM test.assert_less_than_or_equal('abc'::text, 'abc');
  IF retval != '' THEN
    RAISE EXCEPTION 'test.assert_less_than_or_equal() did not return void for (''abc'', ''abc'').';
  END IF;
  
  -- assert_less_than_or_equal() MUST raise an exception if a > b.
  PERFORM test.assert_raises('test.assert_less_than_or_equal(2, 1)', '2 not <= 1', 'P0001');
  PERFORM test.assert_raises('test.assert_less_than_or_equal(''xyz''::text, ''abc'')', 
    'xyz not <= abc', 'P0001');
  
  -- assert_less_than_or_equal() MUST raise an exception if either arg is NULL.
  PERFORM test.assert_raises('test.assert_less_than_or_equal(NULL::int, NULL)', 
    'Assertion arguments may not be NULL.', 'P0001');
  
  -- assert_less_than_or_equal() will raise an undefined_function exception if the args have different types.
  -- It would be nice to find a way around this (without writing M x N overloaded funcs).
  PERFORM test.assert_raises('test.assert_less_than_or_equal(8, ''abc''::text)', 
    'function test.assert_less_than_or_equal(integer, text) does not exist', '42883');
  
  RAISE EXCEPTION '[OK]';
END;
$$ LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION test.test_record_asserter() RETURNS VOID AS $$
-- Assert the correct operation of test.record_asserter
-- module: test_asserts
DECLARE
  assertion    text;
  assertions   text[];
  old          record;
  new          record;
BEGIN
  FOR assertion in
    SELECT * FROM test.record_asserter('old', 'new', 'first, last, city')
  LOOP
    assertions := assertions || assertion;
  END LOOP;
  
  IF assertions <> ARRAY['PERFORM test.assert_equal("old"."first", "new"."first");',
                         'PERFORM test.assert_equal("old"."last", "new"."last");',
                         'PERFORM test.assert_equal("old".city, "new".city);'] THEN
    RAISE EXCEPTION 'record_asserter did not return the proper SQL. %', assertions;
  END IF;
  
  -- Now just for fun, execute the returned SQL.
  CREATE TEMPORARY TABLE _test_user (first text, last text, city text);
  INSERT INTO _test_user VALUES ('Michael', 'Stonebraker', 'New York');
  SELECT INTO old * FROM _test_user WHERE city = 'New York';
  SELECT INTO new * FROM _test_user WHERE city = 'New York';
  FOR i IN array_lower(assertions, 1)..array_upper(assertions, 1)
  LOOP
    PERFORM assertions[i];
  END LOOP;
  
  RAISE EXCEPTION '[OK]';
END;
$$ LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION test.test_assert_values() RETURNS VOID AS $$
-- Assert the correct operation of test.assert_values
-- module: test_asserts
DECLARE
  failed     bool;
BEGIN
  PERFORM test.assert_values(
    'generate_series(1, 10);',
    ARRAY[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);
  
  failed := false;
  BEGIN
    PERFORM test.assert_values('generate_series(1, 10);', ARRAY[1, 2]);
  EXCEPTION WHEN OTHERS THEN
    failed := true;
    IF SQLERRM = 'result: 3 not in array: {1,2}' THEN
      NULL;
    ELSE
      RAISE EXCEPTION 'test.assert_values() did not raise the correct error. Raised: %', SQLERRM;
    END IF;
  END;
  IF NOT failed THEN
    RAISE EXCEPTION 'test.assert_values() did not fail.';
  END IF;
  
  RAISE EXCEPTION '[OK]';
END;
$$ LANGUAGE plpgsql;
