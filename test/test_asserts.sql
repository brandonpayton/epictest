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


CREATE OR REPLACE FUNCTION test.test_assert() RETURNS VOID AS $$
-- Assert the correct operation of test.assert
-- module: test_asserts
DECLARE
  retval    text;
  failed    bool;
BEGIN
  -- assert() MUST return VOID if the given assertion holds.
  SELECT INTO retval * FROM test.assert(true, 'truth is falsehood!');
  IF retval != '' THEN
    RAISE EXCEPTION 'test.assert() did not return void.';
  END IF;
  
  -- assert() MUST raise an exception if the given assertion does not hold.
  failed := false;
  BEGIN
    SELECT INTO retval * FROM test.assert(false, 'falsehood is truth');
  EXCEPTION WHEN OTHERS THEN
    failed := true;
    IF SQLERRM = 'falsehood is truth' THEN
      NULL;
    ELSE
      RAISE EXCEPTION 'test.assert(false) did not raise the given message on falsehood.';
    END IF;
  END;
  IF NOT failed THEN
    RAISE EXCEPTION 'test.assert(false) did not fail.';
  END IF;
  
  -- assert() MUST raise an exception if the given assertion is NULL.
  failed := false;
  BEGIN
    SELECT INTO retval * FROM test.assert(null, 'null should choke');
  EXCEPTION WHEN OTHERS THEN
    failed := true;
    IF SQLERRM = 'Assertion test may not be NULL.' THEN
      NULL;
    ELSE
      RAISE EXCEPTION 'test.assert(null) did not raise the correct exception.';
    END IF;
  END;
  IF NOT failed THEN
    RAISE EXCEPTION 'test.assert(null) did not fail.';
  END IF;
  
  RAISE EXCEPTION '[OK]';
END;
$$ LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION test.test_assert_equal() RETURNS VOID AS $$
-- Assert the correct operation of test.assert_equal
-- module: test_asserts
DECLARE
  retval    text;
  dummy     int;
  failed    bool;
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
  failed := false;
  BEGIN
    SELECT INTO retval * FROM test.assert_equal(1, 2);
  EXCEPTION WHEN OTHERS THEN
    failed := true;
    IF SQLERRM = '1 != 2' THEN
      NULL;
    ELSE
      RAISE EXCEPTION 'test.assert_equal() did not raise the correct message on failure. Expected ''1 != 2'', received ''%''', SQLERRM;
    END IF;
  END;
  IF NOT failed THEN
    RAISE EXCEPTION 'test.assert_equal(1, 2) did not fail.';
  END IF;
  
  failed := false;
  BEGIN
    SELECT INTO retval * FROM test.assert_equal('abc'::text, 'xyz');
  EXCEPTION WHEN OTHERS THEN
    failed := true;
    IF SQLERRM = 'abc != xyz' THEN
      NULL;
    ELSE
      RAISE EXCEPTION 'test.assert_equal() did not raise the correct message on failure. Expected: ''abc != xyz'', received: %', SQLERRM;
    END IF;
  END;
  IF NOT failed THEN
    RAISE EXCEPTION 'test.assert(abc, xyz) did not fail.';
  END IF;
  
  -- assert_equal() MUST return VOID if both args are null.
  IF dummy IS NOT NULL THEN
    RAISE EXCEPTION 'dummy must be NULL for this test to be valid.';
  END IF;
  SELECT INTO retval * FROM test.assert_equal(dummy, dummy);
  IF retval != '' THEN
    RAISE EXCEPTION 'test.assert_equal() did not return void for (null, null).';
  END IF;
  
  -- assert_equal() MUST raise an exception if only one arg is NULL.
  failed := false;
  BEGIN
    SELECT INTO retval * FROM test.assert_equal(8, dummy);
  EXCEPTION WHEN OTHERS THEN
    failed := true;
    IF SQLERRM = '8 != <NULL>' THEN
      NULL;
    ELSE
      RAISE EXCEPTION 'test.assert_equal() did not raise the correct message on failure. Expected ''8 != <NULL>'', received ''%''', SQLERRM;
    END IF;
  END;
  IF NOT failed THEN
    RAISE EXCEPTION 'test.assert_equal(8, null) did not fail.';
  END IF;
  
  failed := false;
  BEGIN
    SELECT INTO retval * FROM test.assert_equal(dummy, 7);
  EXCEPTION WHEN OTHERS THEN
    failed := true;
    IF SQLERRM = '<NULL> != 7' THEN
      NULL;
    ELSE
      RAISE EXCEPTION 'test.assert_equal() did not raise the correct message on failure. Expected ''<NULL> != 7'', received ''%''', SQLERRM;
    END IF;
  END;
  IF NOT failed THEN
    RAISE EXCEPTION 'test.assert_equal(null, 7) did not fail.';
  END IF;
  
  -- assert_equal() will raise an undefined_function exception if the args have different types.
  -- It would be nice to find a way around this (without writing M x N overloaded funcs).
  failed := false;
  BEGIN
    SELECT INTO retval * FROM test.assert_equal(8, 'abc'::text);
  EXCEPTION WHEN undefined_function THEN
    failed := true;
  END;
  IF NOT failed THEN
    RAISE EXCEPTION 'test.assert_equal(8, abc) did not fail.';
  END IF;
  
  RAISE EXCEPTION '[OK]';
END;
$$ LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION test.test_assert_not_equal() RETURNS VOID AS $$
-- Assert the correct operation of test.assert_not_equal
-- module: test_asserts
DECLARE
  retval    text;
  dummy     int;
  failed    bool;
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
  failed := false;
  BEGIN
    SELECT INTO retval * FROM test.assert_not_equal(1, 1);
  EXCEPTION WHEN OTHERS THEN
    failed := true;
    IF SQLERRM = '1 = 1' THEN
      NULL;
    ELSE
      RAISE EXCEPTION 'test.assert_not_equal() did not raise the correct message on failure. Expected ''1 = 1'', received ''%''', SQLERRM;
    END IF;
  END;
  IF NOT failed THEN
    RAISE EXCEPTION 'test.assert_not_equal(1, 1) did not fail.';
  END IF;
  
  failed := false;
  BEGIN
    SELECT INTO retval * FROM test.assert_not_equal('abc'::text, 'abc');
  EXCEPTION WHEN OTHERS THEN
    failed := true;
    IF SQLERRM = 'abc = abc' THEN
      NULL;
    ELSE
      RAISE EXCEPTION 'test.assert_not_equal() did not raise the correct message on failure. Expected: ''abc = abc'', received: %', SQLERRM;
    END IF;
  END;
  IF NOT failed THEN
    RAISE EXCEPTION 'test.assert_not_equal(abc, abc) did not fail.';
  END IF;
  
  -- assert_not_equal() MUST return VOID if only one arg is NULL.
  IF dummy IS NOT NULL THEN
    RAISE EXCEPTION 'dummy must be NULL for this test to be valid.';
  END IF;
  SELECT INTO retval * FROM test.assert_not_equal(8, dummy);
  IF retval != '' THEN
    RAISE EXCEPTION 'test.assert_not_equal() did not return void for (8, null).';
  END IF;
  SELECT INTO retval * FROM test.assert_not_equal(dummy, 7);
  IF retval != '' THEN
    RAISE EXCEPTION 'test.assert_not_equal() did not return void for (null, 7).';
  END IF;
  
  -- assert_not_equal() MUST raise an exception if both args are NULL.
  failed := false;
  BEGIN
    SELECT INTO retval * FROM test.assert_not_equal(dummy, dummy);
  EXCEPTION WHEN OTHERS THEN
    failed := true;
    IF SQLERRM = '<NULL> = <NULL>' THEN
      NULL;
    ELSE
      RAISE EXCEPTION 'test.assert_not_equal() did not raise the correct message on failure. Expected ''<NULL> = <NULL>'', received ''%''', SQLERRM;
    END IF;
  END;
  IF NOT failed THEN
    RAISE EXCEPTION 'test.assert_not_equal(null, null) did not fail.';
  END IF;
  
  -- assert_not_equal() will raise an undefined_function exception if the args have different types.
  -- It would be nice to find a way around this (without writing M x N overloaded funcs).
  failed := false;
  BEGIN
    SELECT INTO retval * FROM test.assert_not_equal(8, 'abc'::text);
  EXCEPTION WHEN undefined_function THEN
    failed := true;
  END;
  IF NOT failed THEN
    RAISE EXCEPTION 'test.assert_not_equal(8, abc) did not fail.';
  END IF;
  
  RAISE EXCEPTION '[OK]';
END;
$$ LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION test.test_assert_less_than_or_equal() RETURNS VOID AS $$
-- Assert the correct operation of test.assert_less_than_or_equal
-- module: test_asserts
DECLARE
  retval    text;
  dummy     int;
  failed    bool;
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
  failed := false;
  BEGIN
    SELECT INTO retval * FROM test.assert_less_than_or_equal(2, 1);
  EXCEPTION WHEN OTHERS THEN
    failed := true;
    IF SQLERRM = '2 not <= 1' THEN
      NULL;
    ELSE
      RAISE EXCEPTION 'test.assert_less_than_or_equal() did not raise the correct message on failure. Expected ''2 not <= 1'', received ''%''', SQLERRM;
    END IF;
  END;
  IF NOT failed THEN
    RAISE EXCEPTION 'test.assert_less_than_or_equal(2, 1) did not fail.';
  END IF;
  
  failed := false;
  BEGIN
    SELECT INTO retval * FROM test.assert_less_than_or_equal('xyz'::text, 'abc');
  EXCEPTION WHEN OTHERS THEN
    failed := true;
    IF SQLERRM = 'xyz not <= abc' THEN
      NULL;
    ELSE
      RAISE EXCEPTION 'test.assert_less_than_or_equal() did not raise the correct message on failure. Expected: ''xyz not <= abc'', received: %', SQLERRM;
    END IF;
  END;
  IF NOT failed THEN
    RAISE EXCEPTION 'test.assert_less_than_or_equal(xyz, abc) did not fail.';
  END IF;
  
  -- assert_less_than_or_equal() MUST raise an exception if either arg is NULL.
  failed := false;
  BEGIN
    SELECT INTO retval * FROM test.assert_less_than_or_equal(dummy, dummy);
  EXCEPTION WHEN OTHERS THEN
    failed := true;
    IF SQLERRM = 'Assertion arguments may not be NULL.' THEN
      NULL;
    ELSE
      RAISE EXCEPTION 'test.assert_less_than_or_equal() did not raise the correct message on failure. Expected ''Assertion arguments may not be NULL.'', received ''%''', SQLERRM;
    END IF;
  END;
  IF NOT failed THEN
    RAISE EXCEPTION 'test.assert_less_than_or_equal(null, null) did not fail.';
  END IF;
  
  -- assert_less_than_or_equal() will raise an undefined_function exception if the args have different types.
  -- It would be nice to find a way around this (without writing M x N overloaded funcs).
  failed := false;
  BEGIN
    SELECT INTO retval * FROM test.assert_less_than_or_equal(8, 'abc'::text);
  EXCEPTION WHEN undefined_function THEN
    failed := true;
  END;
  IF NOT failed THEN
    RAISE EXCEPTION 'test.assert_less_than_or_equal(8, abc) did not fail.';
  END IF;
  
  RAISE EXCEPTION '[OK]';
END;
$$ LANGUAGE plpgsql;
