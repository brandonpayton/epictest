-- Tests for the 'global' function which Epic provides.
-- To run, execute epic.sql, then this script, then test.run_module('test_globals').

CREATE OR REPLACE FUNCTION test._test_global() RETURNS VOID AS $$
DECLARE
  rec    record;
  trec   record;
BEGIN
  rec := global('pg_namespace WHERE nspname = ''test'';');
  
  -- The result of global() should be a normal record.
  PERFORM test.assert_equal(rec.nspname, 'test');
  
  -- The returned record MUST possess a .tablename attribute.
  PERFORM test.assert(rec.tablename LIKE E'\_global\_%', rec.tablename || ' not like _global');
  
  -- The tablename MUST reference a temporary table with the same fields.
  EXECUTE 'SELECT * FROM ' || rec.tablename INTO trec;
  PERFORM test.assert_equal(trec.nspname, 'test');
  PERFORM test.assert_equal(trec.nspowner, rec.nspowner);
  PERFORM test.assert_equal(trec.nspacl, rec.nspacl);
  
  RAISE EXCEPTION '%', rec.tablename;
END;
$$ LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION test.test_global() RETURNS VOID AS $$
-- module: test_globals
DECLARE
  rec    record;
BEGIN
  BEGIN
    PERFORM test._test_global();
  EXCEPTION WHEN raise_exception THEN
    -- The table, since temporary, MUST be DROP'ed on rollback.
    BEGIN
      EXECUTE 'SELECT * FROM ' || SQLERRM INTO rec;
    EXCEPTION WHEN undefined_table THEN
      NULL;
    END;
  END;
  
  PERFORM test.pass();
END;
$$ LANGUAGE plpgsql;