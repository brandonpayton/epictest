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
  
  -- The returned record MUST possess a .__name__ attribute.
  PERFORM test.assert(rec.__name__ LIKE E'\_global\_%', rec.__name__ || ' not like _global');
  
  -- The .__name__ MUST reference a temporary table with the same fields.
  EXECUTE 'SELECT * FROM ' || rec.__name__ INTO trec;
  PERFORM test.assert_equal(trec.nspname, 'test');
  PERFORM test.assert_equal(trec.nspowner, rec.nspowner);
  PERFORM test.assert_equal(trec.nspacl, rec.nspacl);
  
  -- The returned record MUST possess a .__create__ attribute.
  PERFORM test.assert_equal(rec.__create__, 'SELECT * FROM pg_namespace WHERE nspname = ''test''');
  
  -- The returned record MUST possess a .__record__ attribute.
  PERFORM test.assert_equal(rec.__record__,
    'SELECT * FROM _global_record(''' || rec.__name__ || ''', ''' || rec.__create__ || ''')');
  
  -- The returned record MUST possess an .__iter__ attribute.
  PERFORM test.assert_equal(rec.__iter__, 'SELECT * FROM ' || rec.__name__);
  PERFORM test.assert_not_empty(rec.__iter__);
  
  -- The returned record MUST possess an .__attributes__ attribute.
  PERFORM test.assert_equal(rec.__attributes__, 'SELECT attname FROM test.attributes(''' || rec.__name__ || ''')');
  PERFORM test.assert_column(rec.__attributes__, ARRAY['nspname', 'nspowner', 'nspacl']);
  
  -- The returned record MUST possess a .__len__ attribute.
  PERFORM test.assert_equal(rec.__len__, 1);
  
  -- Raise an exception to test deletion of the TEMP table ON COMMIT
  RAISE EXCEPTION '%', rec.__name__;
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