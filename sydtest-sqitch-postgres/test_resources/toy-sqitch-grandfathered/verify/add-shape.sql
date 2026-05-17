-- Verify toy-grandfathered:add-shape on pg
BEGIN;

SELECT shape FROM widget WHERE FALSE;

ROLLBACK;
