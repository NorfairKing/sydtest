-- Verify toy-grandfathered:add-color on pg
BEGIN;

SELECT color FROM widget WHERE FALSE;

ROLLBACK;
