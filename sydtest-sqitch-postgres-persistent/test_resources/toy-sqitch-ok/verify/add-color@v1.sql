-- Verify toy-ok:add-color@v1 on pg
BEGIN;

SELECT color FROM widget WHERE FALSE;

ROLLBACK;
