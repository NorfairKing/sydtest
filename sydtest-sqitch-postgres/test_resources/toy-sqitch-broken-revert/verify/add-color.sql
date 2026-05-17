-- Verify toy-broken-revert:add-color on pg
BEGIN;

SELECT color, shape FROM widget WHERE FALSE;

ROLLBACK;
