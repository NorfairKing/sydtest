-- Verify toy-index-drift:init on pg
BEGIN;

SELECT id, name, color FROM widget WHERE FALSE;

ROLLBACK;
