-- Revert toy-hardcoded-public:init from pg
BEGIN;

DROP TABLE IF EXISTS widget;

COMMIT;
