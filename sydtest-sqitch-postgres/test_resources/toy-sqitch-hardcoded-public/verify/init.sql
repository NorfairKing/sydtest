-- Verify toy-hardcoded-public:init on pg
BEGIN;

-- Intentional anti-pattern: this verify hardcodes the schema. When the
-- harness deploys into a non-public schema, `widget` is not in `public`,
-- so this RAISEs and the deploy --verify fails. The portable form is
-- `table_schema = current_schema()`.
DO $$
BEGIN
    IF NOT EXISTS (
        SELECT 1 FROM information_schema.columns
        WHERE table_schema = 'public' AND table_name = 'widget' AND column_name = 'name'
    ) THEN
        RAISE EXCEPTION 'widget.name is not in the public schema';
    END IF;
END $$;

ROLLBACK;
