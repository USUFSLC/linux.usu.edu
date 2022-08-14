/* Trigger to automatically set the updated_at timestamp on update */
CREATE OR REPLACE FUNCTION trigger_set_timestamp()
RETURNS TRIGGER AS $$
BEGIN
  NEW.updated_at = NOW();
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TABLE IF NOT EXISTS app_user (
  id             SERIAL PRIMARY KEY,
  username       VARCHAR(255) NOT NULL,
  discord_id     VARCHAR(255) NOT NULL,
  discord_tag    VARCHAR(255) NOT NULL,
  created_at     TIMESTAMP NOT NULL DEFAULT NOW(),
  updated_at     TIMESTAMP NOT NULL DEFAULT NOW()
);

/* Set updated_at on update to app_user */
CREATE TRIGGER set_timestamp BEFORE UPDATE ON app_user FOR EACH ROW 
EXECUTE PROCEDURE trigger_set_timestamp();