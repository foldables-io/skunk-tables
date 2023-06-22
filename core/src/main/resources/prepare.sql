-- For TableSuite
CREATE TABLE IF NOT EXISTS  persons (
  id INT8 NOT NULL,
  first_name VARCHAR NOT NULL,
  age INT NOT NULL
);

CREATE TABLE IF NOT EXISTS persons_with_meta (
  id            SERIAL          NOT NULL,
  created_at    TIMESTAMP       DEFAULT current_timestamp,
  first_name    VARCHAR         NOT NULL,
  age           INT             NOT NULL
);
