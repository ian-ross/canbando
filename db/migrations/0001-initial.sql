CREATE SEQUENCE IF NOT EXISTS idgen;

CREATE TABLE boards (
  id         VARCHAR(32)  PRIMARY KEY,
  name       TEXT         NOT NULL,
  bg_colour  TEXT
);

CREATE TABLE board_labels (
  id        VARCHAR(32)  PRIMARY KEY,
  board_id  VARCHAR(32)  REFERENCES boards(id) ON DELETE CASCADE,
  name      TEXT         NOT NULL,
  colour    TEXT
);

CREATE TABLE lists (
  id        VARCHAR(32)  PRIMARY KEY,
  board_id  VARCHAR(32)  REFERENCES boards(id) ON DELETE CASCADE,
  name      TEXT         NOT NULL,
  idx       INT          NOT NULL
);

CREATE TABLE cards (
  id       VARCHAR(32)  PRIMARY KEY,
  list_id  VARCHAR(32)  REFERENCES lists(id) ON DELETE CASCADE,
  title    TEXT,
  idx      INT          NOT NULL
);

CREATE TABLE card_labels (
  id        INT          PRIMARY KEY AUTO_INCREMENT,
  card_id   VARCHAR(32)  REFERENCES cards(id) ON DELETE CASCADE,
  label_id  VARCHAR(32)  REFERENCES board_labels(id) ON DELETE CASCADE
);

CREATE TABLE checklist_items (
  id        INT          PRIMARY KEY AUTO_INCREMENT,
  card_id   VARCHAR(32)  REFERENCES cards(id) ON DELETE CASCADE,
  idx       INT          NOT NULL,
  name      TEXT,
  done      BOOLEAN      NOT NULL
);
