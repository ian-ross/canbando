INSERT INTO boards(id, name, bg_colour)
VALUES
  ('BT0000001', 'Test board', '#EEEEEE');

INSERT INTO board_labels(id, board_id, name, colour)
VALUES
  ('TT0000001', 'BT0000001', 'label-1', '#FF0000'),
  ('TT0000002', 'BT0000001', 'label-2', '#00FF00'),
  ('TT0000003', 'BT0000001', 'label-3', '#0000FF');

INSERT INTO lists(id, board_id, idx, name)
VALUES
  ('LT0000001', 'BT0000001', 1, 'To Do'),
  ('LT0000002', 'BT0000001', 2, 'In Progress'),
  ('LT0000003', 'BT0000001', 3, 'Done');

INSERT INTO cards(id, list_id, idx, title)
VALUES
  ('CT0000001', 'LT0000001', 1, 'Task #1'),
  ('CT0000002', 'LT0000001', 2, 'Task #2'),
  ('CT0000003', 'LT0000002', 1, 'Task #3'),
  ('CT0000004', 'LT0000002', 2, 'Task #4'),
  ('CT0000005', 'LT0000002', 3, 'Task #5'),
  ('CT0000006', 'LT0000003', 1, 'Task #6'),
  ('CT0000007', 'LT0000003', 2, 'Task #7'),
  ('CT0000008', 'LT0000003', 3, 'Task #8'),
  ('CT0000009', 'LT0000003', 4, 'Task #9'),
  ('CT0000010', 'LT0000003', 5, 'Task #10');

INSERT INTO card_labels(card_id, label_id)
VALUES
  ('CT0000001', 'TT0000001'),
  ('CT0000002', 'TT0000002'),
  ('CT0000003', 'TT0000001'),
  ('CT0000003', 'TT0000002'),
  ('CT0000007', 'TT0000001'),
  ('CT0000007', 'TT0000002'),
  ('CT0000007', 'TT0000003');

INSERT INTO checklist_items(card_id, idx, name, done)
VALUES
  ('CT0000005', 1, 'To do 1', true),
  ('CT0000005', 2, 'To do 2', true),
  ('CT0000005', 3, 'To do 3', false),
  ('CT0000005', 4, 'To do 4', false),
  ('CT0000005', 5, 'To do 5', false);
