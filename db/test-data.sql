INSERT INTO boards(id, name, bg_colour)
VALUES
  ('B00000001', 'Test board', '#EEEEEE');

INSERT INTO board_labels(id, board_id, name, colour)
VALUES
  ('T00000001', 'B00000001', 'label-1', '#FF0000'),
  ('T00000002', 'B00000001', 'label-2', '#00FF00'),
  ('T00000003', 'B00000001', 'label-3', '#0000FF');

INSERT INTO lists(id, board_id, name)
VALUES
  ('L00000001', 'B00000001', 'To Do'),
  ('L00000002', 'B00000001', 'In Progress'),
  ('L00000003', 'B00000001', 'Done');

INSERT INTO cards(id, list_id, idx, title)
VALUES
  ('C00000001', 'L00000001', 1, 'Task #1'),
  ('C00000002', 'L00000001', 2, 'Task #2'),
  ('C00000003', 'L00000002', 1, 'Task #3'),
  ('C00000004', 'L00000002', 2, 'Task #4'),
  ('C00000005', 'L00000002', 3, 'Task #5'),
  ('C00000006', 'L00000003', 1, 'Task #6'),
  ('C00000007', 'L00000003', 2, 'Task #7'),
  ('C00000008', 'L00000003', 3, 'Task #8'),
  ('C00000009', 'L00000003', 4, 'Task #9'),
  ('C00000010', 'L00000003', 5, 'Task #10');

INSERT INTO card_labels(card_id, label_id)
VALUES
  ('C00000001', 'T00000001'),
  ('C00000002', 'T00000002'),
  ('C00000003', 'T00000001'),
  ('C00000003', 'T00000002'),
  ('C00000007', 'T00000001'),
  ('C00000007', 'T00000002'),
  ('C00000007', 'T00000003');

INSERT INTO checklist_items(card_id, idx, name, done)
VALUES
  ('C00000005', 1, 'To do 1', true),
  ('C00000005', 2, 'To do 2', true),
  ('C00000005', 3, 'To do 3', false),
  ('C00000005', 4, 'To do 4', false),
  ('C00000005', 5, 'To do 5', false);
