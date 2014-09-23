(in-package :unit-tests)

(def-suite character-record-suite :description "Character Record unit tests")

(in-suite character-record-suite)

(test charisma-is-zero-with-no-edges-and-hindrances
  (let ((record (make-instance savage-worlds::'character-record)))
    (is (= 0
	   (savage-worlds::charisma record)))))

(test charisma-is-2-with-1-edge-giving-bonus
  (let ((record (make-instance savage-worlds::'character-record)))
    (savage-worlds::add-edge savage-worlds::attractive record)
    (is (= 2
	   (savage-worlds::charisma record)))))

(test charisma-is-6-with-2-edges-giving-bonus
  (let ((record (make-instance savage-worlds::'character-record)))
    (savage-worlds::add-edge savage-worlds::attractive record)
    (savage-worlds::add-edge savage-worlds::very-attractive record)
    (is (= 6
	   (savage-worlds::charisma record)))))

(test charisma-is-negative-4-with-1-hindrance-giving-bonus
  (let ((record (make-instance savage-worlds::'character-record)))
    (savage-worlds::add-hindrance savage-worlds::bloodthirsty record)
    (is (= -4
	   (savage-worlds::charisma record)))))

(test charisma-is-negative-8-with-2-hindrances-giving-bonus
  (let ((record (make-instance savage-worlds::'character-record)))
    (savage-worlds::add-hindrance savage-worlds::bloodthirsty record)
    (savage-worlds::add-hindrance savage-worlds::bloodthirsty record)
    (is (= -8
	   (savage-worlds::charisma record)))))
