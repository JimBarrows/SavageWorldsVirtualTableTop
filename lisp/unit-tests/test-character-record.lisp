(in-package :unit-tests)

(def-suite unit-test-suite :description "All Unit tests")

(in-suite unit-test-suite)

(test fighting-value-with-fighting-skill
  (let ((record (make-instance savage-worlds::'character-record)))
    (push (savage-worlds::fighting) (savage-worlds::skills record))
    (is (= 4
	   (savage-worlds::fighting-value record)))))

(test fighting-value-without-fighting-skill
  (let ((record (make-instance savage-worlds::'character-record)))
    (is (= 0
	   (savage-worlds::fighting-value record)))))

(test fighting-modifier-with-fighting-skill
  (let ((record (make-instance savage-worlds::'character-record))
	(fight-skill (savage-worlds::fighting (make-instance savage-worlds::'trait-rank :rank 'd12 :value 12 :modifier 5))))
    (push fight-skill (savage-worlds::skills record))
    (is (= 5
	   (savage-worlds::fighting-modifier record)))))

(test fighting-modifier-without-fighting-skill
  (let ((record (make-instance savage-worlds::'character-record)))
    (is (= 0
	   (savage-worlds::fighting-modifier record)))))

(test shield-bonus-is-0-with-no-shield
  (let ((record (make-instance savage-worlds::'character-record)))
    (is (= 0
	   (savage-worlds::shield-bonus record)))))
    
(test shield-bonus-is-1-with-small-shield
  (let ((record (make-instance savage-worlds::'character-record)))
    (savage-worlds::add-gear savage-worlds::small-shield record)
    (is (= 1
	   (savage-worlds::shield-bonus record)))))

(test armor-bonus-is-0-with-no-armor
  (let ((record (make-instance savage-worlds::'character-record)))
    (is (= 0
	   (savage-worlds::armor-bonus record)))))
    
(test armor-bonus-is-1-with-leather-armor
  (let ((record (make-instance savage-worlds::'character-record)))
    (savage-worlds::add-gear savage-worlds::leather-armor record)
    (is (= 1
	   (savage-worlds::armor-bonus record)))))

(test parry-is-2-with-no-fighting-skill-and-no-shield
  (let ((record (make-instance savage-worlds::'character-record)))
    (is (= 2
	   (savage-worlds::parry record)))))

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
