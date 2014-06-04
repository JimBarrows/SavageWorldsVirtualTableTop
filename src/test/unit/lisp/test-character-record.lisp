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
