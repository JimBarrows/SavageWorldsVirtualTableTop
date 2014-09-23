(in-package :unit-tests)

(def-suite base-record-suite :description "Tests of the base record class.")

(in-suite base-record-suite)

(test fighting-value-with-fighting-skill
  (let ((record (make-instance savage-worlds::'base-record)))
    (push (savage-worlds::fighting) (savage-worlds::skills record))
    (is (= 4
	   (savage-worlds::fighting-value record)))))

(test fighting-value-without-fighting-skill
  (let ((record (make-instance savage-worlds::'base-record)))
    (is (= 0
	   (savage-worlds::fighting-value record)))))

(test fighting-modifier-with-fighting-skill
  (let ((record (make-instance savage-worlds::'base-record))
	(fight-skill (savage-worlds::fighting (make-instance savage-worlds::'trait-rank :rank 'd12 :value 12 :modifier 5))))
    (push fight-skill (savage-worlds::skills record))
    (is (= 5
	   (savage-worlds::fighting-modifier record)))))

(test fighting-modifier-without-fighting-skill
  (let ((record (make-instance savage-worlds::'base-record)))
    (is (= 0
	   (savage-worlds::fighting-modifier record)))))

(test shield-bonus-is-0-with-no-shield
  (let ((record (make-instance savage-worlds::'base-record)))
    (is (= 0
	   (savage-worlds::shield-bonus record)))))
    
(test shield-bonus-is-1-with-small-shield
  (let ((record (make-instance savage-worlds::'base-record)))
    (savage-worlds::add-gear savage-worlds::small-shield record)
    (is (= 1
	   (savage-worlds::shield-bonus record)))))

(test armor-bonus-is-0-with-no-armor
  (let ((record (make-instance savage-worlds::'base-record)))
    (is (= 0
	   (savage-worlds::armor-bonus record)))))
    
(test armor-bonus-is-1-with-leather-armor
  (let ((record (make-instance savage-worlds::'base-record)))
    (savage-worlds::add-gear savage-worlds::leather-armor record)
    (is (= 1
	   (savage-worlds::armor-bonus record)))))

(test parry-is-2-with-no-fighting-skill-and-no-shield
  (let ((record (make-instance savage-worlds::'base-record)))
    (is (= 2
	   (savage-worlds::parry record)))))

