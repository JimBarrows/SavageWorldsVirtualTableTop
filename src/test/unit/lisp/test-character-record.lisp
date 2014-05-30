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
