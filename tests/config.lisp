(in-package :cl-user)
(defpackage usufslc/tests.config
  (:use :cl
        :fiveam
        :usufslc.config
        :usufslc/tests)
  (:export :config-suite))
(in-package :usufslc/tests.config)

(def-suite config-suite
  :description "Unit tests to test the config parser"
  :in usufslc-test-suite)
(in-suite config-suite)

(test trim-lines
  :description "Test that parser ignores everything after comment character, and strips leading and trailing whitespace, and removes blank lines"
  (is (equal (usufslc.config::trim-lines '("wow # comment" "  wow" "  wow   #comment"
                           "#comment" "   "))
             '("wow" "wow" "wow"))))

(test build-sections
  :description "Test that parser builds sections from a list of lines, with the sections not necessarily in order, but the properties are"
  (is (equal (usufslc.config::sections '("[section]" "asdf"))
             '(("section" . ("asdf")))))
  (let* ((sections (usufslc.config::sections '("[section]" "asdf" "fdsa" "asdf" "[section2]" "asdf2")))
         (section (assoc "section" sections :test 'equal))
         (section2 (assoc "section2" sections :test 'equal)))
      (is (equal section '("section" . ("asdf" "fdsa" "asdf"))))
      (is (equal section2 '("section2" . ("asdf2"))))))

(test property-map-builder
  :description "Test that parser builds property map correctly from lines of property-strings"
  (let* ((property-strings
           '(":property1 leopard bear"
             ":bruh-moment bruh"
             "no-property-and-skipped"))
         (property-map (usufslc.config::make-property-map property-strings)))
    (is (= (hash-table-count property-map) 2))
    (is (equal (gethash :|property1| property-map) "leopard bear"))
    (is (equal (gethash :|bruh-moment| property-map) "bruh"))))

(test get-config-value
  :description "Test that parser gets config value correctly"
  (let ((config-map (let ((config (make-hash-table)))
                      (setf (gethash :|section| config)
                            (let ((section (make-hash-table)))
                              (setf (gethash :|property1| section) "value1")
                              (setf (gethash :|property2| section) "nil")
                              section)

                            (gethash :|section2| config)
                            (make-hash-table))
                      config)))
    (is (equal (get-config-value :|section| :|property1| config-map) "value1"))
    (is (not (get-config-value :|section| :|property2| config-map)))
    (is (not (get-config-value :|section| :|property3| config-map)))
    (is (not (get-config-value :|nonexistantsection| :|property1| config-map)))))
                                   
(test config-file-parse
  :description "Test that parser parses config file correctly; the ultimate test"
  (let* ((config-file-contents
           '("[section] # comment"
             "  :property1 leopard bear   "
             "    :bruh-moment bruh"
             "   "
             "  no-property-and-skipped"
             "[section2]"
             "  :property2 leopard bear"
             "# :bruh-moment bruh"
             "#[commented-section]"
             "  :property3 leopard bear"))
          (config-map (usufslc.config::parse-config config-file-contents)))
    (is (equal (get-config-value :|section| :|property1| config-map) "leopard bear"))
    (is (not (get-config-value :|section| :|property2| config-map)))
    (is (equal (get-config-value :|section| :|bruh-moment| config-map) "bruh"))
    (is (equal (get-config-value :|section2| :|property2| config-map) "leopard bear"))
    (is (equal (get-config-value :|section2| :|property3| config-map) "leopard bear"))
    (is (not (get-config-value :|section2| :|bruh-moment| config-map)))
    (is (not (get-config-value :|commented-section| :|property3| config-map)))))
    
