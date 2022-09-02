(in-package :usufslc.config)

;; Trim whitespace, comments, and return only non-empty lines
(defun trim-lines (lines &key (comment-char #\#) (whitespace-chars '(#\Space #\Newline #\Tab)))
  (remove-if
   (lambda (s) (or (null s)
                   (equal "" s)))
   (mapcar (lambda (s)
             (let* ((non-comment (subseq s 0 (position comment-char s)))
                    (trimmed (and non-comment
                                  (string-trim whitespace-chars non-comment))))
               trimmed))
           lines)))

;; Returns a-list of section names . property strings (in-order)
(defun sections (lines &optional (seperator-regex "^\\[[\\w-]+\\]$"))
  (let ((sections nil)
        (current-section nil)
        (current-section-prop-strings nil))
    (flet ((add-current-section ()
             (setf sections (cons `(,current-section . ,(reverse current-section-prop-strings)) sections))))
      (loop for line in lines
            if (cl-ppcre:scan seperator-regex line)
              do
                 (let ((section (subseq line 1 (1- (length line)))))
                   (if current-section
                       (add-current-section))
                   (setf current-section section
                         current-section-prop-strings '()))
            else
              do
                 (setf current-section-prop-strings (cons line current-section-prop-strings))
            finally
               (return (reverse (add-current-section)))))))

(defun make-property-map (prop-strings)
  (let ((property-map (make-hash-table)))
    (loop for prop-string in prop-strings
          do (let* ((name-val-tuple (register-groups-bind (prop-name val)
                                        ("^:([\\w-]+) ?([^#\\n]+)?(?!#)|$" prop-string)
                                      (list prop-name val)))
                    (keyword (car name-val-tuple))
                    (val (cadr name-val-tuple)))
               (if keyword
                   (setf (gethash (make-keyword keyword) property-map) val))))
    property-map))

(defun make-config-map (sections)
  (let ((section-map (make-hash-table)))
    (loop for section in sections
          do (let ((section-name (make-keyword (car section))))
               (setf (gethash section-name section-map) (make-property-map (cdr section)))))
    section-map))

(defun parse-config (file-lines)
  (make-config-map (sections (trim-lines file-lines))))
