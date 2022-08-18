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

;; Returns a list of sections with name, specified in config by [name], as first element and
;; all lines of the section as the second
(defun sections (lines &optional (section-list '()) (current-section "") (current-section-list '()))
  (if (not lines)
      (if (zerop (length current-section))
          section-list
          (cons (list current-section current-section-list) section-list))
      (let* ((line (car lines))
             (linelen (length line)))
        (cond
          ((zerop linelen)
           (sections (cdr lines) section-list current-section current-section-list))
          ((scan "^\\[[\\w-]+\\]$" line)
           (sections (cdr lines) (unless (zerop (length current-section))
                                   (cons (list current-section current-section-list) section-list))
                     (subseq line 1 (1- linelen))))
          (t
           (sections (cdr lines) section-list current-section (append current-section-list (list line))))))))

(defun make-property-map (prop-strings)
  (let ((map (make-hash-table)))
    (loop for prop-string in prop-strings
          do (let* ((name-val-tuple (register-groups-bind (prop-name val)
                                        ("^:([\\w-]+) ?([^#\\n]+)?(?!#)|$" prop-string)
                                      (list prop-name val)))
                    (keyword (car name-val-tuple))
                    (val (cadr name-val-tuple)))
               (if keyword
                   (setf (gethash (make-keyword keyword) map) val))))
    map))

(defun make-config-map (sections)
  (let ((section-map (make-hash-table)))
    (loop for section in sections
          do (let ((section-name (make-keyword (car section))))
               (setf (gethash section-name section-map) (make-property-map (cadr section)))))
    section-map))

(defun parse-config (file-lines)
  (make-config-map (sections (trim-lines file-lines))))
