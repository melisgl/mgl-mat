(in-package :mgl-mat)

;;;; Generating own docs

(defun update-readmes ()
  (with-open-file (stream (asdf:system-relative-pathname :mgl-mat "README.md")
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :supersede)
    (document (list @mat-manual @cube-manual) :stream stream))
  (with-open-file (stream (asdf:system-relative-pathname :mgl-mat "README")
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :supersede)
    (describe @mat-manual stream)
    (describe @cube-manual stream)))

(defun update-html ()
  (document (list mgl-cube:@cube-manual @mat-manual)
            :pages (loop for section in (list @cube-manual @mat-manual)
                         collect (section-to-page-spec section))
            :format :html))

(defun section-to-filename (section)
  (asdf:system-relative-pathname
   :mgl-mat
   (format nil "doc/~a.html"
           (string-downcase (subseq (symbol-name (section-name section)) 1)))))

(defun section-to-page-spec (section &key
                             (open-args '(:if-does-not-exist :create
                                          :if-exists :supersede)))
  (flet ((header (title stream)
           (html-header stream :title title
                        :stylesheet "style.css" :charset "UTF-8"))
         (footer (stream)
           (html-footer stream)))
    `(:objects
      (,section)
      :output (,(section-to-filename section) ,@open-args)
      :header-fn ,(lambda (stream)
                    (header (section-title section) stream))
      :footer-fn ,#'footer)))

(defun html-header (stream &key title stylesheet (charset "UTF-8"))
  (format stream
          "<!DOCTYPE html>~%~
          <html xmlns='http://www.w3.org/1999/xhtml' xml:lang='en' lang='en'>~%~
          <head>~%~
          ~@[<title>~A</title>~]~%~
          ~@[<link type='text/css' href='~A' rel='stylesheet'/>~]~%~
          ~@[<meta http-equiv=\"Content-Type\" ~
                   content=\"text/html; ~
                   charset=~A\"/>~]~%~
          </head>~%~
          <body>"
          title stylesheet charset))

(defun html-footer (stream)
  (format stream "</body>~%</html>~%"))

#|

(update-readmes)

(update-html)

|#
