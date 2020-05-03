(in-package :mgl-mat)

;;;; Register in PAX World

(defun pax-sections ()
  (list @mat-manual @cube-manual))
(defun pax-pages ()
  `((:objects
     (, @mat-manual),
     :source-uri-fn ,(make-github-source-uri-fn
                      :mgl-mat
                      "https://github.com/melisgl/mgl-mat"))
    (:objects
     (, @cube-manual)
     :source-uri-fn ,(make-github-source-uri-fn
                      :mgl-mat
                      "https://github.com/melisgl/mgl-mat"))))
(register-doc-in-pax-world :mgl-mat (pax-sections) (pax-pages))

#+nil
(progn
  (update-asdf-system-readmes (pax-sections) :mgl-mat)
  (update-asdf-system-html-docs (pax-sections) :mgl-mat
                                :pages (pax-pages)))
