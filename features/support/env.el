(require 'f)

(defvar commify-support-path
  (f-dirname load-file-name))

(defvar commify-features-path
  (f-parent commify-support-path))

(defvar commify-root-path
  (f-parent commify-features-path))

(add-to-list 'load-path commify-root-path)

;; Ensure that we don't load old byte-compiled versions
(let ((load-prefer-newer t))
  (require 'commify)
  (require 'espuds)
  (require 'ert))

(Setup
 (setq commify-hex-enable t
       commify-oct-enable t
       commify-bin-enable t)
 ;; Before anything has run
 )

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
