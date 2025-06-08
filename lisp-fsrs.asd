(defsystem lisp-fsrs
  :version "6.0"
  :author "Bohong Huang <bohonghuang@qq.com>"
  :maintainer "Bohong Huang <bohonghuang@qq.com>"
  :license "MIT"
  :homepage "https://github.com/open-spaced-repetition/lisp-fsrs"
  :bug-tracker "https://github.com/open-spaced-repetition/lisp-fsrs/issues"
  :source-control (:git "https://github.com/open-spaced-repetition/lisp-fsrs.git")
  :depends-on (#:alexandria #:local-time)
  :components ((:file "package"))
  :in-order-to ((test-op (test-op #:lisp-fsrs/test))))

(defsystem lisp-fsrs/emacs
  :depends-on (#:asdf #:uiop #:alexandria #:local-time #:slynk #:introspect-environment #:str #:lisp-fsrs)
  :components ((:file "emacs")))

(defsystem lisp-fsrs/test
  :depends-on (#:parachute #:closer-mop #:lisp-fsrs)
  :perform (test-op (op c) (symbol-call '#:parachute '#:test (find-symbol (symbol-name '#:test-lisp-fsrs) '#:lisp-fsrs.test)))
  :pathname "test/"
  :components ((:file "package")))
