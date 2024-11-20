(defsystem lisp-fsrs
  :version "1.0.0"
  :author "Bohong Huang <bohonghuang@qq.com>"
  :maintainer "Bohong Huang <bohonghuang@qq.com>"
  :license "MIT"
  :homepage "https://github.com/bohonghuang/lisp-fsrs"
  :bug-tracker "https://github.com/bohonghuang/lisp-fsrs/issues"
  :source-control (:git "https://github.com/bohonghuang/lisp-fsrs.git")
  :depends-on (#:alexandria #:local-time)
  :components ((:file "package")
               (:file "models" :depends-on ("package"))
               (:file "fsrs" :depends-on ("package" "models")))
  :in-order-to ((test-op (test-op #:lisp-fsrs/test))))

(defsystem lisp-fsrs/test
  :depends-on (#:parachute #:lisp-fsrs)
  :perform (test-op (op c) (symbol-call '#:parachute '#:test (find-symbol (symbol-name '#:test-lisp-fsrs) '#:lisp-fsrs.test)))
  :pathname "test/"
  :components ())
