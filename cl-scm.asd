(defsystem "cl-scm"
  :version "0.1.0"
  :author "Lingao"
  :license "BSD 3-Clause"
  :depends-on ("arrow-macros"
               "cl-dbi"
               "cl-fad"
               "ironclad"
               "osicat"
               "trivial-features"
               "trivial-utf-8"
               "uuid")
  :pathname "src"
  :components 
  ((:file "main" :depends-on ("db-manager"))
   (:file "db-manager" :depends-on ("file"))
   (:file "file" :depends-on ("utils"))
   (:file "utils"))
  :description "simple content manager"
  :in-order-to ((test-op (test-op "cl-smp/tests"))))

(defsystem "cl-scm/tests"
  :author ""
  :license ""
  :depends-on ("cl-scm"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for cl-scm"
  :perform (test-op (op c) (symbol-call :rove :run c)))
