(defsystem "engine"
  :components
  ((:file "display")
   (:file "matrix")
   (:file "draw")
   (:file "parser" :depends-on ("display" "matrix" "draw"))
   (:file "main" :depends-on ("parser"))))
