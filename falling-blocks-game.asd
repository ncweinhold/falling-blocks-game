;;;; falling-blocks-game.asd

(asdf:defsystem #:falling-blocks-game
  :description "Simple game written using Lispbuilder-SDL"
  :author "Nick Weinhold <n.c.weinhold@gmail.com>"
  :depends-on (#:lispbuilder-sdl)
  :serial t
  :components ((:file "package")
               (:file "falling-blocks-game")))

