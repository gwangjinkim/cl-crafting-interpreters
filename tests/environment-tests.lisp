(in-package :cl-lox-tw-tests)

(in-suite :cl-lox-tw-suite)

(test environment-basic-binding
  (let ((env (cl-lox-treewalk:make-environment)))
    (cl-lox-treewalk:define-variable env "a" 1)
    (cl-lox-treewalk:define-variable env "b" 2)
    (is (= 1 (cl-lox-treewalk:get-variable env "a" nil)))
    (is (= 2 (cl-lox-treewalk:get-variable env "b" nil)))))

(test environment-reassignment
  (let ((env (cl-lox-treewalk:make-environment)))
    (cl-lox-treewalk:define-variable env "a" 1)
    (cl-lox-treewalk:assign-variable env "a" 2 nil)
    (is (= 2 (cl-lox-treewalk:get-variable env "a" nil)))))

(test environment-undefined-variable
  (let ((env (cl-lox-treewalk:make-environment)))
    (signals error
      (cl-lox-treewalk:get-variable env "nope" nil))
    (signals error
      (cl-lox-treewalk:assign-variable env "nope" 10 nil))))

(test environment-lexical-scoping
  (let* ((global (cl-lox-treewalk:make-environment))
         (local (cl-lox-treewalk:make-environment global)))
    
    (cl-lox-treewalk:define-variable global "a" "global-a")
    (cl-lox-treewalk:define-variable global "b" "global-b")
    
    (cl-lox-treewalk:define-variable local "a" "local-a")
    (cl-lox-treewalk:define-variable local "c" "local-c")

    ;; The local environment shadows 'a', but 'b' falls through to global
    (is (equal "local-a" (cl-lox-treewalk:get-variable local "a" nil)))
    (is (equal "global-b" (cl-lox-treewalk:get-variable local "b" nil)))
    (is (equal "local-c" (cl-lox-treewalk:get-variable local "c" nil)))
    
    ;; Global environment doesn't see 'c', and still sees 'global-a'
    (is (equal "global-a" (cl-lox-treewalk:get-variable global "a" nil)))
    (signals error (cl-lox-treewalk:get-variable global "c" nil))))

(test interpret-block-scoping
  (let* ((source "
    var a = \"global a\";
    var b = \"global b\";
    var c = \"global c\";
    {
      var a = \"outer a\";
      var b = \"outer b\";
      {
        var a = \"inner a\";
        print a;
        print b;
        print c;
      }
      print a;
      print b;
      print c;
    }
    print a;
    print b;
    print c;
  ")
         (tokens (cl-lox-treewalk:scan-tokens source))
         (stmts (cl-lox-treewalk:parse tokens))
         (output (with-output-to-string (*standard-output*)
                   ;; reset global so we aren't polluted by previous tests
                   (setf cl-lox-treewalk::*global-environment* (cl-lox-treewalk:make-environment))
                   (setf cl-lox-treewalk::*environment* cl-lox-treewalk::*global-environment*)
                   (cl-lox-treewalk:interpret stmts))))
    
    ;; Check the sequenced output via regex/search. 
    ;; Inner block
    (is (search "inner a" output))
    (is (search "outer b" output)) ; nearest scope is outer
    
    ;; Final prints in global scope
    (is (search "global a" output))
    (is (search "global b" output))
    (is (search "global c" output))))
