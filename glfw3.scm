(module glfw3 *

(import chicken scheme foreign)

(use bind data-structures)

(bind-rename/pattern "^GLFW_([A-Z].+)$" "+\\1+")
(bind-rename/pattern "(.*)GLFW(.+)$" "\\1\\2")
(bind-rename/pattern "glfw(.+)$" "\\1")
(bind-options default-renaming: ""
              export-constants: #t)

(bind-file* "glfw3.h")

(define window (make-parameter #f))

(define (make-window w h name #!rest hints #!key [fullscreen? #f])
  (define *hints*
    `((resizable: ,+resizable+ bool:)
      (visible: ,+visible+ bool:)
      (decorated: ,+decorated+ bool:)
      (red-bits: ,+red-bits+) (green-bits: ,+green-bits+) (blue-bits: ,+blue-bits+)
      (alpha-bits: ,+alpha-bits+)
      (depth-bits: ,+depth-bits+) (stencil-bits: ,+stencil-bits+)
      (accum-red-bits: ,+accum-red-bits+) (accum-green-bits: ,+accum-green-bits+)
      (accum-blue-bits: ,+accum-blue-bits+) (accum-alpha-bits: ,+accum-alpha-bits+)
      (aux-buffers: ,+aux-buffers+)
      (samples: ,+samples+)
      (refresh-rate: ,+refresh-rate+)
      (sterio: ,+stereo+ bool:)
      (srgb-capable: ,+srgb-capable+ bool:)
      (client-api: ,+client-api+)
      (context-version-major: ,+context-version-major+)
      (context-version-minor: ,+context-version-minor+)
      (context-robustness: ,+context-robustness+)
      (opengl-forward-compat: ,+opengl-forward-compat+ bool:)
      (opengl-debug-context: ,+opengl-debug-context+ bool:)
      (opengl-profile: ,+opengl-any-profile+)))
  (default-window-hints)
  (let loop ([hints hints])
    (when (>= (length hints) 2)
      (let* ([key (car hints)]
             [val (cadr hints)]
             [hint (alist-ref key *hints*)])
        (when hint
          (window-hint (car hint) (if (and (= (length hint) 2)
                                           (eq? (cadr hint) bool:))
                                      (if val 1 0)
                                      val))))
      (loop (cddr hints))))
  (window (create-window w h name
                                (if fullscreen?
                                    (get-primary-monitor)
                                    #f)
                                #f))
  (make-context-current (window))
  (window))

(define-syntax with-window
  (syntax-rules ()
    [(_ (w h name . keys) body ...)
     (begin
       (init)
       (make-window w h name . keys)
       (when (feature? #:opengl-glew)
           (opengl-glew#init))
       body ...
       (destroy-window (window))
       (terminate))]))

) ; end module
