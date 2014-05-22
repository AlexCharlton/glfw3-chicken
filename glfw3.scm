(module glfw3 (make-window
               with-window
               window
               make-context-current
               init
               error-callback
               window-position-callback
               window-size-callback
               window-close-callback
               window-focus-callback
               window-iconify-callback
               framebuffer-size-callback
               mouse-button-callback
               cursor-enter-callback
               cursor-position-callback
               scroll-callback
               key-callback
               char-callback
               monitor-callback
               set-error-callback!
               set-window-position-callback!
               set-window-size-callback!
               set-window-close-callback!
               set-window-focus-callback!
               set-window-iconify-callback!
               set-framebuffer-size-callback!
               set-mouse-button-callback!
               set-cursor-enter-callback!
               set-cursor-position-callback!
               set-scroll-callback!
               set-key-callback!
               set-char-callback!
               set-monitor-callback!)

(import chicken scheme foreign)
(use data-structures (prefix glfw3-bindings %))

(reexport (except glfw3-bindings
                  init
                  make-context-current
                  set-error-callback
                  set-window-pos-callback
                  set-window-size-callback
                  set-window-close-callback
                  set-window-focus-callback
                  set-window-iconify-callback
                  set-framebuffer-size-callback
                  set-mouse-button-callback
                  set-cursor-enter-callback
                  set-cursor-pos-callback
                  set-scroll-callback
                  set-key-callback
                  set-char-callback
                  set-monitor-callback))

;;; Callbacks
(define error-callback (make-parameter (lambda (code err)
                                         (error 'glfw3 err))))
(define window-position-callback (make-parameter (lambda (window x y) #f)))
(define window-size-callback (make-parameter (lambda (window w h) #f)))
(define window-close-callback (make-parameter (lambda (window) #f)))
(define window-refresh-callback (make-parameter (lambda (window) #f)))
(define window-focus-callback (make-parameter (lambda (window focused?) #f)))
(define window-iconify-callback (make-parameter (lambda (window iconified?) #f)))
(define framebuffer-size-callback (make-parameter (lambda (window w h) #f)))
(define mouse-button-callback (make-parameter (lambda (window button action mods) #f)))
(define cursor-position-callback (make-parameter (lambda (window x y) #f)))
(define cursor-enter-callback (make-parameter (lambda (window entered?) #f)))
(define scroll-callback (make-parameter (lambda (window x y) #f)))
(define key-callback (make-parameter (lambda (window key scancode action mods) #f)))
(define char-callback (make-parameter (lambda (window char) #f)))
(define monitor-callback (make-parameter (lambda (monitor event) #f)))

(define-external (glfw3ErrorCallback (int code) (c-string err))
    void
  ((error-callback) code err))

(define-external (glfw3WindowPositionCallback (c-pointer window) (int x) (int y))
    void
  ((window-position-callback) window x y))

(define-external (glfw3WindowSizeCallback (c-pointer window) (int w) (int h))
    void
  ((window-size-callback) window w h))

(define-external (glfw3WindowCloseCallback (c-pointer window))
    void
  ((window-close-callback) window))

(define-external (glfw3WindowRefreshCallback (c-pointer window))
    void
  ((window-refresh-callback) window))

(define-external (glfw3WindowFocusCallback (c-pointer window) (bool focused?))
    void
  ((window-focus-callback) window focused?))

(define-external (glfw3WindowIconifyCallback (c-pointer window) (bool iconified?))
    void
  ((window-iconify-callback) window iconified?))

(define-external (glfw3FramebufferSizeCallback (c-pointer window) (int w) (int h))
    void
  ((framebuffer-size-callback) window w h))

(define-external (glfw3MouseButtonCallback (c-pointer window) (int button)
                                           (int action) (int mods))
    void
  ((mouse-button-callback) window button action mods))

(define-external (glfw3CursorPositionCallback (c-pointer window)
                                              (double x) (double y))
    void
  ((cursor-position-callback) window x y))

(define-external (glfw3CursorEnterCallback (c-pointer window) (bool entered?))
    void
  ((cursor-enter-callback) window entered?))

(define-external (glfw3ScrollCallback (c-pointer window)
                                       (double x) (double y))
    void
  ((scroll-callback) window x y))

(define-external (glfw3KeyCallback (c-pointer window) (int key) (int scancode)
                                   (int action) (int mods))
    void
  ((key-callback) window key scancode action mods))

(define-external (glfw3CharCallback (c-pointer window) (unsigned-int char))
    void
  ((char-callback) window char))

(define-external (glfw3MonitorCallback (c-pointer window) (int event))
    void
  ((monitor-callback) window event))

(define (set-error-callback! #!optional callback)
  (%set-error-callback (or callback #$glfw3ErrorCallback)))

(define (set-monitor-callback! #!optional callback)
  (%set-monitor-callback (or callback #$glfw3MonitorCallback)))

(define (set-window-position-callback! #!optional win callback)
  (%set-window-pos-callback (or win (window))
                            (or callback #$glfw3WindowPositionCallback)))

(define (set-window-size-callback! #!optional win callback)
  (%set-window-size-callback (or win (window))
                             (or callback #$glfw3WindowSizeCallback)))

(define (set-window-close-callback! #!optional win callback)
  (%set-window-close-callback (or win (window))
                              (or callback #$glfw3WindowCloseCallback)))

(define (set-window-focus-callback! #!optional win callback)
  (%set-window-focus-callback (or win (window))
                              (or callback #$glfw3WindowFocusCallback)))

(define (set-window-iconify-callback! #!optional win callback)
  (%set-window-iconify-callback (or win (window))
                                (or callback #$glfw3WindowIconifyCallback)))

(define (set-framebuffer-size-callback! #!optional win callback)
  (%set-framebuffer-size-callback (or win (window))
                                  (or callback #$glfw3FramebufferSizeCallback)))

(define (set-mouse-button-callback! #!optional win callback)
  (%set-mouse-button-callback (or win (window))
                              (or callback #$glfw3MouseButtonCallback)))

(define (set-cursor-enter-callback! #!optional win callback)
  (%set-cursor-enter-callback (or win (window))
                              (or callback #$glfw3CursorEnterCallback)))

(define (set-cursor-position-callback! #!optional win callback)
  (%set-cursor-pos-callback (or win (window))
                            (or callback #$glfw3CursorPositionCallback)))

(define (set-scroll-callback! #!optional win callback)
  (%set-scroll-callback (or win (window))
                        (or callback #$glfw3ScrollCallback)))

(define (set-key-callback! #!optional win callback)
  (%set-key-callback (or win (window))
                     (or callback #$glfw3KeyCallback)))

(define (set-char-callback! #!optional win callback)
  (%set-char-callback (or win (window))
                      (or callback #$glfw3CharCallback)))


;;; Initialization and window creation
(define (init)
  (set-error-callback!)
  (%init))

(define window (make-parameter #f))

(define (make-context-current win)
  (window win)
  (%make-context-current win))

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
  (%default-window-hints)
  (let loop ([hints hints])
    (when (>= (length hints) 2)
      (let* ([key (car hints)]
             [val (cadr hints)]
             [hint (alist-ref key *hints*)])
        (when hint
          (%window-hint (car hint) (if (and (= (length hint) 2)
                                          (eq? (cadr hint) bool:))
                                       (if val 1 0)
                                       val))))
      (loop (cddr hints))))
  (make-context-current (%create-window w h name
                                        (if fullscreen?
                                            (get-primary-monitor)
                                            #f)
                                        #f))
  (%swap-interval 1)
  (set-monitor-callback!)
  (set-window-position-callback!)
  (set-window-size-callback!)
  (set-window-close-callback!)
  (set-window-focus-callback!)
  (set-window-iconify-callback!)
  (set-framebuffer-size-callback!)
  (set-mouse-button-callback!)
  (set-cursor-enter-callback!)
  (set-cursor-position-callback!)
  (set-scroll-callback!)
  (set-key-callback!)
  (set-char-callback!)
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
       (%destroy-window (window))
       (%terminate))]))

) ; end glfw3
