# glfw3

## Description
Bindings to the [GLFW](http://www.glfw.org/) OpenGL window and event management library, version 3.X. Version 3 of GLFW is not backwards compatible with previous major versions of GLFW.

## Installation
This repository is a [Chicken Scheme](http://call-cc.org/) egg.

It is part of the [Chicken egg index](http://wiki.call-cc.org/chicken-projects/egg-index-4.html) and can be installed with `chicken-install glfw3`.

## Requirements
* Bind

## Documentation
glf3 is separated into two modules: `glfw3-bindingw` and `glfw3`. For almost all purposes, only `glfw3` should be needed.

`glfw3-bindings` provides direct bindings to GLFW generated by [bind](http://wiki.call-cc.org/eggref/4/bind). Names have been converted from camelCase to hyphenated, with GLFW prefixes removed. Constants are surrounded by `+`s (e.g. `+alpha-bits+`).

`glfw3` is the high-level interface that should be used in most cases. At the moment it is largely re-exporting `glfw3-bindings`, although many of these functions could still use wrappers (patches welcome!). The not-exactly-the-same-as-the-glfw-api functions are described in the section [High-level interface](#high-level-interface).

For information regarding the GLFW API, see the official [GLFW documentation](http://www.glfw.org/documentation.html).


### High-level interface
    [procedure] (init)

Initializes glfw, as well as `error-callback` and `monitor-callback`. Not needed when using `with-window`.

    [parameter] window

Contains the window associated with the current context.

    [procedure] (make-context-current WINDOW)

Performs `glfwMakeContextCurrent` while setting `window`.

    [procedure] (make-window WIDTH HEIGHT NAME #!key (fullscreen? #f) resizable visible decorated red-bits green-bits blue-bits alpha-bits depth-bits stencil-bits accum-red-bits accum-green-bits accum-blue-bits accum-alpha-bits aux-buffers samples refresh-rate sterio srgb-capable client-api context-version-major context-version-minor context-robustness opengl-forward-compat opengl-debug-context opengl-profile)

Create a window with title string `NAME` and dimensions `WIDTH` by `HEIGHT`. The keys correspond to the available [GLFW window hints](http://www.glfw.org/docs/latest/window.html#window_hints). `resizable`, `visible`, `decorated`, `sterio`, `srgb-capable`, `opengl-forward-compat`, `opengl-debug-context` accept boolean arguments, while all other accept either an integer or an appropriate GLFW constant as per the documentation. 

Sets the current context to the window that was created. Also sets the swap interval of the window to `1`, for convenience. This can be later set by `swap-interval`. Finally, this initializes all of the window-specific callbacks.

    [macro] (with-window (WIDTH HEIGHT NAME . KEYS) BODY ...)

Initializes GLFW, creates a window as per `make-window`, and runs `BODY` before cleaning up.

### Callbacks
`glfw3` provides parameters which contain the functions that are called from GLFW callbacks. The GLFW callbacks are initialized to call these parameters when `init` and `make-window` or `with-window` are used, but they can be changed with the callback setter functions.

    [parameter] error-callback

Called when a GLFW error occurs. Expects a function with the signature `(lambda (CODE MESSAGE) ...)`. `CODE` is one of `+not-initialized+`, `+no-current-context+`, `+invalid-enum+`, `+invalid-value+`, `+out-of-memory+`, `+api-unavailable+`, `+version-unavailable+`, `+platform-error+`, `+format-unavailable+`. `MESSAGE` is a string containing an error message.

Defaults to a function that throws an error and prints `MESSAGE`.

    [parameter] window-position-callback

Called when a window is moved. Expects a function with the signature `(lambda (WINDOW X Y) ...)`. `WINDOW` is the window that was moved. `X` and `Y` are the coordinates of the upper-left corner of the window.

    [parameter] window-size-callback

Called when a window is resized. Expects a function with the signature `(lambda (WINDOW W H) ...)`. `WINDOW` is the window that was resized. `W` and `H` are the new dimensions  of the window.

    [parameter] window-close-callback

Called when a window is closed. Expects a function with the signature `(lambda (WINDOW) ...)`. `WINDOW` is the window that was closed.

    [parameter] window-focus-callback

Called when a window comes into or goes out of focus. Expects a function with the signature `(lambda (WINDOW FOCUSED?) ...)`. `WINDOW` is the affected window, while `FOCUSED?` is true when the window has been focused and false otherwise.

    [parameter] window-iconify-callback

Called when a window is iconified or restored. Expects a function with the signature `(lambda (WINDOW ICONIFIED?) ...)`. `WINDOW` is the affected window, while `ICONIFIED?` is true when the window has been iconified and false otherwise.

    [parameter] framebuffer-size-callback

Called when a framebuffer is resized. Expects a function with the signature `(lambda (WINDOW W H) ...)`. `WINDOW` is the window whose framebuffer was resized. `W` and `H` are the new dimensions, in pixels, of the framebuffer.

    [parameter] mouse-button-callback

Called when a mouse button is pressed or released. Expects a function with the signature `(lambda (WINDOW BUTTON ACTION MODS) ...)`. `WINDOW` is the window where the button was pressed, `BUTTON` is the name of the mouse button (one of `+mouse-button-1+` through `+mouse-button-8+`, `+mouse-button-last+`, `+mouse-button-left+`, `+mouse-button-right+`, `+mouse-button-middle+`), `ACTION` is one of `+press+` or `+release+`, and `MODS` is a bit field describing the modifier keys that were held down (any of `+mod-shift+`, `+mod-control+`, `+mod-alt+`, or `+mod-super+`).

    [parameter] cursor-enter-callback

Called when a cursor enters or leaves a window. Expects a function with the signature `(lambda (WINDOW ENTERED?) ...)`. `WINDOW` is the affected window, and `ENTERED?` is true when the window was entered and false otherwise.

    [parameter] cursor-position-callback

Called when a cursor moves. Expects a function with the signature `(lambda (WINDOW X Y) ...)`. `WINDOW` is the affected window. `X` and `Y` is the new coordinates of the cursor.

    [parameter] scroll-callback

Called when a scroll occurs. Expects a function with the signature `(lambda (WINDOW X Y) ...)`. `WINDOW` is the affected window. `X` and `Y` are the scroll offsets.

    [parameter] key-callback

Called when a key is pressed or released. Expects a function with the signature `(lambda (WINDOW KEY SCANCODE ACTION MODS) ...)`. `WINDOW` is the window where the button was pressed, `KEY` is the name of the key, `SCANCODE` is the system-specific scancode of the key, `ACTION` is one of `+press+`, `+release+` or `+repeat+`, and `MODS` is a bit field describing the modifier keys that were held down (any of `+mod-shift+`, `+mod-control+`, `+mod-alt+`, or `+mod-super+`).

    [parameter] char-callback

Called when character is entered. Expects a function with the signature `(lambda (WINDOW CHAR) ...)`. `WINDOW` is the affected window, and  `CHAR` is the unicode code point of the character.

    [parameter] monitor-callback

Called when a monitor is connected or disconnected. Expects a function with the signature `(lambda (MONITOR EVENT) ...)`. `MONITOR` is a pointer to the affected monitor, `EVENT` is either `+connected+` or `+disconnected+`.

    [procedure] (set-error-callback! [WINDOW [CALLBACK]])
    [procedure] (set-window-position-callback! [WINDOW [CALLBACK]])
    [procedure] (set-window-size-callback! [WINDOW [CALLBACK]])
    [procedure] (set-window-close-callback! [WINDOW [CALLBACK]])
    [procedure] (set-window-focus-callback! [WINDOW [CALLBACK]])
    [procedure] (set-window-iconify-callback! [WINDOW [CALLBACK]])
    [procedure] (set-framebuffer-size-callback! [WINDOW [CALLBACK]])
    [procedure] (set-mouse-button-callback! [WINDOW [CALLBACK]])
    [procedure] (set-cursor-enter-callback! [WINDOW [CALLBACK]])
    [procedure] (set-cursor-position-callback! [WINDOW [CALLBACK]])
    [procedure] (set-scroll-callback! [WINDOW [CALLBACK]])
    [procedure] (set-key-callback! [WINDOW [CALLBACK]])
    [procedure] (set-char-callback! [WINDOW [CALLBACK]])
    [procedure] (set-monitor-callback! [WINDOW [CALLBACK]])

Set the callback functions associated with `WINDOW`. `WINDOW` defaults to `window`. `CALLBACK` defaults to an external function that calls the corresponding callback parameter.

## Example
``` Scheme
(use (prefix glfw3 glfw:))

(glfw:key-callback (lambda (window key scancode action mods)
                     (cond
                      [(and (eq? key glfw:+key-escape+) (eq? action glfw:+press+))
                       (glfw:set-window-should-close window #t)])))

(glfw:with-window (640 480 "Example" resizable: #f)
    (let loop ()
      (glfw:swap-buffers (glfw:window))
      (glfw:poll-events)
      (unless (glfw:window-should-close (glfw:window))
        (loop))))
```

## Version history
### Version 0.4.0
* Reorganize into two modules
* Add callback functions and parameters
* Add `init`
* Add `make-context-current`
* Fix a number of unsafe bindings that could potentially call back to Scheme

### Version 0.3.0
* Make `*window*` a parameter named `window`

### Version 0.2.0
* Add `make-window`, `with-window`

### Version 0.1.0
* Initial release

## Source repository
Source available on [GitHub](https://github.com/AlexCharlton/glfw3-chicken).

Bug reports and patches welcome! Bugs can be reported via GitHub or to alex.n.charlton at gmail.

## Author
Alex Charlton

## Licence
BSD
