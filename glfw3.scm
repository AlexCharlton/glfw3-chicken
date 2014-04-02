(module glfw3 *

(import chicken scheme bind foreign)

(bind-rename/pattern "^GLFW_([A-Z].+)$" "+\\1+")
(bind-rename/pattern "(.*)GLFW(.+)$" "\\1\\2")
(bind-rename/pattern "glfw(.+)$" "\\1")
(bind-options default-renaming: ""
              export-constants: #t)

(bind-file* "glfw3.h")
)
