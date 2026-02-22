# context.hx
Helix Plugin for context

# Installation
```sh
git clone https://github.com/gerblesh/helix.git -b statusline
```
(currently only the statusline is supported so a fork is required)
I will likely make this into a steel component in the future

then build/install the helix fork with:
```sh
cargo xtask steel
```

to install the plugin with forge:
```sh
forge pkg install --git https://codeberg.org/gwid/context.hx.git
```

add the lines to your init.scm file to configure the context

```scheme
;; init.scm
(require "context/context.scm")

;; add context to the left side of the statusbar
(context-enable 'right)

(tsnode-print-tree (tstree->root (get-tree-at-cursor (get-current-doc-id))))

(define (waaaaa)
  (display "heyo"))
  

(refresh-context-query!)
(set-status! path)
(display path)
(set-path! (get-current-doc-id))

```
