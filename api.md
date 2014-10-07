# API Brainstorm

```bash
elm-make

elm-make --js

elm-make --html

elm-make \
    --set-runtime="elm-runtime.js" \
    --bundle-runtime \
    --bundle-script="path/to/script.js" \
    --file-in="src/Main.elm"
    --file-out="index.html"

elm-make clean
```