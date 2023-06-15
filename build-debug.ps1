elm make src/Main.elm --debug --output=debug/index.js

Copy-Item ./index.html debug/index.html
Copy-Item ./style.css debug/style.css
