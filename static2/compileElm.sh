#! bin/bash

echo "doing elm make src/Main.elm --optimize --output=home.js"

elm make src/Main.elm --optimize --output=home.js

