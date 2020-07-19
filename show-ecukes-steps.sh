#!/bin/bash
# Print the espuds steps from elisp definition file
dir=".cask/26.3/elpa/espuds-20160905.1300"
grep -E "^\((Given|When|Then)" "$dir/espuds.el" | sed 's:^(:- :' | sed 's:"\^::' | sed 's:\$"::'
