#!/bin/bash

First="10.20.0.77"
Others="10.20.0.78 10.20.0.79"
AllNodes="10.20.0.77 10.20.0.78 10.20.0.79"

Stop="sudo floppystore/rel/floppy/bin/floppy stop" 
Remove="sudo rm -rf floppystore/rel/floppy/data/*"
Start="sudo floppystore/rel/floppy/bin/floppy start"
./script/command_to_all.sh "$AllNodes" "$Stop" 
./script/command_to_all.sh "$AllNodes" "$Remove" 
./script/command_to_all.sh "$AllNodes" "$Start" 
./script/command_to_all.sh "$Others" 'sudo floppystore/rel/floppy/bin/floppy-admin cluster join "floppy@10.20.0.77"'
./script/command_to_all.sh "$First" 'sudo floppystore/rel/floppy/bin/floppy-admin cluster plan'
./script/command_to_all.sh "$First" 'sudo floppystore/rel/floppy/bin/floppy-admin cluster commit'
