# aoc2020

run `stack build` to download dependencies and build the project

run `stack exec aoc2020` to run the build

run `stack install ghcid` to install ghcid
ghcid provides a fast and easy way to compile and run Haskell projects when you save changes

run `ghcid -r` which will compile the project and run the main function when you save any changes.

a less fast alternative to ghcid:
`stack build --exec aoc2020 --file-watch --fast`
