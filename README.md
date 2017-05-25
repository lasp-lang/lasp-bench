# benchy
A load-generation and testing tool for basically whatever you can write a returning Erlang function for.

## Difference from Basho Bench
This is a stripped down, bare bones distribution of basho bench, that removes all but the essential dependencies.  
Quite unlike the original, this fork of Basho Bench attempts to use the latest and greatest:

- Uses rebar3, the official build tool for modern Erlang/OTP applications
- Uses Hex packages for dependencies (with a single exception, but there is ongoing work to fix this)
- Requires Erlang/OTP R18 or higher to run, which effectively removes lots of deprecated code 

