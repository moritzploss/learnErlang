# learnErlang

Erlang exercises, examples and code snippets. Mostly based on
[Learn You Some Erlang](https://learnyousomeerlang.com/) (`lyse`), and
[Programming Erlang](https://pragprog.com/titles/jaerlang2/programming-erlang-2nd-edition/) (`pe`).

## Getting Started

Install erlang:

    brew install erlang

## Running Apps

Use the following directory structure and put all `.erl` files in `src`:

    ebin/
    include/
    src/
    Emakefile
    
To compile the project, change to the project root and run:

    erl -make

This will produce `.beam` files in the `ebin` directory. To load the
project in an Erlang shell, run:

    erl -pa ebin/

Or start an Erlang shell and call (also to recompile while in shell):

    make:all([load]).

## Dialyzer

Run the Dialyzer on a all files in a `src` directory:

    dialyzer ./src --src -r
