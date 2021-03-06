# learnErlang

Erlang exercises, examples and code snippets. Mostly based on
[Learn You Some Erlang](https://learnyousomeerlang.com/) (`lyse`), and
[Programming Erlang](https://pragprog.com/titles/jaerlang2/programming-erlang-2nd-edition/) (`pe`).

## Getting Started

Install erlang:

    brew install erlang

Build a local `plt` file for Dialyzer support in VS Code with Erlang LS. Make
sure the `plt_path` in [erlang_ls.config](./erlang_ls.config) points to the
`output_plt` file.

## Build the Project

Compile the project with rebar3 to produce a `_build` directory:

    rebar3 compile
    
To only compile subdirectories of the project that contain an `Emakefile`,
change to the subdirectory and run:

    erl -make

This will produce `.beam` files in the subdirectory's `ebin` directory.
To load the project in an Erlang shell, run:

    erl -pa ebin/

Or start an Erlang shell and call (also to recompile while in shell):

    make:all([load]).
