-module(types).

-export_type([mfargs/0]).

-type mfargs() :: {M :: module(), F :: atom(), A :: [term()] | undefined}.
