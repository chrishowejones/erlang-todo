-module(create_todos_1741790496191000).
-behaviour(strata_migration).
-export([up/0, down/0]).


up() -> [
     <<"CREATE TABLE IF NOT EXISTS todos (">>,
     <<"  id INTEGER PRIMARY KEY AUTOINCREMENT, ">>,
     <<"  body TEXT NOT NULL">>,
     <<")">>
    ].

down() ->
    <<"DROP TABLE IF EXISTS todos">>.
