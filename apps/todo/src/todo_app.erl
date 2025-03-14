%%%-------------------------------------------------------------------
%% @doc todo public API
%% @end
%%%-------------------------------------------------------------------

-module(todo_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    keep:init(),
    {ok, Conn} = esqlite3:open("todo.db"),
    keep:put(db, Conn),

    Migrations = strata_migrations:init(),
    Config = strata:config(Migrations, strata_sqlite3, Conn),
    ok = strata:run(Config),

    nine:compile(#{
        routes => todo_web:routes(),
        generator => nine_cowboy,
        router => todo_router
    }),
    {ok, _} = cowboy:start_clear(todo_http_listener,
        [{port, 8080}],
        #{middlewares => [todo_router]}
    ),

    todo_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
