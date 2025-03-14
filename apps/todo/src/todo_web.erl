-module(todo_web).
-export([routes/0,
         get_todos/1,
         new_todo/1,
         delete_todo/1,
         update_todo/1]).

routes() ->
    [#{path => <<"/api">>,
       handle => [#{path => <<"/todos">>,
                    method => <<"GET">>,
                    handle => {todo_web, get_todos}},
                  #{path => <<"/todo">>,
                    pre => {nine_cowboy_mid, json_request},
                    handle => [#{method => <<"POST">>,
                                 handle => {todo_web, new_todo}},
                               #{method => <<"DELETE">>,
                                 handle => {todo_web, delete_todo}},
                               #{method => <<"PUT">>,
                                 handle => {todo_web, update_todo}}]}]},
     #{path => <<"*">>,
       handle => {nine_cowboy_util, not_found}}].

get_todos(Context) ->
    query_response(Context, <<"SELECT * FROM todos">>).

new_todo(#{json := #{<<"body">> := Body}} = Context) ->
    query_success(Context, <<"INSERT INTO todos (body) VALUES (?)">>, [Body]).

delete_todo(#{json := #{<<"id">> := Id}} = Context) ->
    query_success(Context, <<"DELETE FROM todos WHERE id = ?">>, [Id]).

update_todo(#{json := #{<<"id">> := Id,
                        <<"body">> := Body}} = Context) ->
    query_success(Context, <<"UPDATE todos SET body = ? WHERE id = ?">>, [Body, Id]).

json_response(#{req := Req} = Context, Data) ->
    Context#{resp => nine_cowboy_util:json_response(200, Data, Req)}.

json_success(Context) ->
    json_response(Context, #{success => true}).

query_db(Sql) ->
    query_db(Sql, []).

query_db(Sql, Args) ->
    esqlite3:q(keep:get(db), Sql, Args).

query_response(Context, Sql) ->
    json_response(Context, query_db(Sql)).

query_success(Context, Sql, Args) ->
    query_db(Sql, Args),
    json_success(Context).
