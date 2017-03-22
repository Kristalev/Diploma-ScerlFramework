%%%-------------------------------------------------------------------
%%% @author Данил
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Июнь 2016 21:13
%%%-------------------------------------------------------------------
-module('data_prod').
-author("Данил").

%% API
-export([start/2]).



data_produser(Collector)->
  timer:sleep(rand:uniform(10000)),
  Ch = rand:uniform(10000),
  %%%io:format("~p~n",[Ch]),
  {Collector,'ScalaErlangNode@danil-pc'} ! Ch,
  data_produser(Collector).

start([],_) -> ok;
start([_|T],3500) -> start(T,0);
start(Collectors,Count) ->
  [H|_] = Collectors,
  P = spawn(fun() -> data_produser(H)end),
  io:format("~w produser start. Send to ~w ~n",[P,H]),
  start(Collectors,Count+1).


