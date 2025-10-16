%%%-------------------------------------------------------------------
%%% @author caigou
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. 10月 2025 16:24
%%%-------------------------------------------------------------------
-module(main).
-author("caigou").

%% API
-export([start/0]).


start() ->
	tcp_listener:start(),
	database_queryer:start(false),
	ok.