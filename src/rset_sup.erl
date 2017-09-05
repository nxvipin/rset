-module(rset_sup).
-author("Vipin Nair <swvist@gmail.com>").

-behaviour(supervisor).

%% API
-export([start_link/0,
         create_replica/1]).

%% Supervisor callbacks
-export([init/1]).
-define(SERVER, ?MODULE).


%% -----------------------------------------------------------------------------


start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

create_replica(ReplicaList) ->
    create_replica(ReplicaList, ReplicaList).

create_replica([], _ReplicaList) ->
    ok;

create_replica([ThisReplica|Rest], ReplicaList) ->
    Child = #{
	  id => {replica, ThisReplica},
	  start => {replica, start_link, [ThisReplica, ReplicaList]},
	  restart => permanent,
	  shutdown => 5000,
	  type => supervisor
	 },
    supervisor:start_child(?MODULE, Child),
    create_replica(Rest, ReplicaList).

init([]) ->
    %% [TODO]: Configure restart intensity
    {ok, { {one_for_all, 0, 1}, []} }.
