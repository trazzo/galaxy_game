-module(gen_planet).

-behaviour(gen_server).

-export([start/2,
        ally_planets/2,
        teardown_planet/1,
        nuclear_attack/1,
        laser_attack/1
        ]).


-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state,{name::galaxy_game:planet(),shield::boolean()}).

%%
%% USER API
%%
-spec start(galaxy_game:planet(), boolean()) -> {ok, pid()}.
start(P, S) ->
        gen_server:start({local, P},?MODULE, {P, S},[]).

-spec ally_planets(galaxy_game:planet(), galaxy_game:planet()) -> allied.
ally_planets(P1, P2) ->
       gen_server:call(P1,{ally_planets, P2}).

-spec teardown_planet(galaxy_game:planet()) -> ok.
teardown_planet(P) ->
        gen_server:call(P,teardown_planet).

-spec nuclear_attack(galaxy_game:planet()) -> ok.
nuclear_attack(P) ->
        gen_server:call(P,nuclear_attack).

-spec laser_attack(galaxy_game:planet()) -> ok | defended.
laser_attack(P) ->
        gen_server:call(P,laser_attack).


%%
%% gen_server callbacks
%%

init({P, true}) ->
        process_flag(trap_exit, true),       
        {ok,#state{name = P,shield = true}};
init({P, false}) ->       
        {ok,#state{name = P,shield = false}}.

handle_call({ally_planets, P2}, _From, State) -> 
        true = link(whereis(P2)),
        {reply, allied, State};
handle_call(teardown_planet, _From, State) ->  
        {stop, normal, ok, State};
handle_call(nuclear_attack, _From, State) ->
        {stop, nuclear, ok, State};
handle_call(laser_attack, _From, State) ->
        Shield = State#state.shield,        
        case Shield of
            true ->  
                 io:format("planet defended,try with a nuclear bomb~n"),               
                 {reply, defended, State};
            false ->   
                {stop, laser, ok, State} 
        end;
handle_call(_, _, State) ->
    {noreply, State}.

handle_cast(_,State) ->
    {noreply,State}.

handle_info({'EXIT', Origin , _}, State) ->
    io:format("planet ~p has been attacked and destroyed, "
              "planet ~p survived using his shield~n", [Origin, State#state.name]),
    {noreply, State};
handle_info(_,State) ->
    {noreply,State}.

terminate(_,_) ->
    ok.

code_change(_,State,_) ->
    {ok, State}.
