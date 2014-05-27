%% @doc
%% Implementation module for the galactic battle simulator.
%% The following example shows the expected behavior of the simulator:
%%
%% Planets=[mercury,uranus,venus, earth]
%% Shields=[mercury,uranus]
%% Alliances=[{mercury, uranus}, {venus, earth}]
%% Actions=[{nuclear,mercury},{laser,venus}, {laser, uranus}]
%%
%% ExpectedSurvivors = [uranus]
%% In order to produce this expected results, the following calls will be tested:
%% * ok = setup_universe(Planets, Shields, Alliances)
%% * [uranus] = simulate_attack(Planets, Actions)
%% * ok = teardown_universe(Planets)
%%
%% All the 3 calls will be tested in order to check they produce the expected
%% side effects (setup_universe/3 creates a process per planet, etc)
%% @end

-module(galaxy_game).

-include_lib("eunit/include/eunit.hrl").

-type planet()::atom().
-type shield()::planet().
-type alliance()::{planet(), planet()}.
-type attack()::{laser | nuclear, planet()}.

-export([setup_universe/3, teardown_universe/1, simulate_attack/2]).

-export_type([planet/0]).

%% ================================================================
%% API
%% ================================================================

%% @doc Set up a universe described by the input.
%% The imput is asumed to be minimal and non redundant (i.e. if there is an
%% alliance {a, b} there won't be an alliance {b, a}).
%% Once this function returns, the universe is expected to be fully ready,
%% shields, alliances and all.
-spec setup_universe([planet()], [shield()], [alliance()]) -> ok.
%% @end
setup_universe(Planets, Shields, Alliances) ->
    ok = create_planets(Planets, Shields),
    ok = setup_alliances(Alliances).
    
%% @doc Clean up a universe simulation.
%% This function will only be called after calling setup_universe/3 with the
%% same set of planets.
%% Once this function returns, all the processes spawned by the simulation
%% should be gone.
-spec teardown_universe([planet()]) -> ok.
%% @end
teardown_universe(Planets) ->
    ok = lists:foreach (fun(P) ->
            case whereis(P) of
                Pid when is_pid(Pid) -> gen_planet:teardown_planet(P);
                _ -> ok
            end
         end, Planets).
 
%% @doc Simulate an attack.
%% This function will only be called after setting up a universe with the same
%% set of planets.
%% It returns the list of planets that have survived the attack
-spec simulate_attack([planet()], [attack()]) -> Survivors::[planet()].
%% @end
simulate_attack(Planets, Actions) ->
    lists:map(fun({A,P}) ->
         case A of
            nuclear ->
                   {P, gen_planet:nuclear_attack(P)};
            laser ->
                   {P, gen_planet:laser_attack(P)}%,lists:member(P, Shields));
         end
         end,Actions),
    timer:sleep(200),
    lists:filter(fun (P) -> is_pid(whereis(P)) end,Planets).


%% ================================================================
%% Internal functions
%% ================================================================

-spec create_planets([planet], [shield()]) -> ok.
create_planets(Planets, Shields) ->
    lists:foreach(fun (P) ->
            gen_planet:start(P,lists:member(P, Shields))
        end, Planets).

-spec setup_alliances([alliance()]) -> ok.
setup_alliances(Alliances) ->
    ok = lists:foreach(fun({P1, P2}) ->
            gen_planet:ally_planets(P1,P2)
        end, Alliances).
 
