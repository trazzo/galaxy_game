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

%% @doc Set up a universe described by the input.
%% The imput is asumed to be minimal and non redundant (i.e. if there is an
%% alliance {a, b} there won't be an alliance {b, a}).
%% Once this function returns, the universe is expected to be fully ready,
%% shields, alliances and all.
-spec setup_universe([planet()], [shield()], [alliance()]) -> ok.
%% @end
setup_universe(Planets, Shields, Alliances) ->
    ok = lists:foreach(fun (P) ->
            spawn_planet(P, lists:member(P, Shields))
        end, Planets),
    ok = wait_for_planets(Planets),
    ok = setup_alliances(Alliances).

-spec spawn_planet(planet(), boolean()) -> pid().
spawn_planet(Name, Shielded) ->
    Controller = self(),
    spawn(fun () -> planet(Name, Shielded, Controller) end).

-spec setup_alliances([alliance()]) -> ok.
setup_alliances(Alliances) ->
    lists:foreach(fun({P1, P2}) ->
            P1 ! {ally, P2}
        end, Alliances).

-spec planet(planet(), boolean(), pid()) -> any().
planet(Name, Shield, Pid) ->
    register(Name,self()),
    process_flag(trap_exit, Shield),
    Pid ! {ready, Name},
    planet(Name,Shield).

planet(Name, Shield) -> % i ve catched Shield variable to put it in the case
    receive
        {ally, Planet} ->
            link(whereis(Planet)),
            io:format("~p allied with ~p~n", [Name, Planet]),
            planet(Name,Shield);
        {die, Controller} ->
            io:format("~p told to die~n", [Name]),
            Controller ! {dead, self()},
            ok;
        {die_nuclear, _} ->
            io:format("~p planet destroyed by nuclear attack~n",[Name]);
      % TODO print on exit signal
        {die_laser, _} ->
            case Shield of
                true ->
                    io:format("~p is protected with shield, try with a nuclear bomb~n",[Name]),
                    planet;
                false ->
                    io:format("~p planet destroyed by laser attack~n",[Name])

             end
    end.

% erlang:whereis(Name) -> pid() | undefined
% erlang:process_info(pid())

wait_for_planets([]) -> ok;
wait_for_planets(Planets) ->
    receive
        {ready, Planet} ->
            wait_for_planets(lists:delete(Planet, Planets));
        {dead, Planet} ->
            wait_for_planets(lists:delete(Planet, Planets))
    after 5000 ->
        {error, timeout}
    end.


%% @doc Clean up a universe simulation.
%% This function will only be called after calling setup_universe/3 with the
%% same set of planets.
%% Once this function returns, all the processes spawned by the simulation
%% should be gone.
-spec teardown_universe([planet()]) -> ok.
%% @end
teardown_universe(Planets) ->
    RemainingPlanets = [whereis(P) || P <- Planets],
    lists:foreach(fun(P) ->
                case is_pid(P) of
                    true ->
                        P ! {die, self()};
                    false ->
                        ok
                end
        end, RemainingPlanets),
    wait_for_planets(RemainingPlanets).

%% @doc Simulate an attack.
%% This function will only be called after setting up a universe with the same
%% set of planets.
%% It returns the list of planets that have survived the attack
-spec simulate_attack([planet()], [attack()]) -> Survivors::[planet()].
%% @end
simulate_attack(Planets, Actions) ->
    lists:foreach(fun({A,P}) ->
         case A of
            nuclear ->
                    P ! {die_nuclear, P};
            laser ->
                    P ! {die_laser, P};
            _Another -> io:format("try again using a laser or a nuclear attack~n")
         end
         end,Actions).
