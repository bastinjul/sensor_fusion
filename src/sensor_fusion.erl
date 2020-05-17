% @doc sensor_fusion public API.
% @end
-module(sensor_fusion).

-behavior(application).

% Callbacks
-export([start/2]).
-export([stop/1]).

%--- Callbacks -----------------------------------------------------------------

start(_Type, _Args) -> 
    {ok, Supervisor} = sensor_fusion_sup:start_link(),
    LEDs = [1, 2],
    [grisp_led:color(L, red) || L <- LEDs],
    {ok, Supervisor}.

stop(_State) -> ok.
