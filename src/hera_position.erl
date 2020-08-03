%%%-------------------------------------------------------------------
%%% @author Julien Bastin <julien.bastin@student.uclouvain.be>
%%% @author Guillaume Neirinckx <guillaume.neirinckx@student.uclouvain.be>
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% Module which includes all the functions related to our user case.
%%% Our user case consists in calculating the position of a move person in an empty room in realtime
%%% using GRiSP boards with Diligent pmod_maxsonar.
%%% @reference See <a href="https://grisp.org/" target="_blank">GRiSP site</a> and <a href="https://store.digilentinc.com/pmodmaxsonar-maxbotix-ultrasonic-range-finder/" target="_blank">Diligent site</a> for more information
%%% @end
%%% Created : 02. May 2020 2:22 AM
%%%-------------------------------------------------------------------

-module(hera_position).
-author("Julien Bastin <julien.bastin@student.uclouvain.be>, Guillaume Neirinckx <guillaume.neirinckx@student.uclouvain.be>").

-export([launch_hera/7]).
-export([restart_measurement/3]).
-export([sonar_measurement/0]).
%%====================================================================
%% Macros
%%====================================================================

-define(SERVER, ?MODULE).

%%====================================================================
%% Records
%%====================================================================


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Launch hera with synchronization to follow a person in a room.
%% In this case, a synchronization is performed between the measurement of the pmod_maxsonar in order to avoid cross-talking between them
%%
%% @param PosX The x coordinate of the board in the room
%% @param PosY The y coordinate of the board in the room
%% @param NodeId The id of the board. The first board must have NodeId = 0
%%
%%--------------------------------------------------------------------

launch_hera(_PosX, _PosY, _NodeId, Frequency, MaxIteration, {_MinX, _MinY}, {_MaxX, _MaxY}) ->
    Measurements = [
        hera:get_unsynchronized_measurement(sonar, fun() -> sonar_measurement() end, fun(CurrVal, PrevVal, TimeDiff, UpperBound, DefaultMeas) -> filter_sonar(CurrVal, PrevVal, TimeDiff, UpperBound, DefaultMeas) end, 0.14, MaxIteration, Frequency)
    ],
    Calculations = [], % no calculation
    hera:launch_app(Measurements, Calculations).

restart_measurement(MaxIterations, Frequency, DoFilter) ->
    hera:restart_unsync_measurement(sonar, fun() -> sonar_measurement() end, Frequency, MaxIterations, DoFilter, false). % 1 sonar


%%%===================================================================
%%% Internal functions
%%%===================================================================

sonar_measurement() ->
    case pmod_maxsonar:get() of
        undefined -> {error, "pmod_maxsonar not set up correctly"};
        Value ->
            {ok, Value*2.54}
    end.


%% @private
%% @doc used for sonar measurements only.
%% It returns true if the sonar measure has to be filtered out
-spec(filter_sonar(PrevMeasureVal :: float(), CurrMeasureVal :: float(), DefaultMeasureVal :: float(), UpperBound :: float(), TimeDiff :: integer())->
    boolean()).
filter_sonar(CurrMeasureVal, PrevMeasureVal, TimeDiff, UpperBound, [DefaultMeasureVal]) ->
    PrevIsBackDist = is_background_dist(PrevMeasureVal, DefaultMeasureVal),
    IsDefDist = is_background_dist(CurrMeasureVal, DefaultMeasureVal),
    IsDefDist orelse
        (PrevIsBackDist == false andalso
            abs(CurrMeasureVal - PrevMeasureVal) > UpperBound*TimeDiff). % 0.28*(100=TimeDiff) = 0.28*TimeDiff cm/TimeDiff ms


%% @private
%% @doc used for sonar measurements only.
%% It returns true if the measure is equal to or greater than the distance measured during the warmup phase = the background distance
-spec(is_background_dist(MeasureVal :: float(), DefaultMeasureVal :: float())->
    boolean()).
is_background_dist(MeasureVal, DefaultMeasureVal)->
    if
        DefaultMeasureVal * 0.95 =< MeasureVal ->
            true;
        true ->
            false
    end.
