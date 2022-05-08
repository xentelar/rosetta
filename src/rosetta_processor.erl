%%%-------------------------------------------------------------------
%% @doc rosetta public API
%% @end
%%%-------------------------------------------------------------------

-module(rosetta_processor).

-include_lib("kernel/include/logger.hrl").

-include("rosetta_workflow.hrl").

-export([process/0]).

process() ->
    Worflow = #workflow{process_name = <<"test">>, nodes = #{}, transitions = #{}},
    FileName = <<"/Users/bismark/Work/xentelar/smart-next/src/rsi/docs/change-panel-state.puml">>,
    FileContent = read_lines(FileName),
    ?LOG_INFO("\nFile Name --> ~p", [FileName]),
    process_line(FileContent, Worflow).

read_lines(FileName) ->
    {ok, Data} = file:read_file(FileName),
    binary:split(Data, [<<"\n">>], [global]).

process_line([], Worflow) ->
    ?LOG_INFO("WORKFLOW RESULT --> ~p", [Worflow]);
process_line([Head | Tail], Worflow) ->
    Line = string:trim(Head, leading), %Line = re:replace(Head, "^\\s+", "", [global, {return, binary}]),
    R0 = string:find(Line, <<"state \"">>, leading), %R = binary:matches(Line, [<<"state">>,<<" as ">>]),
    R1 = string:find(Line, <<" --> ">>, leading),
    Worflow0 = process_state(Line, R0, Worflow),
    Worflow1 = process_node(Line, R1, Worflow0),
    process_line(Tail, Worflow1).

process_state(_, nomatch, Worflow) -> 
    Worflow;
process_state(Line, R, Worflow) -> 
%    ?LOG_INFO("WORKFLOW --> ~p", [Worflow]),
    #workflow{nodes = Nodes} = Worflow,
    %?LOG_INFO("LINE IN --> ~p", [Line]),

    Line2 = replace(Line, [<<"\\s+">>, <<"\\\\n">>, <<"state\"">>, <<">>">>], <<"">>),
    Line6 = replace(Line2, [<<"\"as">>, <<"<<package:">>, <<"<<class:">>], <<",">>),
    %?LOG_INFO("LINE OUT --> ~p", [Line6]),

    {State0, VertPack, VertClass, State1} = decode(Line6),
    %?LOG_INFO("Status Line --> ~p -- ~p -- ~p -- ~p", [State0, VertPack, VertClass, State1]),

    Node = maps:find(State0, Nodes),
    NewNode = create_node(State0, VertPack, VertClass, State1, Node),
    Nodes0 = maps:put(State0, NewNode, Nodes),

    Worflow#workflow{nodes = Nodes0}.

process_node(_, nomatch, Worflow) -> 
    Worflow;
process_node(Line, _R, Worflow) -> 
    #workflow{nodes = Nodes} = Worflow,
    #workflow{transitions = Transitions} = Worflow,
    %?LOG_INFO("LINE IN --> ~p", [Line]),
    
    Line0 = replace(Line, [<<"\\s+">>, <<"\\\\n">>], <<"">>),
    Line1 = replace(Line0, [<<"<<([^<]*)>>-->">>, <<"-->">>, <<":">>], <<",">>),
    %?LOG_INFO("LINE OUT --> ~p", [Line1]),
    
    {State, NextState, Event} = decode(Line1),
    
    Node = maps:find(State, Nodes),
    StateTrants = maps:find(State, Transitions),
    
    NewTransitions = create_transitions(State, NextState, Event, Node, StateTrants, Transitions),
    
    Worflow#workflow{transitions = NewTransitions}.

decode(Line) ->
    Tokens = binary:split(Line, [<<",">>], [global]),
    decode(Tokens, {}).

decode([], Values) -> 
    Values;
decode([H | T], Values) -> 
    Values0 = erlang:append_element(Values, H),
    decode(T, Values0).

create_node(_State0, VertPack, VertClass, _State1, error) -> 
    #action{nodule = VertPack, class_name = VertClass};

create_node(State0, _VertPack, _VertClass, _State1, _Node) -> 
    ?LOG_ERROR("ERROR state is duplicate--> ~p", [State0]),
    throw(<<"ERROR state is duplicate">>).

create_transitions(<<"[*]">>, NextState, Event, Node, StateTrants, Transitions) -> 
    create_transitions(<<"start">>, NextState, Event, Node, StateTrants, Transitions);

create_transitions(State, <<"[*]">>, Event, Node, StateTrants, Transitions) -> 
        create_transitions(State, <<"end">>, Event, Node, StateTrants, Transitions);
        
% create_transitions(State, _NextState, _Event, error, _, _) -> 
%     ?LOG_ERROR("ERROR state does not defineded--> ~p", [State]),
%     throw(<<"ERROR state do not defineded">>);

create_transitions(State, NextState, Event, _Node, error, Transitions) -> 
    T = [#transition{state = State, event = Event, next_state = NextState}],
    maps:put(State, T, Transitions);

create_transitions(State, NextState, Event, _Node, {ok, StateTrants}, Transitions) ->
    T = StateTrants ++ [#transition{state = State, event = Event, next_state = NextState}],
    maps:put(State, T, Transitions).

replace(Data0, [], _Replace) -> 
    Data0;
replace(Data, [RE | T], Replace) ->
    Data0 = re:replace(Data, RE, Replace, [global, {return, binary}]),
    replace(Data0, T, Replace).
