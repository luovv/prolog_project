candidate_number(33891).

solve_task(Task,Cost) :-
    ( part(1) -> solve_task_1_3(Task, Cost)
    ; part(3) -> solve_task_1_3(Task, Cost)
    ; part(4) -> solve_task_4(Task, Cost)
    ).

%%%%%%%%%% Part 1 & 3 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
solve_task_1_3(Task,Cost) :-
    agent_current_position(oscar,P),
    Task=go(Target),
    map_distance(P,Target,F),
    solve_task_bt(Task,[c(F,0,P)],0,[],RPath,Cost,_NewPos),!,
    find_path(RPath,Path),
    agent_do_moves(oscar,Path).

solve_task_1_3(Task,Cost) :-
    agent_current_position(oscar,P),
    Task=find(_),
    solve_task_bt(Task,[c(0,0,P)],0,[],RPath,Cost,_NewPos),!,
    find_path(RPath,Path),
    agent_do_moves(oscar,Path).
%%%%%%%%%% Part 1 & 3 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%% Part 4 (Optional) %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
solve_task_4(Task,Cost):-
    my_agent(Agent),
    query_world( agent_current_position, [Agent,P] ),
    Task=go(Target),
    solve_task_bt(Task,[c(F,0,P)],0,[],RPath,Cost,_NewPos),!,
    find_path(RPath,Path),
    query_world( agent_do_moves, [Agent,Path] ).

solve_task_4(Task,Cost):-
    my_agent(Agent),
    query_world( agent_current_position, [Agent,P] ),
    Task=find(_),
    solve_task_bt(Task,[c(F,0,P)],0,[],RPath,Cost,_NewPos),!,
    find_path(RPath,Path),
    query_world( agent_do_moves, [Agent,Path] ).
%%%%%%%%%% Part 4 (Optional) %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%% Useful predicates %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% backtracking depth-first search, needs to be changed to agenda-based A*
solve_task_bt(Task,Current,Depth,RR,RPath,[cost(Cost),depth(Depth)],NewPos) :-
    achieved(Task,Current,RR,RPath,Cost,NewPos).
solve_task_bt(Task,[Current|Rest],D,RR,RPath,Cost,NewPos) :-
    %% Current = [c(F,P)|RPath],
    Current = c(F,G,Pos),
    setof(Child,search(Pos,Task,G,Rest,RR,Rest,Child),NewAgenda),
    D1 is D+1,
    RR1=[Current|RR],
    solve_task_bt(Task,NewAgenda,D1,RR1,RPath,Cost,NewPos).

achieved(go(Exit),Current,RR,RPath,Cost,NewPos) :-
    RR = [c(F,G,Exit)|Path],
    ( Exit=none -> true
    ; otherwise -> RPath = RR, Cost = F
    ).

achieved(find(O),Current,RR,RPath,Cost,NewPos) :-
    RR = [c(F,G,Last)|Path],
    map_adjacent(Last,_,o(Index)),
    O=o(OIndex),
    (OIndex=Index;member(Index,OIndex)),
    ( O=none -> true
    ; otherwise -> RPath = RR, Cost = F
    ).

achieved(find(O),Current,RR,RPath,Cost,NewPos) :-
    O=c(Index),
    RR = [c(F,G,Last)|Path],
    map_adjacent(Last,_,c(Index)),
    ( O=none -> true
    ; otherwise -> RPath = RR
    ).
search(P,Task,G,Rest,RR,Aganda,Child) :-
    Task=go(Target),
    map_adjacent(P,N,empty),
    map_distance(N,Target,H),
    G1 is G+1,
    F is G1+H,
    Child=c(F,G1,N),
    \+ memberchk(Child,Rest),
    \+ memberchk(N,RR).
search(P,Task,G,Rest,RR,Aganda,Child) :-
    Task=find(O),
    map_adjacent(P,N,empty),
    G1 is G+1,
    F is G1,
    Child=c(F,G1,N),
    \+ memberchk(Child,Rest),
    \+ memberchk(N,RR).

search(P,Target,G,Rest,RR,Aganda,Head) :-
    searchAganda(Aganda,Head).
searchAganda([Head|T],Head).
searchAganda([Head|T],H):-
    searchAganda(T,H).

find_path(RPath,R):- 
    RPath=[H|T],
    H=c(F,G,Pos),
    find_path_rec(RPath,G,[],Pos,R).

find_path_rec([H|T],G,L,LastPos,R):-
    H=c(F,G,Pos) ->
        map_distance(Pos,LastPos,D),
        (D<2 ->
            L1=[Pos|L],
            G1 is G-1, 
            find_path_rec(T,G1,L1,Pos,R);
        otherwise ->
            find_path_rec(T,G,L,LastPos,R) 
        );
    otherwise -> 
        find_path_rec(T,G,L,LastPos,R).

find_path_rec([H|T],G,L,LastPos,R):- 
    T=[],
    R=L,!.







