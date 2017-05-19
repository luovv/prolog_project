% Find hidden identity by repeatedly calling agent_ask_oracle(oscar,o(1),link,L)
% find_identity(-A)
find_identity(A) :-
    ( 
    	part(3) -> find_identity_3(A)
    	; part(4) -> find_identity_4(A)
    ).

find_identity_4(A):-
	my_agent(Agent),
	b_setval(visted,[1,2,3,4,5,6,7,8,9,10]),
	findall(Actor,actor(Actor),ActorList),
	find_actor_4(ActorList,A),
	write(A).


find_actor_4(ActorList,Result):-
	ActorList=[Result],!.

find_actor_4(ActorList,Result):-
	my_agent(Agent),
	query_world(agent_current_energy,[Agent,E]),
	check_energy_4(E),
	b_getval(visted,Visted),
	(
		solve_task(find(o(Visted)),_) ->
			query_world(agent_current_position,[Agent,P]),
			(
				map_adjacent(P,_,o(Index)) ->
					delete(Visted,Index,NewVisted),
					b_setval(visted,NewVisted),
					(
						query_world(agent_ask_oracle,[Agent,o(Index),link,Link]) ->
							eliminate(ActorList,[],Link,ResultList),
							find_actor_4(ResultList,Result);
						otherwise ->
							find_actor_4(ActorList,Result),true
					);		
				otherwise ->
					find_actor_4(ActorList,Result),true
			);
		otherwise -> 
			find_actor_4(ActorList,Result),true
	).

check_energy_4(E):-
	E<50 ->
		my_agent(Agent),
		(
			solve_task(find(c(_)),_) -> 
				query_world(agent_current_position,[Agent,P]),
				(
					map_adjacent(P,_,c(Index)) ->
						query_world(agent_topup_energy,[Agent, c(Index)]);
					otherwise -> 
						check_energy_4(E)
				);
			otherwise -> 
				check_energy_4(E)
		),
		true;
	otherwise ->
		true.


find_identity_3(A):-
	b_setval(visted,[1,2,3,4,5,6,7,8,9,10]),
	findall(Actor,actor(Actor),ActorList),
	find_actor_3(ActorList,A),
	say(A),
	write(A).

find_actor_3(ActorList,Result):-
	ActorList=[Result],!.

find_actor_3(ActorList,Result):-
	agent_current_energy(oscar,E),
	check_energy_3(E),
	b_getval(visted,Visted),
	(
		solve_task(find(o(Visted)),_) ->
			agent_current_position(oscar,P),
			(
				map_adjacent(P,_,o(Index)) ->
					delete(Visted,Index,NewVisted),
					b_setval(visted,NewVisted),
					(
						agent_ask_oracle(oscar,o(Index),link,Link) ->
							eliminate(ActorList,[],Link,ResultList),
							find_actor_3(ResultList,Result);
						otherwise ->
							find_actor_3(ActorList,Result),true
					);		
				otherwise ->
					find_actor_3(ActorList,Result),true
			);
		otherwise -> 
			find_actor_3(ActorList,Result),true
	).

check_energy_3(E):-
	E<50 ->
		(
			solve_task(find(c(_)),_) -> 
				agent_current_position(oscar,P),
				(
					map_adjacent(P,_,c(Index)) ->
						agent_topup_energy(oscar, c(Index));
					otherwise -> 
						check_energy_3(E)
				);
			otherwise -> 
				check_energy_3(E)
		),
		true;
	otherwise ->
		true.

eliminate(ActorList,ResultList,Link,Result):-
	ActorList=[],
	Result=ResultList.

eliminate([H|T],ResultList,Link,Result):-
	contained(H,Link) -> eliminate(T,[H|ResultList],Link,Result);
	otherwise -> eliminate(T,ResultList,Link,Result).

contained(Actor,Link):-
	wp(Actor,WT),
	wt_link(WT,Link).