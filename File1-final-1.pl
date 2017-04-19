% Author:
% Date: 25-Mar-16
%
% % the schedule consists of events .. where every event is defined by
% event_in_course , Week number, Day ,Slot ,Group
% event(event_in_course(Course_code,Event,Type),Week , Day,Slot,Group)
% Example
%     event(event_in_course(csen401,quiz1,quiz),1,tuesday,1,group4MET)
%
%
% the predicate schedule , will generate a schedule consists of events
% for all groups we have in the number of weeks
%
% it will be in an order : for example first the events of group6MET
% then at the end group4MET
% if two groups have the same event , it will be appear two times but
% defined with its group and according to its quizslots



event_in_course(csen403, labquiz1, assignment).
event_in_course(csen403, labquiz2, assignment).
event_in_course(csen403, project1, evaluation).
event_in_course(csen403, project2, evaluation).
event_in_course(csen403, quiz1, quiz).
event_in_course(csen403, quiz2, quiz).
event_in_course(csen403, quiz3, quiz).

event_in_course(csen401, quiz1, quiz).
event_in_course(csen401, quiz2, quiz).
event_in_course(csen401, quiz3, quiz).
event_in_course(csen401, milestone1, evaluation).
event_in_course(csen401, milestone2, evaluation).
event_in_course(csen401, milestone3, evaluation).

event_in_course(csen402, quiz1, quiz).
event_in_course(csen402, quiz2, quiz).
event_in_course(csen402, quiz3, quiz).

event_in_course(math401, quiz1, quiz).
event_in_course(math401, quiz2, quiz).
event_in_course(math401, quiz3, quiz).

event_in_course(elct401, quiz1, quiz).
event_in_course(elct401, quiz2, quiz).
event_in_course(elct401, quiz3, quiz).
event_in_course(elct401, assignment1, assignment).
event_in_course(elct401, assignment2, assignment).

event_in_course(csen601, quiz1, quiz).
event_in_course(csen601, quiz2, quiz).
event_in_course(csen601, quiz3, quiz).
event_in_course(csen601, project, evaluation).
event_in_course(csen603, quiz1, quiz).
event_in_course(csen603, quiz2, quiz).
event_in_course(csen603, quiz3, quiz).

event_in_course(csen602, quiz1, quiz).
event_in_course(csen602, quiz2, quiz).
event_in_course(csen602, quiz3, quiz).

event_in_course(csen604, quiz1, quiz).
event_in_course(csen604, quiz2, quiz).
event_in_course(csen604, quiz3, quiz).
event_in_course(csen604, project1, evaluation).
event_in_course(csen604, project2, evaluation).


holiday(3,monday).
holiday(5,tuesday).
holiday(10,sunday).


studying(csen403, group4MET).
studying(csen401, group4MET).
studying(csen402, group4MET).
studying(csen402, group4MET).

studying(csen601, group6MET).
studying(csen602, group6MET).
studying(csen603, group6MET).
studying(csen604, group6MET).

should_precede(csen403,project1,project2).
should_precede(csen403,quiz1,quiz2).
should_precede(csen403,quiz2,quiz3).

quizslot(group4MET, tuesday, 1).
quizslot(group4MET, thursday, 1).
quizslot(group6MET, saturday, 5).







%   this predicate checks (succeeds only if ) the should precede facts are satisfied for this group
%   by getting all should precede facts then calls it on the helper
%   (preced2) predicate
% G .......> the group that you wanna do the check
%   on SC ......> the schedule note that the base case is the last one


precede(G,SC):-
 setof(should_precede(X,Y,W),should_precede(X,Y,W),Z),precede2(G,SC,Z).

 precede2(G,SC,[should_precede(Course_code,E1,E2)|T]):-

        not(member(event(event_in_course(Course_code,E1,Type),W,D,S,G),SC)),
        not(member(event(event_in_course(Course_code,E2,Type),W2,D2,S2,G),SC)),
         precede2(G,SC,T).

        precede2(G,SC,[should_precede(Course_code,E1,E2)|T]):-
     studying(Course_code,G),
         member(event(event_in_course(Course_code,E1,Type),W,D,S,G),SC),
         member(event(event_in_course(Course_code,E2,Type),W2,D2,S2,G),SC),
         W < W2 , precede2(G,SC,T).

precede2(G,SC,[schould_precede(Course_code,E1,E2)|T]):-
        not(member(event(event_in_course(Course_code,E2,Type),W2,D2,S2,G),SC)),
       member(event(event_in_course(Course_code,E1,Type),W,D,S,G),SC)
       ,precede2(G,SC,T).

precede2(G,SC,[]).




% succeeds only when X is the list of events of group G

group_events(G,X):- setof(C,studying(C,G),L),ss(X,L,[]).

ss(X,[H|T],Y):- setof(event_in_course(H,QQ,QQQ),event_in_course(H,QQ,QQQ),A),ss(X1,T,Y),append(X1,A,X).
ss([],[],[]).








% checks that group G doesn't have two quizzes in the same course in to
% consecitive weeks or in the same weeks

no_consec_quizzes(G,[event(A,B,C,D,G)|F]):-
 studying(Course_code,G),
 no_same_week_quiz(G,[event(A,B,C,D,G)|F]),
 A= event_in_course(Course_code,H,quiz) , B1 is B+1 ,
 Q = event_in_course(Course_code,J,quiz),
 not(member(event(Q,B1,_,_,G),F)),
 no_consec_quizzes(G,F).

no_consec_quizzes(G,[event(A,B,C,D,G)|F]):-
        A= event_in_course(Course_code,_,Q), Q \= quiz , no_consec_quizzes(G,F).

no_consec_quizzes(G,[A]).

no_consec_quizzes(G,[]).


 % helper for the no_consec_quizzes that checks that there is no same week quizzes for the same course
no_same_week_quiz(G,[event(A,B,C,D,G)|F]):-
        studying(Course_code,G),
      A= event_in_course(Course_code,H,quiz) ,
 Q = event_in_course(Course_code,J,quiz),
 not(member(event(Q,B,_,_,G),F)),
 no_same_week_quiz(G,F).

no_same_week_quiz(G,[event(A,B,C,D,G)|F]):-
        A= event_in_course(Course_code,_,Q), Q \= quiz , no_same_week_quiz(G,F).
no_same_week_quiz(G,[]).





% checks that the Group G doesn't have more than one quiz in the same day

no_same_day_quiz(G,[event(A,W,D,C,G)|T]):-
    studying(Course_code,G),  A = event_in_course(Course_code,_,quiz),Q= event_in_course(_,_,quiz),not(member(event(Q,W,D,_,G),T)),
                                                                                    no_same_day_quiz(G,T).


no_same_day_quiz(G,[event(A,W,D,C,G)|T]):-
    studying(Course_code,G),A=event_in_course(Course_code,_,Type),Type \=quiz,
    no_same_day_quiz(G,T).

no_same_day_quiz(G,[A]).
no_same_day_quiz(G,[]).




% % checks that the Group G doesn't have more than one assignment in the same day

no_same_day_assignment(G,[event(A,B,C,D,G)|T]):-
        studying(Course_code,G),A= event_in_course(Course_code,_,assignment),
        Q = event_in_course(_,_,assignment),not(member(event(Q,B,C,_,G),T)),
        no_same_day_assignment(G,T).


no_same_day_assignment(G,[event(A,B,C,D,G)|T]):-
        A = event_in_course(Course,_,F),F\=assignment ,
        no_same_day_assignment(G,T).
no_same_day_assignment(G,[A]).

no_same_day_assignment(G,[]).


% gets all holidays that the group G have so it can help to check wether
% the timing is available or not

no_holidays(G,L):-
        setof(holiday(F,B),holiday(F,B),L) .


%  checks that group G doesn't have more than one event in the same slot
%  and also checks about the holidays if any

valid_slot(Group,event(event_in_course(C,X,_),Week,Day,Slot,Group)):-
quizslot(Group,Day,Slot),no_holidays(Group,Z),\+member(holiday(Week,Day),Z).




%   checks that  Z is the list of timings that group G could have for scheduled events to this group (G)

available_timings(G,Z):-
setof(timing(D,S),quizslot(G,D,S),Z).




%
%


% gets the all groups that we have by predicate groups, it uses the
% groups in helper(a helper for schedule) , this helper (helper) loops
% on each group and get its events and timings then it uses the helper
% (scheduling predicate ) to generate its schedule, after finishing the
% second helper(scheduling) predicate it appends the schedule it got to
% the schedule ,
%
schedule(Week_N,SC):-

                setof(studying(A,B),studying(A,B),F),
                groups(F,L),
                sort(L,QQ),

                helper(_,Week_N,QQ,SC).


% helper for schedule , what it does is written before schedule
% the use of scheduling is mentioned before it in a single line comment

helper(Group,Week_N,[Group|T],SC):-

                group_events(Group,X),
                available_timings(Group,E),

                scheduling(Week_N,OO,1,X,Group,E,[]),

                helper(_,Week_N,T,OOO),
                append(OOO,OO,SC).
helper(_,_,[],[]).


% helper to be used in schedule , what it does is written before schedule

groups([studying(A,B)|T],L):-
        groups(T,O),
        append([B],O,L).
groups([],[]).




%  to be used in (helper) gives (helper) the schedule


scheduling(Week_N,II,Current_Week,H,Group,[timing(Day,Slot)|U],MEM):-



                Current_Week =< Week_N ,
                 member(event_in_course(Course_code,X,Y),H),
      Event= event(event_in_course(Course_code,X,Y),Current_Week,Day,Slot,Group),
             append(MEM,[Event],O),
            precede(Group,O),
            no_same_day_quiz(Group,O),
            no_same_day_assignment(Group,O),
            no_consec_quizzes(Group,O),



          ((
              valid_slot(Group,Event),

                delete(H,event_in_course(Course_code,X,Y),Q),
                scheduling(Week_N,II,Current_Week,Q,Group,U,O)
                     );
                ( scheduling(Week_N,II,Current_Week,H,Group,U,MEM))).



scheduling(Week_N,II,Current_Week,H,Group,[],O):-

                F is Current_Week +1 ,
                available_timings(Group,U),

                scheduling(Week_N,II,F,H,Group,U,O).


scheduling(Week_N ,Q,_,[],Group,_,Q).

scheduling(Week_N,Q,F,X,Group,_,Q):- F is Week_N +1 .















































