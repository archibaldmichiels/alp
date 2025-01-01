
%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% A DATA-DRIVEN MWU PARSER
%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Latest Update : March 2009
% ©Archibald Michiels, University of Liege, Belgium
% companion program to "LES LEXIES EN TAL" 



%%%%%%%%%%%%%%
% DECLARATIONS
%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%

% :- use_module(library(gs)).  % included as an appendix here
:- set_prolog_flag(double_quotes, codes).

% :- set_prolog_flag(verbose_load,normal).

:- [lightdic,mt,roget,indic,coll,envir,pesi].
% for the measure of lexical proximity

:- style_check(-singleton).
:- style_check(-discontiguous).
:- dynamic(lex/3).

% the 'lex' predicate is partly fed by a macro-expansion clause
% executed at run-time
% and must therefore be declared dynamic
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%
% operator  declaration 

:- op(800,xfx,--->).

% used in rules : rule_name ---> rule_body (list of 'actions' to be taken) 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% empty check point for quick debugging; call : spy(checkpoint)
% after inserting a call to checkpoint wherever appropriate
% and then start stepping through the goals

checkpoint(_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% better readability in the programmer's eye ;-)

ifthen(Condition,Goal) :- Condition -> Goal ; true.
ifthenelse(Condition, ThenGoal, ElseGoal) :- Condition -> ThenGoal ; ElseGoal.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%
% GO STEP 
%%%%%%%%%

go :- nl,
     protocol(lexies),
     write('A DATA-DRIVEN MWU PARSER'),nl,
     write('-------------------------------------------------------------------------------------------------------'),
     nl,nl,
     write('A.Michiels, University of Liège'),nl,nl,
     write('Expanding the verb macros...'),nl,mkthem,nl,nl,
     write('Input file?  [stdin. or file_name.] --> '),
     read(Input),
     dealwith(Input,HandleIn),
     write('Output file? [file_name.] --> '),
     read(Output),
     concat(Output,'.lst',Outlist),
     !,
     start(Outlist,HandleIn,Input).
   

% Input is from stdin(user's terminal) or from file 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   
   dealwith(stdin,_):- !. % nothing to be done, no file to be opened -
                          % standard input (from the user's terminal)

   dealwith(FileIn,HandleIn):- 
                         open(FileIn,read,HandleIn).
                         % open(File_Name,Mode,Stream)



   start(Outlist,_,stdin) :-
                 open(Outlist,write,Lists),
                 recorda(out,Lists,_), 
                 repeat,
                 clear, % clearing the data base
                 statistics(cputime,TB),
                 recorda(time,TB),
                 nl,
                 write('Key in your sentence or stop. to quit'),
                 nl,  
                 getsentence(Sentence,user,[],[apo,comma],[apo,comma],nocaps),
                 % getting the sentence to be parsed from the user's terminal
                 decontract(Sentence,SD),
                 eraseall(pos),     % decontraction can change number
                 eraseall(fin),     % and position of words
                 % performing decontraction of auxiliaries
                 % e.g. didn + ' + t --> did + not / ' + re --> are
                 process(SD,Lists).

                 
   start(Outlist,HandleIn,Input) :-
                 Input \= stdin,
                 open(Outlist,write,Lists),
                 % open(File_Name,Mode,Stream)
                 recorda(out,Lists,_),
                 nl,
                 repeat,
                 clear,
                 statistics(cputime,TB),
                 recorda(time,TB),
                 nl,  
                 write('>>'),
                 getsentence(Sentence,HandleIn,
                             [],[apo,comma],[apo,comma],nocaps),
                 write(Sentence),nl,
                 decontract(Sentence,SD),
                 eraseall(pos),
                 eraseall(fin),
                 process(SD,Lists).




% start the whole thing 
%%%%%%%%%%%%%%%%%%%%%%%

process(Sentence,Stream) :- statistics(cputime,T1),
                            positions(Sentence,0,L),
                            nl,nl(Stream),write(L),writeq(Stream,L),nl(Stream),
                            runs.


/* 'positions' computes word positions starting from zero; 
the sentence with the word positions fed in (the L list) is then
printed on screen and in the results file

'runs' calls a quartet of run(Pass), where Pass is " stop ",
then " lex "(icon), then " gr "(ammar), then " output "

'run(Pass)' recursively calls itself, going through all
the 'rule_name ---> rule_body' pairs for that pass
when no new info is found, it fails,
because all the clauses for '--->' have a condition
(a 'not recorded' check included in the definition of 
the predicate 'build') which will lead to failure

before failing, the last pass, run(output), gets a chance to print
(once each) one or more parses 
spanning the whole S and displaying a [finite:yes] gapless FV pair not
marked for killing

the failing of 'runs' causes backtracking to the repeat goal in 'start'; 
the db is cleared and the process relaunched with a new S from the user

if the S happens to be 'stop.' the process halts by abortion (abort). */


%% computing string positions 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% each word in the list is assigned a starting and an ending
% position
%% not that the first word spans from position 0 to position 1 

     positions([Head|Tail],Pos,[Pos/Head|Tails]):-
                 Posand1 is Pos + 1,
                 recorda(pos,position(Pos,Posand1,Head),_),
                 positions(Tail,Posand1,Tails).
 
% position of the last word indicates string end 

     positions([],X,[endpos(X)]) :-
                 recorda(fin,fin(X),_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%
%% meta-interpreter 
%%%%%%%%%%%%%%%%%%%%

runs :- run(stop).
runs :- run(lex).
runs :- run(gr).
runs :- run(output).


run(Pass) :-  [Pass,Rule_name] ---> Condition_Action,
        testexec(Condition_Action),
        run(Pass).
        
% the [Pass,Rule_name] part does not give rise to any action ; 
% it is there only for guiding the process (through Pass)
% and for documentation and debugging purposes


testexec([]).
testexec([First|Rest]) :-
                       call(First),
                       testexec(Rest).

% testexec is simply a list traversal with execution
% of each element as Prolog goal



%%%%%%%%%%%%%%%
%% the lexicon 
%%%%%%%%%%%%%%%

%% pattern:
% lex(Textual_form,Record_Box_in_db,Feature_List) 


%%%%%%%%%%%%%%%%%%
%% macro expansion 
%%%%%%%%%%%%%%%%%%

% repeat-fail loop through the fail in mkverbs

% the 'OtherFeatures' feature below has the following format :
% ft:[feature_1, feature_2, ... feature_n]
% it houses features which it would be pointless to assign to all verbs
% such as features concerned with the treatment of negation
% (negtransport and negswitch)
% and features which are meant to embody constraints on the whole clause
% built around the predicate
% the latter are housed in a feature whose value is itself a list:
% pc:[predicate_constraint_1 ... predicate_constraint_n]
% the pc feature stores constraints that can only be met at clause level
% such as polarity
% an example is
%  ft:[pc:[agree(Lex,AgrSubj,AgrRef),
%               prolog:[constraint(AgrRef,AgrPoss)]]]
% where the pc constraints regard the agreement pattern exemplified  by
% [ANYONE [who can be expected [to want [to pride THEMSELVES on THEIR books]]]]
% should be told not to write them
% the call on 'agree', which will be performed only when the required info
% is available, involves the lex of the antecedent (anyone) as well as its
% agreement pattern (agr:[gender:G,number:sing,person:3]) and the agreement
% pattern of the reflexive pronoun
% (themselves: agr:[gender:G,number:plural,person:or([3,indef])]
% the 'prolog' directive infront of the second pc feature indicates
% that the feature that follows will be executed as a standard prolog goal;
% it will check the compatibility of the agreement pattern for the reflexive
% pronoun (themselves) and the possessive (their) - this check, unlike the one
% embodied in 'agree', can be performed locally, i.e. in the nonfinite clause
% [to pride THEMSELVES on THEIR books]


mkthem :-
 repeat,
 mkverbs,
 write('Done').

mkverbs :-

verb(Headlist,Class,Arglist,OtherFeatures),

member(v(Vs,Vpl,Vpret,Ven,Ving,Lex),Headlist),

% third person singular present tense
asserta(lex(Vs,v,[pos:v,class:Class,
                  txt:Vs,lex:Lex,tense:present,type:finite,
                  agr:[number:sing,person:3],Arglist,OtherFeatures])),

% first or second person singular present tense
asserta(lex(Vpl,v,[pos:v,class:Class,
                   txt:Vpl,lex:Lex,tense:present,type:finite,
                   agr:[number:sing,person:or([1,2])], Arglist,OtherFeatures])),
            
% all three persons plural present tense
asserta(lex(Vpl,v,[pos:v,class:Class,
                   txt:Vpl,lex:Lex,tense:present,type:finite,
                   agr:[number:plural,person:P],Arglist,OtherFeatures])),
            
% infinitive
asserta(lex(Vpl,v,[pos:v,class:Class,
                   txt:Vpl,lex:Lex,type:inf,tense:untensed,
                   agr:[number:Nb,person:P],Arglist,OtherFeatures])),
            
% preterite
asserta(lex(Vpret,v,[pos:v,class:Class,
                     txt:Vpret,lex:Lex,tense:past,type:finite,
                     agr:[number:Nb,person:P],Arglist,OtherFeatures])),
            
% past participle  (passive)
asserta(lex(Ven,v,[pos:v,class:Class,
                   txt:Ven,lex:Lex,type:en_passive,tense:untensed,
                   agr:[number:Nb,person:P],Arglist,OtherFeatures])),

% past participle   (active)
asserta(lex(Ven,v,[pos:v,class:Class,
                   txt:Ven,lex:Lex,type:en_active,tense:untensed,
                   agr:[number:Nb,person:P],Arglist,OtherFeatures])),

% ing form
asserta(lex(Ving,v,[pos:v,class:Class,
                    txt:Ving,lex:Lex,type:ing,tense:untensed,
                    agr:[number:Nb,person:P],Arglist,OtherFeatures])),

fail.


mkverbs.
% allows the fail boundary to be crossed



%%%%%%%%%%%
% VERBS
%%%%%%%%%%%

% BE as full verb
%%%%%%%%%%%%%%%%%

% its richer morphology is best handled directly by lex clauses
% not by macro-expansion of verb clauses
% the arglist specifies a complement that can be either a noun phrase
% or an adjective phrase (prepositional and adverbial phrases
% are left out of account here)

% third person singular, present tense
lex(is,v,[pos:v,class:be,
                  txt:is,lex:be,tense:present,type:finite,
                  agr:[number:sing,person:3],
                  arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[]],
                           complement:[type:or([np,adjp]),
                                       canon:1,gappable:yes,oblig:yes,
                                constraints:[]]],
ft:[]]).

% first person singular, present tense
lex(am,v,[pos:v,class:be,
                   txt:am,lex:be,tense:present,type:finite,
                   agr:[number:sing,person:1],
                   arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[]],
                           complement:[type:or([np,adjp]),
                                 canon:1,gappable:yes,oblig:yes,
                                constraints:[]]],
ft:[]]).
                   
% second person singular, present tense
lex(are,v,[pos:v,class:be,
                   txt:are,lex:be,tense:present,type:finite,
                   agr:[number:sing,person:2],
                   arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[]],
                           complement:[type:or([np,adjp]),
                               canon:1,gappable:yes,oblig:yes,
                                constraints:[]]],
ft:[]]).
                   
% all three persons plural present tense
lex(are,v,[pos:v,class:be,
                   txt:are,lex:be,tense:present,type:finite,
                   agr:[number:plural,person:P],
                   arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[]],
                           complement:[type:or([np,adjp]),
                              canon:1,gappable:yes,oblig:yes,
                                constraints:[]]],
ft:[]]).

% infinitive
lex(be,v,[pos:v,class:be,
                   txt:be,lex:be,type:inf,tense:untensed,
                   agr:[number:Nb,person:P],
                 arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[]],
                           complement:[type:or([np,adjp]),
                               canon:1,gappable:yes,oblig:yes,
                                constraints:[]]],
ft:[]]).

% preterite, first and third person, singular
lex(was,v,[pos:v,class:be,
                     txt:was,lex:be,tense:past,type:finite,
                     agr:[number:sing,person:or([1,3])],
                    arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[]],
                           complement:[type:or([np,adjp]),
                              canon:1,gappable:yes,oblig:yes,
                                constraints:[]]],
ft:[]]).
                     
% preterite, second person, singular
lex(were,v,[pos:v,class:be,
                     txt:were,lex:be,tense:past,type:finite,
                     agr:[number:sing,person:2],
                    arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[]],
                           complement:[type:or([np,adjp]),
                             canon:1,gappable:yes,oblig:yes,
                                constraints:[]]],
ft:[]]).

% preterite, all three persons, plural
lex(were,v,[pos:v,class:be,
                     txt:were,lex:be,tense:past,type:finite,
                     agr:[number:plural,person:P],
                    arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[]],
                           complement:[type:or([np,adjp]),
                           canon:1,gappable:yes,oblig:yes,
                                constraints:[]]],
ft:[]]).

% past participle   (active)
lex(been,v,[pos:v,class:be,
                   txt:been,lex:be,type:en_active,tense:untensed,
                   agr:[number:Nb,person:P],
                  arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[]],
                           complement:[type:or([np,adjp]),
                           canon:1,gappable:yes,oblig:yes,
                                constraints:[]]],
ft:[]]).

% ing form
lex(being,v,[pos:v,class:be,
             txt:being,lex:be,type:ing,tense:untensed,
                    agr:[number:Nb,person:P],
                   arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[]],
                           complement:[type:or([np,adjp]),
                             canon:1,gappable:yes,oblig:yes,
                                constraints:[]]],
ft:[]]).


%%%%%%%%%%%%%%%%%%%%
% Intransitive verbs
%%%%%%%%%%%%%%%%%%%%

% The constraints are constraints set by the predicate on its args

verb([v(hears,hear,heard,heard,hearing,hear)],intr,
arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[sem:[hum]]]],
ft:[]).

verb([v(holds,hold,held,held,holding,hold)],intr,
arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[sem:[abstract]]]],
ft:[]).

verb([v(reads,read,read,read,reading,read)],intr,
arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[sem:[hum]]]],
ft:[]).

verb([v(screams,scream,screamed,screamed,screaming,scream)],intr,
arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[sem:[hum]]]],
ft:[]).

verb([v(thinks,think,thought,thought,thinking,think)],intr,
arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[sem:[hum]]]],
ft:[]).

verb([v(walks,walk,walked,walked,walking,walk)],intr,
arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[sem:[hum]]]],
ft:[]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Intransitive verbs with adverbial particle
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% horse about/around
% a mwu

verb([v(horses,horse,horsed,horsed,horsing,horse_about_or_around)],intrprt,
                    arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[sem:[hum]]],
                            athematic:[type:part,canon:1,gappable:no,oblig:yes,
                         constraints:[pos:part,lex:or([about,around])]]],
ft:[]).


%%%%%%%%%%%%%%%%%%
% transitive verbs 
%%%%%%%%%%%%%%%%%%

verb([v(beats,beat,beat,beaten,beating,beat)],tr,
arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[sem:[hum]]],
                       object:[type:np,canon:1,gappable:yes,oblig:yes,
                                constraints:[]]],
ft:[]).

verb([v(budges,budge,budged,budged,budging,budge)],tr,
arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[]],
                       object:[type:np,canon:1,gappable:yes,oblig:yes,
                                constraints:[]]],
ft:[pc:[std([pol:neg])]]).

% the std (standard) directive ensures that the constraint is checked in
% the standard way, i.e. as a constraint on the clause the predicate fits in
% thus BUDGE can only appear in a clause of negative polarity
% (this is an oversimplification - a better treatment is provided for
% the idiom NOT_BUBGE_AN_INCH)


verb([v(buys,buy,bought,bought,buying,buy)],tr,
arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[sem:[hum]]],
                       object:[type:np,canon:1,gappable:yes,oblig:yes,
                                constraints:[sem:[thing]]]],
ft:[]).

verb([v(changes,change,changed,changed,changing,change)],tr,
arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[]],
                       object:[type:np,canon:1,gappable:yes,oblig:yes,
                                constraints:[]]],
ft:[]).

verb([v(considers,consider,considered,considered,considering,consider)],tr,
arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[sem:[hum]]],
                               object:[type:np,canon:1,gappable:yes,oblig:yes,
                                constraints:[sem:[abstract]]]],
ft:[]).

verb([v(digs,dig,dug,dug,digging,dig)],tr,
arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[sem:[hum]]],
                       object:[type:np,canon:1,gappable:yes,oblig:yes,
                                constraints:[sem:[thing]]]],
ft:[]).

verb([v(flogs,flog,flogged,flogged,flogging,flog)],tr,
arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[sem:[hum]]],
                       object:[type:np,canon:1,gappable:yes,oblig:yes,
                                constraints:[]]],
ft:[]).

verb([v(gives,give,gave,given,giving,give)],tr,
arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[]],
                       object:[type:np,canon:1,gappable:yes,oblig:yes,
                                constraints:[]]],
ft:[]).

verb([v(hears,hear,heard,heard,hearing,hear)],tr,
arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[sem:[hum]]],
                       object:[type:np,canon:1,gappable:yes,oblig:yes,
                                constraints:[]],
                       vp_modifier:[type:pp,canon:2,gappable:yes,oblig:no,
                                constraints:[prep:from]]],
ft:[]).

verb([v(hits,hit,hit,hit,hitting,hit)],tr,
arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[]],
                       object:[type:np,canon:1,gappable:yes,oblig:yes,
                                constraints:[]]],
ft:[]).

verb([v(holds,hold,held,held,holding,hold)],tr,
arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[]],
                       object:[type:np,canon:1,gappable:yes,oblig:yes,
                                constraints:[]]],
ft:[]).

verb([v(kicks,kick,kicked,kicked,kicking,kick)],tr,
arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[sem:[hum]]],
                       object:[type:np,canon:1,gappable:yes,oblig:yes,
                                constraints:[sem:[thing]]]],
ft:[]).

verb([v(knows,know,knew,known,knowing,know)],tr,
arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[sem:[hum]]],
                       object:[type:np,canon:1,gappable:yes,oblig:yes,
                                constraints:[]]],
ft:[]).

verb([v(likes,like,liked,liked,liking,like)],tr,
arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[sem:[hum]]],
                       object:[type:np,canon:1,gappable:yes,oblig:yes,
                                constraints:[]]],
ft:[]).

verb([v(makes,make,made,made,making,make)],tr,
arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[]],
                       object:[type:np,canon:1,gappable:yes,oblig:yes,
                                constraints:[]]],
ft:[]).
                                
verb([v(minces,mince,minced,minced,mincing,mince)],tr,
arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[]],
                       object:[type:np,canon:1,gappable:yes,oblig:yes,
                                constraints:[sem:[meat]]]],
ft:[]).
                                
verb([v(moves,move,moved,moved,moving,move)],tr,
arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[]],
                       object:[type:np,canon:1,gappable:yes,oblig:yes,
                                constraints:[]]],
ft:[]).
                                
verb([v(reads,read,read,read,reading,read)],tr,
arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[sem:[hum]]],
                       object:[type:np,canon:1,gappable:yes,oblig:yes,
                                constraints:[sem:[document]]]],
ft:[]).

verb([v(spills,spill,spilled,spilled,spilling,spill),
      v(spills,spill,spilt,spilt,spilling,spill)],tr,
arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[]],
                       object:[type:np,canon:1,gappable:yes,oblig:yes,
                                constraints:[sem:[thing]]]],
ft:[]).


verb([v(swaps,swap,swapped,swapped,swapping,swap)],tr,
arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[]],
                       object:[type:np,canon:1,gappable:yes,oblig:yes,
                                constraints:[]]],
ft:[]).

verb([v(teaches,teach,taught,taught,teaching,teach)],tr,
arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[sem:[hum]]],
                       object:[type:np,canon:1,gappable:yes,oblig:yes,
                                constraints:[]]],
ft:[]).


verb([v(writes,write,wrote,written,writing,write)],tr,
arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[sem:[hum]]],
                       object:[type:np,canon:1,gappable:yes,oblig:yes,
                                constraints:[sem:[document]]]],
ft:[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% transitive verbs with complement clause as object
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% A %%%%

/* with THAT - the THAT prevents gapping of the subject of the complement S :
 *"the teacher who the inspector thinks that cocked a snook at the students" */

verb([v(thinks,think,thought,thought,thinking,think)],trs_that,
                arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[sem:[hum]]],
                       athematic:[type:subordinator,canon:1,gappable:no,
                                  oblig:yes,constraints:[lex:that]],
                       object:[type:pred,canon:2,gappable:no,oblig:yes,
                                constraints:[finite:yes]]],
ft:[negtransport:yes]).
% the negtransport feature ensures that the predicate's ability to send down
% the negation is taken into account:
% she did not think he would mince his words

                                
verb([v(doubts,doubt,doubted,doubted,doubting,doubt)],trs_that,
               arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[sem:[hum]]],
                       athematic:[type:subordinator,canon:1,gappable:no,
                                  oblig:yes,constraints:[lex:that]],
                       object:[type:pred,canon:2,gappable:no,oblig:yes,
                               context:nonaff,
                               constraints:[finite:yes]]],
ft:[negswitch:yes]).
% the negswitch feature ensures that the predicate's ability to reverse the
% polarity of the dependent clause is recognised:
% i doubted that he would want to mince his words


%%% B %%%%

/* no complementizer : the subject of the complement S can be gapped:
       "the teacher who the inspector thinks cocked a snook at the students" */

verb([v(thinks,think,thought,thought,thinking,think)],trs,
              arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[sem:[hum]]],
                       object:[type:pred,canon:2,gappable:no,oblig:yes,
                                constraints:[finite:yes]]],
ft:[negtransport:yes]).


                                
verb([v(doubts,doubt,doubted,doubted,doubting,doubt)],trs,
              arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[sem:[hum]]],
                       object:[type:pred,canon:2,gappable:no,oblig:yes,
                               context:nonaff,
                               constraints:[finite:yes]]],
ft:[negswitch:yes]).


% doubt if, whether
%%%%%%%%%%%%%%%%%%%%

% i doubt whether he will mince his words

% note that this 'doubt' (unlike 'doubt that...') cannot be used negatively
% i doubt whether he will write the book
% i don't doubt that he will write the book
% * he doesn't doubt whether he will write the book

% the ifthenelse action in the pc field ensures that if the clause
% built around this 'doubt' is negative it will be marked as a candidate
% for the filter to get rid of

% the constraints on the object arg clause ensure that the clause
% either has not been marked as to be filtered out or not to be filtered out
% (unspec value of the kill feature)
% or has been marked as to be filtered out because of the wrong polarity
% if it were used on the top level
% a clause such as 'he will mince words' should be filtered out
% UNLESS it is the object of such verbs as doubt which create their own
% non-affirmative context, in which positive is read as negative:
% i doubt whether he will mince words
% * i doubt whether he won't mince words

verb([v(doubts,doubt,doubted,doubted,doubting,doubt)],trs_if_whether,
             arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[sem:[hum]]],
                       athematic:[type:subordinator,canon:1,gappable:no,
                                  oblig:yes,constraints:[lex:or([whether,if])]],
                       object:[type:pred,canon:2,gappable:no,oblig:yes,
                               context:nonaff,
                               constraints:[finite:yes,
                                            kill:or([yes,unspec])]]],
ft:[pc:[ifthenelse(check(pol:neg),add(kill:yes),add(kill:unspec))]]).
                               

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% verbs with object and object_predicate
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

verb([v(considers,consider,considered,considered,considering,consider)],comp,
              arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[sem:[hum]]],
                       object:[type:np,canon:1,gappable:yes,oblig:yes,
                                constraints:[]],
                       object_predicate:[type:np,canon:2,gappable:no,oblig:yes,
                                constraints:[]]],
              ft:[]).

verb([v(declares,declare,declared,declared,declaring,declare)],comp,
               arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[sem:[hum]]],
                       object:[type:np,canon:1,gappable:yes,oblig:yes,
                                constraints:[]],
                       object_predicate:[type:np,canon:2,gappable:no,oblig:yes,
                                constraints:[]]],
              ft:[]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% transitive verbs with adverbial particle
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% brush aside
% a mwu


verb([v(brushes,brush,brushed,brushed,brushing,brush_aside)],trprt,
                   arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[sem:[hum]]],
                            object:[type:np,canon:1,gappable:yes,oblig:yes,
                                constraints:[sem:[abstract]]],
                            athematic:[type:part,canon:1,gappable:no,oblig:yes,
                                constraints:[pos:part,lex:aside]]],
                   ft:[]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% verbs with prepositional complement
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

verb([v(approves,approve,approved,approved,approving,approve)],vprep,
arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[sem:[hum]]],
         pp_arg:[type:pp,canon:1,gappable:yes,oblig:yes,
                         constraints:[prep:of]]],
ft:[]).


verb([v(disapproves,disapprove,disapproved,disapproved,
         disapproving,disapprove)],vprep,
arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[sem:[hum]]],
         pp_arg:[type:pp,canon:1,gappable:yes,oblig:yes,
                         constraints:[prep:of]]],
ft:[]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% transitive verbs with object and prepositional complement
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

verb([v(borrows,borrow,borrowed,borrowed,borrowing,borrow)],trprep,
arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[sem:[hum]]],
                       object:[type:np,canon:1,gappable:yes,oblig:yes,
                                constraints:[]],
                       pp_arg:[type:pp,canon:2,gappable:yes,oblig:no,
                         constraints:[prep:from]]],
ft:[]).

verb([v(explains,explain,explained,explained,explaining,explain)],trprep,
              arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[sem:[hum]]],
                       object:[type:np,canon:1,gappable:yes,oblig:yes,
                                constraints:[sem:[abstract]]],
                       pp_arg:[type:pp,canon:2,gappable:yes,oblig:no,
                         constraints:[prep:to,c_str:[arg_prep:[sem:[hum]]]]]],
              ft:[]).


% in alternation with ditransitive give : gave a book to the teacher/
%                                          gave the teacher a book

verb([v(gives,give,gave,given,giving,give)],trprep,
              arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[sem:[hum]]],
                       object:[type:np,canon:1,gappable:yes,oblig:yes,
                                constraints:[]],
                       pp_arg:[type:pp,canon:2,gappable:yes,oblig:yes,
                         constraints:[prep:to,c_str:[arg_prep:[sem:[hum]]]]]],
              ft:[]).


verb([v(takes,take,took,taken,taking,take)],trprep,
arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[]],
                       object:[type:np,canon:1,gappable:yes,oblig:yes,
                                constraints:[]],
                     pp_arg:[type:pp,canon:2,gappable:yes,oblig:no,
                         constraints:[prep:or([from,out_of,to,into])]]],
ft:[]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ditransitive verbs : indirect object + direct object
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

verb([v(gives,give,gave,given,giving,give)],ditr,
              arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[sem:[hum]]],
                       i_object:[type:np, canon:1,gappable:yes, oblig:yes,
                                constraints:[sem:[hum]]],
                       object:[type:np,canon:2,gappable:yes,oblig:yes,
                                constraints:[]]],
ft:[]).

                         
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TRANSITIVE VERB WITH INFINITIVE PREDICATE COMPLEMENT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ACTIVE VOICE
% INFINITIVE HAS ITS OWN SUBJECT
% they expect [teachers to write books]
% the object arg is a full non-finite predication
% not a truncated predicate taking the form of a non-finite vp
% the difference is implemented in the value of the 'finite' feature:
% it is TO here, whereas it is TOVP when a subjectless VP is expected
% (as in "he seems to write books" / "i want to read them")

verb([v(expects,expect,expected,expected,expecting,expect)],tr_inf,
              arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[sem:[hum]]],
                       object:[type:pred,canon:2,gappable:no,oblig:yes,
                                constraints:[finite:to]]],
ft:[negtransport:yes,pc:[std([voice:active])]]).


 verb([v(wants,want,wanted,wanted,wanting,want)],tr_inf,
              arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[sem:[hum]]],
                       object:[type:pred,canon:2,gappable:no,oblig:yes,
                                constraints:[finite:to ]]],
ft:[negtransport:yes,pc:[std([voice:active])]]).
                                             


verb([v(likes,like,liked,liked,liking,like)],tr_inf,
              arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[sem:[hum]]],
                       object:[type:pred,canon:2,gappable:no,oblig:yes,
                                constraints:[finite:to ]]],
ft:[negtransport:yes,pc:[std([voice:active])]]).
                                             

verb([v(hates,hate,hated,hated,hating,hate)],tr_inf,
               arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[sem:[hum]]],
                       object:[type:pred,canon:2,gappable:no,oblig:yes,
                                constraints:[finite:to]]],
ft:[ negtransport:yes,pc:[std([voice:active])]]).
                                             

% PASSIVE VOICE
% THE SUBJECT OF THE INFINITIVE IS PROMOTED TO SUBJECT OF THE MAIN CLAUSE
% teachers-e are expected [-e to write books]
% the instantiations of the subject in the lower clause
% and the checking of constraints on the subject are taken care of
% in the higher clause
% we simply parse a (subjectless) vp to satisfy the object arg

verb([v(expects,expect,expected,expected,expecting,expect)],tr_inf_pass,
              arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[sem:[hum]]],
                       object:[type:pred,canon:2,gappable:no,oblig:yes,
                                constraints:[finite:tovp]]],
ft:[negtransport:yes,pc:[std([voice:passive])]]).



% ACTIVE VOICE
% THE INFINITIVE HAS NO SUBJECT OF ITS OWN
% the same strategy as in the passive voice is used
% we satisfy the object by parsing a subjectless vp

verb([v(expects,expect,expected,expected,expecting,expect)],tr_vp,
              arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[sem:[hum]]],
                       object:[type:pred,canon:2,gappable:no,oblig:yes,
                                constraints:[finite:tovp ]]],
ft:[negtransport:yes]).


 verb([v(wants,want,wanted,wanted,wanting,want)],tr_vp,
              arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[sem:[hum]]],
                       object:[type:pred,canon:2,gappable:no,oblig:yes,
                                constraints:[finite:tovp ]]],
ft:[negtransport:yes]).



verb([v(likes,like,liked,liked,liking,like)],tr_vp,
              arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[sem:[hum]]],
                       object:[type:pred,canon:2,gappable:no,oblig:yes,
                                constraints:[finite:tovp ]]],
ft:[negtransport:yes]).


verb([v(hates,hate,hated,hated,hating,hate)],tr_vp,
              arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[sem:[hum]]],
                       object:[type:pred,canon:2,gappable:no,oblig:yes,
                                constraints:[finite:tovp ]]],
ft:[negtransport:yes]).

verb([v(refuses,refuse,refused,refused,refusing,refuse)],tr_vp,
              arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[sem:[hum]]],
                       object:[type:pred,canon:2,gappable:no,oblig:yes,
                                constraints:[finite:tovp ]]],
ft:[negswitch:yes]).
% e.g. she refuses to mince words


% type BELIEVE
% there must be a subject to the infinitive
% therefore 'believe' is not included among the tr_vp verbs,
% but only among the 'tr_inf' verbs

% ACTIVE
% cf EXPECT

 verb([v(believes,believe,believed,believed,believing,believe)],
              tr_inf,
             arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[sem:[hum]]],
                       object:[type:pred,canon:2,gappable:no,oblig:yes,
                                constraints:[finite:to ]]],
 ft:[negtransport:yes,pc:[std([voice:active])]]).


% PASSIVE
% cf EXPECT

verb([v(believes,believe,believed,believed,believing,believe)],
              tr_inf_pass,
              arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[sem:[hum]]],
                       object:[type:pred,canon:2,gappable:no,oblig:yes,
                                constraints:[finite:tovp ]]],
ft:[negtransport:yes,pc:[std([voice:passive])]]).


% type SEEM
% the object pred inherits its subject from the main clause
% we parse a subjectless vp for the extraposed subject

 verb([v(seems,seem,seemed,seemed,seeming,seem)],
              tr_inf_gap,
              arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[]],
                       extraposed_subject:[type:pred,canon:2,
                                 gappable:no,oblig:yes,
                                constraints:[finite:tovp ]]],
ft:[negtransport:yes]).
                                             

% it seems (that) S
%%%%%%%%%%%%%%%%%%%%

verb([v(seems,seem,seemed,seemed,seeming,seem)],
              extrapos_subj,
              arglist:[subject:[type:np, canon:0,gappable:no, oblig:yes,
                                constraints:[c_str:[head:[lex:pp3sgneuter]]]],
                       athematic:[type:subordinator,canon:1,gappable:no,
                                  oblig:no,constraints:[lex:that]],
                       extraposed_subject:[type:pred,canon:2,
                                gappable:no,oblig:yes,
                                constraints:[finite:yes]]],
ft:[negtransport:yes]).


% type TELL or ASK
% the indirect object controls the ghosted subject in the clausal object
% it is gappable and can therefore be made the subject of a passive S
% i tell the teacher [[e:the_teacher] to write a book]
% the teacher is told [[e:the_teacher] to write a book]
% the object arg is parsed as a subjectless vp and
% subject assignment and constraint checking is done in the higher clause

verb([v(asks,ask,asked,asked,asking,ask)],tr_io_inf,
              arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[sem:[hum]]],
                       i_object:[type:np, canon:1,gappable:yes, oblig:yes,
                                constraints:[sem:[hum]]],
                       object:[type:pred,canon:2,gappable:no,oblig:yes,
                                constraints:[finite:tovp ]]],
ft:[negtransport:yes]).

verb([v(tells,tell,told,told,telling,tell)],tr_io_inf,
              arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[sem:[hum]]],
                       i_object:[type:np, canon:1,gappable:yes, oblig:yes,
                                constraints:[sem:[hum]]],
                       object:[type:pred,canon:2,gappable:no,oblig:yes,
                                constraints:[finite:tovp ]]],
ft:[ negtransport:yes]).



%%%%%%%%%%%%%%%%%%
% MULTI-WORD UNITS
%%%%%%%%%%%%%%%%%%


% BEAR etc THE BRUNT
%%%%%%%%%%%%%%%%%%%%

verb([v(bears,bear,bore,borne,bearing,bear_the_brunt),
      v(carries,carry,carried,carried,carrying,carry_the_brunt),
      v(takes,take,took,taken,taking,take_the_brunt),
      v(faces,face,faced,faced,facing,face_the_brunt),
      v(catches,catch,caught,caught,catching,catch_the_brunt)],
      mwu_trprep,
      arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[]],
                       object:[type:np,canon:1,gappable:yes,oblig:yes,
                                constraints:[c_str:[det:[lex:the]],
                                  c_str:[head:[lex:brunt,agr:[number:sing]]]]],
                       pp_arg:[type:pp,canon:2,gappable:yes,oblig:no,
                                constraints:[prep:of]]],
      ft:[]).


% BEAT/FLOG A DEAD HORSE
%%%%%%%%%%%%%%%%%%%%%%%%

verb([v(beats,beat,beat,beaten,beating,beat_dead_horse),
      v(flogs,flog,flogged,flogged,flogging,flog_dead_horse)],
               mwutr,
               arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                 constraints:[sem:[hum]]],
                        object:[type:np,canon:1,gappable:yes,oblig:yes,
                            constraints:[c_str:[adjp:[c_str:[head:[lex:dead]]],
                                                    head:[lex:horse]]]]],
               ft:[]).

% CHANGE/SWAP HORSES IN MIDSTREAM
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

verb([v(changes,change,changed,changed,changing,change_horses_in_midstream), 
      v(swaps,swap,swapped,swapped,swapping,swap_horses_in_midstream)],
      mwu_trprep,
      arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[sem:[hum]]],
                       object:[type:np,canon:1,gappable:yes,oblig:yes,
                                constraints:[c_str:[head:[txt:horses,
                                                          type:idiom]]]],
                       pp_arg:[type:pp,canon:2,gappable:no,oblig:yes,
                                constraints:[lex:in_midstream]]],
      ft:[]).


% NOT BUDGE/GIVE/MOVE AN INCH
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% in all the mwus that are meant to be used exclusively in negative contexts
% (or at least non-affirmative contexts - see 'doubt' above)
% we will find the same ifthenelse directive in the pc field:
% the clause containing the idiom is marked for filtering out if its polarity
% is not neg
% if the clause is not used as an object at a higher level
% (e.g. object of 'doubt whether' etc)
% it will be discarded by the last pass the output pass
% it can still be redeemed by a higher clause which will accept
% its kill:yes feature and will not percolate it at its own (top) level,
% the only level the output filter acts on

 verb([v(moves,move,moved,moved,moving,not_move_an_inch),
       v(budges,budge,budged,budged,budging,not_budge_an_inch),
       v(gives,give,gave,given,giving,not_give_an_inch)],
      mwu_tr,
      arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[sem:[hum]]],
               object:[type:np,canon:1,gappable:no,oblig:yes,
                            constraints:[c_str:[det:[lex:a]],
                             c_str:[head:[lex:inch,agr:[number:sing]]]]]
                       ],
      ft:[pc:[ifthenelse(check(pol:neg),add(kill:no),add(kill:yes))]]).

% NOT KNOW THE FIRST THING ABOUT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 verb([v(knows,know,knew,known,knowing,not_know_the_first_thing_about)] ,
      mwu_tr,
      arglist:[
              subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[sem:[hum]]],

              object:[type:np,canon:1,gappable:no,oblig:yes,
                  constraints:[c_str:[det:[lex:the]],
                               c_str:[adjp:[c_str:[head:[lex:first]]]],
                               c_str:[head:[lex:thing,agr:[number:sing]]]]],
                      
              pp_arg:[type:pp,canon:2,gappable:yes,oblig:yes,
                                constraints:[prep:about]]
                ],
     ft:[pc:[ifthenelse(check(pol:neg),add(kill:no),add(kill:yes))]]).

% NOT MINCE (ONE'S) WORDS
%%%%%%%%%%%%%%%%%%%%%%%%%

% the 'agree' predicate ensures that the agreement features of the subject
% do not clash with the agreement features assigned to the possessor
% (agrposs feature associated with the possessives my, your, his, her, etc.)

% it requires an independent predicate with an arg containing the lexeme of
% the subject to deal with the complex agreement patterns displayed by
% the indefinite pronouns someone, somebody, anyone, anybody and noone nobody
% nobody -> his / her / his or her / their
% (similarly : himself, herself, himself or herself, themselves, and even:
%              herself or himself, him or herself, her or himself)

% agree is defined as follows:

/*
agree(Lex,SAgr,PossRefAgr) :-
(member(Lex,[someone,somebody,anyone,anybody,noone,nobody]) ->
   constraint([person:or([3,indef])],PossRefAgr);
   constraint(SAgr,PossRefAgr)).
*/

% won't mince his words
verb([v(minces,mince,minced,minced,mincing,not_mince_ones_words)] ,
      mwu_tr,
      arglist:[
               subject:[type:np, canon:0,gappable:yes, oblig:yes,
                        constraints:[sem:[hum],
                                      lex:Lex,
                                     agr:AgrSubj]],
               object:[type:np,canon:1,gappable:no,oblig:yes,
                       constraints:[c_str:[det:[type:poss_adj,
                          agrposs:AgrPoss]],
                                    c_str:[head:[txt:words]]]]
               ],
      ft:[pc:[ifthenelse(check(pol:neg),add(kill:no),add(kill:yes)),
              agree(Lex,AgrSubj,AgrPoss)]]).

% minces no words
verb([v(minces,mince,minced,minced,mincing,not_mince_words)] ,
      mwu_tr,
      arglist:[
               subject:[type:np, canon:0,gappable:yes, oblig:yes,
                        constraints:[sem:[hum]]],
               object:[type:np,canon:1,gappable:no,oblig:yes,
                       constraints:[c_str:[det:[type:quant]],
                                    c_str:[head:[txt:words]]]]
               ],
      ft:[pc:[ifthenelse(check(pol:neg),add(kill:no),add(kill:yes))]]).

% won't mince words
verb([v(minces,mince,minced,minced,mincing,not_mince_words)] ,
      mwu_tr,
      arglist:[
               subject:[type:np, canon:0,gappable:yes, oblig:yes,
                        constraints:[sem:[hum]]],
               object:[type:np,canon:1,gappable:no,oblig:yes,
                       constraints:[c_str:[det:[det:zero]],
                                    c_str:[head:[txt:words]]]]
               ],
      ft:[pc:[ifthenelse(check(pol:neg),add(kill:no),add(kill:yes))]]).


               
%%%%%%%%%%%%%%%%%%%%
% THE 'HAVOC' FAMILY
%%%%%%%%%%%%%%%%%%%%


% [CAUSE,CREATE,WREAK] HAVOC (PP)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

verb([v(causes,cause,caused,caused,causing,cause_havoc),
      v(creates,create,created,created,creating,create_havoc),
      v(wreaks,wreak,wreaked,wreaked,wreaking,wreak_havoc),
      v(wreaks,wreak,wrought,wrought,wreaking,wreak_havoc)],
      mwu_trprep,
      arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[]],
                       object:[type:np,canon:1,gappable:yes,oblig:yes,
                                constraints:[c_str:[head:[lex:havoc]]]],
                       pp_arg:[type:pp,canon:2,gappable:yes,oblig:no,
          constraints:[prep:or([against,among,around,at,for,in,on,to,with])]]],
      ft:[]).


% MAKE HAVOC OF
%%%%%%%%%%%%%%%

verb([v(makes,make,made,made,making,make_havoc_of)],mwu_trprep,
              arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[]],
                       object:[type:np,canon:1,gappable:yes,oblig:yes,
                                constraints:[c_str:[head:[lex:havoc]]]],
                       pp_arg:[type:pp,canon:2,gappable:yes,oblig:yes,
                                constraints:[prep:of]]],
              ft:[]).
         
% PLAY HAVOC WITH
%%%%%%%%%%%%%%%%%

verb([v(plays,play,played,played,playing,play_havoc_with)],mwu_trprep,
             arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[]],
                       string:[type:string,canon:1,gappable:no,oblig:yes,
                                constraints:[lex:havoc]],
                       pp_arg:[type:pp,canon:2,gappable:yes,oblig:yes,
                                constraints:[prep:with]]],
              ft:[]).


% BORROW/TAKE A LEAF OUT OF/FROM SOMEONE'S BOOK
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

verb([v(borrows,borrow,borrowed,borrowed,borrowing,
                                       borrow_a_leaf_from_someones_book),
      v(takes,take,took,taken,taking,
                                       take_a_leaf_from_someones_book)],
      mwu_trprep,
      arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[sem:[hum]]],
                       object:[type:np,canon:1,gappable:yes,oblig:yes,
                                constraints:[c_str:[head:[lex:leaf]]]],
                       pp_arg:[type:pp,canon:2,gappable:yes,oblig:yes,
      constraints:[prep:or([from,out_of]),
                   c_str:[arg_prep:[c_str:[det:[type:or([poss_adj,genitive])]],
                                    c_str:[head:[txt:book]]]]]]

      ],
      ft:[]).

         
% COCK A SNOOK AT
%%%%%%%%%%%%%%%%%

% in a full size lexicon this entry 'cock' would be found alongside
% other entries for verb cock

verb([v(cocks,cock,cocked,cocked,cocking,cock_a_snook_at)],
            mwu_trprep,
            arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[sem:[hum]]],
                       object:[type:np,canon:1,gappable:yes,oblig:yes,
                     constraints:[c_str:[head:[lex:snook,agr:[number:sing]]]]],
                       pp_arg:[type:pp,canon:2,gappable:yes,oblig:no,
                                constraints:[prep:at]]],
            ft:[]).

% [lex:snook,agr:[number:sing]] OR [txt:snook]


% MAKE AN EXAMPLE OF
%%%%%%%%%%%%%%%%%%%%

% double analysis to enable the double passive

% analysis one : an example was made of the teachers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 verb([v(makes,make,made,made,making,make_an_example_of)],
       mwu_trprep,
       arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                  constraints:[sem:[hum]]],
               object:[type:np,canon:1,gappable:yes,oblig:yes,
                  constraints:[c_str:[det:[txt:an],
                                  head:[txt:example]]]],
               pp_arg:[type:pp,canon:2,gappable:yes,oblig:no,
                                constraints:[prep:of]]],
        ft:[]).

 % analysis two : the teachers were made an example of
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 
 % here the dangling prep is analysed as athematic
 % and the arg_prep inside the pp_arg is raised to top-level arg status
 % and, being gappable, is candidate for promotion to subject
 % in passive clauses
 
 verb([v(makes,make,made,made,making,make_an_example_of)],
        mwu_trprep,
        arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                      constraints:[sem:[hum]]],
                 object:[type:np,canon:1,gappable:yes,oblig:yes,
                      constraints:[c_str:[det:[txt:an],
                                  head:[txt:example]]]],
                 athematic:[type:prep,canon:2,gappable:no,
                                  oblig:yes,constraints:[lex:of]],
                 arg_prep:[type:np,canon:3,gappable:yes,oblig:yes,
                                constraints:[]]],
        ft:[]).


% DIG ONE'S OWN GRAVE
%%%%%%%%%%%%%%%%%%%%%

verb([v(digs,dig,dug,dug,digging,dig_one_s_own_grave)],
  mwutr,
  arglist:[
      subject:[type:np, canon:0,gappable:yes, oblig:yes,
                    constraints:[sem:[hum],lex:Lex,agr:AgrSubj]],
      object:[type:np,canon:1,gappable:yes,oblig:yes,
                    constraints:[c_str:[det:[type:poss_adj,agrposs:AgrPoss]],
                                 c_str:[adjp:[c_str:[head:[lex:own]]]],
                                 c_str:[head:[lex:grave]]]]],
  ft:[pc:[agree(Lex,AgrSubj,AgrPoss)]]).


% PRIDE ONESELF ON ONE'S X
%%%%%%%%%%%%%%%%%%%%%%%%%%%

% the constraint(AgrRef,AgrPoss) ensures that the values checked
% by 'agree' on the AgrRef feature do not clash with the ones associated
% with the possessive:
% we shouldn't pride ourselves on our looks
% agree: we, ourselves  and then constraint: ourselves, our

verb([v(prides,pride,prided,prided,priding,pride_oneself_on)],mwutrprep,
     arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                       constraints:[sem:[hum],
                                   lex:Lex,
                                   agr:AgrSubj]],
                                             
              object:[type:np,canon:1,gappable:no,oblig:yes,
                        constraints:[type:refp,agr:AgrRef]],
                                
              pp_arg:[type:pp,canon:2,gappable:yes,oblig:yes,
                       constraints:[prep:on,
             c_str:[arg_prep:[c_str:[det:[type:poss_adj,agrposs:AgrPoss]]]]]]],
       ft:[pc:[agree(Lex,AgrSubj,AgrRef),prolog:[constraint(AgrRef,AgrPoss)]]]).


% GIVE SBY HIS DUE
%%%%%%%%%%%%%%%%%%

verb([v(gives,give,gave,given,giving,give_somebody_his_due)],ditr,
              arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[sem:[hum]]],
                       i_object:[type:np, canon:1,gappable:yes, oblig:yes,
                                constraints:[sem:[hum],lex:LexIO,agr:AgrIO]],
                       object:[type:np,canon:2,gappable:no,oblig:yes,
                       constraints:[c_str:[det:[type:poss_adj,agrposs:AgrPoss]],
                                    c_str:[head:[lex:due]]]
                                ]],
ft:[pc:[agree(LexIO,AgrIO,AgrPoss)]]).



% HAVE IN COMMON
%%%%%%%%%%%%%%%%%

% teachers have a lot of problems in common with students

verb([v(has,have,had,had,having,have_in_common)],mwu_trprep,
             arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[]],
                       object:[type:np,canon:1,gappable:yes,oblig:yes,
                                constraints:[]],
                       pp_arg:[type:pp,canon:2,gappable:no,oblig:yes,
                                constraints:[lex:in_common]],
                       pp_arg:[type:pp,canon:3,gappable:yes,oblig:yes,
                                constraints:[prep:with]]],
              ft:[]).


% teachers have a lot of problems in common

verb([v(has,have,had,had,having,have_in_common)],mwu_trprep,
             arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[agr:[number:plural]]],
                       object:[type:np,canon:1,gappable:yes,oblig:yes,
                                constraints:[]],
                       pp_arg:[type:pp,canon:2,gappable:no,oblig:yes,
                                constraints:[lex:in_common]]],
              ft:[]).


% HOLD AT BAY
%%%%%%%%%%%%%

verb([v(holds,hold,held,held,holding,hold_at_bay)],mwu_trprep,
              arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[sem:[hum]]],
                       object:[type:np,canon:1,gappable:yes,oblig:yes,
                                constraints:[]],
                       pp_arg:[type:pp,canon:2,gappable:no,oblig:yes,
                                constraints:[lex:at_bay]]],
              ft:[]).


% HOLD ONE'S HORSES
%%%%%%%%%%%%%%%%%%%

verb([v(holds,hold,held,held,holding,hold_one_s_horses)],mwutr,
               arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                            constraints:[sem:[hum],
                                         lex:Lex,
                                         agr:AgrSubj]],
                        object:[type:np,canon:1,gappable:no,oblig:yes,
                            constraints:[c_str:[det:[type:poss_adj,
                                          agrposs:AgrPoss],
                                          head:[txt:horses, type:idiom]]]]],
               ft:[pc:[agree(Lex,AgrSubj,AgrPoss)]]).



% KICK THE BUCKET
%%%%%%%%%%%%%%%%%

verb([v(kicks,kick,kicked,kicked,kicking,kick_the_bucket)],mwutr,
               arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                 constraints:[sem:[hum]]],
                        object:[type:np,canon:1,gappable:no,oblig:yes,
                                constraints:[c_str:[det:[lex:the],
                                              head:[txt:bucket,type:idiom]]]]],
               ft:[]).


% SPILL THE BEANS
%%%%%%%%%%%%%%%%%

verb([v(spills,spill,spilled,spilled,spilling,spill_the_beans), 
      v(spills,spill,spilt,spilt,spilling,spill_the_beans)],
      mwutr,
             arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                 constraints:[sem:[hum]]],
                        object:[type:np,canon:1,gappable:yes,oblig:yes,
                                constraints:[c_str:[det:[lex:the],
                                              head:[txt:beans,type:idiom]]]]],
      ft:[]).


% SHOUT/SCREAM THE PLACE/HOUSE DOWN
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

verb([v(shouts,shout,shouted,shouted,shouting,
                             shout_the_place_or_house_down),
      v(screams,scream,screamed,screamed,screaming,
                             scream_the_place_or_house_down)],
      mwutrprt,
                   arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[sem:[hum]]],
                            string:[type:string,canon:1,gappable:no,oblig:yes,
                                constraints:[lex:or([the_place,the_house])]],
                            athematic:[type:part,canon:2,gappable:no,oblig:yes,
                                constraints:[pos:part,lex:down]]],
      ft:[]).


% 'standard' shout down 

verb([v(shouts,shout,shouted,shouted,shouting,shout_down)],
    trprt,
    arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                constraints:[sem:[hum]]],
             object:[type:np,canon:1,gappable:yes,oblig:yes,
                                constraints:[sem:[hum]]],
             athematic:[type:part,canon:1,gappable:no,oblig:yes,
                                constraints:[pos:part,lex:down]]],
    ft:[]).


% (THE) SHIT HITS THE FAN
%%%%%%%%%%%%%%%%%%%%%%%%%

verb([v(hits,hit,hit,hit,hitting,the_shit_hit_the_fan)],mwupred,
               arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
                                 constraints:[c_str:[head:[txt:shit]]]],
                        object:[type:np,canon:1,gappable:yes,oblig:yes,
                                constraints:[c_str:[det:[lex:the]],
                                             c_str:[head:[txt:fan]]]]],
               ft:[]).


% THE GO THROUGH FAMILY
%%%%%%%%%%%%%%%%%%%%%%%

/*
% Oxford Dictionary of Current Idiomatic English
% Vol 1 : Verbs with Prepositions and Particles
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% it didn't take Albert very long to go through his inheritance
verb([v(goes,go,went,gone,going,go_through_1_consume)],v_mwu_prep,
arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
constraints:[sem:[hum]]],
         athematic:[type:prep,canon:2,gappable:no,
                                  oblig:yes,constraints:[lex:through]],
                 arg_prep:[type:np,canon:3,gappable:yes,oblig:yes,
constraints:[c_str:[head:[lex:Lex]]]]],
        ft:[pc:[coll(arg_prep,Lex,[stock,store,food,beer,fortune])]]).


% it was obvious that the room had been gone through by an intruder
verb([v(goes,go,went,gone,going,go_through_2_search)],v_mwu_prep,
arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
constraints:[sem:[hum]]],
         athematic:[type:prep,canon:2,gappable:no,
                                  oblig:yes,constraints:[lex:through]],
                 arg_prep:[type:np,canon:3,gappable:yes,oblig:yes,
constraints:[c_str:[head:[lex:Lex]]]]],
        ft:[pc:[coll(arg_prep,Lex,[room,pocket,paper])]]).

% they finally went through the marriage ceremony for the sake of their children
verb([v(goes,go,went,gone,going,go_through_3_perform)],v_mwu_prep,
arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
constraints:[sem:[hum]]],
         athematic:[type:prep,canon:2,gappable:no,
                                  oblig:yes,constraints:[lex:through]],
         arg_prep:[type:np,canon:3,gappable:yes,oblig:yes,
                                      constraints:[c_str:[head:[lex:Lex]]]]],
ft:[pc:[coll(arg_prep,Lex,[marriage,initiation,
                          matriculation,ceremony])]]).

% they went through the details of the plan over and over again
verb([v(goes,go,went,gone,going,go_through_4_rehearse)],v_mwu_prep,
arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
constraints:[sem:[hum]]],
         athematic:[type:prep,canon:2,gappable:no,
                                  oblig:yes,constraints:[lex:through]],
                 arg_prep:[type:np,canon:3,gappable:yes,oblig:yes,
constraints:[c_str:[head:[lex:Lex]]]]],
        ft:[pc:[coll(arg_prep,Lex,[fact,argument,scene,text])]]).

% his book went through ten editions in a year
verb([v(goes,go,went,gone,going,go_through_5_be_published)],v_mwu_prep,
arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
constraints:[sem:[document],c_str:[head:[lex:LexSubj]]]],
         athematic:[type:prep,canon:2,gappable:no,
                                  oblig:yes,constraints:[lex:through]],
                 arg_prep:[type:np,canon:3,gappable:yes,oblig:yes,
constraints:[c_str:[head:[lex:LexPrepArg]]]]],
        ft:[pc:[coll(subject,LexSubj,[book,title,article]),
            coll(arg_prep,LexPrepArg,[printing,edition])]]).

% he would have gone through fire for the girl he loved
verb([v(goes,go,went,gone,going,go_through_6_endure_experience)],v_mwu_prep,
arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
constraints:[sem:[hum]]],
         athematic:[type:prep,canon:2,gappable:no,
                                  oblig:yes,constraints:[lex:through]],
                 arg_prep:[type:np,canon:3,gappable:yes,oblig:yes,
constraints:[c_str:[head:[lex:Lex]]]]],
        ft:[pc:[coll(arg_prep,Lex,[operation,pain,ordeal,fire])]]).

% "go through sby's hands", "go through the mill", "go through the motions",
% and "go through (the) proper channels"
% receive special treatment along the lines proposed for other mwu's here,
% in so far as at least one of the args can be LEXICALLY constrained:
% HANDS, MILL, MOTIONS, PROPER CHANNELS should be looked for as textual items,
% or lexical ones, not as heads of collocational (thesauric) classes

% as for GO THROUGH WITH it is another mwu altogether, in this framework

verb([v(goes,go,went,gone,going,go_through_somebodys_hands)],v_mwu_prep,
arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
constraints:[c_str:[head:[lex:LexSubj]]]],
         athematic:[type:prep,canon:2,gappable:no,
                                  oblig:yes,constraints:[lex:through]],
                 arg_prep:[type:np,canon:3,gappable:yes,oblig:yes,
constraints:[c_str:[det:[type:or([poss_adj,genitive])]],
             c_str:[head:[txt:hands]]]]],
        ft:[pc:[coll(subject,LexSubj,
        [pound,jewellery,diamond,paper,document,patient,case])]]).

verb([v(goes,go,went,gone,going,go_through_the_mill)],v_mwu_prep,
arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
constraints:[sem:[hum]]],
         athematic:[type:prep,canon:2,gappable:no,
                                  oblig:yes,constraints:[lex:through]],
                 arg_prep:[type:np,canon:3,gappable:no,oblig:yes,
constraints:[ c_str:[det:[lex:the]],
              c_str:[head:[txt:mill]]]]],
        ft:[]).

verb([v(goes,go,went,gone,going,go_through_the_motions)],v_mwu_prep,
arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
constraints:[sem:[hum]]],
         athematic:[type:prep,canon:2,gappable:no,
                                  oblig:yes,constraints:[lex:through]],
                 arg_prep:[type:np,canon:3,gappable:no,oblig:yes,
constraints:[c_str:[det:[lex:the]],
             c_str:[head:[txt:motions]]]]],
        ft:[]).


verb([v(goes,go,went,gone,going,go_through_the_proper_channels)],v_mwu_prep,
arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
constraints:[c_str:[head:[lex:LexSubj]]]],
         athematic:[type:prep,canon:2,gappable:no,
                                  oblig:yes,constraints:[lex:through]],
                 arg_prep:[type:np,canon:3,gappable:no,oblig:yes,
                 constraints:[c_str:[adjp:[c_str:[head:[lex:proper]]]],
                             c_str:[head:[txt:channels]]]]],
        ft:[pc:[coll(subject,LexSubj,[application,complaint,letter])]]).



% FROM DEFIDIC (through LKP) : Robert&Collins / Oxford&Hachette E->F merged
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

verb([v(goes,go,went,gone,going,go_through_1_dépenser_user)],v_mwu_prep,
arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
constraints:[sem:[hum]]],
         athematic:[type:prep,canon:2,gappable:no,
                                  oblig:yes,constraints:[lex:through]],
                 arg_prep:[type:np,canon:3,gappable:yes,oblig:yes,
constraints:[c_str:[head:[lex:Lex]]]]],
        ft:[pc:[coll(arg_prep,Lex,[money])]]).
        
verb([v(goes,go,went,gone,going,go_through_2_trier)],v_mwu_prep,
arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
constraints:[sem:[hum]]],
         athematic:[type:prep,canon:2,gappable:no,
                                  oblig:yes,constraints:[lex:through]],
                 arg_prep:[type:np,canon:3,gappable:yes,oblig:yes,
constraints:[c_str:[head:[lex:Lex]]]]],
        ft:[pc:[coll(arg_prep,Lex,[clothes,wardrobe])]]).
        
verb([v(goes,go,went,gone,going,go_through_3_éplucher)],v_mwu_prep,
arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
constraints:[sem:[hum]]],
         athematic:[type:prep,canon:2,gappable:no,
                                  oblig:yes,constraints:[lex:through]],
                 arg_prep:[type:np,canon:3,gappable:yes,oblig:yes,
constraints:[c_str:[head:[lex:Lex]]]]],
        ft:[pc:[coll(arg_prep,Lex,[list,book])]]).
        
verb([v(goes,go,went,gone,going,go_through_4_dépouiller)],v_mwu_prep,
arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
constraints:[sem:[hum]]],
         athematic:[type:prep,canon:2,gappable:no,
                                  oblig:yes,constraints:[lex:through]],
                 arg_prep:[type:np,canon:3,gappable:yes,oblig:yes,
constraints:[c_str:[head:[lex:Lex]]]]],
        ft:[pc:[coll(arg_prep,Lex,[mail])]]).


verb([v(goes,go,went,gone,going,go_through_5_fouiller_dans_explorer)],
v_mwu_prep,
arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
constraints:[sem:[hum]]],
         athematic:[type:prep,canon:2,gappable:no,
                                  oblig:yes,constraints:[lex:through]],
                 arg_prep:[type:np,canon:3,gappable:yes,oblig:yes,
constraints:[c_str:[head:[lex:Lex]]]]],
        ft:[pc:[coll(arg_prep,Lex,[pocket])]]).
        
verb([v(goes,go,went,gone,going,go_through_6_discuter_examiner_à_fond)],
v_mwu_prep,
arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
constraints:[sem:[hum]]],
         athematic:[type:prep,canon:2,gappable:no,
                                  oblig:yes,constraints:[lex:through]],
                 arg_prep:[type:np,canon:3,gappable:yes,oblig:yes,
constraints:[c_str:[head:[lex:Lex]]]]],
        ft:[pc:[coll(arg_prep,Lex,[subject])]]).
        
verb([v(goes,go,went,gone,going,go_through_7_passer_par)],v_mwu_prep,
arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
constraints:[sem:[hum]]],
         athematic:[type:prep,canon:2,gappable:no,
                                  oblig:yes,constraints:[lex:through]],
                 arg_prep:[type:np,canon:3,gappable:yes,oblig:yes,
constraints:[c_str:[head:[lex:Lex]]]]],
        ft:[pc:[coll(arg_prep,Lex,[stage,phase])]]).
        
        
verb([v(goes,go,went,gone,going,go_through_8_faire)],v_mwu_prep,
arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
constraints:[sem:[hum]]],
         athematic:[type:prep,canon:2,gappable:no,
                                  oblig:yes,constraints:[lex:through]],
                 arg_prep:[type:np,canon:3,gappable:yes,oblig:yes,
constraints:[c_str:[head:[lex:Lex]]]]],
        ft:[pc:[coll(arg_prep,Lex,[apprenticeship])]]).
        
        
verb([v(goes,go,went,gone,going,go_through_9_suivre)],v_mwu_prep,
arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
constraints:[sem:[hum]]],
         athematic:[type:prep,canon:2,gappable:no,
                                  oblig:yes,constraints:[lex:through]],
                 arg_prep:[type:np,canon:3,gappable:yes,oblig:yes,
constraints:[c_str:[head:[lex:Lex]]]]],
        ft:[pc:[coll(arg_prep,Lex,[course,study])]]).
        
verb([v(goes,go,went,gone,going,go_through_10_remplir_accomplir)],v_mwu_prep,
arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
constraints:[sem:[hum]]],
         athematic:[type:prep,canon:2,gappable:no,
                                  oblig:yes,constraints:[lex:through]],
                 arg_prep:[type:np,canon:3,gappable:yes,oblig:yes,
constraints:[c_str:[head:[lex:Lex]]]]],
        ft:[pc:[coll(arg_prep,Lex,[formality])]]).
        
verb([v(goes,go,went,gone,going,go_through_11_réciter)],v_mwu_prep,
arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
constraints:[sem:[hum]]],
         athematic:[type:prep,canon:2,gappable:no,
                                  oblig:yes,constraints:[lex:through]],
                 arg_prep:[type:np,canon:3,gappable:yes,oblig:yes,
constraints:[c_str:[head:[lex:Lex]]]]],
        ft:[pc:[coll(arg_prep,Lex,[lesson])]]).
        
        
verb([v(goes,go,went,gone,going,go_through_12_exécuter)],v_mwu_prep,
arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
constraints:[sem:[hum]]],
         athematic:[type:prep,canon:2,gappable:no,
                                  oblig:yes,constraints:[lex:through]],
                 arg_prep:[type:np,canon:3,gappable:yes,oblig:yes,
constraints:[c_str:[head:[lex:Lex]]]]],
        ft:[pc:[coll(arg_prep,Lex,[programme,entertainment])]]).
        
        
verb([v(goes,go,went,gone,going,go_through_13_répéter)],v_mwu_prep,
arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
constraints:[sem:[hum]]],
         athematic:[type:prep,canon:2,gappable:no,
                                  oblig:yes,constraints:[lex:through]],
                 arg_prep:[type:np,canon:3,gappable:yes,oblig:yes,
constraints:[c_str:[head:[lex:Lex]]]]],
        ft:[pc:[coll(arg_prep,Lex,[scene])]]).
        
        
verb([v(goes,go,went,gone,going,go_through_14_parcourir)],v_mwu_prep,
arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
constraints:[sem:[hum]]],
         athematic:[type:prep,canon:2,gappable:no,
                                  oblig:yes,constraints:[lex:through]],
                 arg_prep:[type:np,canon:3,gappable:yes,oblig:yes,
constraints:[c_str:[head:[lex:Lex]]]]],
        ft:[pc:[coll(arg_prep,Lex,[document,file,list])]]).
        
        
verb([v(goes,go,went,gone,going,go_through_15_fouiller)],v_mwu_prep,
arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
constraints:[sem:[hum]]],
         athematic:[type:prep,canon:2,gappable:no,
                                  oblig:yes,constraints:[lex:through]],
                 arg_prep:[type:np,canon:3,gappable:yes,oblig:yes,
constraints:[c_str:[head:[lex:Lex]]]]],
        ft:[pc:[coll(arg_prep,Lex,[belongings,baggage,suitcase,trunk])]]).
        
verb([v(goes,go,went,gone,going,go_through_16_endurer_subir_souffrir)],
v_mwu_prep,
arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
constraints:[sem:[hum]]],
         athematic:[type:prep,canon:2,gappable:no,
                                  oblig:yes,constraints:[lex:through]],
                 arg_prep:[type:np,canon:3,gappable:yes,oblig:yes,
constraints:[c_str:[head:[lex:Lex]]]]],
        ft:[pc:[coll(arg_prep,Lex,[experiment,ordeal])]]).
        
verb([v(goes,go,went,gone,going,go_through_17_expliquer)],v_mwu_prep,
arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
constraints:[sem:[hum]]],
         athematic:[type:prep,canon:2,gappable:no,
                                  oblig:yes,constraints:[lex:through]],
                 arg_prep:[type:np,canon:3,gappable:yes,oblig:yes,
constraints:[c_str:[head:[lex:Lex]]]]],
        ft:[pc:[coll(arg_prep,Lex,[procedure])]]).

*/

% VERBA entries
%%%%%%%%%%%%%%

verb([v(goes,go,went,gone,going,go_through_1_consume)],v_mwu_prep,
arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
constraints:[sem:[hum]]],
         athematic:[type:prep,canon:2,gappable:no,
                                  oblig:yes,constraints:[lex:through]],
                 arg_prep:[type:np,canon:3,gappable:yes,oblig:yes,
constraints:[c_str:[head:[lex:Lex]]]]],
        ft:[pc:[coll(arg_prep,Lex,[money,food,drink])]]).
        
        
verb([v(goes,go,went,gone,going,go_through_2_search)],v_mwu_prep,
arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
constraints:[sem:[hum]]],
         athematic:[type:prep,canon:2,gappable:no,
                                  oblig:yes,constraints:[lex:through]],
                 arg_prep:[type:np,canon:3,gappable:yes,oblig:yes,
constraints:[c_str:[head:[lex:Lex]]]]],
        ft:[pc:[coll(arg_prep,Lex,[room,pocket,clothes,cupboard,wardrobe,
                                   luggage, suitcase, trunk])]]).


verb([v(goes,go,went,gone,going,go_through_3_perform_rehearse)],v_mwu_prep,
arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
constraints:[sem:[hum]]],
         athematic:[type:prep,canon:2,gappable:no,
                                  oblig:yes,constraints:[lex:through]],
         arg_prep:[type:np,canon:3,gappable:yes,oblig:yes,
                                      constraints:[c_str:[head:[lex:Lex]]]]],
ft:[pc:[coll(arg_prep,Lex,[marriage,initiation,scene, lesson,programme,
                          ceremony, formality, procedure])]]).
                          
verb([v(goes,go,went,gone,going,go_through_4_examine)],v_mwu_prep,
arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
constraints:[sem:[hum]]],
         athematic:[type:prep,canon:2,gappable:no,
                                  oblig:yes,constraints:[lex:through]],
                 arg_prep:[type:np,canon:3,gappable:yes,oblig:yes,
constraints:[c_str:[head:[lex:Lex]]]]],
        ft:[pc:[coll(arg_prep,Lex,[fact,argument,subject,file,
                                   mail,text,list,document])]]).

verb([v(goes,go,went,gone,going,go_through_5_be_published)],v_mwu_prep,
arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
constraints:[sem:[document],c_str:[head:[lex:LexSubj]]]],
         athematic:[type:prep,canon:2,gappable:no,
                                  oblig:yes,constraints:[lex:through]],
                 arg_prep:[type:np,canon:3,gappable:yes,oblig:yes,
constraints:[c_str:[head:[lex:LexPrepArg]]]]],
        ft:[pc:[coll(subject,LexSubj,[book,title,article]),
            coll(arg_prep,LexPrepArg,[printing,edition])]]).
            
verb([v(goes,go,went,gone,going,go_through_6_endure_experience)],v_mwu_prep,
arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
constraints:[sem:[hum]]],
         athematic:[type:prep,canon:2,gappable:no,
                                  oblig:yes,constraints:[lex:through]],
                 arg_prep:[type:np,canon:3,gappable:yes,oblig:yes,
constraints:[c_str:[head:[lex:Lex]]]]],
        ft:[pc:[coll(arg_prep,Lex,[operation,pain,ordeal, apprenticeship,
                                   fire, phase, stage, process,
                                   experience, experiment])]]).
                                   
                                   
verb([v(goes,go,went,gone,going,go_through_somebodys_hands)],v_mwu_prep,
arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
constraints:[c_str:[head:[lex:LexSubj]]]],
         athematic:[type:prep,canon:2,gappable:no,
                                  oblig:yes,constraints:[lex:through]],
                 arg_prep:[type:np,canon:3,gappable:yes,oblig:yes,
constraints:[c_str:[det:[type:or([poss_adj,genitive])]],
             c_str:[head:[txt:hands]]]]],
        ft:[pc:[coll(subject,LexSubj,
        [pound,jewellery,diamond,paper,document,patient,case])]]).

verb([v(goes,go,went,gone,going,go_through_the_mill)],v_mwu_prep,
arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
constraints:[sem:[hum]]],
         athematic:[type:prep,canon:2,gappable:no,
                                  oblig:yes,constraints:[lex:through]],
                 arg_prep:[type:np,canon:3,gappable:no,oblig:yes,
constraints:[ c_str:[det:[lex:the]],
              c_str:[head:[txt:mill]]]]],
        ft:[]).

verb([v(goes,go,went,gone,going,go_through_the_motions)],v_mwu_prep,
arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
constraints:[sem:[hum]]],
         athematic:[type:prep,canon:2,gappable:no,
                                  oblig:yes,constraints:[lex:through]],
                 arg_prep:[type:np,canon:3,gappable:no,oblig:yes,
constraints:[c_str:[det:[lex:the]],
             c_str:[head:[txt:motions]]]]],
        ft:[]).


verb([v(goes,go,went,gone,going,go_through_the_proper_channels)],v_mwu_prep,
arglist:[subject:[type:np, canon:0,gappable:yes, oblig:yes,
constraints:[c_str:[head:[lex:LexSubj]]]],
         athematic:[type:prep,canon:2,gappable:no,
                                  oblig:yes,constraints:[lex:through]],
                 arg_prep:[type:np,canon:3,gappable:no,oblig:yes,
                 constraints:[c_str:[adjp:[c_str:[head:[lex:proper]]]],
                             c_str:[head:[txt:channels]]]]],
        ft:[pc:[coll(subject,LexSubj,[application,complaint,letter])]]).



%%%%%%%%%%%%%
% AUXILIARIES
%%%%%%%%%%%%%

% the 'requires' feature embodies a constraint on the element
% to the right of the aux
% can+infinitive (have) - have+en_active(been)
% been+en_passive(bought)
% =>
% can have been bought

lex(can,aux,[txt:can,lex:[can],type:finite,tense:present,
   agr:[number:N, person:P,requires:inf]]).
lex(could,aux,[txt:could,lex:[could],type:finite,tense:past,
   agr:[number:N, person:P,requires:inf]]).
lex(may,aux,[txt:may,lex:[may],type:finite,tense:present,
   agr:[number:N, person:P,requires:inf]]).
lex(might,aux,[txt:might,lex:[might],type:finite,tense:past,
   agr:[number:N, person:P,requires:inf]]).
lex(must,aux,[txt:must,lex:[must],type:finite,tense:present,
   agr:[number:N, person:P,requires:inf]]).
lex(shall,aux,[txt:shall,lex:[shall],type:finite,tense:present,
   agr:[number:N, person:P,requires:inf]]).
lex(should,aux,[txt:should,lex:[should],type:finite,tense:past,
   agr:[number:N, person:P,requires:inf]]).
lex(will,aux,[txt:will,lex:[will],type:finite,tense:present,
   agr:[number:N, person:P,requires:inf]]).
lex(would,aux,[txt:would,lex:[would],type:finite,tense:past,
   agr:[number:N, person:P,requires:inf]]).

lex(do,aux,[txt:do,lex:[do],type:finite,tense:present,
   agr:[number:sing, person:or([1,2]),requires:inf]]).
lex(do,aux,[txt:do,lex:[do],type:finite,tense:present,
   agr:[number:plural, person:P,requires:inf]]).
lex(does,aux,[txt:does,lex:[do],type:finite,tense:present,
   agr:[number:sing, person:3,requires:inf]]).
lex(did,aux,[txt:did,lex:[do],type:finite,tense:past,
   agr:[number:N, person:P,requires:inf]]).


% BE as aux (not to be confused with BE as main verb,
% which has its own arglist

lex(is,aux,[txt:is,lex:[be],type:finite,tense:present,
  agr:[number:sing, person:3,requires:or([en_passive,ing])]]).
lex(am,aux,[txt:am,lex:[be],type:finite,tense:present,
  agr:[number:sing, person:1,requires:or([en_passive,ing])]]).
lex(was,aux,[txt:was,lex:[be],type:finite,tense:past,
  agr:[number:sing, person:or([1,3]),requires:or([en_passive,ing])]]).
lex(were,aux,[txt:were,lex:[be],type:finite,tense:past,
  agr:[number:sing, person:2,requires:or([en_passive,ing])]]).
lex(were,aux,[txt:were,lex:[be],type:finite,tense:past,
  agr:[number:plural, person:P,requires:or([en_passive,ing])]]).
lex(are,aux,[txt:are,lex:[be],type:finite,tense:present,
  agr:[number:sing, person:2,requires:or([en_passive,ing])]]).
lex(are,aux,[txt:are,lex:[be],type:finite,tense:present,
  agr:[number:plural, person:P,requires:or([en_passive,ing])]]).
lex(be,aux,[txt:be,lex:[be],type:inf,tense:untensed,
  agr:[number:N, person:P,requires:or([en_passive,ing])]]).
lex(being,aux,[txt:being,lex:[be],type:ing,tense:untensed,
  agr:[number:N, person:P,requires:en_passive]]).
lex(been,aux,[txt:been,lex:[be],type:en_active,tense:untensed,
  agr:[number:N, person:P,requires:or([en_passive,ing])]]).

lex(have,aux,[txt:have,lex:[have],type:finite,tense:present,
 agr:[number:sing, person:or([1,2]),requires:en_active]]).
lex(have,aux,[txt:have,lex:[have],type:finite,tense:present,
 agr:[number:plural, person:P,requires:en_active]]).
lex(have,aux,[txt:have,lex:[have],type:inf,tense:untensed,
 agr:[number:N, person:P,requires:en_active]]).
lex(having,aux,[txt:having,lex:[have],type:ing,tense:untensed,
 agr:[number:N, person:P,requires:en_active]]).
lex(has,aux,[txt:has,lex:[have],type:finite,tense:present,
 agr:[number:sing, person:3,requires:en_active]]).
lex(had,aux,[txt:had,lex:[have],type:finite,tense:past,
 agr:[number:N, person:P,requires:en_active]]).

% the TO of to+infinitive is best parsed as an auxiliary
% in this type of grammar

lex(to,aux,[txt:to,lex:[to],type:to,tense:untensed,
 agr:[number:N, person:P,requires:inf]]).


%%%%%%%%%%%%%
% DETERMINERS
%%%%%%%%%%%%%

lex(a,det,
[pos:det,type:article,txt:a,lex:a,agr:[number:sing,person:3,gender:G]]).
lex(an,det,
[pos:det,type:article,txt:an,lex:a,agr:[number:sing,person:3,gender:G]]).
lex(the,det,
[pos:det,type:article,txt:the,lex:the,agr:[number:Nb,person:3,gender:G]]).

lex(any,det,
[pos:det,type:quant,txt:any,lex:any,agr:[number:Nb,person:3,gender:G]]).
lex(some,det,
[pos:det,type:quant,txt:some,lex:some,agr:[number:Nb,person:3,gender:G]]).
lex(no,det,
[pos:det,type:quant,txt:no,lex:no,pol:neg,agr:[number:Nb,person:3,gender:G]]).
% notice the pol feature, which is assigned only if pol is neg

lex(one,det,
[pos:det,type:quant,txt:one,lex:one,agr:[number:sing,person:3,gender:G]]).

lex(two,det,
[pos:det,type:quant,txt:two,lex:two,agr:[number:plural,person:3,gender:G]]).

lex(three,det,
[pos:det,type:quant,txt:three,lex:three,agr:[number:plural,person:3,gender:G]]).

lex(four,det,
[pos:det,type:quant,txt:four,lex:four,agr:[number:plural,person:3,gender:G]]).

lex(five,det,
[pos:det,type:quant,txt:five,lex:five,agr:[number:plural,person:3,gender:G]]).

% the possessive have two agr features; agr concerns the following noun,
% while agrposs refers to agreement features to be checked on the possessor
% the only value for the agr feature on the accompanying noun is the person:3



lex(my,det,
[pos:det,type:poss_adj,txt:my,lex:my,agr:[number:Nb,person:3,gender:G1],
                            agrposs:[number:sing,person:1,gender:G2]]).
lex(our,det,
[pos:det,type:poss_adj,txt:our,lex:our,agr:[number:Nb,person:3,gender:G1],
                            agrposs:[number:plural,person:1,gender:G2]]).
lex(your,det,
[pos:det,type:poss_adj,txt:your,lex:your,agr:[number:Nb1,person:3,gender:G1],
                            agrposs:[number:Nb2,person:2,gender:G2]]).
lex(his,det,
[pos:det,type:poss_adj,txt:his,lex:his,agr:[number:Nb,person:3,gender:G],
                            agrposs:[number:sing,person:3,gender:masc]]).
lex(her,det,
[pos:det,type:poss_adj,txt:her,lex:her,agr:[number:Nb,person:3,gender:G],
                            agrposs:[number:sing,person:3,gender:fem]]).
lex(its,det,
[pos:det,type:poss_adj,txt:its,lex:its,agr:[number:Nb,person:3,gender:G],
                            agrposs:[number:sing,person:3,gender:neuter]]).
lex(their,det,
[pos:det,type:poss_adj,txt:their,lex:their,agr:[number:Nb,person:3,gender:G1],
                            agrposs:[number:plural,person:or([3,indef]),
                                      gender:G2]]).

 % the 'indef' person enables 'their' (and further on, 'themselves') to refer
 % to the special indefinite pronouns (no,any,some,every)+(one,body)


%%%%%%%%%%%%%%%%%%%%%%%%
% SIMPLE [UN]COUNT NOUNS  
%%%%%%%%%%%%%%%%%%%%%%%%

% "type:idiom" can be used to distinguish nouns as part of idiomatic
% strings, if need be

lex(amount,n,
[type:std,pos:n,count:c,class:std,txt:amount,lex:amount,
agr:[number:sing,person:3,gender:neuter],sem:[abstract]]).

lex(amounts,n,
[type:std,pos:n,count:c,class:std,txt:amounts,lex:amount,
agr:[number:plural,person:3,gender:neuter],sem:[abstract]]).

lex(bean,n,
[type:std,pos:n,count:c,class:std,txt:bean,lex:bean,
agr:[number:sing,person:3,gender:neuter],sem:[thing]]).

lex(beans,n,
[type:idiom,pos:n,count:c,class:std,txt:beans,lex:beans,
agr:[number:plural,person:3,gender:neuter],sem:[beans]]).

lex(beans,n,
[type:std,pos:n,count:c,class:std,txt:beans,lex:bean,
agr:[number:plural,person:3,gender:neuter],sem:[thing]]).

lex(bin,n,
[type:std,pos:n,count:c,class:std,txt:bin,lex:bin,
agr:[number:sing,person:3,gender:neuter],sem:[thing]]).

lex(bins,n,
[type:std,pos:n,count:c,class:std,txt:bins,lex:bin,
agr:[number:plural,person:3,gender:neuter],sem:[thing]]).

lex(book,n,
[type:std,pos:n,txt:book,lex:book,count:c,class:std,
agr:[number:sing,person:3,gender:neuter],sem:[document]]).

lex(books,n,
[type:std,pos:n,count:c,class:std,txt:books,lex:book,
agr:[number:plural,person:3,gender:neuter],sem:[document]]).

lex(bottle,n,
[type:std,pos:n,count:c,class:std,txt:bottle,lex:bottle,
agr:[number:sing,person:3,gender:neuter],sem:[thing]]).

lex(bottles,n,
[type:std,pos:n,count:c,class:std,txt:bottles,lex:bottle,
agr:[number:plural,person:3,gender:neuter],sem:[thing]]).

lex(brunt,n,
[type:std,pos:n,count:u,class:std,txt:brunt,lex:brunt,
agr:[number:sing,person:3,gender:neuter],sem:[abstract]]).

lex(bucket,n,
[type:idiom,pos:n,count:c,class:std,txt:bucket,lex:bucket,
agr:[number:sing,person:3,gender:neuter],sem:[bucket]]).

lex(bucket,n,
[type:std,pos:n,count:c,class:std,txt:bucket,lex:bucket,
agr:[number:sing,person:3,gender:neuter],sem:[thing]]).

lex(buckets,n,
[type:std,pos:n,count:c,class:std,txt:buckets,lex:bucket,
agr:[number:plural,person:3,gender:neuter],sem:[thing]]).

lex(ceremony,n,
[type:std,pos:n,count:c,class:std,txt:ceremony,lex:ceremony,
agr:[number:sing,person:3,gender:neuter],sem:[abstract]]).

lex(ceremonies,n,
[type:std,pos:n,count:c,class:std,txt:ceremonies,lex:ceremony,
agr:[number:plural,person:3,gender:neuter],sem:[abstract]]).

lex(channel,n,
[type:std,pos:n,count:c,class:std,txt:channel,lex:channnel,
agr:[number:sing,person:3,gender:neuter],sem:[thing]]).

lex(channels,n,
[type:std,pos:n,count:c,class:std,txt:channels,lex:channel,
agr:[number:plural,person:3,gender:neuter],sem:[thing]]).

lex(contents,n,
[type:std,pos:n,count:c,class:std,txt:contents,lex:contents,
agr:[number:plural,person:3,gender:neuter],sem:[thing]]).

lex(cut,n,
[type:std,pos:n,txt:cut,lex:cut,count:c,class:std,
agr:[number:sing,person:3,gender:neuter],sem:[thing]]).

lex(cuts,n,
[type:std,pos:n,txt:cuts,lex:cut,count:c,class:std,
agr:[number:plural,person:3,gender:neuter],sem:[thing]]).

lex(decision,n,
[type:std,pos:n,txt:decision,lex:decision,count:c,class:tovp,
agr:[number:sing,person:3,gender:neuter],sem:[abstract],
arglist:[object:[type:pred,canon:2,gappable:no,oblig:no,
                                constraints:[finite:tovp ]]],
ft:[]]).

lex(decisions,n,
[type:std,pos:n,txt:decisions,lex:decision,count:c,class:tovp,
agr:[number:plural,person:3,gender:neuter],sem:[abstract],
arglist:[object:[type:pred,canon:2,gappable:no,oblig:no,
                                constraints:[finite:tovp ]]],
ft:[]]).

lex(due,n,
[type:std,pos:n,count:u,class:std,txt:due,lex:due,
agr:[number:sing,person:3,gender:neuter],sem:[abstract]]).

lex(edition,n,
[type:std,pos:n,txt:edition,lex:edition,count:c,class:std,
agr:[number:sing,person:3,gender:neuter],sem:[abstract]]).

lex(editions,n,
[type:std,pos:n,txt:editions,lex:edition,count:c,class:std,
agr:[number:plural,person:3,gender:neuter],sem:[abstract]]).

lex(example,n,
[type:std,pos:n,txt:example,lex:example,count:c,class:std,
agr:[number:sing,person:3,gender:neuter],sem:[abstract]]).

lex(examples,n,
[type:std,pos:n,txt:examples,lex:example,count:c,class:std,
agr:[number:plural,person:3,gender:neuter],sem:[abstract]]).

lex(explanation,n,
[type:std,pos:n,txt:explanation,lex:explanation,count:c,class:std,
agr:[number:sing,person:3,gender:neuter],sem:[abstract]]).

lex(explanations,n,
[type:std,pos:n,txt:explanations,lex:explanation,count:c,class:std,
agr:[number:plural,person:3,gender:neuter],sem:[abstract]]).

lex(fan,n,
[type:std,pos:n,txt:fan,lex:fan,count:c,class:std,
agr:[number:sing,person:3,gender:neuter],sem:[thing]]).

lex(fans,n,
[type:std,pos:n,txt:fans,lex:fan,count:c,class:std,
agr:[number:plural,person:3,gender:neuter],sem:[thing]]).

lex(figure,n,
[type:std,pos:n,txt:figure,lex:figure,count:c,class:std,
agr:[number:sing,person:3,gender:neuter],sem:[thing]]).

lex(figures,n,
[type:std,pos:n,txt:figures,lex:figure,count:c,class:std,
agr:[number:plural,person:3,gender:neuter],sem:[thing]]).

lex(fly,n,
[type:std,pos:n,txt:fly,lex:fly,count:c,class:std,
agr:[number:sing,person:3,gender:neuter],sem:[thing]]).

lex(flies,n,
[type:std,pos:n,txt:flies,lex:fly,count:c,class:std,
agr:[number:plural,person:3,gender:neuter],sem:[thing]]).

lex(grave,n,
[type:std,pos:n,txt:grave,lex:grave,count:c,class:std,
agr:[number:sing,person:3,gender:neuter],sem:[thing]]).

lex(graves,n,
[type:std,pos:n,txt:graves,lex:grave,count:c,class:std,
agr:[number:plural,person:3,gender:neuter],sem:[thing]]).

lex(inheritance,n,
[type:std,pos:n,txt:inheritance,lex:inheritance,count:c,class:std,
agr:[number:sing,person:3,gender:neuter],sem:[thing]]).

lex(inheritances,n,
[type:std,pos:n,txt:inheritances,lex:inheritance,count:c,class:std,
agr:[number:plural,person:3,gender:neuter],sem:[thing]]).

lex(hand,n,
[type:std,pos:n,txt:hand,lex:hand,count:c,class:std,
agr:[number:sing,person:3,gender:neuter],sem:[thing]]).

lex(hands,n,
[type:std,pos:n,txt:hands,lex:hand,count:c,class:std,
agr:[number:plural,person:3,gender:neuter],sem:[thing]]).

lex(havoc,n,
[type:std,pos:n,count:u,class:std,txt:havoc,lex:havoc,
agr:[number:sing,person:3,gender:neuter],sem:[abstract]]).

lex(horse,n,
[type:std,pos:n,txt:horse,lex:horse,count:c,class:std,
agr:[number:sing,person:3,gender:neuter],sem:[thing]]).

lex(horses,n,
[type:std,pos:n,txt:horses,lex:horse,count:c,class:std,
agr:[number:plural,person:3,gender:neuter],sem:[thing]]).

lex(horses,n,
[type:idiom,pos:n,txt:horses,lex:horse,count:c,class:std,
agr:[number:plural,person:3,gender:neuter],sem:[thing]]).

lex(house,n,
[type:std,pos:n,txt:house,lex:house,count:c,class:std,
agr:[number:sing,person:3,gender:neuter],sem:[thing]]).

lex(houses,n,
[type:std,pos:n,txt:houses,lex:house,count:c,class:std,
agr:[number:plural,person:3,gender:neuter],sem:[thing]]).

lex(inch,n,
[type:std,pos:n,txt:inch,lex:inch,count:c,class:std,
agr:[number:sing,person:3,gender:neuter],sem:[measure]]).

lex(inches,n,
[type:std,pos:n,txt:inches,lex:inch,count:c,class:std,
agr:[number:plural,person:3,gender:neuter],sem:[measure]]).

lex(inspector,n,
[type:std,pos:n,txt:inspector,lex:inspector,count:c,class:std,
agr:[number:sing,person:3,gender:masc],sem:[hum]]).

lex(inspectors,n,
[type:std,pos:n,txt:inspectors,lex:inspector,count:c,class:std,
agr:[number:plural,person:3,gender:masc],sem:[hum]]).

lex(leaf,n,
[type:std,pos:n,txt:leaf,lex:leaf,count:c,class:std,
agr:[number:sing,person:3,gender:neuter],sem:[thing]]).

lex(leaves,n,
[type:std,pos:n,txt:leaves,lex:leaf,count:c,class:std,
agr:[number:plural,person:3,gender:neuter],sem:[thing]]).

lex(linguistics,n,
[type:std,pos:n,txt:linguistics,lex:linguistics,count:u,class:std,
agr:[number:sing,person:3,gender:neuter],sem:[abstract]]).

lex(looks,n,
[type:std,pos:n,txt:looks,lex:looks,count:c,class:std,
agr:[number:plural,person:3,gender:neuter],sem:[thing]]).

lex(man,n,
[type:std,pos:n,txt:man,lex:man,count:c,class:std,
agr:[number:sing,person:3,gender:masc],sem:[hum]]).

lex(meat,n,
[type:std,pos:n,count:u,class:std,txt:meat,lex:meat,
agr:[number:sing,person:3,gender:neuter],sem:[meat]]).

lex(men,n,
[type:std,pos:n,count:c,class:std,txt:men,lex:man,
agr:[number:plural,person:3,gender:masc],sem:[hum]]).

lex(milk,n,
[type:std,pos:n,count:u,class:std,txt:milk,lex:milk,
agr:[number:sing,person:3,gender:neuter],sem:[thing]]).

lex(mill,n,
[type:std,pos:n,txt:mill,lex:mill,count:c,class:std,
agr:[number:sing,person:3,gender:neuter],sem:[thing]]).

lex(mills,n,
[type:std,pos:n,txt:mills,lex:mill,count:c,class:std,
agr:[number:plural,person:3,gender:neuter],sem:[thing]]).

lex(minister,n,
[type:std,pos:n,txt:minister,lex:minister,count:c,class:std,
agr:[number:sing,person:3,gender:or([masc,fem])],sem:[hum]]).

lex(ministers,n,
[type:std,pos:n,txt:ministers,lex:minister,count:c,class:std,
agr:[number:plural,person:3,gender:or([masc,fem])],sem:[hum]]).

lex(motion,n,
[type:std,pos:n,txt:motion,lex:motion,count:c,class:std,
agr:[number:sing,person:3,gender:neuter],sem:[abstract]]).

lex(motions,n,
[type:std,pos:n,txt:motions,lex:motion,count:c,class:std,
agr:[number:plural,person:3,gender:neuter],sem:[abstract]]).

lex(mouth,n,
[type:std,pos:n,txt:mouth,lex:mouth,count:c,class:std,
agr:[number:sing,person:3,gender:neuter],sem:[thing]]).

lex(mouths,n,
[type:std,pos:n,txt:mouths,lex:mouth,count:c,class:std,
agr:[number:plural,person:3,gender:neuter],sem:[thing]]).

lex(ointment,n,
[type:std,pos:n,count:u,class:std,txt:ointment,lex:ointment,
agr:[number:sing,person:3,gender:neuter],sem:[thing]]).

lex(oversimplification,n,
[type:std,pos:n,count:c,class:std,lex:oversimplification,txt:oversimplification,
agr:[number:sing,person:3,gender:neuter],sem:[abstract]]).  

lex(oversimplifications,n,
[type:std,pos:n,count:c,class:std,lex:oversimplification,txt:oversimplifications,
agr:[number:plural,person:3,gender:neuter],sem:[abstract]]).  

lex(pamphlet,n,
[type:std,pos:n,txt:pamphlet,lex:pamphlet,count:c,class:std,
agr:[number:sing,person:3,gender:neuter],sem:[book]]).

lex(pamphlets,n,
[type:std,pos:n,count:c,class:std,txt:pamphlets,lex:pamphlet,
agr:[number:plural,person:3,gender:neuter],sem:[book]]).

lex(pig,n,
[type:std,pos:n,txt:pig,lex:pig,count:c,class:std,
agr:[number:sing,person:3,gender:neuter],sem:[thing]]).

lex(pigs,n,
[type:std,pos:n,txt:pigs,lex:pig,count:c,class:std,
agr:[number:plural,person:3,gender:neuter],sem:[thing]]).

lex(place,n,
[type:std,pos:n,txt:place,lex:place,count:c,class:std,
agr:[number:sing,person:3,gender:neuter],sem:[thing]]).

lex(places,n,
[type:std,pos:n,txt:places,lex:place,count:c,class:std,
agr:[number:plural,person:3,gender:neuter],sem:[thing]]).

lex(problem,n,
[type:std,pos:n,txt:problem,lex:problem,count:c,class:std,
agr:[number:sing,person:3,gender:neuter],sem:[abstract]]).

lex(problems,n,
[type:std,pos:n,txt:problems,lex:problem,count:c,class:std,
agr:[number:plural,person:3,gender:neuter],sem:[abstract]]).


% the entry for 'refusal' exemplifies a noun featuring an arglist
% as well as an additional feature, its ability to switch the polarity
% and rescue an np such as
% 'her refusal to mince her words'
% (in case we introduced a process that inherits the kill feature from
%  the np - not implemented in this grammar)
% the arglist is parallel to the one we would specify for the verb
% except that the subject is not in the arglist
% but will be fished out of the possessive adjective or genitive as determiner:
% her refusal to read books students like / the inspector's refusal to
%                                            mince matters

lex(refusal,n,
[type:std,pos:n,txt:refusal,lex:refusal,count:c,class:tovp,
agr:[number:sing,person:3,gender:neuter],sem:[abstract],
arglist:[object:[type:pred,canon:2,gappable:no,oblig:no,
                                constraints:[finite:tovp ]]],
ft:[negswitch:yes]]).

lex(refusals,n,
[type:std,pos:n,txt:refusals,lex:refusal,count:c,class:std,
agr:[number:plural,person:3,gender:neuter],sem:[abstract],
arglist:[object:[type:pred,canon:2,gappable:no,oblig:no,
                                constraints:[finite:tovp ]]],
ft:[negswitch:yes]]).

lex(report,n,
[type:std,pos:n,txt:report,lex:report,count:c,class:std,
agr:[number:sing,person:3,gender:neuter],sem:[document]]).

lex(reports,n,
[type:std,pos:n,count:c,class:std,txt:reports,lex:report,
agr:[number:plural,person:3,gender:neuter],sem:[document]]).

lex(ritual,n,
[type:std,pos:n,txt:ritual,lex:ritual,count:c,class:std,
agr:[number:sing,person:3,gender:neuter],sem:[abstract]]).

lex(rituals,n,
[type:std,pos:n,txt:rituals,lex:ritual,count:c,class:std,
agr:[number:plural,person:3,gender:neuter],sem:[abstract]]).

lex(shit,n,
[type:std,pos:n,count:u,class:std,txt:shit,lex:shit,
agr:[number:sing,person:3,gender:neuter],sem:[thing]]).

lex(snook,n,
[type:std,pos:n,count:c,class:std,txt:snook,lex:snook,
agr:[number:sing,person:3,gender:neuter],sem:[thing]]).

lex(song,n,
[type:std,pos:n,txt:song,lex:song,count:c,class:std,
agr:[number:sing,person:3,gender:neuter],sem:[abstract]]).

lex(songs,n,
[type:std,pos:n,txt:songs,lex:song,count:c,class:std,
agr:[number:plural,person:3,gender:neuter],sem:[abstract]]).

lex(student,n,
[type:std,pos:n,txt:student,lex:student,count:c,class:std,
agr:[number:sing,person:3,gender:or([masc,fem])],sem:[hum]]).

lex(students,n,
[type:std,pos:n,txt:students,lex:student,count:c,class:std,
agr:[number:plural,person:3,gender:or([masc,fem])],sem:[hum]]).

lex(suffering,n,
[type:std,pos:n,txt:suffering,lex:suffering,count:u,class:std,
agr:[number:sing,person:3,gender:neuter],sem:[abstract]]).

lex(teacher,n,
[type:std,pos:n,txt:teacher,lex:teacher,count:c,class:std,
agr:[number:sing,person:3,gender:or([masc,fem])],sem:[hum]]).

lex(teachers,n,
[type:std,pos:n,txt:teachers,lex:teacher,count:c,class:std,
agr:[number:plural,person:3,gender:or([masc,fem])],sem:[hum]]).

lex(torture,n,
[type:std,pos:n,txt:torture,lex:torture,count:c,class:std,
agr:[number:sing,person:3,gender:neuter],sem:[abstract]]).

lex(tortures,n,
[type:std,pos:n,txt:tortures,lex:torture,count:c,class:std,
agr:[number:plural,person:3,gender:neuter],sem:[abstract]]).

lex(training,n,
[type:std,pos:n,txt:training,lex:training,count:u,class:std,
agr:[number:sing,person:3,gender:neuter],sem:[abstract]]).

lex(treasurer,n,
[type:std,pos:n,txt:treasurer,lex:treasurer,count:c,class:std,
agr:[number:sing,person:3,gender:or([masc,fem])],sem:[hum]]).

lex(treasurers,n,
[type:std,pos:n,txt:treasurers,lex:treasurer,count:c,class:std,
agr:[number:plural,person:3,gender:or([masc,fem])],sem:[hum]]).

lex(thing,n,
[type:std,pos:n,txt:thing,lex:thing,count:c,class:std,
agr:[number:sing,person:3,gender:neuter],sem:[thing]]).

lex(things,n,
[type:std,pos:n,txt:things,lex:thing,count:c,class:std,
agr:[number:plural,person:3,gender:neuter],sem:[thing]]).

lex(university,n,
[type:std,pos:n,txt:university,lex:university,count:c,class:std,
agr:[number:sing,person:3,gender:neuter],sem:[thing,hum]]).

lex(universities,n,
[type:std,pos:n,txt:universities,lex:university,count:c,class:std,
agr:[number:plural,person:3,gender:neuter],sem:[thing,hum]]).

lex(woman,n,
[type:std,pos:n,count:c,class:std,txt:woman,lex:woman,
agr:[number:sing,person:3,gender:fem],sem:[hum]]).

lex(women,n,
[type:std,pos:n,count:c,class:std,txt:women,lex:woman,
agr:[number:plural,person:3,gender:fem],sem:[hum]]).

lex(word,n,
[type:std,pos:n,txt:word,lex:word,count:c,class:std,
agr:[number:sing,person:3,gender:neuter],sem:[thing]]).

lex(words,n,
[type:std,pos:n,txt:words,lex:word,count:c,class:std,
agr:[number:plural,person:3,gender:neuter],sem:[thing]]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% COUNT NOUNS WITH SENTENTIAL CPLT               
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% note that an equivalent process could have the complement S
% as part of an arglist associated with each of these nouns

lex(claim,n,
[type:std,pos:n,count:c,class:sent,txt:claim,lex:claim,
agr:[number:sing,person:3,gender:neuter],sem:[abstract]]).  

lex(claims,n,
[type:std,pos:n,count:c,class:sent,txt:claims,lex:claim,
agr:[number:plural,person:3,gender:neuter],sem:[abstract]]).   

lex(hypothesis,n,
[type:std,pos:n,count:c,class:sent,txt:hypothesis,lex:hypothesis,
agr:[number:sing,person:3,gender:neuter],sem:[abstract]]).

lex(hypotheses,n,
[type:std,pos:n,count:c,class:sent,txt:hypotheses,lex:hypothesis,
agr:[number:plural,person:3,gender:neuter],sem:[abstract]]).

%%%%%%%%%%%%%%
% PROPER NAMES
%%%%%%%%%%%%%%

lex('Mary',n,
[type:std,pos:n,count:c,class:proper,txt:'Mary',lex:mary,
agr:[number:sing,person:3,gender:fem],sem:[hum]]).

lex('Paul',n,
[type:std,pos:n,count:c,class:proper,txt:'Paul',lex:paul,
agr:[number:sing,person:3,gender:masc],sem:[hum]]).                       

lex('Peter',n,
[type:std,pos:n,count:c,class:proper,txt:'Peter',lex:peter,
agr:[number:sing,person:3,gender:masc],sem:[hum]]).

lex(mary,n,
[type:std,pos:n,count:c,class:proper,txt:mary,lex:mary,
agr:[number:sing,person:3,gender:fem],sem:[hum]]).

lex(paul,n,
[type:std,pos:n,count:c,class:proper,txt:paul,lex:paul,
agr:[number:sing,person:3,gender:masc],sem:[hum]]).                       

lex(peter,n,
[type:std,pos:n,count:c,class:proper,txt:peter,lex:peter,
agr:[number:sing,person:3,gender:masc],sem:[hum]]).


%%%%%%%%%%%%%%%%%%%
% NOMINAL COMPOUNDS
%%%%%%%%%%%%%%%%%%%

compound(n,[horse,sense],[lex:horse_sense,type:std,pos:n,txt:horse_sense,
    count:u,class:std,agr:[number:sing,person:3,gender:neuter],sem:[abstract]]).

compound(n,[horse,'\'',s,ass],[lex:horse_s_ass,type:std,pos:n,txt:horse_s_ass,
 count:c,class:std,agr:[number:sing,person:3,gender:or([masc,fem])],sem:[hum]]).

compound(n,[horse,'\'',s,asses],
   [lex:horse_s_ass,type:std,pos:n,txt:horse_s_asses,
 unt:c,class:std,agr:[number:plural,person:3,gender:or([masc,fem])],sem:[hum]]).



%%%%%%%%%%%%
% ADJECTIVES
%%%%%%%%%%%%

% the class feature is used to restrict a given adjective to the role of
% modifier of a noun that is part of a mwu
% the proverbial bucket, the fatal bucket

lex(bloody,adj,
[pos:adj,class:idiom,txt:bloody,lex:bloody]).

lex(bloody,adj,
[pos:adj,class:std,txt:bloody,lex:bloody]).

lex(calculated,adj,
[pos:adj,class:std,txt:calculated,lex:calculated]).

lex(considerable,adj,
[pos:adj,class:std,txt:considerable,lex:considerable]).

lex(daily,adj,
[pos:adj,class:std,txt:daily,lex:daily]).

lex(dead,adj,
[pos:adj,class:std,txt:dead,lex:dead]).

lex(fatal,adj,
[pos:adj,class:idiom,txt:fatal,lex:fatal]).

lex(fatal,adj,
[pos:adj,class:std,txt:fatal,lex:fatal]).

lex(financial,adj,
[pos:adj,class:std,txt:financial,lex:financial]).

lex(first,adj,
[pos:adj,class:std,txt:first,lex:first]).

lex(formidable,adj,
[pos:adj,class:std,txt:formidable,lex:formidable]).

lex(full,adj,
[pos:adj,class:std,txt:full,lex:full]).

lex(heavy,adj,
[pos:adj,class:std,txt:heavy,lex:heavy]).

lex(horrible,adj,
[pos:adj,class:std,txt:horrible,lex:horrible]).

lex(main,adj,
[pos:adj,class:std,txt:main,lex:main]).

lex(own,adj,
[pos:adj,class:std,txt:own,lex:own]).

lex(proper,adj,
[pos:adj,class:std,txt:proper,lex:proper]).

lex(proverbial,adj,
[pos:adj,class:idiom,txt:proverbial,lex:proverbial]).

lex(proverbial,adj,
[pos:adj,class:std,txt:proverbial,lex:proverbial]).

lex(ready,adj,
[pos:adj,class:tovp,txt:ready,lex:ready,
 arglist:[object:[type:pred,canon:2,gappable:no,oblig:yes,
                                constraints:[finite:tovp ]]],
ft:[]]).

lex(real,adj,
[pos:adj,class:std,txt:real,lex:real]).

lex(red,adj,
[pos:adj,class:std,txt:red,lex:red]).

lex(seasonal,adj,
[pos:adj,class:std,txt:seasonal,lex:seasonal]).

lex(untold,adj,
[pos:adj,class:std,txt:untold,lex:untold]).


% arg-bearing adjective

lex(unwilling,adj,
[pos:adj,class:tovp,txt:unwilling,lex:unwilling,
 arglist:[object:[type:pred,canon:2,gappable:no,oblig:yes,
                                constraints:[finite:tovp ]]],
ft:[negswitch:yes]]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% APOSTROPHE AND S AFTER APOSTROPHE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% s after apostrophe
lex(s,apos,[pos:apos,txt:s,lex:apos]).

% apostrophe itself
lex('\'',apo,[pos:apo,txt:'\'',lex:apo]).


%%%%%%%%%%%
% RELATIVES
%%%%%%%%%%%

lex(that,relative,
[pos:rel,txt:that,lex:that,
sem:Sem,function:F,
agr:[number:N,person:P,gender:G],
postprep:no]).

lex(which,relative,
[pos:rel,txt:which,lex:which,
sem:[thing],function:F,
agr:[number:N,person:P,gender:neuter],
postprep:yes]).

lex(which,relative,
[pos:rel,txt:which,lex:which,
sem:[abstract],function:F,
agr:[number:N,person:P,gender:neuter],
postprep:yes]).

lex(who,relative,
[pos:rel,txt:who,lex:who,
sem:[hum],function:F,
agr:[number:N,person:P,gender:G],
postprep:no]).

lex(whom,relative,
[pos:rel,txt:whom,lex:who,
sem:[hum],
function:or([subject_inf,object,i_object,arg_prep]),
agr:[number:N,person:P,gender:G],
postprep:yes]).


%%%%%%%%%%%%%%%%%%%%%%%%
% PERSONAL PRONOUNS
%%%%%%%%%%%%%%%%%%%%%%%%

lex(i ,ppro,[type:pp,lex:i,
       agr:[gender:G,number:sing,person:1],
       function:subject,sem:[hum]]).
lex(me ,ppro,[type:pp,lex:me,
       agr:[gender:G,number:sing,person:1],
       function:or([object,i_object,arg_prep,subject_inf]),sem:[hum]]).

lex(we ,ppro,[type:pp,lex:we,
        agr:[gender:G,number:plural,person:1],
       function:subject,sem:[hum]]).
lex(us ,ppro,[type:pp,lex:us,
        agr:[gender:G,number:plural,person:1],
        function:or([object,i_object,arg_prep,subject_inf]),sem:[hum]]).

lex(you ,ppro,[type:pp,lex:you,
         agr:[gender:G,number:N,person:2],
         function:F,sem:[hum]]).

lex(he ,ppro,[type:pp,lex:he,
         agr:[gender:masc,number:sing,person:3],
         function:subject,sem:[hum]]).
lex(she ,ppro,[type:pp,lex:she,
         agr:[gender:fem,number:sing,person:3],
         function:subject,sem:[hum]]).

lex(they ,ppro,[type:pp,lex:they,
          agr:[gender:G,number:plural,person:3],
          function:subject,sem:Sem]).

lex(it ,ppro,[type:pp,lex:it,
         agr:[gender:neuter,number:sing,person:3],
         function:F,sem:Sem]).

lex(him ,ppro,[type:pp,lex:him,
         agr:[gender:masc,number:sing,person:3],
         function:or([object,i_object,arg_prep,subject_inf]),sem:[hum]]).
lex(her ,ppro,[type:pp,lex:her,
         agr:[gender:fem,number:sing,person:3],
         function:or([object,i_object,arg_prep,subject_inf]),sem:[hum]]).
lex(them ,ppro,[type:pp,lex:them,
         agr:[gender:G,number:plural,person:3],
         function:or([object,i_object,arg_prep,subject_inf]),sem:Sem]).


% REFLEXIVE PRONOUNS
%%%%%%%%%%%%%%%%%%%%

lex(myself ,refpro,[type:refp,lex:myself,
                   agr:[gender:G,number:sing,person:1],
                   function:or([object,i_object,arg_prep,subject_inf]),
                   sem:[hum]]).
lex(ourselves ,refpro,[type:refp,lex:ourselves,
                   agr:[gender:G,number:plural,person:1],
                   function:or([object,i_object,arg_prep,subject_inf]),
                   sem:[hum]]).

lex(yourself ,refpro,[type:refp,lex:yourself,
                   agr:[gender:G,number:sing,person:2],
                   function:or([object,i_object,arg_prep,subject_inf]),
                   sem:[hum]]).
lex(yourselves ,refpro,[type:refp,lex:yourselves,
                   agr:[gender:G,number:plural,person:2],
                   function:or([object,i_object,arg_prep,subject_inf]),
                   sem:[hum]]).
              
lex(himself ,refpro,[type:refp,lex:himself,
                   agr:[gender:masc,number:sing,person:3],
                   function:or([object,i_object,arg_prep,subject_inf]),
                   sem:[hum]]).
lex(herself ,refpro,[type:refp,lex:herself,
                   agr:[gender:fem,number:sing,person:3],
                   function:or([object,i_object,arg_prep,subject_inf]),
                   sem:[hum]]).
lex(itself ,refpro,[type:refp,lex:itself,
                    agr:[gender:neuter,number:sing,person:3],
                    function:or([object,i_object,arg_prep,subject_inf]),
                    sem:[thing]]).
lex(themselves ,refpro,[type:refp,lex:themselves,
                    agr:[gender:G,number:plural,person:or([3,indef])],
                    function:or([object,i_object,arg_prep,subject_inf]),
                    sem:Sem]).


% INDEFINITE PRONOUNS
%%%%%%%%%%%%%%%%%%%%%

lex(somebody,indefpro,[type:indefp,lex:somebody,
                       agr:[gender:G,number:sing,person:3],
                       sem:[hum]]).
                       
lex(someone,indefpro,[type:indefp,lex:someone,
                       agr:[gender:G,number:sing,person:3],
                       sem:[hum]]).

lex(something,indefpro,[type:indefp,lex:something,
                        agr:[gender:G,number:sing,person:3],
                        sem:[thing]]).


lex(everybody,indefpro,[type:indefp,lex:everybody,
                       agr:[gender:G,number:sing,person:3],
                       sem:[hum]]).

lex(everyone,indefpro,[type:indefp,lex:everyone,
                       agr:[gender:G,number:sing,person:3],
                       sem:[hum]]).

lex(everything,indefpro,[type:indefp,lex:everything,
                        agr:[gender:G,number:sing,person:3],
                        sem:[thing]]).

lex(anybody,indefpro,[type:indefp,lex:anybody,context:nonaff,
                       agr:[gender:G,number:sing,person:3],
                       sem:[hum]]).

lex(anyone,indefpro,[type:indefp,lex:anyone,context:nonaff,
                       agr:[gender:G,number:sing,person:3],
                       sem:[hum]]).

lex(anything,indefpro,[type:indefp,lex:anything,context:nonaff,
                        agr:[gender:G,number:sing,person:3],
                        sem:[thing]]).

lex(nobody,indefpro,[type:indefp,lex:nobody,pol:neg,
                        agr:[gender:G,number:sing,person:3],
                        sem:[hum]]).

lex(noone,indefpro,[type:indefp,lex:noone,pol:neg,
                        agr:[gender:G,number:sing,person:3],
                        sem:[hum]]).
                       
lex(nothing,indefpro,[type:indefp,lex:nothing,pol:neg,
                       agr:[gender:G,number:sing,person:3],
                       sem:[thing]]).
                       


%%%%%%%%%%%
% PARTICLES
%%%%%%%%%%%

% Notice that weight and gap are assigned to these elements
% so that they can appear as args, often athematic ones

lex(about,part,
[pos:part,txt:about,lex:about,weight:2,gap:[]]).

lex(around,part,
[pos:part,txt:around,lex:around,weight:2,gap:[]]).

lex(aside,part,
[pos:part,txt:aside,lex:aside,weight:2,gap:[]]).

lex(down,part,
[pos:part,txt:down,lex:down,weight:2,gap:[]]).

lex(through,part,
[pos:part,txt:through,lex:through,weight:2,gap:[]]).



%%%%%%%%%%%%%%%
% SUBORDINATORS
%%%%%%%%%%%%%%%

lex(that,subordinator,
[pos:subordinator,txt:that,lex:that,weight:1,gap:[]]).

lex(if,subordinator,
[pos:subordinator,txt:if,lex:if,weight:1,gap:[]]).

lex(whether,subordinator,
[pos:subordinator,txt:whether,lex:whether,weight:1,gap:[]]).


%%%%%%%%%%%%%%%
% COORDINATORS
%%%%%%%%%%%%%%%
       
lex(and,coord,[pos:coord,txt:and,lex:and]).
lex(',',coord,[pos:coord,txt:',',lex:comma]).

% adding BUT and OR would entail a revision of the coordination rules

% lex(but,coord,[pos:coord,txt:but,lex:but]).
% lex(or,coord,[pos:coord,txt:or,lex:or]).

%%%%%%%%%%%%%%
% PREPOSITIONS
%%%%%%%%%%%%%%

lex(about,prep,[txt:about,lex:about,weight:1,gap:[]]).
lex(against,prep,[txt:against,lex:against,weight:1,gap:[]]).
lex(among,prep,[txt:among,lex:among,weight:1,gap:[]]).
lex(around,prep,[txt:around,lex:around,weight:1,gap:[]]).
lex(at,prep,[txt:at,lex:at,weight:1,gap:[]]).
lex(by,prep,[txt:by,lex:by,weight:1,gap:[]]).
lex(for,prep,[txt:for,lex:for,weight:1,gap:[]]).
lex(from,prep,[txt:from,lex:from,weight:1,gap:[]]).
lex(in,prep,[txt:in,lex:in,weight:1,gap:[]]).
lex(of,prep,[txt:of,lex:of,weight:1,gap:[]]).
lex(on,prep,[txt:on,lex:on,weight:1,gap:[]]).
lex(through,prep,[txt:through,lex:through,weight:1,gap:[]]).
lex(to,prep,[txt:to,lex:to,weight:1,gap:[]]).
lex(with,prep,[txt:with,lex:with,weight:1,gap:[]]).
lex(without,prep,[txt:without,lex:without,weight:1,gap:[]]).

%%%%%%%%%%
% NEGATION
%%%%%%%%%%

lex(not,negation,[txt:not,lex:not]).

%%%%%%%%%%%%%%%%%%%%%
% fixed string idioms
%%%%%%%%%%%%%%%%%%%%%

% they are assigned a POS (Part of Speech) or PS (Phrase Structure) category
% they are recognised in the lexical pass by the [lex,lexicon2] rule
% the third arg is the appropriate feature list

string(det,[lots,of],[lex:a_lot_of,pos:det,type:quant,
      agr:[number:plural,person:3,gender:G]]).
string(det,[a,lot,of],[lex:a_lot_of,pos:det,type:quant,
      agr:[number:plural,person:3,gender:G]]).

% a bunch of possessives and reflexive pronouns
% carry the indef value in the person field of either agrposs (possessives)
% or agr (indefinites) to ensure that they are not used outside of their own
% very peculiar agreement world

string(det,[his,or,her],[lex:his_or_her,pos:det,type:poss_adj,
                         agr:[number:Nb,person:3,gender:G],
                         agrposs:[number:N1,person:indef,gender:G1]]).
string(det,[her,or,his],[lex:his_or_her,pos:det,type:poss_adj,
                         agr:[number:Nb,person:3,gender:G],
                         agrposs:[number:N1,person:indef,gender:G1]]).

string(refpro,[himself,or,herself],
                    [lex:himself_or_herself,
                     type:refp,agr:[gender:G,number:N,person:indef],
                    function:or([object,arg_prep,subject_inf]),sem:[hum]]).
                    
string(refpro,[her,or,himself],
                    [lex:himself_or_herself,
                     type:refp,agr:[gender:G,number:N,person:indef],
                    function:or([object,arg_prep,subject_inf]),sem:[hum]]).
string(refpro,[herself,or,himself],
                    [lex:himself_or_herself,
                     type:refp,agr:[gender:G,number:N,person:indef],
                    function:or([object,arg_prep,subject_inf]),sem:[hum]]).

string(refpro,[him,or,herself],
                    [lex:himself_or_herself,
                     type:refp,agr:[gender:G,number:N,person:indef],
                    function:or([object,arg_prep,subject_inf]),sem:[hum]]).
                    
string(adv,[by,and,large],[lex:by_and_large]).
string(adv,[to,and,fro],[lex:to_and_fro]).

string(pp,[at,bay],[lex:at_bay,weight:3,gap:[]]).
string(pp,[in,common],[lex:in_common,weight:3,gap:[]]).
string(pp,[in,a,poke],[lex:in_a_poke,weight:3,gap:[]]).
string(pp,[in,midstream],[lex:in_midstream,weight:3,gap:[]]).
string(pp,[in,'mid-stream'],[lex:in_midstream,weight:3,gap:[]]).

string(prep,[out,of],[lex:out_of]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% string constituents of idioms
% they are not assigned a POS or PS category
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

string(string,[havoc],[lex:havoc,weight:1,gap:[]]).
  % note the list format, even for a single element
string(string,[the,house],[lex:the_house,weight:1,gap:[]]).
string(string,[the,place],[lex:the_place,weight:1,gap:[]]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DB UTILS 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

build(Box,FS) :- not(recorded(Box,FS,_)), recorda(Box,FS,_).
known(Box,X) :- recorded(Box,X,_).


%%%%%%%%%%%%%%%%
%% Stopping ... 
%%%%%%%%%%%%%%%%

[stop,halting] ---> [recorded(pos,position(0,1,stop),_),
                     recorded(out,Lists,Ref),
                     close(Lists),
                     erase(Ref),
                     noprotocol,
                     abort].

% abort returns to Prolog... 



%%%%%%%%%%%%%%%%%%%%%%
%% LEXICAL PREDICATES 
%%%%%%%%%%%%%%%%%%%%%%

[lex,words] --->[recorded(pos,position(A,B,Word),_),
                 lex(Word,Box,FS),build(Box,[from:A,to:B|FS])].
 
/* a record (a feature bundle) is added to the db in the box corresponding
   to the lexical item's class
   the start and end positions of the item in the S are recorded
   in the first two features; the remainding features
   are read off the lexicon */

% fixed string idioms such as to and fro, by and large, etc.
% we also fill in the Weight and Gap features - the gap value is [].

[lex,strings] --->[recorded(pos,position(A,B,Word),_),
                     % we have the first word of the string
                    string(Box,[Word|OtherWords],FeatureList),
                    match(B,C,OtherWords),
                    % we attempt to match all the others
                    append([from:A,to:C,txt:[Word|OtherWords]],
                                 FeatureList,FullFeatureList),
                    build(Box,FullFeatureList)].   
                        % we pass the whole list as value for the txt feature

[lex,compounds] --->[recorded(pos,position(A,B,Word),_),
              % we have the first word of the compound
                    compound(Box,[Word|OtherWords],FeatureList),
                    match(B,C,OtherWords),
                    % we attempt to match all the others
                    append([from:A,to:C,txt:[Word|OtherWords]],
                           FeatureList,FullFeatureList),
                    build(Box,FullFeatureList)].   
                        % we pass the whole list as value for the txt feature


match(BeginPos,EndPos,[W|Ws]) :-
        recorded(pos,position(BeginPos,NextPos,W),_),
        match(NextPos,EndPos,Ws).

match(EndPos,EndPos,[]).


        
% PP idiom strings (at bay)
% if access to the preposition is needed (doubtful for AT BAY...),
% we expand the info gathered in the pp box by the string predicate,
% assigning Prep to the first element in the list
% making up the value of the lex feature



%%%%%%%%%%%%%%%%%%%%%
%% GRAMMAR PREDICATES 
%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%
% DETERMINERS 
%%%%%%%%%%%%%%%

/* genitive as determiner : the man's book, the teachers'books 
   note that this creates left recursion,
   as the det starts with an np, and an np starts
   with a det */

[gr,det1] --->
[known(apo,[from:B,to:C|FS1]),
known(np,[from:A,to:B|FS2]),
constraint([type:or([std,indefp])],FS2),
ifthenelse(known(apos,[from:C,to:D|FS3]),Next=D,Next=C),
    % with or without s after the apostrophe
build(det,[from:A,to:Next,pos:det, type:genitive,
    agr:[number:Nb,person:P,gender:G],gap:[],c_str:[det:FS2]])].

/*  if we have an np from position A to B,
   and a genitive marker from position B to C,
   or B to D (either ' or 's) we can conclude that we
have a det from position A to C/D;
 we do not yet know the number feature carried by the det; compare
the man's book / the man's books
the men's book / the men's books

we can determine the genitive np's function:
'determiner' with respect to the np it is attached to 
we can establish its constituent structure:
we simply carry over the feature bundle associated with the np */



%%%%%%%%%%%%%%%%%%%
% ADJECTIVE PHRASES 
%%%%%%%%%%%%%%%%%%%

[gr,adjp1] --->
[
 known(adj,[from:A,to:B|FS]),
 constraint([pos:adj,class:Class],FS),
 build(adjp,[from:A,to:B,cat:adjp,class:Class,
              weight:1, gap:[],c_str:[head:FS]])
].

% note that the arglist of the adj, if any, is not satisfied here,
% but only when it is known what function is played by the adj phrase
% with satisfied args, to avoid generating the latter member of the following
% pair:
% a man ready to ready a book
% * a ready to read a book man

%% coordinated adjps 
%%%%%%%%%%%%%%%%%%%%

[gr,adjp2] --->
[ known(coord,[from:B, to:C|FS2]),
  constraint([pos:coord],FS2),
  known(adjp,[from:A,to:B|FS1]),
  constraint([cat:adjp,class:Class],FS1),
  known(adjp,[from:C,to:D|FS3]),
  constraint([cat:adjp,class:Class],FS3),
  build(adjp,[from:A,to:D,cat:adjp,type:coordinated,
        class:Class,weight:2,gap=[],
        c_str:[head:FS1,coord:FS2,adjp:FS3]])].



%%%%%%%%%%%%%%
% NOUN PHRASES 
%%%%%%%%%%%%%%

% personal pronouns
%%%%%%%%%%%%%%%%%%%

[gr,np0] --->
[
 known(ppro,[from:A,to:B|FS]),
 constraint([agr:Agr,sem:Sem,lex:Lex,function:F],FS),
 build(np,[from:A,to:B,cat:np,type:pp,
       agr:Agr,sem:Sem,lex:Lex, function:F,
           weight:1,index:i(A,B),gap:[],c_str:[head:FS]])
].

% Notice that here the function is relevant and is copied over to the np node
% the predicate for satisfying the subject and the other args will ensure
% via funify that there is no incompatibility
% either the function has not been set, or it has been set as it should

% reflexive pronouns
%%%%%%%%%%%%%%%%%%%%

[gr,np0bis] --->
[
 known(refpro,[from:A,to:B|FS]),
 constraint([agr:Agr,sem:Sem,lex:Lex,function:F],FS),
 build(np,[from:A,to:B,cat:np,type:refp,
       agr:Agr,sem:Sem, lex:Lex,function:F,
           weight:1,index:i(A,B),gap:[],c_str:[head:FS]])
].

% indefinite pronouns
%%%%%%%%%%%%%%%%%%%%%

% we need to record the polarity feature in a special way:
% the pol feature is NOT INSTANTIATED when polarity is positive
% but we need to know that we have found neg pol in the np
% as it will percolate to the clause level
% if the np is a clause-level arg:
% "I know nothing" is just as neg as "I don't know anything"
% and "he minces no words" is a neg clause

[gr,np0ter] --->
[
 known(indefpro,[from:A,to:B|FS]),
 constraint([agr:Agr,sem:Sem,lex:Lex],FS),
 (constraint([pol:Polarity],FS) ->
 build(np,[from:A,to:B,cat:np,type:indefp,pol:Polarity,
       agr:Agr,sem:Sem,lex:Lex,
           weight:1,index:i(A,B),gap:[],c_str:[head:FS]]);
 build(np,[from:A,to:B,cat:np,type:indefp,
       agr:Agr,sem:Sem,lex:Lex,
           weight:1,index:i(A,B),gap:[],c_str:[head:FS]]))
].


% simple NP with zero determiner (uncountable N)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

[gr,np1] --->
[
 known(n,[from:A,to:B|FS]),
 constraint([pos:n,type:Type,count:u,class:Class,
            agr:[number:Nb,person:P,gender:G],
            sem:Sem,lex:Lex],FS),
 build(np,[from:A,to:B,cat:np,agr:[number:Nb,person:P,gender:G],sem:Sem,
           class:Class,type:Type,
           lex:Lex,weight:2,index:i(A,B),gap:[],c_str:[det:[det:zero],head:FS]])
].

% gerund without determiner
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

[gr,np1gerund_no_det] --->
[
 known(pred,[from:A,to:B|FS]),
 constraint([finite:gerund,gap:[],c_str:C_str],FS),
 \+ constraint([kill:yes],FS),
 build(np,[from:A,to:B,cat:np,agr:[number:sing,person:3,gender:neuter],
           sem:[abstract],
           class:std,type:gerund,
           lex:lex_gerund,weight:3,index:i(A,B),gap:[],
           c_str:[det:[det:zero],head:C_str]])
].

% gerund with genitive or possadj as determiner : subject of the gerund
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  % the teacher's refusing to mince her words
  % the AGREE check is performed at this level
  % it involves the HER of the lower clause (DAGR)
  % and the subject of the gerund (the teacher) retrieved here

 [gr,np1gerund_with_det_a] --->
[known(det,[from:A,to:B|FSDET]),
 constraint([type:TypeDet],FSDET),
 known(pred,[from:B,to:C|FSPRED]),
  constraint([finite:gerund,gap:[],c_str:C_str_pred],FSPRED),
 \+ constraint([kill:yes],FSPRED),

 \+ (constraint([type:tr_io_inf],FSPRED),
     constraint([voice:active],FSPRED)),
 % if BOTH tr_io_inf and active, the subject is not concerned in the
 % agreement checks carried by the lower clause
 
 constraint([subject:[constraints:Constraints]],C_str_pred),
 constraint([subject:[e:IndexSubj]],C_str_pred),

 (TypeDet=genitive;TypeDet=poss_adj),
 (TypeDet=poss_adj ->
     (constraint([agrposs:AgrSubj1],FSDET),
      lex(T ,ppro,[type:pp,lex:Lex,agr:AgrSubj,function:subject,sem:[hum]]),
      constraint(AgrSubj1,AgrSubj),
      Subject= [index:IndexSubj,lex:Lex,agr:AgrSubj,sem:[hum]]
       ); true),
 (TypeDet=genitive ->
  (constraint([c_str:[det:[agr:AgrSubj,sem:SemSubj,
                          lex:LexSubj,index:IndexSubj]]],FSDET),
   Subject=[index:IndexSubj, lex:LexSubj,agr:AgrSubj,sem:SemSubj]
     );  true),


 (pick(delayed_agr:DAGR,Constraints,NC) -> (Cos=NC, Flag=yes) ;
                                              Cos=Constraints),
  (nonvar(Flag) -> (agree(LexSubj,AgrSubj,DAGR),
                         constraint(Cos,Subject)) ;
                   constraint(Cos,Subject)),

  build(np,[from:A,to:C,cat:np,agr:[number:sing,person:3,gender:neuter],
           sem:[abstract],
           class:std,type:gerund,
           lex:lex_gerund,weight:3,index:i(A,C),gap:[],
           c_str:[det:FSDET,head:C_str_pred]])
].


% telling [the man not to mince his words]
% the AGREE check does not involve the subject of telling
% but should have been performed when building the lower clause
% i.e. the one bracketed in the example

 [gr,np1gerund_with_det_b] --->
[known(det,[from:A,to:B|FSDET]),
 constraint([type:TypeDet],FSDET),
 known(pred,[from:B,to:C|FSPRED]),
  constraint([finite:gerund,gap:[],c_str:C_str_pred],FSPRED),
 \+ constraint([kill:yes],FSPRED),

 constraint([type:tr_io_inf],FSPRED),
 constraint([voice:active],FSPRED),
 % the 2 constraints above make sure we are dealing
 % with the right kind of raising verbs (tell,ask)
 % and in the right voice ("being told not to mince his words"
 % DOES involve the subject of BEING TOLD in the check on the lower clause
 
 constraint([subject:[constraints:Constraints]],C_str_pred),
 constraint([subject:[e:IndexSubj]],C_str_pred),

 (TypeDet=genitive;TypeDet=poss_adj),
 (TypeDet=poss_adj ->
     (constraint([agrposs:AgrSubj1],FSDET),
      lex(T ,ppro,[type:pp,lex:Lex,agr:AgrSubj,function:subject,sem:[hum]]),
      constraint(AgrSubj1,AgrSubj),
      Subject= [index:IndexSubj,lex:Lex,agr:AgrSubj,sem:[hum]]
       ); true),
 (TypeDet=genitive ->
  (constraint([c_str:[det:[agr:AgrSubj,sem:SemSubj,
                          lex:LexSubj,index:IndexSubj]]],FSDET),
   Subject=[index:IndexSubj, lex:LexSubj,agr:AgrSubj,sem:SemSubj]
     );  true),

 (pick(delayed_agr:DAGR,Constraints,NC) -> Cos=NC ; Cos=Constraints),
  % if still in, the delayed_agr constraint should be discarded
  % it involves the IO, not the subject of the higher clause
  
  constraint(Cos,Subject),

  build(np,[from:A,to:C,cat:np,agr:[number:sing,person:3,gender:neuter],
           sem:[abstract],
           class:std,type:gerund,
           lex:lex_gerund,weight:3,index:i(A,C),gap:[],
           c_str:[det:FSDET,head:C_str_pred]])
].



% simple nps with non-zero determiner
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

[gr,np2] --->
[
known(det,[from:A,to:B|FS1]),
 known(n,[from:B,to:C|FS2]),
 constraint([pos:det,agr:[number:Nb]],FS1),
 constraint([pos:n,type:Type,agr:[number:Nb,person:P,gender:G],sem:Sem,
                 class:Class,lex:Lex],FS2),
(constraint([pol:Polarity],FS1) ->
   build(np,[from:A,to:C,cat:np,pol:Polarity,
            agr:[number:Nb,person:P,gender:G],sem:Sem,class:Class,
            type:Type,lex:Lex,
           weight:2,index:i(A,C),gap:[],c_str:[det:FS1,head:FS2]]);
   build(np,[from:A,to:C,cat:np,agr:[number:Nb,person:P,gender:G],
           sem:Sem,class:Class,type:Type,lex:Lex,
           weight:2,index:i(A,C),gap:[],c_str:[det:FS1,head:FS2]]))

].

/* each np is assigned an index (for coreference)
 pointing to the np boundaries ;
 the sharing of the variable will indicate coreference
(see nps and relative clauses) */

% we percolate the polarity from the det to the np
% remember, however, that polarity is set only if negative


% mwu the horses ('he spent all his money betting on the horses')
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

[gr,np2ater] --->
[
known(np,[from:A,to:B|FS1]),
 constraint([c_str:C_str],FS1), 
 constraint([det:[lex:the],head:[txt:horses,type:idiom]],C_str),
 build(np,[from:A,to:B,cat:np,type:std,lex:the_horses,
      agr:[number:plural,person:3,gender:neuter],sem:[abstract],
           weight:2,index:i(A,B),gap:[],c_str:C_str])
].


% mwu the horse's mouth ('he got it straight from the horse's mouth')
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

[gr,np2b] --->
[
known(det,[from:A,to:B|FS1]),
 known(n,[from:B,to:C|FS2]),
 constraint([c_str:[det:[c_str:[head:[txt:horse], det:[lex:the]]]]],FS1),
 constraint([txt:mouth],FS2),
 build(np,[from:A,to:C,cat:np,type:std,lex:the_horse_s_mouth,
           agr:[number:sing,person:3,gender:neuter],sem:[abstract,hum],
           weight:2,index:i(A,C),gap:[],c_str:[det:FS1,head:FS2]])
].

% we have to look inside the det, made up of the np the horse,
% to check that the inner determiner is
% 'the' and the head of the NP has 'horse' as value for txt
% we first look into the c_str value of the outer determiner,
% which includes a det feature whose value
% is the inner NP feature list
% inside that feature list is a c_str feature
% whose value is the constituency structure of the np 'the horse'
% we can look at the values of the det and head features
% to see if our requirements are met
% i.e. if the lex of the det is 'the' and the txt of the head noun is 'horse'
% we then combine the determiner 'the horse's' with the noun mouth
% to make up idiomatic 'the horse's mouth'
% to be used as constituent of the idiomatic pp 'from the horse's mouth'


%  (det+)adjp+n 
%%%%%%%%%%%%%%%

% (det) adjp n
% the value for the 'class' feature of the adjp must correspond 
% to the value of the 'type' feature of the noun
% this allows the restriction to 'idiomatic' of certain noun and adj readings
% 'kick the proverbial bucket' (2 readings) vs 'kick the red bucket' (1 reading)

[gr,np3a] --->
[
known(det,[from:A,to:B|FS1]),
 known(adjp,[from:B,to:C|FS2]),
 known(n,[from:C,to:D|FS3]),
 constraint([pos:det,agr:[number:Nb]],FS1),
 constraint([pos:n,type:Type,agr:[number:Nb,person:P,gender:G],
            sem:Sem,class:Class,lex:Lex],FS3),
 constraint([cat:adjp, class:Type],FS2),
 (constraint([pol:Polarity],FS1) ->
 build(np,[from:A,to:D,cat:np,pol:Polarity,
       agr:[number:Nb,person:P,gender:G],sem:Sem,
       type:Type,lex:Lex,class:Class,gap:[],
           weight:2,index:i(A,D),c_str:[det:FS1,adjp:FS2,head:FS3]]) ;
  build(np,[from:A,to:D,cat:np,
       agr:[number:Nb,person:P,gender:G],sem:Sem,lex:Lex,
       type:Type,class:Class,gap:[],
           weight:2,index:i(A,D),c_str:[det:FS1,adjp:FS2,head:FS3]]) )
].
% det adjp n 


[gr,np3b] --->
[
 known(adjp,[from:B,to:C|FS2]),
 known(n,[from:C,to:D|FS3]),
 constraint([pos:n,count:u,type:Type,class:Class,
     agr:[number:Nb,person:P,gender:G],sem:Sem,lex:Lex],FS3),
 constraint([cat:adjp, class:Type],FS2),
 build(np,[from:B,to:D,cat:np,
        agr:[number:Nb,person:P,gender:G],sem:Sem,lex:Lex,
        type:Type,class:Class,gap:[],
           weight:2,index:i(B,D),c_str:[det:[det:zero],adjp:FS2,head:FS3]])
].
% adjp n : uncountable, sg or plural : 'undue haste', 'insufficient staff'


[gr,np3c] --->
[
 known(adjp,[from:B,to:C|FS2]),
 known(n,[from:C,to:D|FS3]),
 constraint([pos:n,count:c,type:Type,
      agr:[number:plural,person:P,gender:G],sem:Sem,class:Class,lex:Lex],FS3),
 constraint([cat:adjp, class:Type],FS2),
 build(np,[from:B,to:D,cat:np,
       agr:[number:plural,person:P,gender:G],sem:Sem,lex:Lex,
       type:Type,class:Class,gap:[],
           weight:2,index:i(B,D),c_str:[det:[det:zero],adjp:FS2,head:FS3]])
].
% adjp n : countable, must be plural 'big stars', * 'big star'



% simple plural nps with zero det and no adj 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

[gr,np4] --->
[known(n,[from:A,to:B|FS1]),
 constraint([pos:n,type:Type,count:c,agr:[number:plural,person:P,gender:G],
      sem:Sem,class:Class,lex:Lex],FS1),
 build(np,[from:A,to:B,cat:np,
           agr:[number:plural,person:P,gender:G],sem:Sem,lex:Lex,
           type:Type,class:Class,
           gap:[],
           weight:1,index:i(A,B),c_str:[det:[det:zero],head:FS1]])].

% n : plural and countable: stars *star


% proper names
%%%%%%%%%%%%%%

[gr,np5] --->
[known(n,[from:A,to:B|FS1]),
 constraint([class:proper,type:Type,agr:[number:Nb,person:P,gender:G],
              sem:Sem,lex:Lex],FS1),
 build(np,[from:A,to:B,cat:np,
        agr:[number:Nb,person:P,gender:G],sem:Sem,type:Type,lex:Lex,gap:[],
           weight:1,index:i(A,B),c_str:[head:FS1]])].



% nps with pps
%%%%%%%%%%%%%%

% remmed out because giving rise to too many spurious parses
% as the link between np and pp is left unspecified

/*
[gr,np6] --->
[ known(np,[from:A,to:B|FS1]),
  known(pp,[from:B,to:C|FS2]),
  constraint([cat:np,agr:Agr,sem:Sem,lex:Lex,class:Class,gap:[]],FS1),
  (constraint([pol:Polarity],FS1) ->
  build(np,[from:A,to:C,cat:np,index:i(A,C),type:with_pp,class:Class,
     pol:Polarity,gap:[],
     agr:Agr,sem:Sem,lex:Lex,weight:3,c_str:[head:FS1,pp:FS2]]);
  build(np,[from:A,to:C,cat:np,index:i(A,C),type:with_pp,class:Class,gap:[],
     agr:Agr,sem:Sem,lex:Lex,weight:3,c_str:[head:FS1,pp:FS2]]))
     ].
 */

% MWU fly in the ointment
%%%%%%%%%%%%%%%%%%%%%%%%

[gr,np6a] --->
[ known(np,[from:A,to:B|FS1]),
  constraint([cat:np,agr:Agr,gap:[],c_str:[head:[lex:fly]]],FS1),
  known(pp,[from:B,to:C|FS2]),
  constraint([prep:in,
    c_str:[arg_prep:[c_str:[det:[lex:the],head:[txt:ointment]]]]],FS2),
  build(np,[from:A,to:C,cat:np,index:i(A,C),type:fly_in_the_ointment,
        gap:[],
     agr:Agr,sem:[abstract],lex:fly_in_the_ointment,
     weight:3,c_str:[head:FS1,pp:FS2]])].


% MWU pig in a poke
%%%%%%%%%%%%%%%%%%%

[gr,np6b] --->
[ known(np,[from:A,to:B|FS1]),
  constraint([cat:np,agr:Agr,gap:[],c_str:[head:[txt:pig],det:[lex:a]]],FS1),
  known(pp,[from:B,to:C|FS2]),
  constraint([lex:in_a_poke],FS2),
  build(np,[from:A,to:C,cat:np,index:i(A,C),type:a_pig_in_a_poke,gap:[],
              agr:Agr,sem:Sem,lex:a_pig_in_a_poke,
              weight:3,c_str:[head:FS1,pp:FS2]])].



%% coordinated nps 
%%%%%%%%%%%%%%%%%%

[gr,np7] --->
[ known(coord,[from:B, to:C|FS2]),
  constraint([pos:coord],FS2),
  known(np,[from:A,to:B|FS1]),
  constraint([cat:np,sem:Sem1,lex:Lex1,gap:Gap],FS1),
  known(np,[from:C,to:D|FS3]),
  constraint([cat:np,sem:Sem2,lex:Lex2,gap:Gap],FS3),
  union(Sem1,Sem2,Union_Sem),
  
  ( (constraint([pol:Polarity],FS1),constraint([pol:Polarity],FS3))  ->
  build(np,[from:A,to:D,cat:np,index:i(A,D),type:coordinated,
      pol:Polarity,gap:Gap,
     agr:[number:plural,person:P,gender:G],
      sem:Union_Sem,lex:Lex1,weight:3,c_str:[head:FS1,coord:FS2,np:FS3]]);
  build(np,[from:A,to:D,cat:np,index:i(A,D),type:coordinated,gap:Gap,
     agr:[number:plural,person:P,gender:G],
      sem:Union_Sem,lex:Lex1,weight:3,c_str:[head:FS1,coord:FS2,np:FS3]]))
   ].



/* whatever the number feature of the participant nps,
the whole is marked as plural:
the man and the woman are... the man and the women are...
men and women are... etc. */

% the semantic feature list of the resulting np
% is the union of the sem feature lists of the coordinated nps
% this is an oversimplification
% but note that it cannot be the intersection:
% mary and her house, the book and its author, etc

% polarity is set on the whole np only if set in both, i.e. neg all through

/* note that we can place the constraint on the coordinator top of the list,
to fail as early in possible -
the aim of a good Prolog program, as Sterling and Shapiro remind us... */


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% nps with sentential nouns and cplts 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

[gr,np8] --->
[ known(np,[from:A,to:B|FS1]),
  constraint([agr:[number:Nb,person:P,gender:G]],FS1),
  constraint([c_str:[head:[pos:n,class:sent,sem:Sem,lex:Lex]]],FS1),
  (known(subordinator,[from:B, to:C|FS4]) ->
  constraint([lex:that],FS4) ; % with subordinator
  C=B), % without
  known(pred,[from:C,to:D|FS3]),
  constraint([cat:pred,gap:[]],FS3),
  (constraint([pol:Polarity],FS1) ->
  build(np,[from:A,to:D,cat:np,pol:Polarity,agr:[number:Nb,person:P,gender:G],
         sem:Sem,lex:Lex,gap:[],
            weight:3,index:i(A,D),type:sentential,
           c_str:[head:FS1,pred:FS3]]) ;
   build(np,[from:A,to:D,cat:np,agr:[number:Nb,person:P,gender:G],
         sem:Sem,lex:Lex,gap:[],
            weight:3,index:i(A,D),type:sentential,
           c_str:[head:FS1,pred:FS3]]))
].

/* note that the pred(S) governed by the sent_noun must be gapless:
"the claim (that) the woman read a book" vs.
*"the book he believes the claim (that) the man read" */


% if the sentential np is followed by a rel clause and the S follows
% the whole NP-with-relative, the subordinator cannot be left out:
% he likes the claim she made that she had made a claim
% vs * he likes the claim she made she had made a claim
% this constraint is reflected in np8bis
% where the constraint on the sentential type of the n
% has to be fished from the depths on account of the additional
% layer of structure provided by the relative clause

[gr,np8bis] --->
[ known(np,[from:A,to:B|FS1]),
  constraint([agr:[number:Nb,person:P,gender:G],lex:Lex],FS1),
  constraint([c_str:[head:[c_str:[head:[pos:n,class:sent,sem:Sem]]]]],FS1),
  known(subordinator,[from:B, to:C|FS4]),
  constraint([lex:that],FS4), % the subordinator cannot be left out here
  known(pred,[from:C,to:D|FS3]),
  constraint([cat:pred,gap:[]],FS3),
  (constraint([pol:Polarity],FS1) ->
  build(np,[from:A,to:D,cat:np,pol:Polarity,agr:[number:Nb,person:P,gender:G],
         sem:Sem,lex:Lex,gap:[],
            weight:3,index:i(A,D),type:sentential,
           c_str:[head:FS1,pred:FS3]]) ;
   build(np,[from:A,to:D,cat:np,agr:[number:Nb,person:P,gender:G],
         sem:Sem,lex:Lex,gap:[],
            weight:3,index:i(A,D),type:sentential,
           c_str:[head:FS1,pred:FS3]]))
].



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% nps with relative clauses 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

[gr,np9a] --->
[known(np,[from:A,to:B|FS1]),
 constraint([cat:np,index:i(A,B),agr:[number:Nb,person:P,gender:G],
             sem:SemNP,lex:Lex],FS1),
 known(relative_clause,[from:B, to:C|FS2]),
 constraint([agr:[number:Nb,person:P,gender:G]],FS2),
 constraint([index:i(A,B), sem:SemRel,constraints:Constraints],FS2),
 (var(Constraints) -> CoCo=[];CoCo=Constraints),
 (pick(delayed_agr:DAGR,CoCo,NC) ->
    (agree(Lex,[number:Nb,person:P,gender:G],DAGR),
     CCheck=NC);
     (CCheck=CoCo)),
     
 % the delayed_agr feature enables us to deal with the agreement pattern
 % required by such mwus as 'pride oneself on one's X', where oneself and
 % one's have to agree with the subject, which is not available in the
 % predicate or the rel+predicate construction making up the relative clause:
 % the antecedent must be available, since we need access to its lex feature,
 % in order to deal with the special agr patterns of the
 % anybody,nobody,somebody etc set :
 % i don't like anyone who prides themselves on their looks
 % the Constraints are passed on from the gap in the pred to the relative
 % clause, and are therefore available here

(pick(delayed_coll:Collocates,CCheck,NC2) ->
    (nonvar(Collocates) -> (accu_w(Lex,Collocates,0-nil,CW),
                            NCo=NC2);NCo=NC2);
     (NCo=CCheck)),


 constraint(NCo,FS1), % here the constraints are checked
 (var(CW) -> WColl=0;WColl=CW),
 constraint([sem:SemRel],[sem:SemNP]),
 (constraint([pol:Polarity],FS1) ->
 build(np,[from:A, to:C, cat:np,type:with_rel_clause,pol:Polarity,gap:[],
             weight_coll:WColl,weight:3,index:i(A,C),
             agr:[number:Nb,person:P,gender:G],sem:SemNP,
             lex:Lex,   c_str:[head:FS1,rel_clause:FS2]]);
             
 build(np,[from:A, to:C, cat:np,type:with_rel_clause,gap:[],
             weight_coll:WColl,weight:3,index:i(A,C),
             agr:[number:Nb,person:P,gender:G],sem:SemNP,
             lex:Lex,   c_str:[head:FS1,rel_clause:FS2]]))
                ].


 %%%%%%%%%%%%%%%%%%%%%%%%%
 % NP WITH OTHER POST-MOD
 %%%%%%%%%%%%%%%%%%%%%%%%%
 
 
 % arg-bearing adjective in adjective phrase
 % a teacher unwilling to mince his words
 
 % ing-predication
 % a teacher refusing to mince his words
 
 % in both cases the subject of the outer verb and of the inner verb
 % within the complement of the adj or the verb is associated with
 % the head of the resulting noun phrase
 
 % a teacher [ teacher_e refusing [ teacher_e mince his words]]
 % a teacher [ teacher_e unwilling [ teacher_e mince his words]]


  [gr,np9b] --->
[known(np,[from:A,to:B|FS1]),
 constraint([cat:np,index:i(A,B),agr:[number:Nb,person:P,gender:G],
             sem:SemNP,lex:Lex],FS1),
 known(np_post_mod,[from:B, to:C|FS2]),
 constraint([subject:[e:i(A,B),constraints:Constraints]],FS2),
  % np_post_mod have a variable index slot and a slot storing constraints
  % to be satisfied by the 'subject', i.e. the head noun here
 
 (pick(delayed_agr:DAGR,Constraints,NC) ->
    (agree(Lex,[number:Nb,person:P,gender:G],DAGR),
     CCheck=NC);
     (CCheck=Constraints)),

 constraint(CCheck,FS1), % here the constraints are checked
 (constraint([pol:Polarity],FS1) ->
 build(np,[from:A, to:C, cat:np,type:with_postmod,pol:Polarity,gap:[],
             weight:3,index:i(A,C),agr:[number:Nb,person:P,gender:G],sem:SemNP,
             lex:Lex,   c_str:[head:FS1,post_mod:FS2]]);
 build(np,[from:A, to:C, cat:np,type:with_postmod,gap:[],
             weight:3,index:i(A,C),agr:[number:Nb,person:P,gender:G],sem:SemNP,
             lex:Lex,   c_str:[head:FS1,post_mod:FS2]]))
                ].


%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% RELATIVE CLAUSES %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%


%% contact clauses 
%%%%%%%%%%%%%%%%%%

% (the book) the teacher wrote

[gr,rel_clause_1] --->
[known(pred,[from:A, to:B|FS1]),
 constraint([gap:GAPARG,c_str:C_str],FS1),
 nonvar(GAPARG),
 constraint([cat:pred,finite:Finiteness],FS1),
 (Finiteness=yes;Finiteness=tovp),
 constraint([weight_coll:WC],FS1),
 GAPARG=[gap:[type:np,
                           index:i(X,Y),
                           function:F_in_relative,
                           F_in_relative:[e:i(X,Y)],
                           constraints:Constraints
% here the constraints are read in from the pred feature bundle
                           ]],
 F_in_relative \= subject,
  build(relative_clause,[from:A, to:B,
                         gap:GAPARG,
                         index:i(X,Y),
                         constraints:Constraints,
                         sem:Sem,
                         agr:[number:N,person:P,gender:G],
                         weight_coll:WC,
                         finite:Finiteness,
                         c_str:C_str])].

/* the function of the np is independent
from its function in the relative clause:
"the book the man read is good" 
the book is subject in the main clause and object in the relative
the index is shared; it reports the positions spanned by the np 
the relative clause is an S displaying a gap
corresponding to the antecedent: same 
cat and shared index */

/* the constraint on the pred contains in arg1 a feature
whose feature name is a variable; the call is bound to produce rubbish
if the variable is not instantiated by the time the call is made;
this is not a problem because when an arg is satisfied in the pred,
it is assigned a function,
which gets copied in the gap slot*/

/* the "F_in_relative:[e:i(A,B)]" bit allows
the instantiation of the slot for the 
gapped function in the c_str of the pred containing the deletion site
the info provided will be the function name
(set in the pred itself by the 'satisfy' predicate)
and the span of the np filling the
gap (set here by the antecedent's 'from:From' and 'to:To' features)  */

/* the gap site can specify any type of constraints
on the constituent structure of the antecedent NP;
this power is necessary to deal with mwus
where the deletion site can point to an NP
that must be lexically described,
not just in terms of features such as number and broad semantic category */



%% with a relative pronoun 
%%%%%%%%%%%%%%%%%%%%%%%%%%

% "(the man) who read the book" ; "(the book) that the man read"

[gr,rel_clause_2] --->
[known(relative,[from:A,to:B|FS1]),
constraint([agr:Agr],FS1),
 constraint([pos:rel,function:Functions,sem:SemRel],FS1),
 known(pred,[from:B, to:C|FS2]),
 constraint([cat:pred,finite:Finiteness],FS2),
 (Finiteness=yes;Finiteness=tovp),
 constraint([gap:GAPARG,c_str:C_str],FS2),
 nonvar(GAPARG),
 constraint([weight_coll:WC],FS2),
 GAPARG=[gap:[type:np,
                           index:i(X,Y),
                           function:F_in_relative,
                           F_in_relative:[e:i(X,Y)],
                             constraints:Constraints
                           ]],
 constraint([function:F_in_relative],[function:Functions]),
  build(relative_clause,[from:A, to:C,
                         gap:GAPARG,
                         index:i(X,Y),
                         sem:SemRel,
                         agr:Agr,
                         weight_coll:WC,
                         finite:Finiteness,
                         constraints:Constraints,
                         c_str:C_str]) ].


%% with prep and rel pro
%%%%%%%%%%%%%%%%%%%%%%%%

% "(the man) to whom the teacher gave the book" [pp-gap here]

[gr,rel_clause_3] --->
[known(prep,[from:A, to:B|FS1]),
 constraint([lex:Prep],FS1),
 known(relative,[from:B, to:C|FS2]),
 constraint([agr:Agr],FS2),
 constraint([postprep:yes,sem:SemRel],FS2),
 known(pred,[from:C, to:D|FS3]),
 constraint([cat:pred,finite:Finiteness], FS3),
 (Finiteness=yes;Finiteness=tovp),
 constraint([weight_coll:WC],FS3),
 constraint([gap:GAPARG,c_str:C_str],FS3),
 nonvar(GAPARG),
 GAPARG=[gap:[type:pp,
                           index:i(X,Y),
                           function:F_in_relative,
                           F_in_relative:[e:i(X,Y)],
                           constraints:Constraints
                           ]],

 select_ppconstraints(Constraints,PPConstraints),
 select_npconstraints(Constraints,NPConstraints),
% the NP constraints are projected to the relative clause constraints
 constraint(PPConstraints,[prep:Prep]),
 build(relative_clause,[from:A, to:D,
                         gap:GAPARG,
                         index:i(X,Y),
                         sem:SemRel,
                         agr:Agr,
                         weight_coll:WC,
                         finite:Finiteness,
                         constraints:NPConstraints,
                         c_str:C_str]) ].



 % conjoined relative clauses
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%
 
 % the man who reads books and who teaches linguistics
 % the man [you know[e] and she likes[e]]
 % the man who knows her and from whom she borrows books
 
  [gr,rel_clause_4] --->
[ known(coord,[from:B, to:C|FS2]),
  constraint([pos:coord],FS2),
  known(relative_clause,[from:A,to:B|FS1]),
  constraint([finite:Finiteness,
              sem:Sem1,
              agr:Agr,
              gap:GAPARG1,
              index:i(X,Y),
              constraints:Co1,
              weight_coll:WC1,
              c_str:C_str1],FS1),
  known(relative_clause,[from:C,to:D|FS3]),
  constraint([finite:Finiteness,
              sem:Sem2,
              agr:Agr,
              gap:GAPARG2,
              index:i(X,Y),
              weight_coll:WC2,
              constraints:Co2,
              c_str:C_str2],FS3),
  funify(Co1,Co2,UnifiedCos),
  funify(Sem1,Sem2,Sem),
  myappend([WC1],[WC2],WC),
  myappend(GAPARG1,GAPARG2,GAPARG),
  build(relative_clause,[from:A, to:D,
                         gap:GAPARG,
                         index:i(X,Y),
                         sem:Sem,
                         agr:Agr,
                         weight_coll:WC,
                         finite:Finiteness,
                         constraints:UnifiedCos,
                         c_str:[relative_clause:C_str1, coord:FS2,
                                relative_clause:C_str2]]) ].



 %%%%%%%%%%%%%%%%%%%
 % OTHER NP POSTMOD
 %%%%%%%%%%%%%%%%%%%


 % unwilling to mince his words
 
[gr,np_postmod_1] --->
[known(adjp,[from:A, to:B|FS1]),

 constraint([class:tovp,c_str:C_str],FS1),
 constraint([subject:[e:IndexSubj,
                          constraints:Constraints]],C_str),
 % the adjp stores the constraints to be placed on the subject of the
 % inner verb
 build(np_post_mod,[from:A, to:B,type:adjp,
                         subject:[e:IndexSubj,constraints:Constraints],
                         c_str:C_str])].


 % refusing to mince his words
 % here, as in relative clauses, we have a full predication,
 % but gapped - the gapped element must be the subject here
 % since the clause is a non-finite one, the subject is subject_inf :
 % i don't approve of him (*he) reading books students seem to like

[gr,np_postmod_2] --->
[known(pred,[from:A, to:B|FS1]),
 constraint([cat:pred,finite:ing],FS1),
 \+ constraint([kill:yes],FS1),
 constraint([gap:GAPARG,c_str:C_str],FS1),
 nonvar(GAPARG),
 GAPARG=[gap:[type:np,
                           index:i(X,Y),
                           function:subject_inf,
                           subject_inf:[e:i(X,Y)],
                             constraints:Constraints
                           ]],
 build(np_post_mod,[from:A, to:B, type:ing,
                         subject:[e:i(X,Y),constraints:Constraints],
                         c_str:C_str])].


 % coord postmod  : i know a teacher
 % unwilling to mince his words and not refusing to read books students like

   [gr,np_postmod_3] --->
[ known(coord,[from:B, to:C|FSCO]),
  constraint([pos:coord],FSCO),
  known(np_post_mod,[from:A, to:B|FS1]),
  known(np_post_mod,[from:C, to:D|FS2]),
  constraint([subject:[e:Index,constraints:Co1]],FS1),
  constraint([c_str:C_str1],FS1),
  constraint([type:Type],FS1),
  constraint([c_str:C_str2],FS2),
  constraint([subject:[e:Index,constraints:Co2]],FS2),
  funify(Co1,Co2,UnifiedCos),
  build(np_post_mod,[from:A, to:D, type:Type,
                         subject:[e:Index,constraints:UnifiedCos],
                         c_str:[post_mod:C_str1, coord:FSCO,
                                post_mod:C_str2]]) ].
                                

%%%%%%%%%%%%%%%%%%%%%%%
% PREPOSITIONAL PHRASES 
%%%%%%%%%%%%%%%%%%%%%%%

% no gap 
%%%%%%%%

[gr,pp1] --->
[ known(prep,[from:A, to:B|FS1]),
  known(np,[from:B,to:C|FS2]),
  constraint([lex:Prep],FS1),
  (constraint([pol:Polarity],FS2) ->
     build(pp,[from:A,to:C,cat:pp,pol:Polarity,prep:Prep,
            gap:[],weight:3,c_str:[head:FS1,arg_prep:FS2]]);
     build(pp,[from:A,to:C,cat:pp,prep:Prep,
            gap:[],weight:3,c_str:[head:FS1,arg_prep:FS2]]))
].


% np gap ; the function is set here
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

[gr,pp2] --->
[ known(prep,[from:A, to:B|FS1]),
  constraint([lex:Prep],FS1),
  build(pp,[from:A,to:B,cat:pp,prep:Prep,
           gap:[gap:[type:np,index:I,function:arg_prep,
                arg_prep:[e:I],
               constraints:Constraints]],
           weight:3,c_str:[head:FS1,arg_prep:[e:I]]
           ])
].

% mwu from the horse's mouth
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

[gr,pp1a] --->
[ known(prep,[from:A, to:B|FS1]),
  known(np,[from:B,to:C|FS2]),
  constraint([lex:the_horse_s_mouth],FS2),
  constraint([lex:from],FS1),
  build(pp,[from:A,to:C,cat:pp,lex:from_the_horse_s_mouth,prep:from,
            gap:[],weight:3,c_str:[head:FS1,arg_prep:FS2]])].



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% VERB GROUP
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% aux-v nexus


% auxiliaries
%%%%%%%%%%%%%%

prop(_,requires:Req,prop:[aspect:perfect],_) :-
constraint([requires:en_active], [requires:Req]).

prop(_,requires:Req,prop:[aspect:progressive],_) :-
constraint([requires:ing], [requires:Req]).

prop(_,requires:Req,prop:[voice:passive],_) :-
constraint([requires:en_passive], [requires:Req]).


% 'do' contributes to the modality field only when it is used
% with positive polarity

prop([do],requires:Req,prop:[mod:do-emphasis],pos) :-
constraint([requires:inf], [requires:Req]).

prop([do],requires:Req,prop:[],neg) :-
constraint([requires:inf], [requires:Req]).

prop([to],requires:Req,prop:[],_) :-
constraint([requires:inf], [requires:Req]).

% the Lex field is copied over in the modality field
% for all the aux's that require an infinitive to their right
% except do and to(the others are modal aux's)

prop(Lex,requires:Req,prop:[mod:Lex],_) :-
Lex \= [do],
Lex \= [to],
constraint([requires:inf], [requires:Req]).


% a single aux
%%%%%%%%%%%%%%

 % finite : "should"
[gr,auxgroup1] --->
[ known(aux,[from:A, to:B|FS1]),
  constraint([type:finite,lex:Lex,tense:Tense,
        agr:[number:N,person:P,requires:Req]],FS1),
  build(auxgroup,[from:A,to:B,type:Req,finite:yes,
        lex:Lex,tense:Tense,agr:[number:N,person:P,requires:Req]])].

 % non finite : "being"
[gr,auxgroup1bis] --->
[ known(aux,[from:A, to:B|FS1]),
  constraint([type:Type,lex:Lex,tense:Tense,
        agr:[number:N,person:P,requires:Req]],FS1),
  (Type=to;Type=ing),
  build(auxgroup,[from:A,to:B,type:Req,finite:Type,
        lex:Lex,tense:Tense,agr:[number:N,person:P,requires:Req]])].

 % finite with neg  : "should not"
 % the neg follows the aux
[gr,auxgroup1neg] --->
[ known(aux,[from:A, to:B|FS1]),
  known(negation,[from:B, to:C|FS2]),
  constraint([type:finite,lex:Lex,tense:Tense,
        agr:[number:N,person:P,requires:Req]],FS1),
  build(auxgroup,[from:A,to:C,type:Req,finite:yes,pol:neg,
        lex:Lex,tense:Tense,agr:[number:N,person:P,requires:Req]])].

 % non finite with neg : "not to" (remember that TO is an aux), "not having"
 % the neg precedes the aux
 
[gr,auxgroup1negbis] --->
[ known(negation,[from:A, to:B|FS2]),
  known(aux,[from:B, to:C|FS1]),
  constraint([type:Type,lex:Lex,tense:Tense,
        agr:[number:N,person:P,requires:Req]],FS1),
  (Type=to;Type=ing),
  build(auxgroup,[from:A,to:C,type:Req,finite:Type,pol:neg,
        lex:Lex,tense:Tense,agr:[number:N,person:P,requires:Req]])].


% the 'requires' feature of the auxgroup
% makes it ready to select the right verbal form to its right


% an aux followed by an auxchain
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

% neg polarity
[gr,auxgroup2neg] --->
[ known(aux,[from:A, to:B|FS1]),
  known(negation,[from:B, to:C|FSneg]),
  known(auxchain,[from:C,to:D|FS2]),
  constraint([lex:Lex,type:finite,tense:Tense,
       agr:[number:N,person:P,requires:Req]],FS1),
  constraint([type:Type,prop:OtherProperties,agr:[requires:NR]],FS2),
  constraint([type:Type],[type:Req]),
  prop(Lex,requires:Type,prop:Property,neg),
  prop(Lex,requires:Req,prop:Property,neg),
  append(Property,OtherProperties,Properties),
  build(auxgroup,[from:A,to:D,lex:Lex,finite:yes,pol:neg,
         tense:Tense,prop:Properties,type:Type,
         agr:[number:N,person:P,requires:NR]])].

 [gr,auxgroup2negbis] --->
[ known(negation,[from:A, to:B|FSneg]),
  known(aux,[from:B, to:C|FS1]),
  known(auxchain,[from:C,to:D|FS2]),
  constraint([lex:Lex,type:NF,tense:Tense,
       agr:[number:N,person:P,requires:Req]],FS1),
  (NF=to;NF=ing),
  constraint([type:Type,prop:OtherProperties,agr:[requires:NR]],FS2),
  constraint([type:Type],[type:Req]),
  prop(Lex,requires:Type,prop:Property,neg),
  prop(Lex,requires:Req,prop:Property,neg),
  append(Property,OtherProperties,Properties),
  build(auxgroup,[from:A,to:D,lex:Lex,finite:NF,pol:neg,
         tense:Tense,prop:Properties,type:Type,
         agr:[number:N,person:P,requires:NR]])].


 % positive polarity
[gr,auxgroup2] --->
[ known(aux,[from:A, to:B|FS1]),
  known(auxchain,[from:B,to:C|FS2]),
  constraint([lex:Lex,type:Finite,tense:Tense,
           agr:[number:N,person:P,requires:Req]],FS1),
  (Finite=finite -> Finiteness=yes; Finiteness=Finite),
  constraint([type:Type,prop:OtherProperties,agr:[requires:NR]],FS2),
  constraint([type:Type],[type:Req]),
  prop(Lex,requires:Type,prop:Property,pos),
  prop(Lex,requires:Req,prop:Property,pos),
  append(Property,OtherProperties,Properties),
  build(auxgroup,[from:A,to:C,lex:Lex,finite:Finiteness,
        tense:Tense,prop:Properties,type:Type,
        agr:[number:N,person:P,requires:NR]])].

% in an auxgroup of a finite verb group the first aux should be finite
% it can be followed by an auxchain of the right type,
% i.e. a type which can be constrained to fit the requires feature of the aux
% the Property is the one that results
% from both the 'requires' feature and the 'type' feature
% by specifying that it should be selected by both features
% we deal with OR-lists for either or both features
% the Property list is built up by both
% the opening aux and the aux's within the auxchain
% the new requires feature of the resulting auxgroup
% is culled from the auxchain, i.e. the rightmost element


% coordinated auxgroups
%%%%%%%%%%%%%%%%%%%%%%%%

[gr,coord_aux] --->
[ known(coord,[from:B, to:C|FS2]),
  constraint([pos:coord],FS2),
  known(auxgroup,[from:A, to:B|FS1]),
  constraint([agr:Agr1,finite:Finiteness],FS1),
  \+ constraint([requires:inf],Agr1),
  known(auxgroup,[from:C, to:D|FS3]),
  constraint([agr:Agr2,finite:Finiteness],FS3),
  funify(Agr1,Agr2,Agr),
  pick(agr:Agr1,FS1,FSS1),
  pick(agr:Agr2,FS3,FSS3),
  append(FSS1,FSS3,FSBoth), 
  build(auxgroup,[from:A, to:D, finite:Finiteness,coord:yes,agr:Agr|FSBoth])].


[gr,coord_aux_bis] --->
[ known(coord,[from:B, to:C|FS2]),
  constraint([pos:coord],FS2),
  known(auxgroup,[from:A, to:B|FS1]),
  constraint([lex:Lex1,finite:Finiteness],FS1),
  (Lex1 = [do] -> Lex1n = [do-emphasis]; Lex1n=Lex1),
  constraint([agr:Agr1],FS1),
  constraint([requires:inf],Agr1),
  known(auxgroup,[from:C, to:D|FS3]),
  constraint([lex:Lex2,finite:Finiteness],FS3),
  (Lex2 = [do] -> Lex2n = [do-emphasis]; Lex2n=Lex2),
  constraint([agr:Agr2],FS3),
  constraint([requires:inf],Agr2),
  funify(Agr1,Agr2,Agr),
  pick(agr:Agr1,FS1,FSS1),
  pick(agr:Agr2,FS3,FSS3),
  append(FSS1,FSS3,FSBoth),
  append(Lex1n,Lex2n,Lex),
  build(auxgroup,[from:A, to:D, coord:yes,finite:Finiteness,lex:Lex,
                  agr:Agr|FSBoth])].

% the agr features should funify to produce the resulting Agr value
% the auxgroup is marked as coord
% for the next rule to have a restricted scope
% we look for auxgroup+auxchain only if the auxgroup is coordinated:
% he [ [should have] and [might have] ] [been] reading
% auxgroup + auxgroup = coordinated auxgroup
% coordinated auxgroup + auxchain (been) is auxgroup
% auxgroup + required verbal form type (reading) makes up vg

% we need the aux_bis rule because the modality property is based on the lexical
% form of the modal auxiliaries, and therefore the value of the lex feature must
% result from the appending of the list values of the lex features of the two
% aux : he can and should read the book -->
% lex:[can] , lex:[should] --> lex:[can,should]
% which will be the basis for the computing of the mod feature of the Property
% list of the aux prop:[mod:[can,should]]


% coordinated auxgroup followed by an auxchain

[gr,coord_aux2] --->
[ known(auxgroup,[from:A, to:B|FSAux]),
  known(auxchain,[from:B,to:C|FSChain]),
  constraint([lex:Lex,finite:Finiteness,coord:yes,tense:Tense,
            agr:[number:N,person:P,requires:Req]],FSAux),
  constraint([type:Type,prop:OtherProperties,agr:[requires:NR]],FSChain),
  constraint([type:Type],[type:Req]),
  prop(Lex,requires:Type,prop:Property,_),
  prop(Lex,requires:Req,prop:Property,_),
  append(Property,OtherProperties,Properties),
  (constraint([pol:Polarity],FSAux) ->
  build(auxgroup,[from:A,to:C,lex:Lex,finite:Finiteness,pol:Polarity,
          tense:Tense,
          prop:Properties,type:Type,agr:[number:N,person:P,requires:NR]]);
  build(auxgroup,[from:A,to:C,lex:Lex,finite:Finiteness,
          tense:Tense,
          prop:Properties,type:Type,agr:[number:N,person:P,requires:NR]]))

  ].


% auxchains
%%%%%%%%%%%

[gr,auxchain1] --->
[ known(aux,[from:A, to:B|FS1]),
  constraint([type:Type,lex:Lex,agr:Agr],FS1),
  build(auxchain,[from:A,to:B,prop:[],type:Type,lex:Lex,agr:Agr])]. 

% an auxchain can be built out of a single aux;
% it has then an empty prop feature

[gr,auxchain2] --->
[ known(aux,[from:A, to:B|FS1]),
  constraint([lex:Lex1,agr:[requires:Req]],FS1),
  constraint([type:OuterType],FS1),
  known(auxchain,[from:B, to:C|FS2]),
  constraint([type:Type,lex:Lex,prop:OtherProperties,agr:Agr],FS2),
  constraint([type:Type],[type:Req]),
  prop(Lex1,requires:Type,prop:Property,_),
  prop(Lex,requires:Req,prop:Property,_),
  append(Property,OtherProperties,Properties), 
  build(auxchain,[from:A,to:C,lex:Lex,prop:Properties,type:OuterType,agr:Agr])]. 

% an auxchain can also be made up
% of an aux followed by the required auxchain type
% the property field is contributed by both
% the first aux and the required auxchain



% Verb Groups
%%%%%%%%%%%%%


% WITH AUX
%%%%%%%%%%%

[gr,vg1] --->
[ known(auxgroup,[from:A, to:B|FS1]),
  known(v,[from:B,to:C|FS2]),
  constraint([finite:Finiteness,lex:Lex,
              agr:[number:N,person:P,requires:Req]],FS1),
  constraint([type:Type, class:Class],FS2),
  constraint([type:Type],[type:Req]),
  (constraint([pol:Pol],FS1) ->
  ( prop(Lex,requires:Type,Property,neg),
       build(vg,[from:A,to:C,aux:FS1,Property,finite:Finiteness,
            class:Class,pol:Pol,
            agr:[number:N,person:P,gender:G]|FS2])
            ) ;
  ( prop(Lex,requires:Type,Property,pos),
  build(vg,[from:A,to:C,aux:FS1,Property,finite:Finiteness,class:Class,
            agr:[number:N,person:P,gender:G]|FS2]) ))

].

% an auxgroup and the required V
% remember that TO is treated as an aux so that 'to read', for instance,
% is parsed by the above as a vg (with the 'finite' feature set to 'to')
 

 % MAIN VERB ALONE
 %%%%%%%%%%%%%%%%%%

 % POS POL
 %%%%%%%%%

[gr,vg2] --->
[ known(v,[from:A, to:B|FS1]),
  constraint([agr:[number:N,person:P],type:Type],FS1),
  (Type=finite -> Finiteness=yes;Finiteness=Type),
  build(vg,[from:A, to:B,cat:vg,prop:[],
        finite:Finiteness,agr:[number:N,person:P,gender:G]|FS1])].

% the V alone
% the agr feature is extended to include the gender feature,
% as it is passed in the reading of the subject
% where it is matched with the agr feature on nps, which does include
% gender to take care of nps where it is relevant (pronouns)

 % NEG POL
 %%%%%%%%%
 
% with a negation : the V cannot be finite : *not refuses vs not refusing

[gr,vg2] --->
[ known(negation,[from:A, to:B|FSneg]),
  known(v,[from:B, to:C|FS1]),
  constraint([agr:[number:N,person:P],type:Type],FS1),
  Type \= finite,
  build(vg,[from:A, to:C,cat:vg,prop:[],pol:neg,
        finite:Type,agr:[number:N,person:P,gender:G]|FS1])].


%%%%%%%%%%%%%%
% full verb BE
%%%%%%%%%%%%%%

% POS POL
%%%%%%%%%

[gr,vg3bepos] --->
[ known(v,[from:A, to:B|FS1]),
  constraint([agr:[number:N,person:P],class:be,type:Type],FS1),
  (Type=finite -> Finiteness=yes;Finiteness=Type),
  build(vg,[from:A, to:B,cat:vg,prop:[],
        finite:Finiteness,agr:[number:N,person:P,gender:G]|FS1])].

% NEG POL
%%%%%%%%%%

% finite : the negation follows : is not
[gr,vg3beneg_a] --->
[ known(v,[from:A, to:B|FS1]),
  known(negation,[from:B, to:C|FSneg]),
  constraint([agr:[number:N,person:P],class:be,type:finite],FS1),
  build(vg,[from:A, to:C,cat:vg,prop:[],pol:neg,
        finite:yes,agr:[number:N,person:P,gender:G]|FS1])].

% non finite : the negation precedes :  not being
[gr,vg3beneg_b] --->
[ known(v,[from:B, to:C|FS1]),
  known(negation,[from:A, to:B|FSneg]),
  constraint([agr:[number:N,person:P],class:be,type:Type],FS1),
  Type \= finite,
  build(vg,[from:A, to:C,cat:vg,prop:[],pol:neg,
        finite:Type,agr:[number:N,person:P,gender:G]|FS1])].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PREDICATES : Subject+VP nexus
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TYPE 1
% WITH A SUBJECT (which can be a gapped one)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Note the distinction between "to" and "tovp" as values
% for the Finiteness feature
% when the value is "to" a subject is expected for the infinitive clause
% even if it is liable to be gapped
% when the value is "tovp" the subject cannot appear in the surface structure
% of the infinitive clause : it must be retrieved from higher up
%  i believe [her to have written a book] ("to")
%  the man I believe [the man_e to have written a book] ("to")
%  i want [to write a book] / the book i want [to write the book_e] ("tovp")

[gr,pred1a] --->
[known(vg,[from:B,to:C|FSVerb]),
 % we have a verb group

 constraint([class:Class,finite:Finiteness,prop:Properties,
             agr:Agr,arglist:Arglist,ft:Features],FSVerb),

 (Finiteness=to;Finiteness=yes;Finiteness=ing),
 \+ raising(Class),
 Class \= be,
 % raising verbs and main verb BE are allowed to look
 % into their dependent clauses
 % to 'send down' a subject, and therefore receive a special treatment
 % for which see below
 
 (constraint([voice:passive],Properties) -> Voice=passive; Voice=active),
  % we work out the value of the voice feature
  % and set it if necessary (active voice)
 (constraint([pol:Polarity],FSVerb) -> PolClause=Polarity;  true),
  % we do the same for polarity
  % polarity info can also be gleaned from the args (nobody reads books...)

 reorg(Class,Arglist,NewArglist,Voice),
 % we reorganize the arglist in case of need (i.e. passive voice)

 satisfy_subject(Agr,NewArglist,OtherArgs,FSSubject,
                 Gap1,BeginSubject,B,PrecSubject,PolSubject,Finiteness),
  % we satisfy the subject arg, building its feature bundle
 % the B position (beginning of verb) should match
 % the ending position of the subject

 ( (nonvar(PolSubject),var(PolClause)) -> PolClause=PolSubject; true),
 % another chance to set polarity if not yet done
 % note that we do not overwrite polarity once set

 satisfylist(OtherArgs,FSArgs,Gap2,C,Upto,Prec1,PolArgs),
 % we satisfy all the other args of the arglist
 % we start from the end position of the predicate, up to the Upto position
 % to be computed by satisfylist

 ( (nonvar(PolArgs),var(PolClause)) -> PolClause=PolArgs; true),

 checkgap(Class,FSArgs,Yes_or_No),
 % this check eliminates that-cplts that feature complementizer "that"
 % and a subject gap in their sentential complement:
 % * the teacher likes the students the inspector thinks that made the claims
 % versus '... thinks made the claims', which is ok
%  'no' is returned only if the cpltizer is present
% and the gapped element is the subject

 Yes_or_No = yes,

 (nonvar(PolClause) -> PC=[pol:PolClause] ; PC=[]),
  funify([voice:Voice],FSVerb,FSV),
  funify(PC,FSV,FullF),
 % if polarity is set we add it to the clause features, alongside with voice

 (constraint([pc:PConstraints],Features) -> PCC=PConstraints;PCC=[]),
 % we look whether we have constraints to satisfy at clause level,
 % the constraints in the pc box of the ft box (Features)
 
 myappend(Gap1,Gap2,ALLGaps),
 % myappend is an append that deals on the fly with uninstantiated vars
 % and empty lists
 
 dw(PCC,ALLGaps,NewGaps,FullF,FSArgs,Added,WeightColl),
 % we deal here with the constraints to be satisfied on the whole clause
 % dw is allowed to modify the gapped element if needed
 
 (var(NewGaps) -> Gaps=ALLGaps; Gaps=NewGaps),

  insort(FSArgs,OrderedFS),
 % the parse gives the args in canonical order in the c_str feature

 append(FSSubject,OrderedFS,Args),
 append([head:FullF],Args,C_str),
 
 
 % in case the kill feature has not been set at this level
 % by the PC constraints, it can be imported from a clause at a lower level
 % and manipulated by negtransportation or negswitch

 (var(Added) -> % THEN 1
               (constraint([negtransport:yes],Features) ->
                     % THEN 2
                    ( (Function=object;Function=extraposed_subject),
                        constraint([Function:Object],FSArgs),
                        constraint([kill:KillValue],Object),
                         (nonvar(PolClause) ->  % we are in a neg clause
                                                  % THEN 3
                                         (rev(KillValue,RevValue),
                                             Added=kill:RevValue); % we reverse
                                             Added=kill:KillValue)) % ELSE 3
                                                                     % we don't
                       ;  true); % ELSE 2 we are not in a negtransport context
                                  % do nothing
 true),  % ELSE 1 the KILL feature has been set locally
         % do nothing
                

 (var(Added) -> (constraint([negswitch:yes],Features) ->
                   ((Function=object;Function=extraposed_subject),
                    constraint([Function:Object],FSArgs),
                    constraint([kill:KillValue],Object),
                    rev(KillValue,RevValue), % we reverse the value
                    Added=kill:RevValue); true)
                ;  true),

  (var(WeightColl) -> WC=0; WC=WeightColl),
  
 % addition of the kill feature if set, otherwise we add 'kill:unspec'
 (nonvar(Added) ->
    build(pred,[from:BeginSubject, to:Upto,
                cat:pred,finite:Finiteness, voice:Voice,
                Added,weight_coll:WC,
                type:Class,weight:3,gap:Gaps,c_str:C_str]);

    build(pred,[from:BeginSubject, to:Upto,
                cat:pred,finite:Finiteness, voice:Voice,
                kill:unspec,weight_coll:WC,
                type:Class,weight:3,gap:Gaps,c_str:C_str]))].



% RAISING VERBS
%%%%%%%%%%%%%%%%


% WANT / SEEM
%%%%%%%%%%%%%

% + MAIN VERB BE on account of the predicates which can be part of the arg:
% the teacher is unwilling to mince his words
% here too the subject of the verb (the teacher) must be 'sent down' as
% subject of the verb within the arg adjective phrase, i.e. mince_one's_words


[gr,pred1b_a] --->
[known(vg,[from:B,to:C|FSVerb]),
 % we have a verb group

 constraint([class:Class,
             finite:Finiteness,prop:Properties,
             agr:Agr,arglist:Arglist,ft:Features],FSVerb),
   (Finiteness=to;Finiteness=yes;Finiteness=ing),
 (Class=tr_vp ; Class=tr_inf_gap;Class=be),
  % tr_vp is the class assigned to want in 'I want to teach linguistics'
  % note that 'I want her to teach linguistics' exemplifies want in a
  % different class, a non-raising one, namely tr_inf
  % tr_inf_gap is the class assigned to seem in 'he seems to be writing a book'
 
  \+ constraint([voice:passive],Properties),
  % these verbs cannot display a passive voice
  % 'he is expected to teach linguistics' exemplifies the tr_inf expect
  
 (constraint([pol:Polarity],FSVerb) -> PolClause=Polarity;  true),
  % we establish polarity
  % polarity info can also be gleaned from the args (nobody reads books...)

 satisfy_subject(Agr,Arglist,OtherArgs,FSSubject,
                 Gap1,BeginSubject,B,PrecSubject,PolSubject,Finiteness),
  % we satisfy the subject arg, building its feature bundle
 % the B position (beginning of verb) should match
 % the ending position of the subject
 % the GapFlag is set as soon as a gap is found,
 
 FSSubject=[subject:FSS],
 % we isolate the subject info
 
 (constraint([e:i(X,Y)],FSS) -> IndexS=i(X,Y) ; % empty subject
         constraint([agr:AgrS,lex:LexS,index:IndexS],FSS)), % full subject

  % in the case of an empty subject (gapped) we cannot get at the info
  % we need to satisfy agreement requirements
  % otherwise we collect the relevant info; equivalent to:
   %(Gap1=[] -> constraint([agr:AgrS,lex:LexS,index:IndexS],FSS) ; true),

 ( (nonvar(PolSubject),var(PolClause)) -> PolClause=PolSubject; true),
 % another chance to set polarity if not yet done
 % note that we do not overwrite polarity once set

 satisfylist(OtherArgs,FSArgs,Gap2,C,Upto,Prec1,PolArgs),
 % we satisfy all the other args of the arglist
 % we start from the end position of the predicate, up to the Upto position
 % to be computed by satisfylist

 % sending down the subject:

    % WANT-type verbs
   (constraint([object:Object],FSArgs) ->
   (constraint([c_str:[subject:[constraints:Constraints]]],Object),
    constraint([c_str:[subject:[e:IndexS]]],Object));true),

    % SEEM
   (constraint([extraposed_subject:Object],FSArgs) ->
   (constraint([c_str:[subject:[constraints:Constraints]]],Object),
    constraint([c_str:[subject:[e:IndexS]]],Object));true),

    % BE
    % here we must make sure that the complement
    % is of the right type, i.e. tovp
    % the subject is to be found at two levels
    % that of the arg itself, and that of the predication it contains

  (constraint([complement:Object],FSArgs) ->
   (constraint([class:tovp],Object) ->
   (constraint([c_str:[subject:[constraints:Constraints]]],Object),
   constraint([c_str:[subject:[e:IndexS]]],Object),
constraint([c_str:[object:[c_str:[subject:[e:IndexS,constraints:Constraints]]]]],
                Object)); true) ; true),


     % the Constraints will be uninstantiated if we are dealing
     % with be with a non-tovp arg, e.g. 'the bucket is red'
     %                                 or 'he is a teacher'
    (nonvar(Constraints); Constraints=[]),
 
  (Gap1=  [gap:[type:T,index:IndexS,function:Su,
                      Su:[e:IndexS],
                     constraints:GapConstraints]] ->
                     
  (append(Constraints,GapConstraints,NewGapConstraints),
  % in the case of a gapped subject
  % we can only copy the constraints inherited from the subject of the lower
  % clause (a subject that does not exist as such, but on which requirements
  % can be set
  
  NGap1=  [gap:[type:T,index:IndexS,function:Su,
                      Su:[e:IndexS],
                     constraints:NewGapConstraints]])
  % the new constraints are set on the gapped subject at this level
  
   ;
  ( NGap1=[],
   (pick(delayed_agr:DAGR,Constraints,NC) -> (Cos=NC, Flag=yes) ;
                                              Cos=Constraints),
  (nonvar(Flag) -> (agree(LexS,AgrS,DAGR), constraint(Cos,FSS)) ;
                   constraint(Cos,FSS)))),

   % we do not have a gapped subject, i.e. we have a full one
   % the delayed_agr feature can then be checked on the subject
   % we provide the values for the subject and execute the agree goal
   % if we have no agreement check pending, we simply execute the
   % constraints on the subject set in the lower clause
   % of course we do so too after satisfying the agreement check


   ( (nonvar(PolArgs),var(PolClause)) -> PolClause=PolArgs; true),
   (nonvar(PolClause) -> PC=[pol:PolClause] ; PC=[]),
   
  funify([voice:active],FSVerb,FSV),  % recall that voice is active here
  funify(PC,FSV,FullF),
 % if pol is set we add it to the clause features
 
 (constraint([pc:PConstraints],Features) -> PCC=PConstraints;PCC=[]),
 dw(PCC,NGap1,GapS,FullF,FSArgs,Added,WeightColl),
 % we deal here with the constraints to be satisfied on the whole clause


 (var(GapS) -> GapSubj=NGap1; GapSubj=GapS),

 myappend(GapSubj,Gap2,ALLGaps),
% flatten(Gaps,GapF),

(ALLGaps=[]; ALLGaps=[gap:GGG]),
% only one gap allowed in all finite clauses
% the non-finite ones are allowed several gaps to deal with reduced clauses
% for instance reduced relative clauses
% i know a reacher whom [gapped subject] to borrow books from [gapped arg_prep]

(var(Added) -> (constraint([negtransport:yes],Features) ->
                  (   constraint([kill:KillValue],Object),
                      (nonvar(PolClause) -> (rev(KillValue,RevValue),
                                             Added=kill:RevValue);
                                             Added=kill:KillValue))
                       ;  true);
 true),


 (var(Added) -> (constraint([negswitch:yes],Features) ->
                   (constraint([kill:KillValue],Object),
                    rev(KillValue,RevValue),
                    Added=kill:RevValue); true)
                ;  true),

  (var(WeightColl) -> WC=0; WC=WeightColl),
 insort(FSArgs,OrderedFS),
 % the parse gives the args in canonical order in the c_str feature

 append(FSSubject,OrderedFS,Args),
 append([head:FullF],Args,C_str),

 % addition of the kill feature if set, otherwise we add 'kill:unspec'
 (nonvar(Added) ->
    build(pred,[from:BeginSubject, to:Upto,
                cat:pred,finite:Finiteness, voice:active,
                Added,weight_coll:WC,
                type:Class,weight:3,gap:ALLGaps,c_str:C_str]);

    build(pred,[from:BeginSubject, to:Upto,
                cat:pred,finite:Finiteness, voice:active,
                kill:unspec,weight_coll:WC,
                type:Class,weight:3,gap:ALLGaps,c_str:C_str]))].



% TELL, ASK
%%%%%%%%%%%%

% active voice : the indirect object is the controller
% they ask the teacher [e_the teacher to write a book]


[gr,pred1b_b_a] --->
[known(vg,[from:B,to:C|FSVerb]),
 % we have a verb group

 constraint([class:tr_io_inf,
             finite:Finiteness,prop:Properties,
             agr:Agr,arglist:Arglist,ft:Features],FSVerb),
    (Finiteness=to;Finiteness=yes;Finiteness=ing),
  \+ constraint([voice:passive],Properties),
  % we check we do not have a passive voice

 (constraint([pol:Polarity],FSVerb) -> PolClause=Polarity;  true),
  % we do the same for polarity
  % polarity info can also be gleaned from the args (nobody reads books...)

  satisfy_subject(Agr,Arglist,OtherArgs,FSSubject,
                 Gap1,BeginSubject,B,PrecSubject,PolSubject,Finiteness),
  % we satisfy the subject arg, building its feature bundle
 % the B position (beginning of verb) should match
 % the ending position of the subject
 % the GapFlag is set as soon as a gap is found,

  ( (nonvar(PolSubject),var(PolClause)) -> PolClause=PolSubject; true),
 % another chance to set polarity if not yet done
 % note that we do not overwrite polarity once set

 satisfylist(OtherArgs,FSArgs,Gap2,C,Upto,Prec1,PolArgs),
 % we satisfy all the other args of the arglist
 % we start from the end position of the predicate, up to the Upto position
 % to be computed by satisfylist

 % sending down the subject:
  (Function=object;Function=extraposed_subject),
  constraint([i_object:I_Object],FSArgs),
  (I_Object=[e:i(X,Y)|Remainder] -> IndexIO=i(X,Y);
                 constraint([agr:AgrIO,lex:LexIO,index:IndexIO],I_Object)),
  constraint([Function:Object],FSArgs),
  constraint([c_str:[subject:[constraints:Constraints]]],Object),
  constraint([c_str:[subject:[e:IndexIO]]],Object),
  
  (Gap2=  [gap:[type:T,index:IndexIO,function:i_object,
                      i_object:[e:IndexIO],
                     constraints:GapConstraints]] ->

  (append(Constraints,GapConstraints,NewGapConstraints),
   NGap2=  [gap:[type:T,index:IndexIO,function:i_object,
                      i_object:[e:IndexIO],
                     constraints:NewGapConstraints]])
   ;
   ( NGap2=[],
  (pick(delayed_agr:DAGR,Constraints,NC) -> (Cos=NC, Flag=yes) ;
                                              Cos=Constraints),
  (nonvar(Flag) -> (agree(LexIO,AgrIO,DAGR), constraint(Cos,I_Object)) ;
                   constraint(Cos,I_Object)))),


  ( (nonvar(PolArgs),var(PolClause)) -> PolClause=PolArgs; true),
   (nonvar(PolClause) -> PC=[pol:PolClause] ; PC=[]),
  funify([voice:active],FSVerb,FSV),
  funify(PC,FSV,FullF),
 % if pol is set we add it to the clause features
 
 (constraint([pc:PConstraints],Features) -> PCC=PConstraints;PCC=[]),
 dw(PCC,Gap1,GapS,FullF,FSArgs,Added,WeightColl),
 % we deal here with the constraints to be satisfied on the whole clause
 % they are housed in the [pc:List] in lexical entries

 (var(GapS) -> GapSubj=Gap1; GapSubj=GapS),

 myappend(GapSubj,NGap2,ALLGaps),
(ALLGaps=[]; ALLGaps=[gap:GGG]),

(var(Added) -> (constraint([negtransport:yes],Features) ->
                  (   constraint([kill:KillValue],Object),
                      (nonvar(PolClause) -> (rev(KillValue,RevValue),
                                             Added=kill:RevValue);
                                             Added=kill:KillValue))
                       ;  true);
 true),


 (var(Added) -> (constraint([negswitch:yes],Features) ->
                   (constraint([kill:KillValue],Object),
                    rev(KillValue,RevValue),
                    Added=kill:RevValue); true)
                ;  true),

   (var(WeightColl) -> WC=0; WC=WeightColl),
 insort(FSArgs,OrderedFS),

 % the parse gives the args in canonical order in the c_str feature

 append(FSSubject,OrderedFS,Args),
 append([head:FullF],Args,C_str),
 % addition of the kill feature if set, otherwise we add 'kill:unspec'
 (nonvar(Added) ->
    build(pred,[from:BeginSubject, to:Upto,
                cat:pred,finite:Finiteness, voice:active,
                Added,weight_coll:WC,
                type:tr_io_inf,weight:3,gap:ALLGaps,c_str:C_str]);

    build(pred,[from:BeginSubject, to:Upto,
                cat:pred,finite:Finiteness, voice:active,
                kill:unspec,weight_coll:WC,
                type:tr_io_inf,weight:3,gap:ALLGaps,c_str:C_str]))].



 % TELL/ASK
 % PASSIVE VOICE
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 % the controller is the subject of the higher clause
 % the teacher was told [the teacher_e to write a book]
 % also : tr_inf_pass : the teacher is believed [the teacher_e] to write books


 [gr,pred1b_b_b] --->
[known(vg,[from:B,to:C|FSVerb]),
 % we have a verb group

 constraint([class:Class,
             finite:Finiteness,prop:Properties,
             agr:Agr,arglist:Arglist,ft:Features],FSVerb),
 (Class=tr_inf_pass; Class=tr_io_inf),
   (Finiteness=to;Finiteness=yes;Finiteness=ing),
 % tr_inf_pass is the class associated with the passive voice of such verbs
 % as expect or believe:
 % "he is expected to teach linguistics"
 % whose treatment is parallel to that of
 % "he is told to teach linguisctics"
 
 constraint([voice:passive],Properties),
  % we check that we have a passive voice
  (constraint([pol:Polarity],FSVerb) -> PolClause=Polarity;  true),
  % we check polarity
  % polarity info can also be gleaned from the args (nobody reads books...)

 reorg(Class,Arglist,NewArglist,passive),
 % we reorganize the arglist

 satisfy_subject(Agr,NewArglist,OtherArgs,FSSubject,
                 Gap1,BeginSubject,B,PrecSubject,PolSubject,Finiteness),
  % we satisfy the subject arg, building its feature bundle
 % the B position (beginning of verb) should match
 % the ending position of the subject

 FSSubject=[subject:FSS],
 (constraint([e:i(X,Y)],FSS) -> IndexS=i(X,Y) ;
                             constraint([agr:AgrS,lex:LexS,index:IndexS],FSS)),

   ( (nonvar(PolSubject),var(PolClause)) -> PolClause=PolSubject; true),
 % another chance to set polarity if not yet done
 % note that we do not overwrite polarity once set

 satisfylist(OtherArgs,FSArgs,Gap2,C,Upto,Prec1,PolArgs),
 % we satisfy all the other args of the arglist
 % we start from the end position of the predicate, up to the Upto position
 % to be computed by satisfylist

 % sending down the subject:
  (Function=object;Function=extraposed_subject),
  constraint([Function:Object],FSArgs),
  constraint([c_str:[subject:[constraints:Constraints]]],Object),
  constraint([c_str:[subject:[e:IndexS]]],Object),

   (Gap1=  [gap:[type:T,index:IndexS,function:Su,
                      Su:[e:IndexS],
                     constraints:GapConstraints]] ->

  (append(Constraints,GapConstraints,NewGapConstraints),
  NGap1=  [gap:[type:T,index:IndexS,function:Su,
                      Su:[e:IndexS],
                     constraints:NewGapConstraints]])

   ;
  ( NGap1=[],
   (pick(delayed_agr:DAGR,Constraints,NC) -> (Cos=NC, Flag=yes) ;
                                              Cos=Constraints),
  (nonvar(Flag) -> (agree(LexS,AgrS,DAGR), constraint(Cos,FSS)) ;
                   constraint(Cos,FSS)))),



  ( (nonvar(PolArgs),var(PolClause)) -> PolClause=PolArgs; true),
   (nonvar(PolClause) -> PC=[pol:PolClause] ; PC=[]),
  funify([voice:passive],FSVerb,FSV),
  funify(PC,FSV,FullF),
 % if pol is set we add it to the clause features

 (constraint([pc:PConstraints],Features) -> PCC=PConstraints;PCC=[]),
 dw(PCC,NGap1,GapS,FullF,FSArgs,Added,WeightColl),
 % we deal here with the constraints to be satisfied on the whole clause
 % they are housed in the [pc:List] in lexical entries

 (var(GapS) -> GapSubj=NGap1; GapSubj=GapS),

 myappend(GapSubj,Gap2,ALLGaps),

(ALLGaps=[]; ALLGaps=[gap:GGG]),

(var(Added) -> (constraint([negtransport:yes],Features) ->
                  (   constraint([kill:KillValue],Object),
                      (nonvar(PolClause) -> (rev(KillValue,RevValue),
                                             Added=kill:RevValue);
                                             Added=kill:KillValue))
                       ;  true);
 true),


 (var(Added) -> (constraint([negswitch:yes],Features) ->
                   (constraint([kill:KillValue],Object),
                    rev(KillValue,RevValue),
                    Added=kill:RevValue); true)
                ;  true),
  (var(WeightColl) -> WC=0; WC=WeightColl),
                
 insort(FSArgs,OrderedFS),
 % the parse gives the args in canonical order in the c_str feature

 append(FSSubject,OrderedFS,Args),
 append([head:FullF],Args,C_str),

 % addition of the kill feature if set, otherwise we add 'kill:unspec'
 (nonvar(Added) ->
    build(pred,[from:BeginSubject, to:Upto,
                cat:pred,finite:Finiteness, voice:passive,
                Added,weight_coll:WC,
                type:Class,weight:3,gap:ALLGaps,c_str:C_str]);

    build(pred,[from:BeginSubject, to:Upto,
                cat:pred,finite:Finiteness, voice:passive,
                kill:unspec, weight_coll:WC,
                type:Class,weight:3,gap:ALLGaps,c_str:C_str]))].



                
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TYPE TWO
% VP only
% the subject is to be left uninstantiated :
% a teacher from whom [to borrow books]
% or instantiated through raising :
% they want [to read a book]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% the 'finite' feature is "to" or "ing" in the verb group
% but gets translated as "tovp" or "gerund" in the clause that is built around
% this verb group
% on account of the fact that the subject is not parsed at this level,
% even as a gap,
% but is expected to be available higher up, if at all:
% "reading books is fun" does not get an instantiated subject



% NONRAISING VERBS
%%%%%%%%%%%%%%%%%%
% to read a book / reading a book

[gr,pred2nonraising] --->
[known(vg,[from:B,to:C|FSVerb]),
 % we have a verb group

 constraint([class:Class,finite:Finiteness,prop:Properties,
             agr:Agr,arglist:Arglist,ft:Features],FSVerb),
  (Finiteness=to;Finiteness=ing),
   \+ raising(Class),
   (Finiteness=to -> F=tovp;F=gerund),
  (constraint([voice:passive],Properties) -> Voice=passive; Voice=active),
  % we work out the value of the voice feature
  % and set it if necessary (active voice)
 (constraint([pol:Polarity],FSVerb) -> PolClause=Polarity;  true),
  % we do the same for polarity
  % polarity info can also be gleaned from the args (nobody reads books...)

 reorg(Class,Arglist,NewArglist,Voice),
 % we reorganize the arglist in case of need (i.e. passive voice)

 pick(subject:Subject,NewArglist,OtherArgs),
 constraint([constraints:SubjCons],Subject),
 satisfylist(OtherArgs,FSArgs,Gap,C,Upto,Prec1,PolArgs),
 % we satisfy the args of the arglist
 % we start from the end position of the predicate, up to the Upto position
 % to be computed by satisfylist

 ( (nonvar(PolArgs),var(PolClause)) -> PolClause=PolArgs; true),


 (nonvar(PolClause) -> PC=[pol:PolClause] ; PC=[]),
  funify([voice:Voice],FSVerb,FSV),
  funify(PC,FSV,FullF),
 % if pol is set we add it to the clause features

 (constraint([pc:PConstraints],Features) -> PCC=PConstraints;PCC=[]),
 
 ( pick(agree(A1,A2,A3),PCC,NPCC) ->
   (dw([agree(A1,A2,A3)],
      [subj_cons:SubjCons],NSubjCons,_FullF,_FSArgs,_Added,_WeightColl),
     ClCo=NPCC);ClCo=PCC),

  dw(ClCo,Gap,GapS,FullF,FSArgs,Added,WeightColl),
 % we deal here with the constraints to be satisfied on the whole clause

  (var(NSubjCons) -> SC=SubjCons; SC=NSubjCons),
   (var(GapS) -> NewGap=Gap;NewGap=GapS),


 (var(Added) -> (constraint([negtransport:yes],Features) ->
                  ( (Function=object;Function=extraposed_subject),
                    constraint([Function:Object],FSArgs),
                    constraint([kill:KillValue],Object),
                      (nonvar(PolClause) -> (rev(KillValue,RevValue),
                                             Added=kill:RevValue);
                                             Added=kill:KillValue))
                       ;  true);
 true),


 (var(Added) -> (constraint([negswitch:yes],Features) ->
                   ((Function=object;Function=extraposed_subject),
                    constraint([Function:Object],FSArgs),
                    constraint([kill:KillValue],Object),
                    rev(KillValue,RevValue),
                    Added=kill:RevValue); true)
                ;  true),

  (var(WeightColl) -> WC=0; WC=WeightColl),
 insort(FSArgs,OrderedFS),
 % the parse gives the args in canonical order in the c_str feature

 append([subject:[e:i(X,Y),constraints:SC]],OrderedFS,Args),
 append([head:FullF],Args,C_str),

 % addition of the kill feature if set, otherwise we add 'kill:unspec'
 (nonvar(Added) ->
    build(pred,[from:B, to:Upto,
                cat:pred,finite:F, voice:Voice,
                Added,weight_coll:WC,
                type:Class,weight:3,gap:NewGap,c_str:C_str]);

    build(pred,[from:B, to:Upto,
                cat:pred,finite:F, voice:Voice,
                kill:unspec, weight_coll:WC,
                type:Class,weight:3,gap:NewGap,c_str:C_str]))].




% RAISING VERBS I
%%%%%%%%%%%%%%%%%

% to want [to read a book]  / wanting [to read a book]

[gr,pred_non_finite_raising_I] --->
[known(vg,[from:B,to:C|FSVerb]),
 % we have a verb group

 constraint([class:Class,finite:Finiteness,prop:Properties,
             agr:Agr,arglist:Arglist,ft:Features],FSVerb),
 (Finiteness=to;Finiteness=ing),
 (Class=tr_vp ; Class=tr_inf_gap;Class=be),
 (Finiteness=to -> F=tovp;F=gerund),

 \+ constraint([voice:passive],Properties),

 (constraint([pol:Polarity],FSVerb) -> PolClause=Polarity;  true),
  % we do the same for polarity
  % polarity info can also be gleaned from the args (nobody reads books...)

 pick(subject:Subject,Arglist,OtherArgs),
 constraint([constraints:SubjCons],Subject),
 % the subject is never a constituent of such clauses
 % we are only interested in propagating the constraints
 % i.e. adding them up to the constraints put on the subject of
 % the lower clause
 
 satisfylist(OtherArgs,FSArgs,Gap,C,Upto,Prec1,PolArgs),
 % we satisfy the args of the arglist
 % we start from the end position of the predicate, up to the Upto position
 % to be computed by satisfylist

 % sending down the subject:

    % WANT-type verbs
   (constraint([object:Object],FSArgs) ->
   (constraint([c_str:[subject:[constraints:Constraints]]],Object),
    constraint([c_str:[subject:[e:i(X,Y)]]],Object));true),

    % SEEM
   (constraint([extraposed_subject:Object],FSArgs) ->
   (constraint([c_str:[subject:[constraints:Constraints]]],Object),
    constraint([c_str:[subject:[e:i(X,Y)]]],Object));true),

  % BE
    % here we must make sure that the complement
    % is of the right type, i.e. tovp
    % the subject is to be found at two levels
    % that of the arg itself, and that of the predication it contains

  (constraint([complement:Object],FSArgs) ->
   (constraint([class:tovp],Object) ->
   (constraint([c_str:[subject:[constraints:Constraints]]],Object),
   constraint([c_str:[subject:[e:i(X,Y)]]],Object),
constraint([c_str:[object:[c_str:[subject:[e:i(X,Y),constraints:Constraints]]]]],
                Object)); true) ; true),


     % the Constraints will be uninstantiated if we are dealing
     % with be with a non-tovp arg, e.g. 'the bucket is red'
     %                                 or 'he is a teacher'
    (nonvar(Constraints); Constraints=[]),

  append(Constraints,SubjCons,NewSubjCons),

 ( (nonvar(PolArgs),var(PolClause)) -> PolClause=PolArgs; true),


 % we choose the first of  the two treatments below:
 
   (nonvar(PolClause) -> PolClause=neg;PolClause=pos),
 %  (nonvar(PolClause) -> PC=[pol:PolClause] ; PC=[]),

 % in order to make polarity visible in all cases (also when pol is pos)

  funify([voice:active],FSVerb,FSV),
  funify(PC,FSV,FullF), % without effect if PC is left as a variable

 % if pol is set we add it to the clause features
 
 (constraint([pc:PConstraints],Features) -> PCC=PConstraints;PCC=[]),
 dw(PCC,[subj_cons:NewSubjCons],NSubjCons,FullF,FSArgs,_Added,WeightColl),
 % we deal here with the constraints to be satisfied on the whole clause
 % we are interested only in adding to the constraints we have gleaned so far

  (var(NSubjCons) -> SC=NewSubjCons; SC=NSubjCons),

   constraint([kill:KillValue],Object),

 (constraint([negtransport:yes],Features) ->
                       (PolClause=neg -> rev(KillValue,RevValue);
                                         RevValue=KillValue)
                                          ;
                                          RevValue=KillValue),

  (constraint([negswitch:yes],Features) ->
                              rev(RevValue,RevValue2);
                              RevValue2=RevValue),

 insort(FSArgs,OrderedFS),
 % the parse gives the args in canonical order in the c_str feature

 append([subject:[e:i(X,Y),constraints:SC]],OrderedFS,Args),
 % the subject that is added to the other args
 % has the full list of constraints
 append([pol:PolClause],Args,NArgs),     % here we add polarity -cf above
 append([head:FullF],NArgs,C_str),
  (var(WeightColl) -> WC=0; WC=WeightColl),
   build(pred,[from:B, to:Upto,
                cat:pred,finite:F, voice:active,
                kill:RevValue2,weight_coll:WC,
                type:Class,weight:3,gap:Gap,c_str:C_str]) ].


% RAISING VERBS II
%%%%%%%%%%%%%%%%%%

% to tell Mary [to read a book]  / telling her [to read a book]

[gr,pred_non_finite_raising_II] --->
[known(vg,[from:B,to:C|FSVerb]),
 % we have a verb group

 constraint([class:tr_io_inf,finite:Finiteness,prop:Properties,
             agr:Agr,arglist:Arglist,ft:Features],FSVerb),
 (Finiteness=to;Finiteness=ing),
 (Finiteness=to -> F=tovp;F=gerund),

  \+ constraint([voice:passive],Properties),

 (constraint([pol:Polarity],FSVerb) -> PolClause=Polarity;  true),
  % we do the same for polarity
  % polarity info can also be gleaned from the args (nobody reads books...)

  pick(subject:Subject,Arglist,OtherArgs),
 constraint([constraints:SubjCons],Subject),
 % the subject is never a constituent of such clauses
 % we are only interested in propagating the constraints
 % i.e. adding them up to the constraints put on the subject of
 % the lower clause

 satisfylist(OtherArgs,FSArgs,Gap,C,Upto,Prec1,PolArgs),
 % we satisfy the args of the arglist
 % we start from the end position of the predicate, up to the Upto position
 % to be computed by satisfylist

   % sending down the subject:
  (Function=object;Function=extraposed_subject),
  constraint([i_object:I_Object],FSArgs),
   (I_Object=[e:i(X,Y)|Remainder] -> IndexIO=i(X,Y);
                 constraint([agr:AgrIO,lex:LexIO,index:IndexIO],I_Object)),
  constraint([Function:Object],FSArgs),
  constraint([c_str:[subject:[constraints:Constraints]]],Object),
  constraint([c_str:[subject:[e:IndexIO]]],Object),

 ( (nonvar(PolArgs),var(PolClause)) -> PolClause=PolArgs; true),
  (nonvar(PolClause) -> PC=[pol:PolClause] ; PC=[]),
  
  funify([voice:active],FSVerb,FSV),
  funify(PC,FSV,FullF),
 % if pol is set we add it to the clause features

 (constraint([pc:PConstraints],Features) -> PCC=PConstraints;PCC=[]),
 dw(PCC,[subj_cons:SubjCons],NSubjCons,FullF,FSArgs,_Added,WeightColl),
 % we deal here with the constraints to be satisfied on the whole clause
 % we are interested only in adding to the constraints we have gleaned so far

  (var(NSubjCons) -> SC=SubjCons; SC=NSubjCons),

  (pick(delayed_agr:DAGR,Constraints,NC) -> (Cos=NC, Flag=yes) ;
                                              Cos=Constraints),
  (nonvar(Flag) -> (agree(LexIO,AgrIO,DAGR), constraint(Cos,I_Object)) ;
                   constraint(Cos,I_Object)),

  (var(Added) -> (constraint([negtransport:yes],Features) ->
                  (   constraint([kill:KillValue],Object),
                      (nonvar(PolClause) -> (rev(KillValue,RevValue),
                                             Added=kill:RevValue);
                                             Added=kill:KillValue))
                       ;  true);
 true),


 (var(Added) -> (constraint([negswitch:yes],Features) ->
                   (constraint([kill:KillValue],Object),
                    rev(KillValue,RevValue),
                    Added=kill:RevValue); true)
                ;  true),

  (var(WeightColl) -> WC=0; WC=WeightColl),
 insort(FSArgs,OrderedFS),
 % the parse gives the args in canonical order in the c_str feature

 append([subject:[e:i(ZZ,ZZZ),constraints:SC]],OrderedFS,Args),
 % the subject that is added to the other args
 % has the full list of constraints
 % but its index is free - it does no correspond to that of the indirect object!

 append([head:FullF],Args,C_str),

  % addition of the kill feature if set, otherwise we add 'kill:unspec'
 (nonvar(Added) ->
    build(pred,[from:B, to:Upto,
                cat:pred,finite:F, voice:active,
                Added,weight_coll:WC,
                type:tr_io_inf,weight:3,gap:Gap,c_str:C_str]);

    build(pred,[from:B, to:Upto,
                cat:pred,finite:F, voice:active,
                kill:unspec,weight_coll:WC,
                type:tr_io_inf,weight:3,gap:Gap,c_str:C_str]))].



% RAISING VERBS III
%%%%%%%%%%%%%%%%%%%

% to be told [to read a book]  / being told [to read a book]

[gr,pred_non_finite_raising_III] --->
[known(vg,[from:B,to:C|FSVerb]),
 % we have a verb group

 constraint([class:Class,finite:Finiteness,prop:Properties,
             agr:Agr,arglist:Arglist,ft:Features],FSVerb),
 (Finiteness=to;Finiteness=ing),
(Class=tr_inf_pass; Class=tr_io_inf),
 (Finiteness=to -> F=tovp;F=gerund),

 constraint([voice:passive],Properties),
 
 (constraint([pol:Polarity],FSVerb) -> PolClause=Polarity;  true),
  % we do the same for polarity
  % polarity info can also be gleaned from the args (nobody reads books...)

 reorg(Class,Arglist,NewArglist,passive),
 % we reorganize the arglist in case of need (i.e. passive voice)

 pick(subject:Subject,NewArglist,OtherArgs),
 constraint([constraints:SubjCons],Subject),
 % the subject is never a constituent of such clauses
 % we are only interested in propagating the constraints
 % i.e. adding them up to the constraints put on the subject of
 % the lower clause

 satisfylist(OtherArgs,FSArgs,Gap,C,Upto,Prec1,PolArgs),
 % we satisfy the args of the arglist
 % we start from the end position of the predicate, up to the Upto position
 % to be computed by satisfylist

 (Function=object;Function=extraposed_subject),
 constraint([Function:Object],FSArgs),
  constraint([c_str:[subject:[constraints:Constraints]]],Object),
  constraint([c_str:[subject:[e:i(X,Y)]]],Object),

  append(Constraints,SubjCons,NewSubjCons),

 ( (nonvar(PolArgs),var(PolClause)) -> PolClause=PolArgs; true),
  (nonvar(PolClause) -> PC=[pol:PolClause] ; PC=[]),

  funify([voice:passive],FSVerb,FSV),
  funify(PC,FSV,FullF),
 % if pol is set we add it to the clause features

 (constraint([pc:PConstraints],Features) -> PCC=PConstraints;PCC=[]),
 dw(PCC,[subj_cons:NewSubjCons],NSubjCons,FullF,FSArgs,_Added,WeightColl),
 % we deal here with the constraints to be satisfied on the whole clause
 % we are interested only in adding to the constraints we have gleaned so far

  (var(NSubjCons) -> SC=NewSubjCons; SC=NSubjCons),

 (var(Added) -> (constraint([negtransport:yes],Features) ->
                  (   constraint([kill:KillValue],Object),
                      (nonvar(PolClause) -> (rev(KillValue,RevValue),
                                             Added=kill:RevValue);
                                             Added=kill:KillValue))
                       ;  true);
 true),


 (var(Added) -> (constraint([negswitch:yes],Features) ->
                   (constraint([kill:KillValue],Object),
                    rev(KillValue,RevValue),
                    Added=kill:RevValue); true)
                ;  true),
 (var(WeightColl) -> WC=0; WC=WeightColl),

 insort(FSArgs,OrderedFS),
 % the parse gives the args in canonical order in the c_str feature

 append([subject:[e:i(X,Y),constraints:SC]],OrderedFS,Args),
 % the subject that is added to the other args
 % has the full list of constraints

 append([head:FullF],Args,C_str),


  % addition of the kill feature if set, otherwise we add 'kill:unspec'
 (nonvar(Added) ->
    build(pred,[from:B, to:Upto,
                cat:pred,finite:F, voice:passive,
                Added,weight_coll:WC,
                type:Class,weight:3,gap:Gap,c_str:C_str]);

    build(pred,[from:B, to:Upto,
                cat:pred,finite:F, voice:passive,
                kill:unspec,weight_coll:WC,
                type:Class,weight:3,gap:Gap,c_str:C_str]))].


 % NOUNS
%%%%%%%%%%%%%%%%

% her refusal [to read a book, to mince her words,
%              to pride herself on her looks]

[gr,pred2nouns] --->
[known(np,[from:B,to:C|FSNP]),
 % we have a noun phrase

  % the class is the right one : tovp
 constraint([class:tovp,agr:AgrNP,sem:SemNP,lex:LexNP],FSNP),

 constraint([c_str:[det:FSDET,head:FSNOUN]],FSNP),
 % get at the determiner which is liable to feature the subject of the inf
 
 constraint([arglist:ArgList,ft:Features],FSNOUN),
 % get the arglist to go through, and the features to satisfy on the whole np

 /*            DET IS OF THE POSS_ADJ TYPE
 c_str:[det:[pos:det, type:poss_adj, txt:her, lex:her,
     agr:[number:sing, person:3, gender:_G1074],
     agrposs:[number:sing, person:3, gender:fem]] */
 
 (constraint([type:poss_adj],FSDET) ->
     (constraint([agrposs:AgrSubj1],FSDET),
      lex(T ,ppro,[type:pp,lex:Lex,agr:AgrSubj,function:subject,sem:[hum]]),
      constraint(AgrSubj1,AgrSubj),
      Subject= [index:IndexSubj,lex:Lex,agr:AgrSubj,sem:[hum]]);
 true),
 % here we recover the personal pronoun corresponding to the possessive
 % we select the standard subject form for the parse tree
 % we make sure that the agreement features match (we do not use
 % standard unification because the fts making up the agreement bundle
 % are not necessarily in the same order : number, person and gender
 % since possessive adjectives are not assigned an index in this grammar
 % IndexSubj here will be left as a variable


 /*           DET IS OF TYPE 'GENITIVE'
         c_str:[det:[pos:det, type:genitive,
         agr:[number:sing, person:_G1108, gender:_G1114],
         c_str:[det:[cat:np, agr:[number:sing, person:3, gender:fem], sem:[hum],
         lex:mary, gap:[], weight:1, index:i(2, 3),
         c_str:[head:[type:std, pos:n, count:c, class:proper, txt:mary,
         lex:mary, agr:[number:sing, person:3, gender:fem], sem:[hum]]]]]] */
         
  (constraint([type:genitive],FSDET) ->
  (constraint([c_str:[det:[agr:AgrSubj,sem:SemSubj,
                          lex:LexSubj,index:IndexSubj]]],FSDET),
   Subject=[index:IndexSubj, lex:LexSubj,agr:AgrSubj,sem:SemSubj]);
  true),
  % here the subject is simply fished out of the genitive serving as determiner:
  % Mary's refusal --> Mary

  (constraint([pol:Polarity],FSNP) -> NPPOL=Polarity;  true),
   (known(negation,[from:C, to:D|FSneg]) -> (Next=D, ClausePol=neg);
                                             (ClausePol=pos,Next=C)),
  satisfylist(ArgList,FSArgs,[],Next,Upto,Prec1,PolArgs),
 % we satisfy the args of the arglist
 % we start from the end position of the NP, up to the Upto position
 % to be computed by satisfylist
 % we do not accept gaps (although a fuller grammar would prhaps have to be
 % able to deal with the marginal construction:
 % "the book her refusal to review which caused trouble was left unpublished"

  constraint([object:Object],FSArgs),
  constraint([c_str:[subject:[constraints:Constraints]]],Object),
  constraint([c_str:[subject:[e:IndexSubj]]],Object),
  constraint([kill:KillValue],Object),
 (pick(delayed_agr:DAGR,Constraints,NC) -> (Cos=NC, Flag=yes) ;
                                              Cos=Constraints),
  (nonvar(Flag) -> (agree(LexSubj,AgrSubj,DAGR),
                         constraint(Cos,Subject)) ;
                   constraint(Cos,Subject)),
  (ClausePol=neg -> rev(KillValue,KillValue2); KillValue2=KillValue),
  

  (constraint([negswitch:yes],Features) ->
                    rev(KillValue2,RevValue);RevValue=KillValue2),


   RevValue \= yes,
   % we DO NOT admit of a complement clause that is still marked as kill:yes
   % once the above switches of polarity have been performed
   
   insort(FSArgs,OrderedFS),
  append([subject:Subject],OrderedFS,Args),
  append([pol:ClausePol],Args,NArgs),
  (nonvar(NPPOL) ->
     build(np,[from:B,to:Upto,cat:np,pol:NNPOL,
            agr:AgrNP,sem:SemNP,class:tovp,lex:LexNP,
           weight:3,index:i(B,Upto),gap:[],
           c_str:[det:FSDET,head:FSNOUN,args:NArgs]]);

     build(np,[from:B,to:Upto,cat:np,
            agr:AgrNP,sem:SemNP,class:tovp,lex:LexNP,
           weight:3,index:i(B,Upto),gap:[],
           c_str:[det:FSDET,head:FSNOUN,args:NArgs]])) ].


 % ADJECTIVES
%%%%%%%%%%%%%%%%

% with opening negation
%%%%%%%%%%%%%%%%%%%%%%%%

% not unwilling/ready  [to read a book, to pride herself on her looks]

[gr,pred2adj] --->
[ known(negation,[from:A, to:B|FSneg]),
 % we have an opening negation
 known(adjp,[from:B,to:C|FSADJP]),
 % we have an adjective phrase

 constraint([class:tovp],FSADJP),
 % the class is the right one : tovp

 constraint([c_str:[head:FSADJ]],FSADJP),

 constraint([arglist:ArgList,ft:Features],FSADJ),
 % get the arglist to go through, and the features to satisfy on the whole np

 satisfylist(ArgList,FSArgs,Gap,C,Upto,Prec1,PolArgs),

 % we satisfy the args of the arglist
 % we start from the end position of the ADJP, up to the Upto position
 % to be computed by satisfylist

  constraint([object:Object],FSArgs),
  constraint([c_str:[subject:[constraints:Constraints]]],Object),
  constraint([c_str:[subject:[e:i(X,Y)]]],Object),
  constraint([kill:KillValue],Object),

  \+ constraint([c_str:[head:[aux:[pol:neg]]]],Object),
  % avoid double neg
 
   (constraint([pc:PConstraints],Features) -> PCC=PConstraints;PCC=[]),
 dw(PCC,[subj_cons:Constraints],NSubjCons,FullF,FSArgs,_Added,WeightColl),
 % we deal here with the constraints to be satisfied on the whole clause
 % we are interested only in adding to the constraints we have gleaned so far

  (var(NSubjCons) -> SC=Constraints; SC=NSubjCons),

  rev(KillValue,KillValue2),  % we are in a neg clause

  (constraint([negswitch:yes],Features) ->
                    rev(KillValue2,RevValue);RevValue=KillValue2),


   RevValue \= yes,
   % cf nouns above
    (var(WeightColl) -> WC=0; WC=WeightColl),
   insort(FSArgs,OrderedFS),
  append([subject:[e:i(X,Y),constraints:SC]],OrderedFS,Args),
 % the subject that is added to the other args
 % has the full list of constraints
   append([pol:neg],Args,NArgs),
  append([head:FSADJP],NArgs,C_string),
  build(adjp,[from:A,to:Upto,cat:adjp,
            class:tovp,weight_coll:WC,
           weight:3,gap:Gap,
           c_str:C_string]) ].


% without opening negation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 %  unwilling/ready (not) [to read a book, to pride herself on her looks]

[gr,pred2adj] --->
[ known(adjp,[from:B,to:C|FSADJP]),
 % we have an adjective phrase

 constraint([class:tovp],FSADJP),
 % the class is the right one : tovp

 constraint([c_str:[head:FSADJ]],FSADJP),

 constraint([arglist:ArgList,ft:Features],FSADJ),
 % get the arglist to go through, and the features to satisfy on the whole np

 satisfylist(ArgList,FSArgs,Gap,C,Upto,Prec1,PolArgs),

 % we satisfy the args of the arglist
 % we start from the end position of the ADJP, up to the Upto position
 % to be computed by satisfylist

  constraint([object:Object],FSArgs),
  constraint([c_str:[subject:[constraints:Constraints]]],Object),
  constraint([c_str:[subject:[e:i(X,Y)]]],Object),
  constraint([kill:KillValue],Object),

  (constraint([c_str:[head:[aux:[pol:neg]]]],Object) -> ClausePol=neg;
                                                         ClausePol=pos),

  (constraint([pc:PConstraints],Features) -> PCC=PConstraints;PCC=[]),
 dw(PCC,[subj_cons:Constraints],NSubjCons,FullF,FSArgs,_Added,WeightColl),
 % we deal here with the constraints to be satisfied on the whole clause
 % we are interested only in adding to the constraints we have gleaned so far

  (var(NSubjCons) -> SC=Constraints; SC=NSubjCons),
  
  % (ClausePol=neg -> rev(KillValue,KillValue2);  % we are in a neg clause
 %                  KillValue2=KillValue),
 % DO NOT DO THAT -the reversal has already been performed in the lower clause

                   
  (constraint([negswitch:yes],Features) ->
                    rev(KillValue,RevValue);RevValue=KillValue),


   RevValue \= yes,
   % cf nouns above
 (var(WeightColl) -> WC=0; WC=WeightColl),
   insort(FSArgs,OrderedFS),
  append([subject:[e:i(X,Y),constraints:SC]],OrderedFS,Args),
 % the subject that is added to the other args
 % has the full list of constraints
   append([pol:ClausePol],Args,NArgs),
  append([head:FSADJP],NArgs,C_string),
  build(adjp,[from:B,to:Upto,cat:adjp,
            class:tovp,weight_coll:WC,
           weight:3,gap:Gap,
           c_str:C_string]) ].



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DW : dealing with the predicate-level constraints
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dw([],GS,NGS,ClauseF,FSArgs,Added,CW) :- !.
% out of recursion

dw([std(Constraint)|OtherC],GS,NGS,ClauseF,FSArgs,Added,CW) :-
   constraint(Constraint,ClauseF),
   dw(OtherC,GS,NGS,ClauseF,FSArgs,Added,CW).

% the std directive ensures that the Constraint is treated like any other
% i.e. by being imposed on the clause features at this level


dw([ifthenelse(check(F1),add(F2),add(F3))|OtherC],
                       GS,NGS,ClauseF,FSArgs,Added,CW) :-
   (constraint([F1],ClauseF) -> Added=F2; Added=F3),
    dw(OtherC,GS,NGS,ClauseF,FSArgs,Added,CW).

% the check in F1 is constrained on the clause features
% F2 or F3 is copied onto the Added var according to success or failure
% of the F1 check

% MORE INFO:
% if the clause-level PC constraints have a special format, namely
 % the 'ifthenelse' one, in front, they generally embody
 % a check on polarity to be
 % followed by value assignment to the kill feature
 % this enables  a lexical item to constraint the environment it
 % fits in; a kill feature set to yes will ensure that the clause
 % will be killed, unless it is accepted as arg at a higher level
 % he minces words -> kill:yes
 % i doubt if he will mince words -> the kill:yes is accepted by
 % the 'doubt' clause, and the object (clausal object of 'doubt') survives

dw([prolog:PrologConstraints|OtherC],GS,NGS,ClauseF,FSArgs,Added,CW) :-
    testexec(PrologConstraints),
    dw(OtherC,GS,NGS,ClauseF,FSArgs,Added,CW).

% the prolog directive ensures that the constraints are executed as
% straight prolog goals, without any foreign input


dw([agree(X,Y,Z)|OtherC],[],NGS,ClauseF,FSArgs,Added,CW) :-
     agree(X,Y,Z),
     dw(OtherC,[],NGS,ClauseF,FSArgs,Added,CW).

% when the subject is not gapped, the agree check is performed

dw([agree(X,Y,Z)|OtherC],[gap:Gapinfo],NGS,ClauseF,FSArgs,Added,CW) :-
      pick(constraints:C,Gapinfo,TGS),
       append([delayed_agr:Z],C,NC),
       append(TGS,[constraints:NC],NewGapinfo),
       NGS=[gap:NewGapinfo],
        dw(OtherC,[gap:Gapinfo],NGS,ClauseF,FSArgs,Added,CW).

% agreement can be checked here if the subject is available,
 % i.e. has not been gapped;
 % otherwise we keep the last arg of agree (PossRefAgr)
 % for a delayed agreement check
 % to be performed when the lex feature of the subject is available
 % i don't like ANYONE who prides themselves on their looks
 
 % in other words:
% when the subject is gapped, agree is reduced to copying its third arg
% (the one that will have to wait for the subject agr values to be present)
% as value of the delayed_agr feature, which will be appended to the
% constraints housed in the gap
% in this case NGS will be instantiated and can then be used as new gap

dw([agree(X,Y,Z)|OtherC],[subj_cons:SubjCons],NC,ClauseF,FSArgs,Added,CW) :-
          append([delayed_agr:Z],SubjCons,NC),
          dw(OtherC,[subj_cons:SubjCons],NC,ClauseF,FSArgs,Added,CW).

% here the delayed_agr is copied directly onto the subject constraints
% and NC is instantiated - the process is applied when we do not have
% a full subject and do not have a gap either, as in the case of the
% subject of raising verbs in non-finite vps:
% he does not want [to be expected [to mince words]]
% when we are considering the non-finite clause [to be expected] and its
% object [to mince words]


% no gap - we can deal with the relevant collocate list
dw([coll(Function,Lex,Collocates)|OtherC],[],NGS,ClauseF,FSArgs,Added,CWTot) :-
      accu_w(Lex,Collocates,0-nil,CW),
       dw(OtherC,[],NGS,ClauseF,FSArgs,Added,CW2),
       myappend([CW],[CW2],CWTot).

% gap does not concern collocate-bearing arg : idem
dw([coll(Function,Lex,Collocates)|OtherC],[gap:Gapinfo],
                                           NGS,ClauseF,FSArgs,Added,CWTot) :-
      \+ constraint([function:Function],Gapinfo),
       accu_w(Lex,Collocates,0-nil,CW),
       dw(OtherC,[gap:Gapinfo],NGS,ClauseF,FSArgs,Added,CW2),
       myappend([CW],[CW2],CWTot).
       
% gap on the collocate bearer - delay treatment until lex is available
% (e.g. the antecedent of a relative clause : the ritual he went through)
 dw([coll(Function,Lex,Collocates)|OtherC],[gap:Gapinfo],
                                           NGS,ClauseF,FSArgs,Added,CW) :-
      constraint([function:Function],Gapinfo),
      pick(constraints:C,Gapinfo,TGS),
       append([delayed_coll:Collocates],C,NC),
       append(TGS,[constraints:NC],NewGapinfo),
       NGS=[gap:NewGapinfo],
        dw(OtherC,[gap:Gapinfo],NGS,ClauseF,FSArgs,Added,CW).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% REORG : changes in the Arglist due to passive voice
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% sample args and arglist : 
% object:[type:np,canon:1,gappable:yes,oblig:yes,constraints:[]]
% pp_arg:[type:pp,canon:2,gappable:no,oblig:yes,constraints:[]]
% arglist:[subject:[type:np, canon:0,gappable:yes,
%       oblig:yes,constraints:[sem:[hum]]],
%          object:[type:np,canon:1,gappable:yes,
%           oblig:yes,constraints:[sem:[thing]]]]).


reorg(Class,Arglist,NewArglist,passive) :-
  Class \= tr_inf_pass,
  (PassiveSubj=object;PassiveSubj=i_object;PassiveSubj=arg_prep),

% note that the indirect object can be promoted to subject of a passive clause
% he was asked/told to write a book
% so can the arg_prep if marked as gappable:
% he was made an example of

  pick(PassiveSubj:Object,Arglist,OtherArgs),
  constraint([gappable:yes],Object),
              % passivisation is only possible with gappable objects
  pick(canon:Canon,Object,NObject),
  funify([canon:0],NObject,NewObject),
  pick(subject:Subject,OtherArgs,TailArgs),
  constraint([constraints:SubjCons],Subject),
   (SubjCons=[] -> NewArglist = [subject:NewObject,
                            pp_arg:[type:pp,canon:2,gappable:yes,oblig:no,
                            constraints:[prep:by]]|TailArgs] ;
              
                NewArglist = [subject:NewObject,
                            pp_arg:[type:pp,canon:2,gappable:yes,oblig:no,
                            constraints:[prep:by,
                                c_str:[arg_prep:SubjCons]]]|TailArgs]).

% subject is 'replaced' by optional pp_arg whose head
% should answer the constraints set on the subject of the active, if the
% constraint slot is not empty


reorg(tr_inf_pass,Arglist,NewArglist,passive) :-
  pick(subject:Subject,Arglist,TailArgs),
  pick(constraints:SubjCons,Subject,NS),
  funify([constraints:[]],NS,NSubj),
  (SubjCons=[] -> NewArglist = [subject:NSubj,
                            pp_arg:[type:pp,canon:2,gappable:yes,oblig:no,
                            constraints:[prep:by]]|TailArgs] ;

                  NewArglist = [subject:NSubj,
                            pp_arg:[type:pp,canon:2,gappable:yes,oblig:no,
                            constraints:[prep:by,
                                c_str:[arg_prep:SubjCons]]]|TailArgs]).

% such verbs should not be submitted to 'standard' reorg, as the subject of the
% passive results from the promotion of an arg embedded in the clausal object:
% he is expected to write a book
% the teacher is believed to have written a book
% here the subject is simply emptied of its constraints
% the constraints are passed on to the by-phrase


reorg(Class,Args,Args,active).
% nothing to do if active voice


%%% the CHECKGAP predicate
%%%%%%%%%%%%%%%%%%%%%%%%%%

% checking the gap in the sentential object of a +that verb:
% trs_that indicates that the cpltizer is present
% no is returned if we have both 'that' and a gapped subject
% the 'no' value causes the calling predicate to fail (subject-verb nexus)

checkgap(trs_that,FSArgs,no) :-
    constraint([object:[cat:pred,gap:Gaps]],FSArgs),
    member(gap:[type:_,index:_,function:subject,constraints:C,_],Gaps),
    !.

checkgap(Class,FSSpecs,yes).
% otherwise return yes
   

%%%%%%%%%%%%%%%%%%%%%%%%% 
%% COORDINATED STRUCTURES 
%%%%%%%%%%%%%%%%%%%%%%%%%

% except NPs, where plural number is computed : sing+sing --> plural 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% structures containing no gap info 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% coordinated verbs
%%%%%%%%%%%%%%%%%%%%

%% the student should [read and like] the book

[gr,coord_v] --->
[ known(coord,[from:B, to:C|FS2]),
  constraint([pos:coord],FS2),
  known(v,[from:A, to:B|FS1]),
  constraint([type:Type,agr:Agr1,
              tense:Tense,class:Class,ft:Features],FS1),
  known(v,[from:C, to:D|FS3]),
  constraint([type:Type,agr:Agr2,class:Class,
              ft:Features, arglist:Arg],FS3),
  funify(Agr1,Agr2,Agr),
  build(v,[from:A, to:D,type:Type,agr:Agr,tense:Tense,
                        class:Class,ft:Features,arglist:Arg,
                        c_str:[head:FS1,coord:FS2,head:FS3]])].
                        
  % the constraint of Class identity is too severe
  % a fuller grammar would work out which pairs of classes
  % can make up a coordinate v

% coordinated verb groups 
%%%%%%%%%%%%%%%%%%%%%%%%%

%% "the man [likes and reads] the book" 

[gr,coord_vg] --->
[ known(coord,[from:B, to:C|FS2]),
  constraint([pos:coord],FS2),
  known(vg,[from:A, to:B|FS1]),
  constraint([type:Type,finite:Finiteness,agr:Agr1,prop:Props1,
              tense:Tense,class:Class,ft:Features,arglist:Arg],FS1),
  known(vg,[from:C, to:D|FS3]),
  constraint([finite:Finiteness,agr:Agr2,class:Class,
              ft:Features,prop:Props2],FS3),
  funify(Agr1,Agr2,Agr),
  funify(Props1,Props2,Props),
  build(vg,[from:A, to:D, finite:Finiteness,cat:vg,prop:Props,
         type:Type,agr:Agr,tense:Tense,class:Class,ft:Features,arglist:Arg,
              c_str:[head:FS1,coord:FS2,head:FS3]])].

  % same comment as above

%% coordinated vps (partial preds)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% the second pred has the same subject as the first 
%% "the man [teaches linguistics and likes students]"

[gr,coord_pred] --->
[ known(coord,[from:B, to:C|FS2]),
  constraint([pos:coord],FS2),
  known(pred,[from:A, to:B|FS1]),
  % exploring the constituency structure to retrieve properties of the subject
  constraint([cat:pred,finite:Finiteness,kill:Kill,
                 gap:[],
                 c_str:Constituents],FS1),
  constraint([subject:Subject],Constituents),
  constraint([index:I],Subject),
  known(pred,[from:C, to:D|FS3]),
  constraint([cat:pred,finite:Finiteness, kill:Kill,
              gap:[gap:[type:np,index:I,function:or([subject,subject_inf]),
            constraints:Constraints,_]]],FS3),

  constraint(Constraints,Subject),
  build(pred,[from:A, to:D, cat:pred,finite:Finiteness,kill:Kill,
             gap:[],weight:3,type:coordinated,
              c_str:[head:FS1,coord:FS2,pred:FS3]])].


% coordinated full preds
%%%%%%%%%%%%%%%%%%%%%%%%

% "the man likes students and the woman teaches linguistics"

[gr,coord_pred1] --->

[ known(coord,[from:B, to:C|FS2]),
  constraint([pos:coord],FS2),
  known(pred,[from:A, to:B|FS1]),
  constraint([cat:pred,finite:Finiteness,kill:Kill,gap:[]],FS1),
  known(pred,[from:C, to:D|FS3]),
  constraint([cat:pred,finite:Finiteness,kill:Kill,gap:[]],FS3),
  build(pred,[from:A, to:D,cat:pred,finite:Finiteness,kill:Kill,
              gap:[],weight:3,type:coordinated,
              c_str:[head:FS1,coord:FS2,pred:FS3]])].
 



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% S's with an opening sentence adverbial
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% toy : S --> adv S
% just to check fixed string idioms such as 'by and large'
% "by and large the teacher likes the book"

[gr,adv_s] --->

[ known(adv,[from:A, to:B|FS1]),
  ifthenelse(known(coord,[from:B, to:C,pos:coord,txt:',',lex:comma]),
           Next=C,Next=B),
  known(pred,[from:Next, to:D|FS2]),
  constraint([gap:Gap, kill:Kill,finite:Finiteness],FS2),
  (constraint([pol:Pol],FS2) ->
      build(pred,[from:A, to:D, kill:Kill, pol:Pol,cat:pred,finite:Finiteness,
               gap:Gap,weight:3,
              type:with_S_adverbial,c_str:[adv:FS1,pred:FS2]]) ;
              
      build(pred,[from:A, to:D, kill:Kill, cat:pred,finite:Finiteness,
              gap:Gap,weight:3,
              type:with_S_adverbial,c_str:[adv:FS1,pred:FS2]]) )

              ].
 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% DEALING WITH THE PREDICATE'S ARGLIST 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%
% SATISFYING THE SUBJECT
%%%%%%%%%%%%%%%%%%%%%%%%

%% no gap in subject 
%%%%%%%%%%%%%%%%%%%%%

% the third arg is the feature bundle
% that will be added to the predicate c-structure

satisfy_subject(Agr,Arglist,OtherArgs,
    [subject:FSSubject],[],BeginSubject,BeginPred,
                     posprec(Canon,Weight),Pol,Finiteness):-

% posprec(Canon,Weight) is used in making sure
% that the args are duly ordered in the S to be parsed
% the Canon feature is set in the predicate's arglist,
% whereas the Weight is computed when the args
% are built, i.e prior to their being used in the arglist satisfaction process

pick(subject:Subject,Arglist,OtherArgs),
constraint([type:Type,canon:Canon,constraints:Constraints],Subject),
known(Type,[from:BeginSubject,to:BeginPred|FSSubject]),
(Finiteness=yes -> funify([function:subject],FSSubject,NewFS);
                   funify([function:subject_inf],FSSubject,NewFS) ),
(constraint([pol:Polarity],FSSubject) -> Pol=Polarity; true),
constraint([weight:Weight],FSSubject),
constraint([agr:Agr],FSSubject),
constraint(Constraints,FSSubject).

% the funify clause ensures that in case the function has been set
% on the np node because it was a personal pronoun
% it has been set to 'subject' or 'subject_inf'



%% gapped subject
%%%%%%%%%%%%%%%%%

satisfy_subject(Agr,Arglist,OtherArgs,[subject:[e:I]],
               [gap:[type:Type,index:I,function:S,
                      S:[e:I],
                     constraints:GapConstraints]],
               BeginPred,BeginPred,
               posprec(Canon,0),Pol,Finiteness):-

pick(subject:Subject,Arglist,OtherArgs),
constraint([type:Type,canon:Canon,gappable:yes,constraints:Constraints],
           Subject),
 append([agr:Agr],Constraints,GapConstraints),
(Finiteness=yes -> S=subject;S=subject_inf).


% a gapped constituent simply puts its constraints in the Gap feature,
% to be satisfied when the pred is connected to the antecedent



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SATISFYING THE OTHER ARGS ON THE ARGLIST
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

satisfylist(Arglist,FSArgs,GapF,From,Upto,Prec1,Pol) :-
  pick(Arg,Arglist,OtherArgs),
  satisfy(Arg,FSArg,From,Next,Prec1,GapArg,Pol),
  % satisfy is the predicate used
  % for the consumption of a non-subject arg
  % the end position reached is the starting point for the next arg

  satisfylist(OtherArgs,FSOtherArgs,GapOtherArgs,Next,Upto,Prec2,Pol),
  precede(Prec1,Prec2),
  % but the args in the text must appear in an order
  % licensed by both their weight (structural makeup)
  % and their position in canonical order
  myappend(GapArg,GapOtherArgs,Gaps),
  flatten(Gaps,GapF),
  append(FSArg,FSOtherArgs,FSArgs).



%% all arguments consumed
%%%%%%%%%%%%%%%%%%%%%%%%%

satisfylist(Arglist,[],Gap,EndArgs,EndArgs,Prec,Pol) :-
allopt(Arglist),!.

% endpoint reached : all the obligatory args on the arglist
% must have been consumed
% the EndArgs is copied over into the position of the Upto variable


%% the ALLOPT predicate
%%%%%%%%%%%%%%%%%%%%%%%

allopt([F:V|OtherArgs]) :- constraint([oblig:no],V),!, allopt(OtherArgs).
% only optional args may be let unsatisfied

allopt([]).
% nothing left over



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% satisfying an arg of the arglist 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% the arg is not gapped
%%%%%%%%%%%%%%%%%%%%%%%

satisfy(Function:Specs,[Function:FSSpecs],From,Next,
           posprec(Canon,Weight),Gap,Pol) :-
Function \= subject,
constraint([type:Type,canon:Canon,constraints:Constraints],Specs),
(T=Type; (Type=or(OrList), member(T,OrList))),
known(T,[from:From,to:Next|FSSpecs]),
(constraint([pol:Polarity],FSSpecs) -> Pol=Polarity; true),
% we set the arg pol to the pol set in the np or clause filling the arg
% in case it has not been set there, we leave it uninstantiated
constraint([weight:Weight,gap:Gaparg],FSSpecs),
constraint(Constraints,FSSpecs),
funify([function:Function],FSSpecs,FSNew),

(var(Gaparg) -> Gap=[];Gap=Gaparg).
% the else-goal makes sure that the Gap of the arg is inherited:
%  np[np-head:the man-index_i
%                    relcl:s1[the woman thinks
%                                         object:s2[the inspector likes e-i]
%                            ]
%    ]
% suppose we are at s1 inheriting the gap from s2;
% Gaparg will have been instantiated
% when building the object clause of thinks, i.e. s2
% we must set the gap flag and pass the value for the 'gap' feature

% the funify clause ensures that if the function has been carried over
% to the np node from the personal pronoun
% acting as its head, then that function is the right one

% gapped arg
%%%%%%%%%%%%

satisfy(F:Specs,[F:[e:I]],From,From,
           Prec,
[gap:[type:Type,index:I,function:F,F:[e:I],constraints:Constraints]],
           Pol) :-
F \= subject,
constraint([gappable:yes],Specs),
constraint([type:Type,constraints:Constraints],Specs).

% here the whole arg is gapped;
% very different from the previous case, where the gap was only inherited
% here it is created



% satisfying a pp_arg with stranded preposition:
%  the man the teacher gave the book TO 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

satisfy(pp_arg:Specs,
       [pp_arg:FSSpecs],
        From,Next,
        posprec(Canon,Weight),
        [gap:[type:np,index:I,function:arg_prep,
                         arg_prep:[e:I],constraints:NPConstraints]
        ],
        Pol) :-

constraint([type:pp,canon:Canon,constraints:Constraints],Specs),
known(pp,[from:From,to:Next|FSSpecs]),
(constraint([pol:Polarity],FSSpecs) -> Pol=Polarity; true),

% we distribute the constraints, separating those that concern the pp
% (only prep:Prep here) from those that concern the NP inside the PP

select_ppconstraints(Constraints,PPConstraints),
select_npconstraints(Constraints,NPConstraints),
% the NP constraints are projected to the Gap

constraint([gap:[gap:[type:np,index:I,function:arg_prep,
               constraints:NPConstraints,
               arg_prep:[e:I]]]],FSSpecs),
% the gap will hold the NP constraints

constraint(PPConstraints,FSSpecs).
% we can solve the PP constraints here





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% np constraint selection
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

select_npconstraints([],[]) :- !.
% no constraints at all

select_npconstraints(Constraints,NPconstraints) :-
pick(c_str:[arg_prep:NPconstraints],Constraints,_),!.

% constraints on the NP are expressed as constraints on the arg_prep of the Prep


select_npconstraints(Constraints,[]).
% other constraints do not concern the NP


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% parallel treatment for PP constraints:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

select_ppconstraints([],[]) :- !.

select_ppconstraints(Constraints,[prep:Prep]) :-
pick(prep:Prep,Constraints,_),!.

select_ppconstraints(Constraints,[]).



%%%%%%%%%%%%%
%% PRECEDENCE 
%%%%%%%%%%%%%


% precedence rules : (except for subject) : weight first, canon second
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% posprec(Canon,Weight)

precede(posprec(0,_),posprec(_,_)) :- !.
  % subject precedes everything 


precede(posprec(Pos1,Prec1),posprec(Pos2,Prec2)) :- 
                       nonvar(Prec1), nonvar(Prec2),
                          Prec1 < Prec2,!.
  % lighter weight before heavier
  

precede(posprec(Pos1,Prec1),posprec(Pos2,Prec2)) :- 
                       nonvar(Prec1), nonvar(Prec2),
                        Prec1 > Prec2,!  ,
                       precstrict(Pos1,Pos2).
  % if the heavier element precedes, it must be strictly before
  % the other in the canonical order


precede(posprec(Pos1,Prec1),posprec(Pos2,Prec2)) :- 
                       nonvar(Prec1), nonvar(Prec2),
                        Prec1 == Prec2,Prec1 =< 3,!,
                       prec(Pos1,Pos2).
  % same weight, the canonical order decides if the weight is not too high


precede(posprec(Pos1,Prec1),posprec(Pos2,Prec2)) :- 
                       nonvar(Prec1), nonvar(Prec2),
                        Prec1 == Prec2,Prec1 > 3,!.
   % if the weight is the same and very heavy, let them through
                       
precede(posprec(Pos1,Prec1),posprec(Pos2,Prec2)) :- 
                       (var(Prec1);var(Prec2)),
                       prec(Pos1,Pos2),!.
  % one of the weights is not instantiated - let the canonical values prevail


% precede(posprec(X,Y),posprec(X2,Y2)) :- var(Y2),!.
precede(posprec(X,Y),posprec(X2,Y2)) :- var(X2),!.
  % canonical value not set - simply let the order found through 
  


%% the PRECSTRICT predicate
%%%%%%%%%%%%%%%%%%%%%%%%%%%

  precstrict(X,Y) :- nonvar(X),nonvar(Y),!, X < Y.
  precstrict(_,_).
  % strictly before


%% the PREC predicate
%%%%%%%%%%%%%%%%%%%%%

  prec(X,Y) :- nonvar(X),nonvar(Y),!, X =< Y.
  prec(_,_).
  % before or same position
  


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INSORT : arg insertion sort - drops athematic args
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

insort([H:V|T],S) :- drop(H),
                   insort(T,S).

insort([H:V|T],S) :- not(drop(H)),
                   insort(T,L),
                   insert(H:V,L,S).

insort([],[]) :- !.


%% the INSERT predicate
%%%%%%%%%%%%%%%%%%%%%%%

insert(F:V,[F1:V1|T],[F1:V1|L]) :- 
                 before(F1,F),
                 !,
                 insert(F:V,T,L).

insert(F:V,L,[F:V|L]).


%% the DROP predicate
%%%%%%%%%%%%%%%%%%%%%

drop(athematic).


%% the BEFORE predicate
%%%%%%%%%%%%%%%%%%%%%%%

before(F1,F2) :- 
                      assoc(F1,Rank1), 
                      assoc(F2,Rank2),
                      Rank1<Rank2.


%% the ASSOC predicate
%%%%%%%%%%%%%%%%%%%%%%

% assigns a canonical order in the returned parse

assoc(subject,1).
assoc(i_object,2).
assoc(object,3).
assoc(complement,2).
assoc(extraposed_subject,3).
assoc(pp_arg,4).
assoc(arg_prep,4).
assoc(object_predicate,5).
assoc(string,6).
assoc(vp_modifier,7).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%
%% outputting results 
%%%%%%%%%%%%%%%%%%%%%%

[output,results] --->
[known(pred,[from:0,to:B|FS]),
 constraint([kill:or([unspec,no]),finite:yes,gap:[]],FS), % works as a filter !
 recorded(fin,fin(B),_), 
 not(recorded(parse,parse(0,B,FS),_)),
 recorded(out,Lists,_),
 nl,nl(Lists),
 prpr(FS,0,user),
 prpr(FS,0,Lists),
 nl(Lists),
 writeq(Lists,FS), % writing raw parses for debugging purposes
 nl,nl(Lists),
 statistics(cputime,TE),
 recorded(time,TB),
 TimeUsed is TE-TB,
 nl,write('cputime : '), write(TimeUsed), nl,
 nl(Lists), write(Lists,'cputime : '), write(Lists,TimeUsed), nl(Lists),
 recorda(parse,parse(0,B,FS),_)].
 

/* the parse box contains the parses that have already
   been produced and printed -
   it ensures that here too we fail when no new parse comes along */


% example query : getting the nps
/* [output,nps] --->
[known(np,[from:A,to:X|FS]),
 not(recorded(parse,parse(A,X,FS),_)),
 recorded(out,Lists,_),
 nl,nl(Lists),
 prpr(FS,0,user),
 prpr(FS,0,Lists),
 nl,nl(Lists),
 recorda(parse,parse(A,X,FS),_)].
 */

  /*
 [output,nonfinitepreds] --->
[known(pred,[from:A,to:X|FS]),
 constraint([finite:tovp],FS),
 not(recorded(parse,parse(A,X,FS),_)),
 recorded(out,Lists,_),
 nl,nl(Lists),
 prpr(FS,0,user),
 prpr(FS,0,Lists),
 nl,nl(Lists),
 recorda(parse,parse(A,X,FS),_)].
 */

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%
%% Clearing the database 
%%%%%%%%%%%%%%%%%%%%%%%%


clear :-
       eraseall(time),
       eraseall(pos),
       eraseall(fin),
       eraseall(parse),
       eraseall(det),
       eraseall(apo),
       eraseall(apos),
       eraseall(aux),
       eraseall(auxchain), 
       eraseall(auxgroup),
       eraseall(part),
       eraseall(n),
       eraseall(adj),
       eraseall(adjp),
       eraseall(v),
       eraseall(vg),
       eraseall(vp),
       eraseall(coord),
       eraseall(np),
       eraseall(pp),
       eraseall(ppro),
       eraseall(indefpro),
       eraseall(refpro),
       eraseall(adv),
       eraseall(pred),
       eraseall(subordinator),
       eraseall(negation),
       eraseall(relative),
       eraseall(relative_clause),
       eraseall(np_post_mod),
       eraseall(coord_relative_clause),
       eraseall(prep),
       eraseall(string),
       eraseall(relclause).
       
      
 eraseall(X) :-
     recorded(X,_,Ref),
     erase(Ref),
     fail.
     
 eraseall(_).         



%%%%%%%%%%%%%%%%%%%%%%%%%%
% FEATURE LIST UNIFICATION
%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% feature unification in verification mode 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% adapted from GAL et al. "Prolog for NLP" Appendix 12 p.244-245


/* note that 'constraint' should not be called
   with a feature whose ***name*** is uninstantiated:

  !!!! constraint([Featurename:Featurevalue,sem:[thing]],
       [sem:[hum], gender:masc,Cat:N])

will succeed, instantiating Featurename to sem, and Featurevalue to [hum];
sem:[thing] will match Cat:N,
and the whole predicate call will succeed,
in spite of the incompatibility between the two sem features */



%% constraint : 2 args 
%%%%%%%%%%%%%%%%%%%%%%


constraint(F,F1) :- nonvar(F1),F=F1,!.
% identity of the two sets to be unified 


constraint([],X) :-!.
% first set is empty - end of recursion clause

constraint([F:V|Tail1],[F:V|Second]) :-
    !, constraint(Tail1,Second).

% identical feature-value pair 
% we still have to constraint the tails


constraint([sem:Set1|Tail1],[sem:Set2|Second]) :-
is_list(Set1),
is_list(Set2),
sset(Set1,Set2),
constraint(Tail1,Second),!.

% semantic features : the values are instantiated lists;
% the first must be a subset of the second
% the first arg should house the restrictions
% the second the sem values of the object meant to satisfy these restrictions

constraint([F:or(Set1)|Tail1],[F:or(Set2)|Second]) :-
is_list(Set1),
is_list(Set2),
checkinter(Set1,Set2),
constraint(Tail1,Second),!.

% OR-lists : the values are instantiated OR-lists;
% they must have a non-empty intersection

constraint([F:or(Set)|Tail1],[F:V|Second]) :-
is_list(Set),
atomic(V),
member(V,Set),
constraint(Tail1,Second),!.

constraint([F:V|Tail1],[F:or(Set)|Second]) :-
is_list(Set),
atomic(V),
member(V,Set),
constraint(Tail1,Second),!.

% OR-lists against atomic value : the atomic value is a member of the OR-list

% BEGIN EXCEPT BLOCK
%%%%%%%%%%%%%%%%%%%%%

constraint([F:except(Set)|Tail1],[F:V|Second]) :-
is_list(Set),
atomic(V),
\+ member(V,Set),
constraint(Tail1,Second),!.

constraint([F:V|Tail1],[F:except(Set)|Second]) :-
atomic(V),
is_list(Set),
\+ member(V,Set),
constraint(Tail1,Second),!.

constraint([F:or(Or_Set)|Tail1],[F:except(Except_Set)|Second]) :-
is_list(Or_Set),
is_list(Except_Set),
member(M,Or_Set),
\+ member(M,Except_Set),
constraint(Tail1,Second),!.

constraint([F:except(Except_Set)|Tail1],[F:or(Or_Set)|Second]) :-
is_list(Or_Set),
is_list(Except_Set),
member(M,Or_Set),
\+ member(M,Except_Set),
constraint(Tail1,Second),!.

% END EXCEPT BLOCK
%%%%%%%%%%%%%%%%%%

constraint([F:Set1|Tail1],[F:Set2|Second]) :-
not(atomic(Set1)),
not(atomic(Set2)),
constraint(Set1,Set2),
constraint(Tail1,Second),!.

% set-valued features : we unify the values with constraint

constraint([F:V|_],[F:V2|Second]) :-
   !,
   fail.

% we have the same feature name, but a different value: we have to fail...


constraint([F1:V1|Tail1],[F2:V2|Tail2]) :-
   constraint([F1:V1],Tail2),
   constraint(Tail1,[F2:V2|Tail2]),!.
   

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% in construction mode : funify : 3 args
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

funify(F,F,F) :- !.

% identity of the two sets to be unified 


funify(X,[],X) :-!.
funify([],X,X) :-!.

% either set is empty - end of recursion clause


funify([F:V|Tail1],[F:V|Second],[F:V|NewTail]) :-
    !, funify(Tail1,Second,NewTail).

% identical feature-value pair is just copied over
% we still have to funify the tails


funify([sem:Set1|Tail1],[sem:Set2|Second],[sem:ResF|NewTail]) :-
is_list(Set1),
is_list(Set2),
union(Set1,Set2,ResF),
funify(Tail1,Second,NewTail),!.

% semantic features: we return the union of the two lists



funify([F:or(Set1)|Tail1],[F:or(Set2)|Second],[F:ResF|NewTail]) :-
is_list(Set1),
is_list(Set2),
intersec(Set1,Set2,ResF),
funify(Tail1,Second,NewTail),!.

% OR-lists: we return the intersection of the two lists

funify([F:or(Set)|Tail1],[F:V|Second],[F:V|NewTail]) :-
is_list(Set),
atomic(V),
member(V,Set),
funify(Tail1,Second,NewTail),!.

funify([F:V|Tail1],[F:or(Set)|Second],[F:V|NewTail]) :-
is_list(Set),
atomic(V),
member(V,Set),
funify(Tail1,Second,NewTail),!.

% OR-lists and atomic value: we return the atomic value

% BEGIN EXCEPT CASES
%%%%%%%%%%%%%%%%%%%%

% OR-SET // EXCEPT-SET
%---------------------

funify([F:or(Or_Set)|Tail1],[F:except(Except_Set)|Second],[F:or(NSet)|NewTail]) :-
is_list(Or_Set),
is_list(Except_Set),
exclude(Except_Set,Or_Set,NSet),
NSet \= [],
funify(Tail1,Second,NewTail),!.

funify([F:except(Except_Set)|Tail1],[F:or(Or_Set)|Second],[F:or(NSet)|NewTail]) :-
is_list(Or_Set),
is_list(Except_Set),
exclude(Except_Set,Or_Set,NSet),
NSet \= [],
funify(Tail1,Second,NewTail),!.


% ATOM // EXCEPT-SET
%--------------------

funify([F:V|Tail1],[F:except(Except_Set)|Second],[F:V|NewTail]) :-
atomic(V),
is_list(Except_Set),
\+ member(V,Except_Set),
funify(Tail1,Second,NewTail),!.

funify([F:except(Except_Set)|Tail1],[F:V|Second],[F:V|NewTail]) :-
atomic(V),
is_list(Except_Set),
\+ member(V,Except_Set),
funify(Tail1,Second,NewTail),!.

% EXCEPT-SET // EXCEPT-SET
%--------------------------

funify([F:except(Except_Set1)|Tail1],[F:except(Except_Set2)|Second],[F:except(UnionSet)|NewTail]) :-
is_list(Except_Set1),
is_list(Except_Set2),
union(Except_Set1,Except_Set2,UnionSet),
funify(Tail1,Second,NewTail),!.


% END EXCEPT CASES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



funify([F:Set1|Tail1],[F:Set2|Second],[F:ResF|NewTail]) :-
not(atomic(Set1)),
not(atomic(Set2)),
funify(Set1,Set2,ResF),
funify(Tail1,Second,NewTail),!.

% set-valued features : we unify the values with funify

funify([F:V|_],[F:V2|Second],_) :-
   !,
   fail.

% we have the same feature name, but a different value: we have to fail...


funify([F1:V1|Tail1],[F2:V2|Tail2],Result) :-
   funify([F1:V1],Tail2,NewTail2),
   funify(Tail1,[F2:V2|NewTail2],Result),!.
   

% different features:
% we funify the first with the remainder of the second feature set
% and then we funify the remainder of the first set
% with the result of the first unification
% tacked on to the second feature

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

exclude([H|T],V,T1) :- \+ member(H,V),!, exclude(T,V,T1).
exclude([H|T],V,T1) :- pick(H,V,V1), exclude(T,V1,T1).
exclude([],X,X).

% exclude eliminates a certain set from a Set
% case of : funify([one:except([a,b]), two:b],[one:or([b,c,a,d]), two:b],S).
% S = [one:or([c,d]), two:b]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% pick : non-deterministic selection in a list 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pick(_,Var,_) :- var(Var),!, fail.  % making sure pick does not generate !

pick(A,[A|B],B).
pick(A,[B|C],[B|D]) :- pick(A,C,D).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% sets : subset and union
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% for semantic features
% we extend set membership to include inheritance 

% the left semantic set expresses the restriction,
% the right semantic set the values the restriction is matched against

sset([First|Others],Set) :- sfok(First,Set), sset(Others,Set).
sset([],_).


sfok(Sem,Semlist) :- inlist(Sem,Semlist),!.


sfok(Sem,Semlist) :- subclass(Hypo,Sem), inlist(Hypo,Semlist),!.
% we have sth more specific than what we are looking for
% we are looking for an animal and we've got a poodle
% recall that the left arg houses sem restrictions
% the right arg the sem potential of the object meant to satisfy
% these restrictions

inlist(Sem,[Sem|_]) :- !.
inlist(Sem,[_|Remainder]) :- inlist(Sem,Remainder).

subclass(X,Y) :- ako(X,Y).
subclass(X,Y) :- ako(X,Z) , subclass(Z,Y).


ako(book,document).
ako(document,thing).
ako(document,information).
ako(information,abstract).
ako(thing,concrete).
ako(human,concrete).
ako(meat,thing).

 
% checkinter(+L1,+L2)
% checks that the intersection of the two sets is not empty 

checkinter(L1,L2) :- member(Member,L1),member(Member,L2).

% intersec(tion)(+L1,+L2,-Inter) 
% returns the intersection of two sets 
% fails if the intersection is empty

intersec(L1,L2,Inter) :-
    setof(Member,(member(Member,L1),member(Member,L2)),Inter).

% set union : PREDEFINED in SWI Prolog

% union(+L1,+L2,-Union) 
% union(L1,L2,Union) :-
%        setof(Member,(member(Member,L1);member(Member,L2)),Union).
    



%%%%%%%%%%%%%%%%
% PRETTY PRINTER 
%%%%%%%%%%%%%%%%
  
% DECLARATIONS
%--------------

  % special(auxgroup).
  special(c_str).
  special(head).
  special(object).
  special(complement).
  special(i_object).
  special(subject).
  special(extraposed_subject).
  special(object_predicate).
  special(arg_prep).
  special(pp_arg).
  special(vp_modifier).
  special(determiner).
  special(rel_clause).
  special(coord_relative_clause).
  special(relative_clause).
  special(np_postmod).
  special(post_mod).
  special(det).
  special(pred).
  special(coord).
  special(rel).
  special(adjp).
  special(np).
  special(pp).
  special(vg).
  special(adv).
  special(string).
  special(args).
  special(arglist).


% STANDARD
%----------

  prpr(X,I,Handle) :- var(X),!.

  prpr([],I,Handle) :- !. 

  prpr([H|T],I,Handle):-
                  !,J is I+4,prpr(H,J,Handle),prprx(T,J,Handle).

% NOT PRINTED
%------------

  prpr(F:Value,I,Handle) :- var(Value),!.
          % features with uninstantiated values are not printed 

  prpr(F:[],I,Handle) :- !.

  prpr(gap:V,I,Handle) :- !.
  prpr(prolog:G,I,Handle) :- !.
  prpr(agrposs:V,I,Handle) :- !.
  prpr(class:V,I,Handle) :- !.   
  prpr(count:V,I,Handle) :- !.
  prpr(arglist:V,I,Handle) :- !.
  prpr(agr:V,I,Handle) :- !.
  prpr(constraints:V,I,Handle) :- !.
  prpr(txt:V,I,Handle) :- !.
  prpr(weight:V,I,Handle) :- !.
  prpr(finite:V,I,Handle) :- !.
  prpr(type:V,I,Handle) :- !.
  prpr(pc:V,I,Handle) :- !.
  prpr(kill:V,I,Handle) :- !.
  prpr(function:V,I,Handle) :- !.
  prpr(negtransport:V,I,Handle) :- !.
  prpr(negswitch:V,I,Handle) :- !.
  prpr(ft:V,I,Handle) :- !.
  prpr(aux:L,I,Handle) :- cleanaux(L,LClean), prpr(auxgroup:LClean,I,Handle),!.


% PRINTED AS LISTS
%------------------

  prpr(Special:V,I,Handle) :- special(Special), 
                              !,
                              tab(Handle,I),
                              write(Handle,Special),
                              nl(Handle),
                              prpr(V,I,Handle).

   % special cases: printed as lists, i.e. with each element on its own line 
   % following a header line made up of the name of the Special element

  
% STANDARD 
%---------
  
  prpr(X,I,Handle):- tab(Handle,I),write(Handle,X),nl(Handle).


  prprx([],_,Handle):- !. 
  prprx(X,_,_) :- var(X),!.
  prprx([H|T],I,Handle):- prpr(H,I,Handle),prprx(T,I,Handle).


% TREATMENT OF THE AUX GROUP
%---------------------------

% Cleaning the aux list of additional info
% only the Property field and the Tense feature is kept

  cleanaux([prop:P|T],[prop:P|T1]) :- !,cleanaux(T,T1).
  cleanaux([tense:P|T],[tense:P|T1]) :- !,cleanaux(T,T1).
  cleanaux([H|T],T1) :- cleanaux(T,T1).
  cleanaux([],[]).


% GETTING RID OF CONTRACTED AUXs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% non-deterministic
%------------------

% s --> is or has ; d --> had or would
decontract(['\'',s|R],[has|R1]) :- decontract(R,R1).
decontract(['\'',s|R],[is|R1]) :- decontract(R,R1).
decontract(['\'',d|R],[would|R1]) :- decontract(R,R1).
decontract(['\'',d|R],[had|R1]) :- decontract(R,R1).


% deterministic
%--------------

% positive

% be
decontract(['\'',m|R],[am|R1]) :- !, decontract(R,R1).
decontract(['\'',re|R],[are|R1]) :- !,decontract(R,R1).

% have
decontract(['\'',ve|R],[have|R1]) :- !,decontract(R,R1).


% negative

decontract([mayn,'\'',t|R],[may,not|R1]) :- !, decontract(R,R1).
decontract([mightn,'\'',t|R],[might,not|R1]) :- !, decontract(R,R1).

decontract([can,'\'',t|R],[can,not|R1]) :- !, decontract(R,R1).
decontract([cannot|R],[can,not|R1]) :- !, decontract(R,R1).
decontract([couldn,'\'',t|R],[could,not|R1]) :- !, decontract(R,R1).

decontract([mustn,'\'',t|R],[must,not|R1]) :- !, decontract(R,R1).

decontract([shouldn,'\'',t|R],[should,not|R1]) :- !, decontract(R,R1).
decontract([wouldn,'\'',t|R],[would,not|R1]) :- !, decontract(R,R1).

decontract([shan,'\'',t|R],[shall,not|R1]) :- !, decontract(R,R1).
decontract([won,'\'',t|R],[will,not|R1]) :- !, decontract(R,R1).

decontract([hasn,'\'',t|R],[has,not|R1]) :- !, decontract(R,R1).
decontract([haven,'\'',t|R],[have,not|R1]) :- !, decontract(R,R1).
decontract([hadn,'\'',t|R],[had,not|R1]) :- !, decontract(R,R1).

decontract([don,'\'',t|R],[do,not|R1]) :- !, decontract(R,R1).
decontract([doesn,'\'',t|R],[does,not|R1]) :- !, decontract(R,R1).
decontract([didn,'\'',t|R],[did,not|R1]) :- !, decontract(R,R1).

decontract([isn,'\'',t|R],[is,not|R1]) :- !, decontract(R,R1).
decontract([aren,'\'',t|R],[are,not|R1]) :- !, decontract(R,R1).
decontract([wasn,'\'',t|R],[was,not|R1]) :- !, decontract(R,R1).
decontract([weren,'\'',t|R],[were,not|R1]) :- !, decontract(R,R1).

% nothing to do

decontract([H|T],[H|T1]) :- decontract(T,T1).
decontract([],[]).


% myappend
%%%%%%%%%%

% gets rid of vars on the fly

 myappend([],[],[]) :- !.
 myappend([],X,X) :- !.
 myappend(X,[],X) :- !.
 myappend(X,Y,[]) :- var(X), var(Y), !.
 myappend(X,Y,X) :- var(Y),!.
 myappend(X,Y,Y) :- var(X),!.
 myappend(X,[Y],X) :- var(Y),!.
 myappend([Head|L1],L2,[Head|L3]) :- myappend(L1,L2,L3).

% RAISING VERBS
%%%%%%%%%%%%%%%

raising(tr_vp).
raising(tr_inf_pass).
raising(tr_inf_gap).
raising(tr_io_inf).

% REVERSE KILL FLAG
%%%%%%%%%%%%%%%%%%%

rev(no,yes).
rev(yes,no).
rev(unspec, unspec).

% AGREE
%%%%%%%%

% agreement between subject agreement features and poss or reflex agr features
% he doesn't mince his words
% she doesn't mince her words
% nobody minces his words, her words, their words
% nobody prides herself, himself, themselves, himself or herself on his, etc.

agree(Lex,SAgr,PossRefAgr) :-
(  (nonvar(Lex),
   member(Lex,[someone,somebody,anyone,anybody,
               noone,nobody,everyone,everybody])) ->
                           constraint([person:or([3,indef])],PossRefAgr);
                            constraint(SAgr,PossRefAgr)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%
% Dealing with collocates
%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% working out a medium weight
% adding the proximity factors for all the collocats
% and then dividing the total by the nber of elements in the collocate list

% we also keep the best match between lex of the textual arg head
% and member of the collocate list

accu_w(Lex,Collocates,MaxW-L,Max-CM-W) :-
length(Collocates,Length),
accu(Lex,Collocates,MaxW-L,Max-CM,SW),
W is SW/Length.

accu(Lex,[Coll|OtherColls],MaxW-L,Max-CM,TotW) :-
nonvar(Lex),
(lexdis(Coll,n,Lex,Weight) -> W=Weight;W=0),
!,
(W > MaxW -> NMax=W-Coll; NMax=MaxW-L),
accu(Lex,OtherColls,NMax,Max-CM,OtherW),
TotW is W+OtherW.


accu(_,[],M,M,0).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% THE MEASURE OF LEXICAL PROXIMITY
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lexdis(Coll,Pos,Lex,Weight) :- recorded(lexdis,lexdis(Coll,Pos,Lex,Weight),_),
                               !.
% lemma function - we do not recompute lexical proximity !

lexdis(L,n,L,1000) :- !.
% the lex of the textual item is a member of the collocate list
% -> top mark

lexdis(_,_,me,0) :- !.
lexdis(_,_,i,0) :- !.
lexdis(_,_,you,0) :- !.
lexdis(_,_,him,0) :- !.
lexdis(_,_,he,0) :- !.
lexdis(_,_,she,0) :- !.
lexdis(_,_,her,0) :- !.
lexdis(_,_,it,0) :- !.
lexdis(_,_,we,0) :- !.
lexdis(_,_,us,0) :- !.
lexdis(_,_,they,0) :- !.
lexdis(_,_,them,0) :- !.
lexdis(_,_,something,0) :- !.
lexdis(_,_,anything,0) :- !.
lexdis(_,_,nothing,0) :- !.
lexdis(_,_,everything,0) :- !.
lexdis(_,_,somebody,0) :- !.
lexdis(_,_,anybody,0) :- !.
lexdis(_,_,nobody,0) :- !.
lexdis(_,_,everybody,0) :- !.
lexdis(_,_,someone,0) :- !.
lexdis(_,_,anyone,0) :- !.
lexdis(_,_,noone,0) :- !.
lexdis(_,_,everyone,0) :- !.
lexdis(_,_,thing,0) :- !.
lexdis(_,_,person,0) :- !.
lexdis(_,_,people,0) :- !.


lexdis(Word1,Pos,Word2,NW) :-
    setof(data(Lablist1,Gwlist1,Deflex1,Exlex1),
          Def^Dic^Idnum^Lablist1^Gwlist1^Deflex1^Exlex1^(mono(lem(Word1),
             ori(Dic),
             idnum(Idnum),
             pos(Pos),
             lab(Lablist1),
             gw(Gwlist1),
             deflex(Deflex1),
             exlex(Exlex1),
             def(Def)   )),
           DL1),

    setof(data(Lablist2,Gwlist2,Deflex2,Exlex2),
         Def^Dic2^Idnum2^Lablist2^Gwlist2^Deflex2^Exlex2^(mono(lem(Word2),
             ori(Dic2),
             idnum(Idnum2),
             pos(Pos),
             lab(Lablist2),
             gw(Gwlist2),
             deflex(Deflex2),
             exlex(Exlex2),
             def(Def)   )),
         DL2),

   collect(DL1,Lab1,Gw1,Def1,Ex1),
   collect(DL2,Lab2,Gw2,Def2,Ex2),

% we distribute the data just collected :
% labels, guidewords, definition core items, example core items

   flatten(Lab1,L1),
   flatten(Lab2,L2),
   flatten(Gw1,G1),
   flatten(Gw2,G2),
   flatten(Def1,D1),
   flatten(Def2,D2),
   flatten(Ex1,E1),
   flatten(Ex2,E2),

   compute(lb,L1,L2,ResLabs,WeightLabs),
   compute(gw,G1,G2,ResGws,WeightGws),

   % computing the weight to be assigned to both labels and guidewords


   cw(def,Word1,Word2,D1,D2,ResDeflex,WeightDeflex),
   cw(ex,Word1,Word2,E1,E2,ResExlex,WeightExlex),

   % idem for definition core items and example core items

   metameet(Word1,Word2,Metaweight),
   rogetmeet(Word1,Word2,Rogetweight),
   indicmeet(Word1,Pos1,Word2,Pos2,Indicweight),
   collmeet(Word1,Pos1,Word2,Pos2,Collweight),
   envirmeet(Word1,Word2,Envirweight),

   % weight assignation to collocate field sharing, Roget's category sharing,
   % indicator sharing, collocate sharing, sharing of environment

   accu([WeightLabs,WeightGws,WeightDeflex,WeightExlex,
         Metaweight,Rogetweight,Indicweight,Collweight,Envirweight],GW),
   adjustweight(Word1,Pos1,Word2,Pos2,Adjust),
   NW is GW /Adjust,
         
   recorda(lexdis,lexdis(Word1,Pos,Word2,NW),_).




   collect([],[],[],[],[]).
   collect([data(L,G,D,E)|MoreData],
         [L|MoreL], [G|MoreG],
         [D|MoreD],[E|MoreE]) :-
         collect(MoreData,MoreL,MoreG,MoreD,MoreE).


compute(lb,L1,L2,R,Weight) :-
        (   is_set(L1) -> Set1=L1 ; list_to_set(L1,Set1)),
        (   is_set(L2) -> Set2=L2 ; list_to_set(L2,Set2)),
        intersection(Set1,Set2,R),
        length(R,Len),
        Weight is Len*4.

% weighting for labels : each shared label is worth 4.


compute(gw,L1,L2,R,Weight) :-
        (   is_set(L1) -> Set1=L1 ; list_to_set(L1,Set1)),
        (   is_set(L2) -> Set2=L2 ; list_to_set(L2,Set2)),
        intersection(Set1,Set2,R),
        length(R,Len),
        Weight is Len*12.

% weighting for guidewords : each shared guideword is worth 12



cw(def,Word1,Word2,L1,L2,Res,Weight) :-
        (   is_set(L1) -> Set1=L1 ; list_to_set(L1,Set1)),
        (   is_set(L2) -> Set2=L2 ; list_to_set(L2,Set2)),
        (member(Word1,Set2) -> Bonus1=5 ; Bonus1=0 ),
        % bonus is granted when the words 'interdefine', i.e.
        % A is used in the definition of B, or vice-versa
        (member(Word2,Set1) -> Bonus2=5 ; Bonus2=0 ),
        intersection(Set1,Set2,Res),
        length(Res,Len),
        (Len <3 -> DefLen is Len*2 ; DefLen is Len*3),
        % we increase the weight if we believe
        % that the sharing exceeds a threshold
        % at which it could still be regarded as incidental (set to 2)
        accu([DefLen,Bonus1,Bonus2],Weight).

cw(ex,Word1,Word2,L1,L2,Res,Weight) :-
        (   is_set(L1) -> Set1=L1 ; list_to_set(L1,Set1)),
        (   is_set(L2) -> Set2=L2 ; list_to_set(L2,Set2)),
        (member(Word1,Set2) -> Bonus1=2 ; Bonus1=0 ),
        (member(Word2,Set1) -> Bonus2=2 ; Bonus2=0 ),
        intersection(Set1,Set2,Res),
        length(Res,Len),
        accu([Len,Bonus1,Bonus2],Weight).


accu([],0).
accu([H|T],Total) :- accu(T,N), Total is N + H.


metameet(W1,W2,Weight) :-
        W1 @< W2,
        mt(W1,List),
        member([W2,Cooc],List),
        ( (W1=person;W2=person;W1=object;W2=object) ->
                     Weight is Cooc // 16;
                     Weight is Cooc // 2),
               !.

metameet(W1,W2,Weight) :-
        W1 @> W2,
        mt(W2,List),
        member([W1,Cooc],List),
        ( (W1=person;W2=person;W1=object;W2=object),
                     Weight is Cooc // 16;
                     Weight is Cooc // 2),
              !.

metameet(_,_,0).

%---------------------------------------------------------------------------

% connectedness through the sharing of Roget's categories
% three levels of delicacy in thesaurus organisation

% a r line looks like the following:

% r('antiquarian',[['n','122','4','4'],['n','492','4','2']]).
% which means that the word antiquarian is a noun
% that belongs to two category triples
% 122/4/4 and 492/4/2 where the broadest category is first (492),
% followed by sub-category(4) and sub-sub-category(2)


% we retrieve the list of categories associated with the two items
% and then we compute their intersection, accumulating the weights
% according to type of category matched

% the check on order of the two items (W1 and W2) ensures
% that the match is always exactly the same
% independent of the ordering of the two items in the query
% cat should rogetmeet dog exactly like dog rogetmeets cat
% we ensure this by always matching cat-dog, never dog-cat

rogetmeet(W1,W2,Weight) :-
        W1 @< W2,
        r(W1,W1list),
        r(W2,W2list),
        i_roget(W1list,W2list,Commonlist),
        accu(Commonlist,Weight),
        !.

rogetmeet(W1,W2,Weight) :-
        W2 @< W1,
        r(W1,W1list),
        r(W2,W2list),
        i_roget(W2list,W1list,Commonlist),
        accu(Commonlist,Weight),
        !.

rogetmeet(_,_,0).



% computing the intersection

i_roget([],_,[]).

i_roget([[Pos,Cat,Sub1,Sub2]|Tail],Otherlist,[Weight|Tail2]) :-
        member([P,Cat,Othersub1,Othersub2],Otherlist),!,

% we leave POS as a variable, desinstantiating it to P
% the POS field should not be relevant to thesauric organisation,
% although it is (for instance, in Roget's itself !)

        irogetweight(Sub1,Sub2,Othersub1,Othersub2,Weight),
        i_roget(Tail,Otherlist,Tail2).

i_roget([A|Tail],Otherlist,Tail2) :-
        i_roget(Tail,Otherlist,Tail2).

irogetweight(A,B,A,B,3) :-   !.  % all three categories
irogetweight(A,_,A,_,2) :-   !.  % cat and sub-cat
irogetweight(_,_,_,_,1) :-   !.  % only broadest cat

%--------------------------------------------------------------------------

% Connectedness through indic sharing in RC/OH indicator data base

% db struc (indic.pl) : e.g.
% ind(lemma('abacus'),pos(n),indic(['counting','frame'])).


indicmeet(Word1,Pos1,Word2,Pos2,IndicWeight) :-
     setof(Indic,
           Word1^Pos1^ind(lemma(Word1),pos(Pos1),indic(Indic)),
           IndicList1),
     setof(Indic2,
           Word2^Pos2^ind(lemma(Word2),pos(Pos2),indic(Indic2)),
           IndicList2),
     flatten(IndicList1, IL1),
     flatten(IndicList2, IL2),
     (is_set(IL1) -> Set1=IL1 ; list_to_set(IL1,Set1)),
     (is_set(IL2) -> Set2=IL2 ; list_to_set(IL2,Set2)),
     intersection(Set1,Set2,Res),
     length(Res,Len),
     (Len <3 -> IndicWeight is Len*2; IndicWeight is Len*4),
     !.

indicmeet(_W,_P,_W2,_P2,0).

%----------------------------------------------------------------------------

% Connectedness through collocate sharing in RC/OH collocate data base

% db struc (coll.pl) : e.g.
% coll(lemma('abandonment'),pos(n),coll(['property','right'])).
% here, contrary to what we get through metameet,
% the two items are related if they POSSESS common elements
% in their collocate lists
% in metameet it is the copresence within a collocate list
% (associated with whatever item) that is significant

collmeet(Word1,Pos1,Word2,Pos2,CollWeight) :-
     setof(Coll,
           Word1^Pos1^coll(lemma(Word1),pos(Pos1),coll(Coll)),
           CollList1),
     setof(Coll2,
           Word2^Pos2^coll(lemma(Word2),pos(Pos2),coll(Coll2)),
           CollList2),
     flatten(CollList1, CL1),
     flatten(CollList2, CL2),
     (is_set(CL1) -> Set1=CL1 ; list_to_set(CL1,Set1)),
     (is_set(CL2) -> Set2=CL2 ; list_to_set(CL2,Set2)),
     intersection(Set1,Set2,Res),
     length(Res,Len),
     (Len <3 -> CollWeight is Len*2 ; CollWeight is Len*4),  % cf indicator
     !.

collmeet(_W,_P,_W2,_P2,0).

%------------------------------------------------------------------------

% Connectedness through envir sharing in RC/OH envir data base

% db struc (envir.pl) : e.g. e(hdwd('dative'),envir(['case','ending'])).

% the POS are not significant here

envirmeet(Word1,Word2,EnvirWeight) :-
     setof(Envir,
           Word1^e(hdwd(Word1),envir(Envir)),
           EnvirList1),
     setof(Envir2,
           Word2^e(hdwd(Word2),envir(Envir2)),
           EnvirList2),
     flatten(EnvirList1, EL1),
     flatten(EnvirList2, EL2),
     (is_set(EL1) -> Set1=EL1 ; list_to_set(EL1,Set1)),
     (is_set(EL2) -> Set2=EL2 ; list_to_set(EL2,Set2)),
     intersection(Set1,Set2,Res),
     length(Res,Len),
     (Len >1 -> EnvirWeight is Len; EnvirWeight is 0),
     !.

envirmeet(_W,_W2,0).


%------------
% Utilities :
%------------

% allmembers(Subset,List)

allmembers([],_).
allmembers([H|Tail],List) :- member(H,List), allmembers(Tail,List).

% anymember(Candidates,List)

anymember(L,L1) :- intersection(L,L1,[_|_]).



% adjustweight(Word1,Pos1,Word2,Pos2,Adjust)
% working out decrease factor for heavy items
% calls on pesi db, with clauses such as:
% w(fox, v, 189).

adjustweight(Word1,Pos1,Word2,Pos2,Adjust) :-
 (w(Word1,Pos1,Weight1) -> W1 is Weight1 ; W1 is 1),
 (w(Word2,Pos2,Weight2) -> W2 is Weight2 ; W2 is 1),
  Both is W1+W2,
 ( Both < 200 ->  Adjust is 1 ; Adjust is Both/200).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Appendix : the GS module
%%%%%%%%%%%%%%%%%%%%%%%%%%

%% GET_SENTENCE
%%%%%%%%%%%%%%%

%%% GS %%%%
%%%%%%%%%%%

% :- module(gs,[getsentence/6]).

% :- style_check(-singleton).

/* this file should be included in the pl library
   programs making use of it should load it and compile it
   with the following directive:
   :-use_module(library(gs)). */


/* String to word list converter */
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* first arg of getsentence is the wordlist to be obtained from the string
   second arg is either user (keyboard input)
   or an input stream identifier computed by the system

   third arg is a list of sentence delimiters
   fourth arg is a list of word delimiters
   fifth arg is a list of punctuation signs to be returned as quoted atoms
   last arg is either caps (capitals preserved)
                      or nocaps (capitals turned into small letters)

   a licit call is for example:
   getsentence(Sentence, user,[col, semicol],[apo],[],nocaps).

   Sentence delimiters:
   Period (pt), exclamation mark (excl) and question mark (int)
   and eof are always sentence delimiters.
   Colon (col) and semi-colon (semicol) are sentence delimiters
   only if specified in the third arg
   Sentence delimiters are all ipso facto also word delimiters


   Word delimiters
   All sentence delimiters
   (including the optional sentence delimiters col and semicol)
   are word delimiters
   Comma, eol, space, quotes, parentheses, brackets, curly brackets
   and slash are also word delimiters
   Apostrophe (apo) and hyphen (hyphen) are word delimiters
   only if members of the list entered as fourth arg

   Returned punctuation signs
   Only the punctuation signs specified in the fifth arg
   are returned as quoted atoms. They can be any of the following:

   Period                       pt                      .
   Exclamation mark             excl                    !
   Question mark                int                     ?
   Colon                        col                     :
   Semi-colon                   semicol                 ;
   Apostrophe                   apo                     '
   Hyphen                       hyphen                  -
   Comma                        comma                   ,
   Quotes                       quotes                  "
   Parenthesis                  par                     ()
   Brackets                     br                      []
   Curly brackets               curly                   {}
   Slash                        slash                   /

   */


% THE WORDS ARE REPRESENTED AS ATOMS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% they should be written with writeq
% when the results are printed to file
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


getsentence(Wordlist,Handle,SD, WD, PS,CapsFlag) :-
           get_code(Handle,Char),
           (Char = -1 -> (!,fail) ; true),
           getrest(Handle,Char,Wordlist,SD, WD, PS,CapsFlag).

/* we read a character in from the stream pointed to by Handle (get_code)
   we use this character as look-ahead character
   the end result is the Wordlist corresponding to the input string

   we FAIL and stay failed (the !) if we get the eof character (-1)

   for reading in a whole text, the process calling 'getsentence'
   should therefore call itself recursively,
   and then SUCCEED vacuously when 'getsentence' fails

   this solution is adequate if the calling process uses an accumulator;
   on a vacuous success the accumulator is copied into the relevant variable

   we can also use stop. as last line of a file,
   and interrupt the repeated getsentence process
   as soon as [stop,'.']  or [stop]  is returned,
   depending on whether pt is a returned puctuation sign */

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* end of sentence markers */
%---------------------------

/* eof 1 */
getrest(Handle,-1,[],_,_,_,_):- !.


/* ! 2   */
getrest(Handle,33,['!'],SD,WD,PS,CapsFlag):- member(excl, PS),!.
getrest(Handle,33,[],SD,WD,PS,CapsFlag):-    !.


/* . 3   */
getrest(Handle,46,['.'],SD,WD,PS,CapsFlag):-member(pt,PS),!.
getrest(Handle,46,[],SD,WD,PS,CapsFlag):-!.

/* ? 4   */
getrest(Handle,63,['?'],SD,WD,PS,CapsFlag):- member(int,PS),!.
getrest(Handle,63,[],SD,WD,PS,CapsFlag):-!.

/* : 5   */
getrest(Handle,58,[':'],SD,WD,PS,CapsFlag):- member(col,SD),
                                             member(col,PS),!.
getrest(Handle,58,[],SD,WD,PS,CapsFlag):-    member(col,SD),!.


/* ; 6   */
getrest(Handle,59,[';'],SD,WD,PS,CapsFlag):-
                                             member(semicol,SD),
                                             member(semicol,PS),!.
getrest(Handle,59,[],SD,WD,PS,CapsFlag):-    member(semicol,SD),!.


/* at the end of the sentence getrest simply succeeds,
   and thereby puts an end to the getsentence process too  */

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* end of word markers */
%-------------------------

/* eol       1 */
getrest(Handle,10,Wordlist,SD,WD,PS,CapsFlag) :-
         !,getsentence(Wordlist,Handle,SD,WD,PS,CapsFlag).

/* space     2 */
getrest(Handle,32,Wordlist,SD,WD,PS,CapsFlag) :-
         !,getsentence(Wordlist,Handle,SD,WD,PS,CapsFlag).




/* "         3 */
getrest(Handle,34,['\"'|Wordlist],SD,WD,PS,CapsFlag) :-
         member(quotes,PS),!,
         getsentence(Wordlist,Handle,SD,WD,PS,CapsFlag).
  
getrest(Handle,34,Wordlist,SD,WD,PS,CapsFlag) :-
        !,getsentence(Wordlist,Handle,SD,WD,PS,CapsFlag).


/* '        4 */
getrest(Handle,39,['\''|Wordlist],SD,WD,PS,CapsFlag):-
         member(apo,WD),
         member(apo,PS),!,
         getsentence(Wordlist,Handle,SD,WD,PS,CapsFlag).
  
getrest(Handle,39,Wordlist,SD,WD,PS,CapsFlag):-
         member(apo,WD),!,
         getsentence(Wordlist,Handle,SD,WD,PS,CapsFlag).


/* (         5 */
getrest(Handle,40,['('|Wordlist],SD,WD,PS,CapsFlag) :-
         member(par,PS),!,
         getsentence(Wordlist,Handle,SD,WD,PS,CapsFlag).
  
getrest(Handle,40,Wordlist,SD,WD,PS,CapsFlag) :-
         !,getsentence(Wordlist,Handle,SD,WD,PS,CapsFlag).

/* )        6  */
getrest(Handle,41,[')'|Wordlist],SD,WD,PS,CapsFlag) :-
         member(par,PS),!,
         getsentence(Wordlist,Handle,SD,WD,PS,CapsFlag).
 
getrest(Handle,41,Wordlist,SD,WD,PS,CapsFlag) :-
         !,getsentence(Wordlist,Handle,SD,WD,PS,CapsFlag).



 /* ,        7  */
getrest(Handle,44,[','|Wordlist],SD,WD,PS,CapsFlag) :-
         member(comma,PS),!,
         getsentence(Wordlist,Handle,SD,WD,PS,CapsFlag).
   
getrest(Handle,44,Wordlist,SD,WD,PS,CapsFlag) :-
         !,getsentence(Wordlist,Handle,SD,WD,PS,CapsFlag).


/* -         8  */
getrest(Handle,45,['-'|Wordlist],SD,WD,PS,CapsFlag) :-
         member(hyphen,WD),
         member(hyphen,PS),!,
         getsentence(Wordlist,Handle,SD,WD,PS,CapsFlag).
    
getrest(Handle,45,Wordlist,SD,WD,PS,CapsFlag) :-
         member(hyphen,WD),!,
         getsentence(Wordlist,Handle,SD,WD,PS,CapsFlag).


/* /         9  */
getrest(Handle,47,['/'|Wordlist],SD,WD,PS,CapsFlag) :-
         member(slash,PS),!,
         getsentence(Wordlist,Handle,SD,WD,PS,CapsFlag).
   
getrest(Handle,47,Wordlist,SD,WD,PS,CapsFlag) :-
        !,getsentence(Wordlist,Handle,SD,WD,PS,CapsFlag).


/* ;        10 */
getrest(Handle,58,[':'|Wordlist],SD,WD,PS,CapsFlag) :-
         not(member(col,SD)),
         member(col,PS),!,
         getsentence(Wordlist,Handle,SD,WD,PS,CapsFlag).
  
getrest(Handle,58,Wordlist,SD,WD,PS,CapsFlag) :-
          not(member(col,SD)),!,
          getsentence(Wordlist,Handle,SD,WD,PS,CapsFlag).


/* :        11 */
getrest(Handle,59,[';'|Wordlist],SD,WD,PS,CapsFlag) :-
          not(member(semicol,SD)),
          member(semicol,PS),!,
          getsentence(Wordlist,Handle,SD,WD,PS,CapsFlag).
   
getrest(Handle,59,Wordlist,SD,WD,PS,CapsFlag) :-
         not(member(semicol,SD)),!,
         getsentence(Wordlist,Handle,SD,WD,PS,CapsFlag).

/* [        12 */
getrest(Handle,91,['['|Wordlist],SD,WD,PS,CapsFlag) :-
        member(br,PS),!,
        getsentence(Wordlist,Handle,SD,WD,PS,CapsFlag).
    
getrest(Handle,91,Wordlist,SD,WD,PS,CapsFlag) :-
        !,getsentence(Wordlist,Handle,SD,WD,PS,CapsFlag).

/* ]        13 */
getrest(Handle,93,[']'|Wordlist],SD,WD,PS,CapsFlag) :-
        member(br,PS),!,
        getsentence(Wordlist,Handle,SD,WD,PS,CapsFlag).
   
getrest(Handle,93,Wordlist,SD,WD,PS,CapsFlag) :-
         !,getsentence(Wordlist,Handle,SD,WD,PS,CapsFlag).

/* {        14 */
getrest(Handle,123,['{'|Wordlist],SD,WD,PS,CapsFlag) :-
        member(curly,PS),!,
        getsentence(Wordlist,Handle,SD,WD,PS,CapsFlag).
  
getrest(Handle,123,Wordlist,SD,WD,PS,CapsFlag) :-
        !,getsentence(Wordlist,Handle,SD,WD,PS,CapsFlag).

/* }        15 */
getrest(Handle,125,['}'|Wordlist],SD,WD,PS,CapsFlag) :-
       member(curly,PS),!,
       getsentence(Wordlist,Handle,SD,WD,PS,CapsFlag).
  
getrest(Handle,125,Wordlist,SD,WD,PS,CapsFlag) :-
        !,getsentence(Wordlist,Handle,SD,WD,PS,CapsFlag).


/* in these clauses getrest has come to the end of a word,
   and relaunches getsentence with the
   current Wordlist, to which new words will be added */

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getrest(Handle,Letter,[Word|Wordlist],SD,WD,PS,CapsFlag):-
             getletters(Handle,Letter,Letters,Nextchar,WD,CapsFlag),
             name(Word,Letters),
             getrest(Handle,Nextchar,Wordlist,SD,WD,PS,CapsFlag).

/* getletters collects the letters of a word in a list 'Letters'
   then the predicate name turns the list of letters
   into an atom representing the word read
   the word is put in front of the resulting wordlist
   getrest should get the other words of the string */


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getletters(H,-1,[],-1,WD,CapsFlag):- !.          /* eof                  1 */
getletters(H,10,[],10,WD,CapsFlag):-!.           /* eol                  2 */
getletters(H,32,[],32,WD,CapsFlag):-!.          /* space                 3 */
getletters(H,33,[],33,WD,CapsFlag):-!.          /* !                     4 */
getletters(H,34,[],34,WD,CapsFlag):-!.          /* "                     5 */


getletters(H,39,[],39,WD,CapsFlag):- member(apo,WD),!.
        /* apostrophe  used as a word delimiter: c'est...                6 */

getletters(H,40,[],40,WD,CapsFlag):-!.                  /* (             7 */
getletters(H,41,[],41,WD,CapsFlag):-!.                  /* )             8 */
getletters(H,44,[],44,WD,CapsFlag):-!.                  /* ,             9 */

getletters(H,45,[],45,WD,CapsFlag):- member(hyphen,WD),!.
         /* hyphen used as word delimiter: est-ce que ...                10 */

getletters(H,46,[],46,WD,CapsFlag):-!.           /* .                    11 */
getletters(H,47,[],47,WD,CapsFlag):-!.           /* /                    12 */
getletters(H,58,[],58,WD,CapsFlag):-!.          /* :                     13 */
getletters(H,59,[],59,WD,CapsFlag):-!.          /* ;                     14 */
getletters(H,63,[],63,WD,CapsFlag):-!.          /* ?                     15 */
getletters(H,91,[],91,WD,CapsFlag):-!.          /* [                     16 */
getletters(H,93,[],93,WD,CapsFlag):-!.          /* ]                     17 */
getletters(H,123,[],123,WD,CapsFlag):-!.        /* {                     18 */
getletters(H,125,[],125,WD,CapsFlag):-!.        /* }                     19 */

/* when we hit a word delimiter we get out of getletters,
   adding nothing to the letter list */

/* NOTE
  the program making use of the resulting wordlist can use
  the special quoted atoms in the obvious way, e.g.
  comma([','|X],X).
  to read a comma from the resulting wordlist */

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getletters(Handle,Let,[Let1|Letters],Nextchar,WD,CapsFlag):-
                 transform(Let,Let1,CapsFlag),!,
                 get_code(Handle,Char),
                 getletters(Handle,Char,Letters,Nextchar,WD,CapsFlag).

/* we examine the lookahead letter 'Let'
   we see if we need to keep it
   we keep it only if we can transform it

   transform operates vacuously on most chars
   but if CapsFlag is set to nocaps it turns capital letters into small letters
   transform fails for elements below ascii 32, i.e. non-printing chars
   except for -1, the eof char

   we then add the resulting letter to the front of the letter list
   we get a fresh char with get_code
   we use this char as look-ahead char for the next getletters goal */

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getletters(Handle,Let,Letters,Nextchar,WD,CapsFlag):-
                 get_code(Handle,Char),
                 getletters(Handle,Char,Letters,Nextchar,WD,CapsFlag).

/* here the letter carried in Let is simply dropped */


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* checking and transforming the char */
%---------------------------------------

transform(C,C1,nocaps):-C>64,C<91,!,C1 is C+32.
transform(C,C1,nocaps):-C>191,C<222,!,C1 is C+32.

/* nocaps : small letters are capital letters+32;
   e.g. A is 65 and a is 97(=65+32) */

transform(C,C,_) :- C>31, C<513.

/* chucks non-printing and widely foreign ;-) chars out */
% C gets transformed into itself, i.e. kept,
% only if its ascii number is above 31

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
