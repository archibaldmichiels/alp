
% ALP, A Latin Parser v 198
%%%%%%%%%%%%%%%%%%%%%%%%%%%

% more specifically :
% A data-driven feature-unification parser for a micro-subset of classical Latin


% Latest Update : Feb 2025
% © Archibald Michiels
% amichiels@uliege.be

% a SWI-Prolog program
% https://www.swi-prolog.org/


% alp196 changes and additions to be marked by alp198 token


% Directions for use
%%%%%%%%%%%%%%%%%%%%

% To start the program running type "go." when it has finished loading.
% Do not type in the quotes but make sure you do type in the dot (: go.)

% The Latin sentences can be read in from a file or from stdin, the standard input, i.e. the keyboard
% Type "stdin." or the name of a file, followed by a dot, e.g. "testalp." (do not type in the quotes: stdin. or filename. (e.g. testalp.)

% The files must end on a line that is made up of the single word 'stop' followed by a dot, i.e. "stop.", without the quotes (stop.)


:- style_check(-singleton).
:- style_check(-discontiguous).

:- [vocfile,template]. % vocfile is produced by makelex.pl
       % vocfile should be in the same dir as alp itself
       % idem for template.pl, containing the verb templates


%%%%%%%%%%%%%%%
% DECLARATIONS
%%%%%%%%%%%%%%%

:- set_prolog_flag(double_quotes, codes). % only necessary if home-made 'getsentence' is used (equivalent of readln)
                  % 'getsentence' IS made use of in the present version !!!!!!!!!!!!!!!!!!!



% the 'lex' predicate is fed by macro-expansion clauses executed by makelex
% and must therefore be declared dynamic
% a declaration that is taken care of in template.pl


% operator  declaration:

:- op(800,xfx,--->).

% used in rules : rule_name ---> rule_body (=list of 'actions' to be taken)


% empty check point for quick debugging; call : spy(checkpoint)
% after inserting a call or several calls to 'checkpoint(1)'... 'checkpoint(n)' wherever appropriate
% then start stepping through the goals

checkpoint(_).



% better readability in the programmer's eye ;-)

ifthen(Condition,Goal) :- Condition -> Goal ; true.
ifthenelse(Condition, ThenGoal, ElseGoal) :- Condition -> ThenGoal ; ElseGoal.



%%%%%%%%%%
% GO STEP
%%%%%%%%%%

go :- nl,

     protocola(alp),               % protocol file stored in the current directory
     set_default,                  % default settings : see below

     write('ALP, a data-driven feature-unification parser for a subset of classical Latin' ),nl,
     write('-----------------------------------------------------------------------------'),
     nl,nl,
     write('A.Michiels, amichiels@uliege.be'),nl,nl,
     nl,nl,
     recorded(stg,[current,Current],_),                           % settings concern depth of analysis, nber of parses, structures displayed

        write('Current settings are: '),
        write(Current),
        write('. Modify ? (y/n) '),
        flush_output,
        get_single_char(X),
        ifthenelse((X is 89;X is 121),
                    customize,(nl,nl)),

     write('Input file?  [stdin. or file_name.] --> '),
     read(Input),
     dealwith(Input,HandleIn),
     write('Output file? [file_name.] --> '),
     read(Output),
     concat(Output,'.lst',Outlist),

    !,

     start(Outlist,HandleIn,Input).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SET_DEFAULT: activates default settings
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_default :-
          eraseall(stg),
         recorda(stg,[current,'DEFAULT'],_),

         recorda(stg,[type_of_structures,1]),  % parses are displayed for full S's only
         recorda(stg,[depth,1]),               % shallow
         recorda(stg,[number_of_parses,1]).    % one parse, and a second one if as good as the first


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  CUSTOMIZE : user's choice of settings
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

customize :-
        nl,
        nl,
        flush_output,
        recorded(stg,[current,Current],Key),
        write(' Customizing settings (current is '),
        write(Current),
        write('): choose one the following items...'),
        nl,
        nl,
        write(' 0. - Depth Level for Finite Passes'), nl,
        write(' 1. - Type of Structures Displayed'), nl,
        write(' 2. - Number of Parses Displayed'),nl,
        write(' 3. - Back to default'), nl,nl,
        write(' Enter quit. to quit'), nl, nl,
        flush_output,

        read(X),
        custom(X),
        ifthenelse( X\=quit,
                    (ifthen(recorded(stg,[current,'DEFAULT'],Key), erase(Key)),
                    recorda(stg,[current,'CUSTOM']),
                    customize),
                    true).

custom(quit) :- nl.


custom(0) :-
        write('Customizing Depth Level for Finite Pass '), nl,
        write('Enter 1. for Shallow, 2. for Intermediate, 3. for Deep: '),
        getvalue(depth).


custom(1) :-
        write('Customizing Type of Parses Displayed '), nl,
        write('Enter 1. for Whole Sentences Only, 2. for Phrases and Clauses: '),
        getvalue(type_of_structures).


custom(2) :-
        write('Customizing Number of Parses Displayed '), nl,
        write('Enter 0. for one parse only '), nl,
        write('Enter 1. for one parse only, or two if the second parse has the same ranking as the first '), nl,
        write('Enter 2. for two parses, whatever the ranking of the second '), nl,
        write('Enter 3. for all available parses: '),nl,
        getvalue(number_of_parses).

custom(3) :-
        set_default.


getvalue(X) :-
        read(Value),
        recorded(stg,[X,_],Key),
        erase(Key),
        recorda(stg,[X,Value]).

writesettings(Hout) :-
        recorded(stg,X,_),
        write(Hout,X), tab(Hout,1),
        fail.

writesettings(Hout) :-
        nl(Hout),
        nl(Hout).




% Input is from stdin(user's terminal) or from file
%

   dealwith(stdin,_):- !. % nothing to be done, no file to be opened -
                          % standard input (from the user's terminal)

   dealwith(FileIn,HandleIn):-
                         open(FileIn,read,HandleIn).
                                        % open(File_Name,Mode,Stream)


   start(Outlist,_,stdin) :-
                 open(Outlist,write,Lists),
                 write(Lists,'Settings: '),
                 writesettings(Lists),
     recorda(out,Lists,_),
                 recorda(alltime,0,_),


                 repeat,
                 clear,       % clearing the data base
                 statistics(cputime,TB),
                 recorda(time,TB),              % record starting time
                 nl,
                 nl,
                 write('Key in your sentence or stop. to quit'),
                 nl,
                 getsentence(Sentence,     % list of words corresponding to string
                      user,          % where the string is to be found
                      ".?!/",         % sentence delimiters : final dot / question mark / exclamation mark / slash
                      ",:",          % word delimiters : comma / colon
                      ",:",          % returned punctuation signs : comma / colon
                      nocaps,        % decapitalize or leave alone (nocaps - caps)
                      EndSign),            % EndSign is slash in case of partial structure

                                     % getsentence is also responsible for the v/u distinction to be dropped
                                     % it turns all v's into u's (graviter -> grauiter, etc.)

                ifthenelse(EndSign=slash,

                             ( recorded(stg,[type_of_structures,_],Key),
                               erase(Key),
                               recorda(stg,[type_of_structures,2]) ),

                              ( recorded(stg,[type_of_structures,_],Key),
                               erase(Key),
                               recorda(stg,[type_of_structures,1]) )
                              ),


                 pair(Sentence,_,NS), % if deemed appropriate, a powerful matcher is at hand
                                      % the code is to be found at the very end of this file

                 eraseall(pos),      % clearing database
                 eraseall(fin),
                 deque(NS, Clean),   % getting rid of enclitic -que (and other minor text manipulations)
                 process(Clean,Lists).

   start(Outlist,HandleIn,Input) :-
                 Input \= stdin,
                 open(Outlist,write,Lists),
                 % open(File_Name,Mode,Stream)
                 write(Lists,'Settings: '),
                  writesettings(Lists),
                 recorda(out,Lists,_),
                 nl,
                 recorda(alltime,0,_),
                 repeat,
                 clear,
                 statistics(cputime,TB),
                 recorda(time,TB),
                 nl,
                 getsentence(Sentence,HandleIn,
                               ".?!/",
                               ",:",
                               ",:",
                               nocaps,
                               EndSign),          % see above


                ifthenelse(EndSign=slash,

                             ( recorded(stg,[type_of_structures,_],Key),
                               erase(Key),
                               recorda(stg,[type_of_structures,2]) ),

                              ( recorded(stg,[type_of_structures,_],Key),
                               erase(Key),
                               recorda(stg,[type_of_structures,1]) )
                              ),




     Sentence \= ['%'|_],         % comment line
                              % these lines silently fail
                  % a failure which sends us back to the beginning of
                  % the repeat-fail loop
                 write(Sentence),nl,
                 pair(Sentence,_,NS),         % if deemed appropriate, a powerful matcher is at hand - see above
                 eraseall(pos),
                 eraseall(fin),
                 deque(NS, Clean),
                 process(Clean,Lists).









% start the whole thing
%%%%%%%%%%%%%%%%%%%%%%%%

process(Sentence,Stream) :- positions(Sentence,0,L),
                            nl,
                            nl(Stream),
                            write(L),
                            writeq(Stream,L),
                            nl(Stream),
                            nl,
                            runs.

/* 'positions' computes word positions starting from zero;
the sentence with the word positions fed in (the L list) is then
printed on screen and in the results file

'runs' calls a gamut of run(Pass), where Pass is " stop ",
then " lex "(icon), then (passes making up) " gr "(ammar), then " output "

Each run is supposed to be independent - we do not come back to it (except at the very end of the parsing process);
so when it has done its job we have to make sure
that a run *** never *** requires information that one of the next runs provides

'run(Pass)' recursively calls itself, going through all
the 'rule_name ---> rule_body' pairs for that pass

when no new info is found, it fails,
because all the clauses for '--->' have a condition
(a 'not recorded' check included in the definition of
the predicate 'map') which will lead to failure

before failing, the last pass, run(output), gets a chance to retry everything and then print
(once each) one or more parses spanning the whole S

the failing of 'runs' causes backtracking to the repeat goal in 'start';
the db is cleared and the process relaunched with a new S from the user

if the S happens to be 'stop.' the process halts by abortion (abort). */




% computing string positions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% each word in the wordlist is assigned a starting and an ending position
% note that the first word spans position 0 to position 1

positions([Head|Tail],Pos,Listofwords) :-
     ifthen((Head=se;Head=sibi;Head=sui), recorda(flags,active(se)) ),  % trying to reduce time spent on reflexive pronoun binding
                  % use recorda rather than map because there may be more than one SE in the string


     phrase_words(Phrase_words),   % list of phrasewords, i.e. words which belong only to phrases such as 'pili'
                                   % this list is to be carefully updated whenever a new phrase is entered

     bag_words(Bag_words),         % words discarded, so as to improve efficiency - they are supposed not to interfere with the analysis process

     ifthenelse( ((lex(Head,_,_), NewHead=Head, not(member(Head,Bag_words)) );
                           % the word is known and is to be entered as such

                  (member(Head,Phrase_words), NewHead=Head) ;
                     % OR the word is a member of the list of phrase words

                   (atom_concat(First1,ne,Head),         % OR we have 'ne' attached to a word registered as such e.g. 'romamne'
                    lex(First1,_,_), First1\=non,        % we keep particle 'nonne' as a single lex
                    NewHead=First1,Part=yes);          % we keep track of the fact that there WAS a particle attached


                  % amauisti > amasti   /   deleueram > deleram  /  audiueram > audieram
                  % the contracted form gets divided, ui/ue/u are restored, and we check that the proposed expanded variant is
                  % a licit variant, i.e. generated by a 'makelex' macro-clause as the first arg of a lex clause

                  % e.g. amasti : a u masti  NO   a ui masti NO a ue masti NO
                  %               am u asti  NO   am ui asti NO am ue asti NO
                  %               ama u sti  NO   ama ui sti YES > amauisti
                  % lex(amauisti, v, [pos:v, class:tr_cod, type:finite, lex:amare, voice:act, txt:amauisti, tense:perfect, kind:_,
                  %      mood:indicative, number:sing, person:2]).

                  (atom_concat(First,Second,Head),            % only Head is known - atom divisions are put forward by atom_concat
                      (atom_concat(First,u,FirstExpand) ;
                       atom_concat(First,ui,FirstExpand);
                       atom_concat(First,ue,FirstExpand) ),  % contracted pft
                   atom_concat(FirstExpand,Second,Known),
                   lex(Known,v,_),                          % the full form is registered as a verb form
                   NewHead=Known);
                       % OR the word is a contracted form of a verb; we register the non-contracted form


                   (atom_concat(First1,Second1,Head),           % isolating the prefix before assimilation (adsequor : ads+equor)
                    prefix_derive(First1,Derived),    % retrieving assimilated prefix : prefix_derive(ads,ass).
                    atom_concat(Derived,Second1,Known1),  % building up the assimilated variant recorded in the lexicon (ass+equor)
                %   lex(Known1,v,_),                             % the full form is registered as a verb form - no, not necessarily : inritus, irritus
                   NewHead=Known1);




                            % OR the word bears a suffix with established meaning and function:
                            % we register the suffix in a suffix box which keeps track of the position of the word
                            % to which the suffix is attached (Pos,Posand1)
                            % the suffix must be attached to a registered word - otherwise we let it go
                            % the suffixes concerned are:
                            % cumque / nam / piam / dam / uis / libet / quisque / que  ?????????
                            % code to be added here !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!







                   (Head=stop,NewHead=Head)
                       % OR the word is the word Stop
                ),   % end of IF-clause
                       % we register only known words (lex items or phrase members) or the stop. command

                               (Posand1 is Pos + 1,                                     % beginning of THEN-clause
                                ifthenelse(var(Part),         % no particle detected (no attached 'ne')

                                             (recorda(pos,position(Pos,Posand1,NewHead),_),
                                             Next=Posand1,Listofwords=[Pos/NewHead|Tails]),  % THEN only one word added to the word list

                                            (Next is Posand1 +1,                 % OR ELSE
                                            recorda(pos,position(Pos,Posand1,ne_int),_),     % record the particle
                                            recorda(pos,position(Posand1,Next,NewHead),_),   % record the word
                                            Listofwords=[Pos/ne_int,Posand1/NewHead|Tails]   % add both particle and word to the word list
                                            )),

                                            % we record the word, expanded if necessary


                                positions(Tail,Next,Tails)),                             % we start from Next, which is either Posand1 or
                       % succ(Posand1)

                                (positions(Tail,Pos,Tails), Listofwords=Tails)
                  % MAIN ELSE : word is unmapped or member of bag of discarded words
                                                                        % Listofwords is not affected

                          ).




    % position of the last word indicates string end

     positions([],X,[endpos(X)]) :-
                 recorda(fin,fin(X),_).
                                        % the 'fin' box records the end positition of the string,
          % which will be used by the 'path' procedure to ensure
          % that the proposed parse covers the whole string









%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% meta-interpreter
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% runs predicate : ALP works its way down the various definitions of runs, each of them being a run(X)
% be careful to note the difference between predicate *** runs *** and the various *** run ***  predicates

% Stopping...

runs :- run(stop). % will only succeed on 'stop' as input (i.e. sentence to be parsed)


% vocabulary

runs :- run(lex).     % words and fixed phrases (in ius, in dubium)


% GRAMMAR RUNS

% adjective phrases and verbs

runs :- run(adv).
runs :- run(adj).
runs :- run(verb).

% nps

runs :- run(core).  % core nps

                    % full nps, i.e. with args or genitives attached or nps with relative clauses
                    % must be dealt with later, in the 'finite' run
                    % the information is simply not available yet...


% coordinated PHRASES (not CLAUSES)

runs :- run(coord).


% clauses and complex nps and pps (which can hold nps with finite rel clauses...)
% sentence level : nonfinite and  finite
% most of the grammatical parsing occurs at this level

runs :- ifthen( recorded(stg,[depth,1]),
                                        run(finite,shallow)),
        ifthen( recorded(stg,[depth,2]),
                                        run(finite,inbetween)),
        ifthen( recorded(stg,[depth,3]),
           run(finite,deep)).


% coordinated CLAUSES
% fired only if coordination of phrases has ***not*** been able to put the conjuncts together

runs :- run(coord2).



% Outputting results

runs :- run(output).





% THE RUNS
%%%%%%%%%%

% general def for all run except three: output, finite and coord2

run(Pass) :- Pass \= output, Pass \= finite, Pass \= coord2,
                            % Pass \= output     THIS CHECK TO BE REMOVED BEFORE USING THE DEBUGGING OUTPUT PROCEDURES !!!
             [Pass,_] ---> Condition_Action,
             testexec(Condition_Action),
             run(Pass).

% run(Pass) will lead to failure (no new info) and pass control to the next run





% SPECIAL RUN

% COORD 2 : run twice

run(coord2) :- not(recorded(flags,nocoord2)),
               [coord2,_]  ---> Condition_Action,
                testexec(Condition_Action),
                runstop(coord2).


runstop(coord2) :- [coord2,_] ---> Condition_Action,
             testexec(Condition_Action),
             fail.


% DEEP FINITE: runs recursively, as do the simple def of run (lexis and simple phrase building)
% here the recursivity can prove dreadfully expensive


run(finite,deep) :- [finite,_] ---> Condition_Action,
             testexec(Condition_Action),
             run(finite,deep).



% the other finite runs are limited in nber - as few as possible !!!


% SHALLOW

run(finite,shallow) :- [finite,_] ---> Condition_Action,
             testexec(Condition_Action),
             runstop(finite2).

runstop(finite2) :- [finite,_] ---> Condition_Action,
             testexec(Condition_Action),
             fail.


% INTERMEDIATE


run(finite,inbetween) :- [finite,_] ---> Condition_Action,
             testexec(Condition_Action),
             run(finite2).

run(finite2) :- [finite,_] ---> Condition_Action,
             testexec(Condition_Action),
             run(finite3).


run(finite3) :- [finite,_] ---> Condition_Action,
             testexec(Condition_Action),
             run(finite4).

run(finite4) :- [finite,_] ---> Condition_Action,
             testexec(Condition_Action),
             run(finite5).

run(finite5) :- [finite,_] ---> Condition_Action,
             testexec(Condition_Action),
             fail.




% OUTPUT %


run(output) :-  [_,_] ---> Condition_Action,       % one last run through everything : necessary
             testexec(Condition_Action),
             fail.



%  output needs to be executed only once, and then fail to allow
% the processing of the following S

% the [Pass,Rule_name] part does not give rise to any action ;
% it is there only for guiding the process (through Pass)
% and for documentation and debugging purposes

testexec([]).
testexec([First|Rest]) :-
                       call(First),
                       testexec(Rest).

% testexec is simply a list traversal with execution
% of each element as a Prolog goal



%%%%%%%%%%
% LEXICON
%%%%%%%%%%

% An awful amount of morphological generation is involved here.
% It should be realised that while a parser is being developped, tested and debugged,
% it is far better to have an EXACT account of the information associated with each morphological variant
% and to be able to modify at will the information carried by each variant.

% The vocabulary file (called 'vocfile'),
% once sorted, gives us a very simple way of inspecting that information
% for any wordform the parser works with.

% Overgeneration is a ***sin***, comparable to allowing the grammar to parse ungrammatical strings,
% leads to spurious ambiguities and increases the cost of the parsing procedure.

% A morphological analyser is better left aside until the parsing procedures
% are felt to be accurate and robust.
% That time is, I'm afraid, not around the corner.

% Sorting the vocfile is achieved by a call to a unix-like 'sort' procedure :
% e.g. 'sort vocfile > alplex.pl'
% alplex, being a Prolog file, can then be examined by Prolog calls
% as well as inspected (as the text file it is) by any textual inspection program.

% The entries below feed the lex predicate.

% The word form (e.g. ambularent) is first arg within the lex structure to facilitate indexing.
% The pos (Part of Speech - v etc.) is arg for the lex predicate
% (and will be the name of the record box where the info
% will be stored when the form is found in the text to be parsed).

% The list of features is copied over from lex to the Box.

% Example:

% the lexical entry:
% verb([v(ambulare,1,ambul,ambulau,ambulat)],intr,std).

% will, thanks to the relevant macro clause, generate forms such as:

/*
lex(ambularent,    % word form
      v,             % box
      [pos:v, class:intr, type:finite, lex:ambulare, voice:act, txt:ambularent,
       tense:imperfect, kind:std, mood:subjunctive, number:pl, person:3]).      % list of features
*/

% The info here will be associated with a wordform found in text (such as 'ambularent' as third word in the string)
% by the lexical pass:

/*
[lex,words] --->[recorded(pos,position(A,B,Word),_), % we have the word form Word in the string, e.g. ambularent
                 lex(Word,Box,FS),                   % we have a lex for the Word (ambularent) to put in the relevant Box (the v Box)
                 map(Box,[from:A,to:B|FS])].         % we store its position there accompanied by the feature list FS (third arg of lex)
                 % the feature list will have features for both word variant and lemma:
                 %  [lex:ambulare, txt:ambularent] is part of the FS
*/








%%%%%%%%%
% STRINGS
%%%%%%%%%

% !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
% the phrase_words list should be carefully kept uptodate
% it should contain all items that do not have a lex entry
% !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

phrase_words([bellique,domi,dubium,
              humi,igni,ius,integro,
              marique,militiaeque,nihil,nil,neque,
              pensi,pili,ruri,sancti,tanti,terra]).

string(phrase,[aqua,et,igni],[lex:aqua_et_igni,w:1]).
string(phrase,[belli,domique],[lex:domi_bellique,w:1, value:time]).
string(phrase,[domi,bellique],[lex:domi_bellique,w:1, value:time]).
string(phrase,[domi,militiaeque],[lex:domi_militiaeque,w:1, value:time]).
string(phrase,[terra,marique],[lex:terra_marique,w:1, value:loc]).
string(phrase,[domi],[lex:domi,w:1,value:loc]).
string(phrase,[humi],[lex:humi,w:1,value:loc]).
string(phrase,[id,temporis],[lex:id_temporis,w:1, value:time]).
string(phrase,[in,dubium],[lex:in_dubium,w:1]).
string(phrase,[in,ius],[lex:in_ius,w:1]).
string(phrase,[de,integro],[lex:de_integro,w:1,value:manner_means]).
string(phrase,[militiae,domique],[lex:domi_militiaeque,w:1, value:time]).
string(phrase,[nihil,pensi,neque,sancti],[lex:nihil_pensi_neque_sancti,w:1]).
string(phrase,[nil,pensi,neque,sancti],[lex:nihil_pensi_neque_sancti,w:1]).
string(phrase,[pili],[lex:pili,w:1]).
string(phrase,[tanti],[lex:tanti,w:1]).
string(phrase,[ruri],[lex:ruri,w:1,value:loc]).



%%%%%%%%%%%
% BAG_WORDS
%%%%%%%%%%%

% !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

% bag_words: words temporarily discarded to increase efficiency
% they are supposed not to impinge on structure

bag_words([autem,
           certe,
           denique,
           edepol,eheu,enim,enimuero,equidem,ergo,etiam,
           hercle,heu,
           igitur,itaque,item,
           mehercule,mehercle,
           nam,namque,nempe,nimirum,
           o,
           pol,profecto,
           quare,quidem,quippe,quoque,
           scilicet,
           tamen,
           uidelicet]).

% !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

%%%%%%%%%%
% DB UTILS
%%%%%%%%%%

% The internal Prolog db (accessed by 'record', 'recorded' and 'erase') can be used as blackboard
% recording the information pertaining to the sentence being parsed,
% as opposed to the 'assert/retract' accessible db, which has the same lifespan as the program itself.


map(Box,FS) :- not(recorded(Box,FS,_)), recorda(Box,FS,_).  % the not recorded is essential !!!!!
                                                            % for keeping track of a 'no new solution' situation
                  % it enables the various runs to come to an end

mapped(Box,X) :- recorded(Box,X,_).




%%%%%%%%%%%%%%%
% Stopping ...
%%%%%%%%%%%%%%%

[stop,halting] ---> [recorded(pos,position(0,1,stop),_),   % this happens when the word stop. is entered
                     recorded(out,Lists,Ref),
                     recorded(alltime,Time,Reftime),
                     nl(Lists),
                     write(Lists,'TOTAL TIME : '),
                     write(Lists,Time),
                     nl(Lists),
                     nl,
                     write('TOTAL TIME : '),
                     write(Time),
                     nl,
                     close(Lists),
                     erase(Ref),
                     erase(Reftime),
                     noprotocol,
                     abort].

% abort returns to Prolog...



% LEXICAL PREDICATES
%%%%%%%%%%%%%%%%%%%%


% specific entries

[lex,necesse] --->[recorded(pos,position(A,B,necesse),_),
                   map(adj,[from:A,to:B,lex:necesse,txt:necesse,type:std,pos:adj,case:or([nom,acc]),
                             gender:neuter, number:sing, degree:pos])].

[lex,propterea_quod] --->[recorded(pos,position(A,B,propterea),_),
                          recorded(pos,position(B,C,quod),_),
                          map(sub,[from:A,to:C,lex:propterea_quod,pos:sub,argbound:no,mood:_,value:reason])].


[lex,eo_ipso_quod] --->[recorded(pos,position(A,B,eo),_),
                          recorded(pos,position(B,C,ipso),_),
                          recorded(pos,position(C,D,quod),_),
                          map(sub,[from:A,to:D,lex:eo_ipso_quod,pos:sub,argbound:no,mood:_,value:reason])].

[lex,nonnumquam] --->[recorded(pos,position(A,B,non),_),
                          recorded(pos,position(B,C,numquam),_),
                          map(sub,[from:A,to:C,lex:nonnumquam,pos:adv,type:vpbound,sem:time])].


[lex,cum_primum] --->[recorded(pos,position(A,B,cum),_),
                          recorded(pos,position(B,C,primum),_),
                          map(sub,[from:A,to:C,lex:cum_primum,pos:sub,argbound:no,mood:indicative,value:time])].

[lex,ubi_primum] --->[recorded(pos,position(A,B,ubi),_),
                          recorded(pos,position(B,C,primum),_),
                          map(sub,[from:A,to:C,lex:ubi_primum,pos:sub,argbound:no,mood:indicative,value:time])].

[lex,ut_primum] --->[recorded(pos,position(A,B,ut),_),
                          recorded(pos,position(B,C,primum),_),
                          map(sub,[from:A,to:C,lex:ut_primum,pos:sub,argbound:no,mood:indicative,value:time])].


[lex,cum_iam] --->[recorded(pos,position(A,B,cum),_),
                          recorded(pos,position(B,C,iam),_),
                          map(sub,[from:A,to:C,lex:cum_iam,pos:sub,argbound:no,mood:indicative,value:time])].

[lex,ut_iam] --->[recorded(pos,position(A,B,ut),_),
                          recorded(pos,position(B,C,iam),_),
                          map(sub,[from:A,to:C,lex:ut_iam,pos:sub,argbound:no,mood:indicative,value:time])].

[lex,ubi_iam] --->[recorded(pos,position(A,B,ubi),_),
                          recorded(pos,position(B,C,iam),_),
                          map(sub,[from:A,to:C,lex:ubi_iam,pos:sub,argbound:no,mood:indicative,value:time])].


[lex,simul_ac] --->[recorded(pos,position(A,B,simul),_),
                          recorded(pos,position(B,C,ac),_),
                          map(sub,[from:A,to:C,lex:simul_ac,pos:sub,argbound:no,mood:indicative,value:time])].
[lex,simul_ac] --->[recorded(pos,position(A,B,simul),_),
                          recorded(pos,position(B,C,ac),_),
                          map(coord,[from:A,to:C,lex:simul_ac,pos:coord])].

[lex,simul_atque] --->[recorded(pos,position(A,B,simul),_),
                          recorded(pos,position(B,C,atque),_),
                          map(sub,[from:A,to:C,lex:simul_atque,pos:sub,argbound:no,mood:indicative,value:time])].

[lex,ac_si] --->[recorded(pos,position(A,B,ac),_),
                          recorded(pos,position(B,C,si),_),
                          map(sub,[from:A,to:C,lex:ac_si,pos:sub,argbound:no,mood:subjunctive,value:reason])].

[lex,tamquam_si] --->[recorded(pos,position(A,B,tamquam),_),
                          recorded(pos,position(B,C,si),_),
                          map(sub,[from:A,to:C,lex:tamquam_si,pos:sub,argbound:no,mood:subjunctive,value:reason])].

[lex,ut_si] --->[recorded(pos,position(A,B,ut),_),
                          recorded(pos,position(B,C,si),_),
                          map(sub,[from:A,to:C,lex:ut_si,pos:sub,argbound:no,mood:subjunctive,value:reason])].

[lex,uelut_si] --->[recorded(pos,position(A,B,uelut),_),
                          recorded(pos,position(B,C,si),_),
                          map(sub,[from:A,to:C,lex:uelut_si,pos:sub,argbound:no,mood:subjunctive,value:reason])].

[lex,haud_facile] --->[recorded(pos,position(A,B,haud),_),
                          recorded(pos,position(B,C,facile),_),
                          map(adv,[from:A,to:C,lex:haud_facile,pos:adv,type:clausal,sem:discourse])].

[lex,satis_facere] --->[recorded(pos,position(A,B,satis),_),
                          recorded(pos,position(B,C,Facere),_),
                          lex(Facere,v,FS),
                          pick(lex:facere,FS,FS1),
                          pick(txt:Txt,FS1,FS2),
                          atom_concat(satis,Facere,Satisfacere),
                          append([txt:Satisfacere,lex:satisfacere],FS2,FSNew),
                          map(v,[from:A,to:C | FSNew])].





% the comma is read as a possible coordinator if it is followed by ET as the next word but one or the next word but two
% filius, mater et pater
% filius bonus, mater bona et pater

% VERY SKETCHY
% COORDINATION EXPENSIVE AND TO BE MAINTAINED WITHIN STRICT BOUNDS

/*
[lex,comma_as_coord] --->[recorded(pos,position(A,B,','),_),
                          (Next is B+1; Next is B+2),
                          recorded(pos,position(Next,_,et),_),
                          map(coord,[from:A,to:B,lex:comma,pos:coord])].

*/

% extensive and expensive !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

[lex,comma_as_coord] --->[recorded(pos,position(A,B,','),_),
                          map(coord,[from:A,to:B,lex:comma,pos:coord])].



[lex,words] --->[recorded(pos,position(A,B,Word),_),
                 lex(Word,Box,FS),
                 map(Box,[from:A,to:B|FS])].

/* a record (a feature bundle) is added to the db in the box corresponding to the lexical item's POS.
   The start and end positions of the item in the S are recorded in the first two features; the remaining features
   are read off the lexicon */



[lex,strings] --->[recorded(pos,position(A,B,Word),_),
          % we have the first word of the string
                    string(Box,[Word|OtherWords],FeatureList),
                    match(B,C,OtherWords),
          % we attempt to match all the others
                    append([pathlist:[p(A,B),p(B,C)],txt:[Word|OtherWords]],FeatureList,FullFeatureList),
                    map(Box,FullFeatureList)].
          % we pass the whole list as value for the txt feature

match(BeginPos,EndPos,[W|Ws]) :-
        recorded(pos,position(BeginPos,NextPos,W),_),
        match(NextPos,EndPos,Ws).

match(EndPos,EndPos,[]).






% PARSE FLAG SETTINGS
%%%%%%%%%%%%%%%%%%%%%%

% These flags are meant to speed up the parsing process -
% we flag the rules dealing with gaps (in relative clauses),
% coordination and nonfinite clauses.

% another flag is set for the SE family right at input (in the process turning string into wordlist)

[lex,flags] ---> [mapped(relative,_),     % we have a relative pronoun somewhere in the S
                  map(flags,active(gap))].

[lex,flags] ---> [mapped(v,FSverb),     % we have a nonfinite verb form (infinitive) somewhere in the S
                  constraint([type:nonfinite],FSverb),
                  map(flags,active(nonfinite))].

% the following clauses are necessary because ESSE can be understood : amatum, dicturum, delendam,...

[lex,flags] ---> [mapped(p_p,_),      % we have a nonfinite verb form (past participle) somewhere in the S
                  map(flags,active(nonfinite))].

[lex,flags] ---> [mapped(p_f,_),      % we have a nonfinite verb form (future participle) somewhere in the S
                  map(flags,active(nonfinite))].


[lex,flags] ---> [mapped(gdiv,_),     % we have a nonfinite verb form (gerundive) somewhere in the S
                  map(flags,active(nonfinite))].


















%%%%%%%%%%%%%%%%%%%%%%
% GRAMMAR PREDICATES
%%%%%%%%%%%%%%%%%%%%%%


% CLAUSE EXPANSIONS
%%%%%%%%%%%%%%%%%%%


% *****VERY EXPENSIVE**** because not linked to anything in particular
% every time a clause is built, it is ready for expansion
% and so is a clause that has already been expanded

% but we need to allow for pps , adjuncts not further specified, adverbials relating to parts of the predication (graviter angi)
% and advps of the clausal type such as autem or etiam
% and ablative absolute, of course


% The full expansion set should be reserved for finite clauses (the ablative absolute should not be thought as a normal expansion
% for a gerundive clause or another ablative absolute), but now we use a single expand process, so the aa IS included

% The expansions are meant to cover grammatical structures that are not arguments,
% i.e. not linked to specific predicates in the lexicon.

% It is therefore not surprising that all these elements should be optional.


full_expansions([ adverbial_adjunct:[type:advp,    constraints:[sem:or([discourse,duration,eval,manner,manner_means,place,reason,time,when])]],
                  prep_phrase_adjunct_1:[type:pp,  constraints:[prep:or([ante,apud,causa,circum,contra,cum,de,ex,inter,intra, iniussu])]],
      prep_phrase_adjunct_2:[type:pp,  constraints:[prep:or([iussu,ob,per,praeter,pro,propter,sine,specie,sub,trans])]],
                  prep_phrase_adjunct_3:[type:pp,  constraints:[prep:in, case:abl]],
                  adjunct:[type:adjunct,           constraints:[value:or([time,place])]],
                  adjunct:[type:adjunct,           constraints:[value:or([purpose,manner_means])]],
                  dative_of_interest:[type:np,     constraints:[case:dat,lex:or([pp1sg,pp2sg,pp1pl,pp2pl])]],
                  ablative_absolute:[type:aa,      constraints:[]] ,
                  address:[type:address, constraints:[]]   ],
                  finite).

% 'address' for finite clauses only : ***iudices*** etc.



full_expansions([ adverbial_adjunct:[type:advp,    constraints:[sem:or([discourse,duration,eval,manner,manner_means,place,reason,time,when])]],
                  prep_phrase_adjunct_1:[type:pp,  constraints:[prep:or([ante,apud,causa,circum,contra,cum,de,ex,inter,intra, iniussu])]],
      prep_phrase_adjunct_2:[type:pp,  constraints:[prep:or([iussu,ob,per,praeter,pro,propter,sine,specie,sub,trans])]],
                  prep_phrase_adjunct_3:[type:pp,  constraints:[prep:in, case:abl]],
                  adjunct:[type:adjunct,           constraints:[value:or([time,place])]],
                  adjunct:[type:adjunct,           constraints:[value:or([purpose,manner_means])]],
                  dative_of_interest:[type:np,     constraints:[case:dat,lex:or([pp1sg,pp2sg,pp1pl,pp2pl])]],
                  ablative_absolute:[type:aa,      constraints:[]] ]).

% we need to have the three prep_phrase_adjuncts to account for more than one pp with adjunct status in a single S

partial_expansions([]). % dummy NOT USED




% A FEW REMARKS BEFORE PLUNGING...
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Let's make clear, at the outset of this grammar section, what the 'pathlist', 'distance' and 'w(eight)'
% features are meant to convey.

% 'pathlist' has as value a list of 'p(X,Y)' structures, where X and Y are string positions,
% each one of these structures covering a portion of the input string.
% The pathlist will be used to ensure that the whole string is covered by the proposed parse (when all local pathlists are
% appended into one long one); it is also used to establish adjacency, contiguity, and other path properties that the
% parser needs to know about, partly in order to work out the value of the next feature discussed here,

% namely 'distance' - distance is computed on the basis of the distance between the members of a grammatical structure,
% if such a distance is felt to be a possible cause of straining, i.e. the stylistically felt non-contiguity that might
% lead to the downscaling of the likelihood of a given parse being the correct one.
% Distances are cumulated and yield the 'straining factor' for the input string under a given parse.

% The 'weight' feature works in the opposite direction - it makes a parse more likely to be adopted, by specifying the
% coherence factor that the belonging of a structure to a whole brings to that whole. It is easy to understand that
% the final weight given to a parse (supposed to be the degree of confidence it inspires of being right) will be computed on
% the basis of subtracting the straining factor from the weight accumulated by the individual weights of the constituents.

% Let's also look at the 'constraint' and 'map' predicates, which are used throughout the parsing process.
% 'constraint' is a two-place predicate that takes as arguments feature lists -
% it can be used to retrieve feature values or to impose them.

% The very first time we meet it here, it is used to retrieve feature values such as that for the feature 'lex';
% such features are supposed to be part of the lexical entry for the adj, which appears here as the FS variable
% (feature structure).

% We check the availability of the FS structure by means of the 'mapped' predicate, which is just sugar for 'recorded',
% the Prolog predicate that retrieves info stored on the fly, here the info associated with a wordform occurring in
% the string at a certain position (between A and B we are looking for an adj - note that A and B are free variables at
% this stage - it is the call to 'mapped' that is going to instantiate them and yield (if it is available!) the feature bundle
% associated with the adj. Note that 'adj', the part of speech, is the name of the record 'box' or 'folder' where we store
% information pertaining to adjectives.

% These boxes can also be created by the grammar - they are not necessarily lexical.
% As a matter of fact, each of the grammar 'rules' of the parser ends up by storing a new structure in its own box,
% so that this parser pass, or a following one, has access to it.

% This creating of a new structure to be stored in its own box is the responsibility of the 'map' predicate -
% 'map' is essentially sugar for 'recorda' (used in Prolog to store info on the fly, info to be retrieved
% by the 'recorded' predicate mentioned just above) but with an important proviso -
% the info is NOT stored if it is already there in the box.

% This procedure ensures control of the parsing process, because for each pass, the parsing process is restarted,
% until it fails because it cannot add anything new -
% the control on this 'not adding anything new' is precisely the one built in in 'map', which does not
% succeed if it doesn't have anything new to store.


% Let's start
















%%%%%%%%%%%%%%%%%%%
% ADJECTIVE PHRASES
%%%%%%%%%%%%%%%%%%%

% standard adj (bonae)
%%%%%%%%%%%%%%%%%%%%%%


% lex(bonae, adj, [pos:adj, txt:bonae, lex:bonus, type:std, case:dat, gender:fem, number:sing, degree:pos]).


[adj,adjp1] --->
[mapped(adj,[from:A,to:B|FS]),
 constraint([case:C, number:N, gender:G, type:Type, lex:Lex],FS),
 map(adjp,[cat:adjp,pathlist:[p(A,B)],distance:[0],hp:[p(A,B)],
             case:C,number:N, gender:G,lex:Lex,type:Type,w:1,constituent_structure:Lex])].


% fortunatos nimium
%%%%%%%%%%%%%%%%%%%

% based on the recognition of adverbs that are liable to be bound to adj
% lex(prope,    adv,[lex:prope,      pos:adv,type:adjbound, sem:eval]).

 [adj,adjp2a] --->
 [mapped(adj,[from:A,to:B|FSadj]),
 constraint([case:Case, number:N, gender:G,type:Typeadj, lex:Lexadj],FSadj),

 mapped(adv,[from:B,to:C|FSadv]),
 constraint([type:adjbound, lex:Lexadv, sem:Sem],FSadv),

 map(adjp,[cat:adjp,pathlist:[p(A,B),p(B,C)],distance:[0],hp:[p(A,B)],
             case:Case,number:N, gender:G,lex:Lexadj,type:Typeadj,w:1,constituent_structure:[Lexadv,Lexadj]])].


[adj,adjp2b] --->
 [mapped(adj,[from:B,to:C|FSadj]),
 constraint([case:Case, number:N, gender:G,type:Typeadj, lex:Lexadj],FSadj),

 mapped(adv,[from:A,to:B|FSadv]),
 constraint([type:adjbound, lex:Lexadv, sem:Sem],FSadv),

 map(adjp,[cat:adjp,pathlist:[p(A,B),p(B,C)],distance:[0],hp:[p(B,C)],
             case:Case,number:N, gender:G,lex:Lexadj,type:Typeadj,w:1,constituent_structure:[Lexadv,Lexadj]])].



% tool adj + adjp
%%%%%%%%%%%%%%%%%

% hic bonus cives / uterque Germanicus exercitus
% lex(isti, adj, [pos:adj, txt:isti, lex:iste,gender:masc,type:tool,case:nom, number:pl]).

 [adj,adjp3] --->

 [mapped(adj,[from:A,to:B|FS]),
  constraint([type:or([tool,poss]),lex:LexTOOL, gender:G,case:C,number:N],FS),
  mapped(adjp,ADJP),
  constraint([pathlist:PADJ,distance:[0],hp:HP,case:C,number:N,gender:G,
             lex:LexADJ,type:Type,w:1], ADJP),
 not(Type=tool),                                   % the second adj should be standard
 not(Type=poss),
 msort(PADJ,PADJsorted),                 % ALP 194
 distance([p(A,B)],PADJsorted,Dis),
 append([p(A,B)],PADJsorted,PADJ2),
 map(adjp,[cat:adjp,pathlist:PADJ2,distance:[Dis],hp:HP,
             case:C,number:N, gender:G,lex:LexADJ,type:Type,w:2,constituent_structure:[LexTOOL,LexADJ]])].


% comparative adj with ablative comp_cplt
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ... doctior Petro
% lex(doctior, adj, [pos:adj, txt:doctior, lex:doctus, type:std, case:nom, gender:masc, number:sing, degree:comp]).

[core,adjp4] --->
[mapped(adj,[from:A,to:B|FS]),
 constraint([case:Case, number:N, gender:G, type:Type, lex:Lex, degree:comp],FS),
            % comparative degree needed
 constraint([case:or([nom,acc])],[case:Case]),
            % ablative after comparative restricted to nominative or accusative adjective
 mapped(np,FSnp),
 constraint([pathlist:PL1,hp:HL1,distance:Distance,w:W,case:abl],FSnp),  % we need an ablative np
  HL1 \= [],                      % avoid dummy nps
                                  % this is one way of catching them - they do not cover head string position
 append([p(A,B)],PL1,Path),
 msort(Path,Sorted),             % a useful duo : sorting the path and making sure it does not feature duplicates
 \+ dup(Sorted),
 % Weight is W+1,     % weight is derived from np and increased to account for the presence of the adjective
 myplus(W,1,Weight),
 map(adjp,[cat:adjp,pathlist:Sorted,distance:Distance,hp:[p(A,B)],
             case:Case,number:N, gender:G,degree:comp,lex:Lex,type:Type,w:Weight,
             constituent_structure:[Lex,comp_cplt:FSnp]])].

 % here we see the introduction of a constituent_structure(constituent structure) feature to record info to be displayed in the parse tree
 % we note that adjective phrases (and also noun phrases) are sometimes assigned a constituent_structure feature, and sometimes not
 % if we wish to retrieve info from the value of that feature, we will have to take care of the possibility of the feature
 % not being present in the info box associated with the phrase



% comparative adj with comp_cplt introduced by quam
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% - note how the presence of QUAM slows down the parsing process
% Quam is very ambiguous, and can also be a coordinator as the second member of a correlative pair (tam ... quam, magis ... quam))

% ... doctior quam Petrus

[core,adjp5] --->
[mapped(adj,[from:A,to:B|FS]),
 constraint([case:Case, number:N, gender:G, type:Type, lex:Lex, degree:comp],FS),

 mapped(adj,[from:C,to:D|FSadj]),  % quam is registered as an adj
 constraint([txt:quam],FSadj),      % we insist on having 'quam', nothing else

 mapped(np,FSnp),
 constraint([pathlist:PL1,hp:HL1,distance:Distance,w:W,case:Case],FSnp),
 HL1 \= [],                      % avoid dummy nps
 start(PL1,D),       % np directly follows quam
         % for such checks we use the 'start' and 'extremity' predicates
 append([[p(A,B)],[p(C,D)]],PL1,Path),  % the quam needs to enter the path, too
 msort(Path,Sorted),
 \+ dup(Sorted),
 % Weight is W+1,
 myplus(W,1,Weight),
 map(adjp,[cat:adjp,pathlist:Sorted,distance:Distance,hp:[p(A,B)],
             case:Case,number:N, gender:G,degree:comp,lex:Lex,type:Type,w:Weight,
             constituent_structure:[Lex,comp_cplt:FSnp]])].




% ... doctior quam ut...

[finite,adjp5a] --->
[mapped(adj,[from:A,to:B|FS]),
 constraint([case:Case, number:N, gender:G, type:Type, lex:Lex, degree:comp],FS),

 mapped(adj,[from:B,to:C|FSadj]),  % quam is registered as an adj
 constraint([txt:quam],FSadj),      % we insist on having 'quam', nothing else

 mapped(sub,[from:C,to:D|Fsubordinator]),        % subordinatOR
 constraint([argbound:no,lex:ut,mood:subjunctive],Fsubordinator),

 mapped(pred,SubClause),
 constraint([type:finite,class:m,pathlist:PLSub,distance:Dis, gap:[],

             mood:subjunctive, tense:TenseSub,w:W1,constituent_structure:HeadSub],SubClause),

 start(PLSub,D),       % pred directly follows quam
                 % for such checks we use the 'start' and 'extremity' predicates
 append([[p(A,B)],[p(B,C),p(C,D)]],PLSub,Path),  % the quam needs to enter the path, too
 msort(Path,Sorted),
 \+ dup(Sorted),
 % Weight is W1+20,
 myplus(W1,20,Weight),            % needs heavy weight to counteract reading of ut+subj as
                % independent subclause
 map(adjp,[cat:adjp,pathlist:Sorted,distance:Dis,hp:[p(A,B)],
             case:Case,number:N, gender:G,degree:comp,lex:Lex,type:Type,w:Weight,
             constituent_structure:[Lex,comp_cplt:[ut,HeadSub]]])].



% superlative with genitive sup_cplt
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Omnium fortissimi sunt Belgae.
% lex(fortissimi, adj, [pos:adj, txt:fortissimi, lex:fortis, type:std, case:gen, gender:masc, number:sing, degree:sup]).


[core,adjp6] --->
[mapped(adj,[from:A,to:B|FS]),
 constraint([case:Case, number:N, gender:G, type:Type, lex:Lex, degree:sup],FS), % we have a superlative
 mapped(np,FSnp),
 constraint([pathlist:PL1,hp:HL1,distance:Distance,w:W,case:gen,number:pl],FSnp),  % genitive case needed, plural nber
 HL1 \= [],                      % avoid dummy nps
 append([p(A,B)],PL1,Path),
 msort(Path,Sorted),
 \+ dup(Sorted),
 % Weight is W+1,
 myplus(W,1,Weight),
 map(adjp,[cat:adjp,pathlist:Sorted,distance:Distance,hp:[p(A,B)],
             case:Case,number:N, gender:G,degree:sup,lex:Lex,type:Type,w:Weight,
             constituent_structure:[Lex,sup_cplt:FSnp]])].




% the three categories below were meant to apply to gerundives, present and future participles used alone,
% without their arg structure  (and therefore reducible to adjective status ?????? - a hoary issue...)

% gerundives
%%%%%%%%%%%%

% delenda
% lex(delenda, gdiv, [pos:gdiv, txt:delenda, case:nom, gender:fem, number:sing,
%                     lex:delere, class:tr_cod, type:gdiv, kind:std, mood:gerund, person:3]).



[adj,adjp7] --->
[ mapped(gdiv,[from:A,to:B|FS]),
        % gdiv gerundives (NOT gerund)
 constraint([case:C, number:N, gender:G, txt:Txt],FS),
 \+constraint([constituent_structure:_],FS), % the gdiv is not coordinate !

 map(adjp,[cat:adjp,pathlist:[p(A,B)],distance:[0],hp:[p(A,B)],
             case:C,number:N, gender:G,type:gdiv,lex:Txt,w:1,constituent_structure:Txt]) ].



% present participle
%%%%%%%%%%%%%%%%%%%%

% recubans
% lex(recubans, p_pr, [pos:p_pr, txt:recubans, case:nom, gender:_, number:sing,
%                      lex:recubare, class:intr, type:p_pr, kind:std, mood:participle, person:3]).


/*
[adj,adjp8] --->
[ mapped(p_pr,[from:A,to:B|FS]),
        % present participle p_pr
 constraint([case:C, number:N, gender:G, lex:Lex],FS),
 map(adjp,[cat:adjp,pathlist:[p(A,B)],distance:[0],hp:[p(A,B)],
             case:C,number:N, gender:G,lex:Lex,type:ppr,w:1,
             constituent_structure:[head:FS]])].

*/

% Register those, where needed, as simple adjectives, not present participles.
% If they are apt to take arguments, they are dealt with in the 'participial clause' section below.


% future participle
%%%%%%%%%%%%%%%%%%%

% amaturus
% lex(amaturus, p_f, [pos:p_f, txt:amaturus, case:nom, gender:masc, number:sing,
%                     lex:amare, class:tr_cod, type:p_f, kind:std, mood:participle, person:3]).

/*

[adj,adjp9] --->
[ mapped(p_f,[from:A,to:B|FS]),
        % future participle p_f
 constraint([case:C, number:N, gender:G, lex:Lex],FS),
 map(adjp,[cat:adjp,pathlist:[p(A,B)],distance:[0],hp:[p(A,B)],
             case:C,number:N, gender:G,lex:Lex,type:ppf,morph:ppf,w:1])].

*/

% the type and morph features carry the same info, namely that we have to do with a future participle
% the difference being that reference to the type feature is made elsewhere in the grammar,
% but the type feature does not get printed, whereas the morph feature does (but is not referred to elsewhere)



% past participle
%%%%%%%%%%%%%%%%%%

% The past participle needs to be recorded as an adjective - it would not do
% to have to specify the past participles that can be used as adjectives
% as if they were special cases

% why ? !!!!!!!!!!!!!!!!! DOUBTFUL !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
%%%%%%%%%% SEE BELOW %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% but we need to prioritize passive voice when the p_p occurs with a form of esse


% whether we recognize it as an adjective or not, it will still get parsed as a past participle
% and give rise to analysis of the past participle alone as a reduced participial clause

% so it is not a question of whether the pp as adj speeds up the process
% it clearly slows it down !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

% amatus
% lex(amatus, p_p, [pos:p_p, txt:amatus, case:nom, gender:masc, number:sing,
%                   lex:amare, class:tr_cod, type:p_p, kind:std, mood:participle, person:3]).

% Only true adjectives with pp 'look' should be registered as adj. The litmus test is whether they can have args
% If they can, they are NOT adjectives


/*
[adj,adjp10] --->
[ mapped(p_p,[from:A,to:B|FS]),
        % past participle p_p
 constraint([case:C, number:N, gender:G, txt:Txt],FS),
 map(adjp,[cat:adjp,pathlist:[p(A,B)],distance:[0],hp:[p(A,B)],
             case:C,number:N, gender:G,lex:Txt,type:ppt,morph:ppt,w:0])].

% type and morph : see above
% 0 weight as against true passives - which, included in the verb phrase, get their weight there

*/

% arg-bearing adj
%%%%%%%%%%%%%%%%%%


% true adj with arg structure
% ... cupidus pecuniae

% lex(cupidus, adj, [pos:adj, txt:cupidus, lex:cupidus, type:std, case:nom, gender:masc, number:sing, degree:pos]).

% lexarg(cupidus,
%      arg:[type:np,oblig:no,constraints:[case:gen]]).
% lexarg(cupidus,
%      arg:[type:pred, oblig:no,constraints:[local_case:gen,type:gerund]]).


[finite,adjp11] --->
[mapped(adjp,Fadjp),
 constraint([pathlist:PL1,hp:HL1,distance:[Dist1],
            number:Nb,gender:G,type:std,case:C,lex:Lex], Fadjp),

 lexarg(Lex,arg:Arg),
       % connecting with the arglist via lexarg/2

 qm(arg:Arg,arg:Tree,PathlistArg,[DistanceArg],w:Wtot), % QUICK MATCH

 adjacent(PL1,PathlistArg),     % args before or after adj, but contiguous
                                                % this is what the adjacency predicate is for

 % Dis is Dist1+DistanceArg,               % they come from two sources
  myplus(Dist1,DistanceArg,Dis),           % myplus guards against uninstantiated variables, which throw up an error
                                           % if standard 'is' is used
 append(PL1,PathlistArg,Path),
 msort(Path,Sorted),

                % contiguous(Sorted),
     start(Sorted,STS),
     extremity(Sorted,ES),
     between(STS,ES,Here),
     \+mapped(punct,[from:Here,to:There, lex:comma|_]),

 % W is Wtot+3,
 myplus(Wtot,3,W),                     % bonus given to args as opposed to info ending up in non-args, i.e. adjuncts
 map(adjp,[pathlist:Sorted,hp:HL1,distance:[Dis],
           cat:adjp,w:W,type:std,     % complex type to make sure we do not expect it any place an adjective can fit
           number:Nb,gender:G, lex:Lex,
           case:C,constituent_structure:[head:Fadjp,arg:Tree]])].              % the constituent structure is the head adj followed by its argument




% present participle with arg structure inherited from verb
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% ... sub tegmine fagi recubante

[finite,participle_clause1] --->
[mapped(p_pr,[from:A,to:B|FS]),       % present participle
 constraint([number:Nb,gender:G,case:C,lex:Clex], FS),
 lexarg(Clex,arglist:ArgList),
           % connecting with the arglist via lexarg/2

 % selecting the subject to remove it from the arglist:              % the subject is be found in the np
                     % the participial clause gets attached to
   pick(ws(Lex,_,clause:_,mwuw:MW,args:Args), ArgList,_),  % finding the arglist
   pick(subject:_,Args,NewArgs),                             % exploring it


 expand(NewArgs,ExpandTrees,ExpandPaths,ExpandDistances,w:Weight),    % using expand for adj and noun args should prove sufficient
 flatten(ExpandPaths,Flat),
 msort(Flat,Sorted),
  \+dup(Sorted),

      % Sorted \= [],               % not reduced to the present participle alone, which is then best viewed as an adjective
             % see the stance taken above :
                               % although we could simply register as adj, not present participles,
                               % those participles that do not need any arg position filled

      % adjacent([p(A,B)],Sorted),  % args before or after adj, but contiguous / better not use adjacent if Sorted can be []

      % contiguous(Sorted),        % too strict - we may also try 'quasicontiguous'
           %  which allows for one word to get inserted
           %  if we wish to parse Virgil's patulae recubans sub tegmine fagi...
                             % see below contiguity test when the expansions are added
 insort(ExpandTrees,ST),
 flatten(ExpandDistances,ED),
 sum(ED,Dis),
 append([p(A,B)],Sorted,Path),
 % W is Weight+MW+1,
 myplus(Weight,MW,T1),
 myplus(T1,1,W),

 % bonus weight for clause over np
 myplus(W,2,WeightClause),


 full_expansions(FE),
  expandkernel(FE,ExpandT,ExpandP,ExpandD,w:WExpand, _, partial),           % here too weight is computed
                      % and the interrogative can hide in the adjuncts

  ifthenelse(WExpand \= 0, WExpandN is WExpand+1, WExpandN=WExpand),

  flatten(ExpandP,EFlat),
  adjacent(EFlat,Path),
  append(EFlat,Path,AllPaths),

  flatten(ExpandD, FlatD),
  sum(FlatD, DistanceD),
  myplus(DistanceD,Dis,NDistance),

  flatten(AllPaths, FlatAll),
  msort(FlatAll, NSorted),
  \+dup(NSorted),
  contiguous(NSorted),                                % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%


  append(ST,ExpandT,Trees),
  insort(Trees, NST),
  myplus(WeightClause,WExpandN,WWC),
  myplus(W,WExpandN,WW),



map(participle_clause,[pathlist:NSorted,hp:[p(A,B)],distance:[NDistance],
           cat:ppclause,w:WWC,type:pprcentered,
           number:Nb,gender:G, lex:Lex,
           case:C,constituent_structure:[present_participle,Lex,NST]]),


% mapping as np leads to overgeneration
% perhaps OK only if plural and masc/fem ???


 ifthen( (G=masc;G=fem;G=or([masc,fem])),     % only map as NP if masc/fem

           map(np,[pathlist:NSorted,hp:[p(A,B)],
           index:i(p(A,B)),distance:[NDistance],
           cat:np,class:pc2np,sem:[hum],
           number:Nb,person:_,gender:G, type:clausal,lex:Lex,lextype:full,
           case:C,w:WW,
           constituent_structure:[present_participle,Lex,NST]]))    ].




% deponent past participle with arg structure inherited from verb
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% mapped as participle clause only
%---------------------------------

% Rex imperatorem urbem cepisse ratus insanivit.

% in the lexicon we have entries such as:
% lex(ratus,
%      p_p,
%      [pos:p_p, txt:ratus, case:nom, gender:masc, number:sing, lex:reri, class:tr_inf,
%      type:p_p, kind:dep, mood:participle, person:3]).

[finite,participle_clause2] --->
[mapped(p_p,[from:X,to:Y|FS]),               % past participle of a deponent verb

 constraint([case:C, number:Nb, gender:G, lex:Clex, kind:dep],FS),   % we insist on a deponent verb
                     % this is NOT passive voice !

 lexarg(Clex,arglist:ArgList),               % connecting with the arglist via lexarg/2

 % selecting the subject to remove it from the arglist:
   pick(ws(Lex,_,clause:_,mwuw:MW,args:Args), ArgList,_),
   pick(subject:_,Args,NewArgs),

 expand(NewArgs,ExpandTrees,ExpandPaths,ExpandDistances,w:Weight),    % using expand for adj and noun args should prove sufficient
 flatten(ExpandPaths,Flat),
 msort(Flat,Sorted),
 Sorted \= [],                    % not reduced to the participle alone / here the restriction makes sense
 adjacent([p(X,Y)],Sorted),               % args before or after pp, but contiguous
 \+dup(Sorted),
        % contiguous(Sorted),
 insort(ExpandTrees,ST),
 flatten(ExpandDistances,ED),
 sum(ED,Distance),
 append([p(X,Y)],Sorted,Path),


 % W is Weight+MW+1,
 myplus(Weight,MW,T1),
 myplus(T1,1,W),

 full_expansions(FE),
  expandkernel(FE,ExpandT,ExpandP,ExpandD,w:WExpand, _, partial),  % here too weight is computed
                      % and the interrogative can hide in the adjuncts

 ifthenelse(WExpand \= 0, WExpandN is WExpand+1, WExpandN=WExpand),

  flatten(ExpandP,EFlat),
  adjacent(EFlat,Path),
  append(EFlat,Path,AllPaths),

  flatten(ExpandD, FlatD),
  sum(FlatD, DistanceD),
  myplus(DistanceD,Distance,NDistance),

  flatten(AllPaths, FlatAll),
  msort(FlatAll, NSorted),
  \+dup(NSorted),
   contiguous(NSorted),                                   % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%


  append(ST,ExpandT,Trees),
  insort(Trees, NST),
  myplus(W,WExpandN,WW),


 map(participle_clause,[pathlist:NSorted,hp:[p(X,Y)],distance:[NDistance],
           cat:ppclause,w:WW,type:depppcentered,
           number:Nb,gender:G, lex:Lex,
           case:C,constituent_structure:[deponent_p_p,Lex,NST]])].




% standard past participle with arg structure inherited from verb
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% mapped as both participle clause and NP
%----------------------------------------

% Rex a regina amatus epistulas longas ad servam Marci scribit.

% we have entries such as:
% lex(amatus,
%      p_p,
%      [pos:p_p, txt:amatus, case:nom, gender:masc, number:sing, lex:amare, class:tr_cod, type:p_p,
%       kind:std, mood:participle, person:3]).

[finite,participle_clause3] --->
[mapped(p_p,[from:X,to:Y|FS]),                                   % a true past participle involved in passive voice
 constraint([case:C, number:Nb, gender:G, lex:Clex, kind:std],FS),

 lexarg(Clex,arglist:ArgList),
 pick(ws(Lex,_,clause:_,mwuw:0,args:Args), ArgList,_),     % mwu's excluded - see reason below

 % tinkering with the arglist:

 % selecting the object to remove it from the arglist:

 pick(object:_,Args,Args1),  % note that there must be an object if a pp was produced !!
                             % exclude mwus where the object is the reason for idiom building
                             % here all mwus are excluded

 % turning the subject into an optional (a+) abl pp arg

 pick(subject:SubjectSpecs,Args1,Args2),
 pick(constraints:Sconstraints,SubjectSpecs,_),

       % make_arg(_,[type:np,oblig:no,constraints:[case:abl]]).

       % make_arg(Constraints,[type:pp,oblig:no,constraints:[prep:ab,sem:[hum]]]) :- constraint([sem:[hum]],Constraints).

       % make_arg(Sconstraints,NewArg),

 ifthenelse( Sconstraints=[],
                                   % no constraint on subj: both types of agent are OK  IF-CLAUSE
                        ( NewArg=[type:pp,oblig:no,constraints:[prep:ab,sem:[hum]]] ;        % THEN-CLAUSE
                          NewArg=[type:np,oblig:no,constraints:[case:abl]])  ,
                                                                                     % ELSE-CLAUSE:
                                    ifthenelse(   constraint([sem:[hum]],Sconstraints),   % IF2-clause     % there are constraints: we act accordingly
                                                             NewArg=[type:pp,oblig:no,constraints:[prep:ab,sem:[hum]]],    % THEN2-clause
                                                             NewArg=[type:np,oblig:no,constraints:[case:abl]])),          % ELSE2-clause

 append([agent:NewArg],Args2,Args3),   % adding it to the args

 expand(Args3,ExpandTrees,ExpandPaths,ExpandDistances,w:Weight),    % using expand for adj and noun args should prove sufficient
 flatten(ExpandPaths,Flat),
 msort(Flat,Sorted),
 \+dup(Sorted),

           % Sorted \= [],               % not reduced to the past participle alone, which is then best viewed as an adjective : doubtful !!!
     % adjacent([p(X,Y)],Sorted),  % args before or after adj, but contiguous - contiguity test better if arglist is allowed to be empty

           % contiguous(Sorted),

insort(ExpandTrees,ST),
 flatten(ExpandDistances,ED),
 sum(ED,Distance),
 append([p(X,Y)],Sorted,Path),
 % W is Weight+MW+1,
 myplus(Weight,0,W),        % no MWU - preferable since object is left aside
% myplus(T1,1,W),

 % bonus weight for clause over np
  ifthenelse(Sorted=[], true,myplus(W,5,WeightClause)),


  full_expansions(FE),
  expandkernel(FE,ExpandT,ExpandP,ExpandD,w:WExpand, _, partial),  % here too weight is computed
                         % and the interrogative can hide in the adjuncts

  ifthenelse(WExpand \= 0, WExpandN is WExpand+1, WExpandN=WExpand),

  flatten(ExpandP,EFlat),
  adjacent(EFlat,Path),
  append(EFlat,Path,AllPaths),

  flatten(ExpandD, FlatD),
  sum(FlatD, DistanceD),
  myplus(DistanceD,Distance,NDistance),

  flatten(AllPaths, FlatAll),
  msort(FlatAll, NSorted),
  \+dup(NSorted),
  contiguous(NSorted),                                    % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%


  append(ST,ExpandT,Trees),
  insort(Trees, NST),
  myplus(WeightClause,WExpandN,WWC),
  myplus(W,WExpandN,WW),


map(participle_clause,[pathlist:NSorted,hp:[p(X,Y)],distance:[NDistance],
           cat:ppclause,w:WWC,type:ppcentered,
           number:Nb,gender:G, lex:Lex,
           case:C,constituent_structure:[past_participle,Lex,NST]]),

  ifthenelse((G=masc;G=fem;G=or([masc,fem])),NPSem=[hum],NPSem=[thing]), % if it's not a neuter, treat as +HUM

map(np,[pathlist:NSorted,hp:[p(X,Y)],
           index:i(p(X,Y)),distance:[NDistance],
           cat:np,class:pc2np,sem:NPSem,
           number:Nb,person:_,gender:G, type:clausal,lex:Lex,lextype:full,
           case:C,w:WW,
           constituent_structure:[past_participle,Lex,NST]])    ].




% future participle with arg structure inherited from verb
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



[finite,participle_clause4] --->
[mapped(p_f,[from:A,to:B|FS]),                                     % p_f is future participle
 constraint([number:Nb,gender:G,case:C,lex:Clex], FS),

 lexarg(Clex,arglist:ArgList), % connecting with the arglist via lexarg/2

 % selecting the subject to remove it from the arglist:
   pick(ws(Lex,_,clause:_,mwuw:_,args:Args), ArgList,_),
   pick(subject:_,Args,NewArgs),


 expand(NewArgs,ExpandTrees,ExpandPaths,ExpandDistances,w:Weight),    % using expand for adj and noun args should prove sufficient
 flatten(ExpandPaths,Flat),
 msort(Flat,Sorted),
 \+dup(Sorted),

    % Sorted \= [],               % not reduced to the future participle alone,
      % which is then best viewed as an adjective pure and simple (which it is...) DOUBTFUL

    % adjacent(PL1,Sorted),  % args before or after adj, but contiguous

    % contiguous(Sorted),
 insort(ExpandTrees,ST),
 flatten(ExpandDistances,ED),
 sum(ED,Dis),
 % Dis is Distnp1+Distance,
 % myplus(Distnp1,Distance,Dis),
 append([p(A,B)],Sorted,Path),
 % W is Weight+1,
 myplus(Weight,1,W),

 full_expansions(FE),
  expandkernel(FE,ExpandT,ExpandP,ExpandD,w:WExpand, _, partial),  % here too weight is computed
                      % and the interrogative can hide in the adjuncts

  ifthenelse(WExpand \= 0, WExpandN is WExpand+1, WExpandN=WExpand),

  flatten(ExpandP,EFlat),
  adjacent(EFlat,Path),
  append(EFlat,Path,AllPaths),

  flatten(ExpandD, FlatD),
  sum(FlatD, DistanceD),
  myplus(DistanceD,Dis,NDistance),

  flatten(AllPaths, FlatAll),
  msort(FlatAll, NSorted),
  \+dup(NSorted),
  contiguous(NSorted),                                   % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%

  append(ST,ExpandT,Trees),
  insort(Trees, NST),
  myplus(W,WExpandN,WW),

            map(participle_clause,[pathlist:NSorted,hp:[p(A,B)],distance:[NDistance],
           cat:ppclause,w:WW,type:fpcentered,sem:[hum],
           number:Nb,gender:G, lex:Lex,
           case:C,constituent_structure:[future_participle,Lex,NST]]),


  ifthen( (G=masc;G=fem;G=or([masc,fem])),  % only map as NP if masc/fem

         map(np,[pathlist:NSorted,hp:[p(A,B)],
           index:i(p(A,B)),distance:[NDistance],
           cat:np,class:pc2np,sem:[hum],
           number:Nb,person:_,gender:G, type:clausal,lex:Lex,lextype:full,
           case:C,w:WW,
           constituent_structure:[future_participle,Lex,NST]]))    ].





















%%%%%%%%%%%%%%%%%%%%
% ADVERBIAL PHRASES
%%%%%%%%%%%%%%%%%%%%

% Adverbial phrases are elementary in this parser -
% they are simply derived from adverbs 'tout court'.

% The semantic value and type are likely to be the most useful features to attach the adverb where it belongs
% and specify its contribution to adjuncts.

% adverbial phrases built out of a single adverb such as semper
% lex(semper, adv,  [lex:semper,  pos:adv,  type:vpbound,   sem:time]).
% lex(modo,     adv,  [lex:modo,      pos:adv,  type:vpbound,   sem:time]).
% lex(grauiter, adv,  [lex:grauiter,  pos:adv,  type:vpbound,         sem:manner_means]).

[core,advp1] --->
[mapped(adv,[from:A,to:B|FS]),
 constraint([type:Type, lex:Lex, sem:Sem],FS),
 map(advp,[cat:advp,pathlist:[p(A,B)],distance:[0],
             type:Type,lex:Lex,sem:Sem,w:1, constituent_structure:[head:Lex]])].


% with tam
% clearly AD HOC

[core,advp2] --->
[mapped(adv,[from:A,to:B|FS1]),
 mapped(adv,[from:B,to:C|FS2]),
 constraint([lex:tam],FS1),
 constraint([type:Type, lex:Lex, sem:Sem],FS2),
 map(advp,[cat:advp,pathlist:[p(A,C)],distance:[0],
             type:Type,lex:Lex,sem:Sem,w:2, constituent_structure:[intens:tam,head:Lex]])].


% ... apertius quam ut fallerent

[finite,advp3] --->
[mapped(adv,[from:A,to:B|FS]),
 constraint([lex:Lex, sem:Sem,type:Type,degree:comp],FS),

 mapped(adj,[from:B,to:C|FSadj]),  % quam is registered as an adj
 constraint([txt:quam],FSadj),      % we insist on having 'quam', nothing else

 mapped(sub,[from:C,to:D|Fsubordinator]),        % subordinatOR
 constraint([argbound:no,lex:ut,mood:subjunctive],Fsubordinator),

 mapped(pred,SubClause),
 constraint([type:finite,class:m,pathlist:PLSub,distance:Dis, gap:[],

             mood:subjunctive, tense:TenseSub,w:W1,constituent_structure:HeadSub],SubClause),

 start(PLSub,D),       % pred directly follows quam ut
                 % for such checks we use the 'start' and 'extremity' predicates
 append([[p(A,B)],[p(B,C),p(C,D)]],PLSub,Path),  % the quam needs to enter the path, too
 msort(Path,Sorted),
 \+ dup(Sorted),
 % Weight is W1+20,
 myplus(W1,20,Weight),            % needs heavy weight to counteract reading of ut+subj as
                % independent subclause
 map(advp,[cat:advp,pathlist:Sorted,distance:Dis,hp:[p(A,B)],
             lex:Lex,type:Type,degree:comp,w:Weight,sem:Sem,
             constituent_structure:[Lex,comp_cplt:[ut,HeadSub]]])].




















%%%%%%%%%%%%%%%
% NOUN PHRASES
%%%%%%%%%%%%%%%


% the core NPS are assembled before the other NPs, for which they can serve as building blocks
% this is the reason for having two passes for nps : core and finite
% the core NPs are simple nps that do not involve predications
% therefore no relatives, no arg-bearing nouns, just the simple buiding blocks:
% nouns as nps, names as nps, adj+n as np, and so on...

% Each np is associated with an index which refers to the positions it spans in the input string
% The index is useful to make sense of gaps, i.e. traces (t or e in syntactic parlance) 'left' by elements
% 'moved out of place' by 'transformations'. The quotes are meant to show distance with respect to the syntactic
% theory underlining such treatment.

% But undoubtedly a similar treatment is needed. If the trace cannot be associated with the relative pronoun, and,
% via the relative, and more importantly, the antecedent, all the controls we wish to perform, such as semantic controls
% on arg bearers, will prove impossible in relative clauses, to give one example.





% simple NP
%%%%%%%%%%%

% rex
% lex(rex, noun, [pos:noun, txt:rex, lex:rex, case:nom, gender:masc, class:common, number:sing, sem:[hum]]).


[core,np1] --->
[mapped(noun,[from:A,to:B|FS]),
 constraint([lex:Lex,class:common,sem:Sem,txt:Text,
            number:Nb,gender:G,case:C],FS),

ifthenelse(constraint([origin:adj],FS),
          ( W is (0.5), Origin=adj),
          ( W is 1,     Origin=noun)
            ),        % nouns out of adjs have lighter weight

 map(np,[pathlist:[p(A,B)],hp:[p(A,B)],index:i(p(A,B)),distance:[0],
           cat:np,sem:Sem,class:common,lextype:full,origin:Origin,
           number:Nb,person:3,gender:G, type:core,lex:Lex,txt:Text,
           case:C,w:W,constituent_structure:Text])].


% np opening the sentence with relative adjective
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Quas urbes hostis diripere coepit.
% lex(quas, relative, [pos:relative, txt:quas, lex:relaccfempl,gender:fem,
%                      case:acc, number:pl, function:[object,prep_cplt,subject]]).

[core,np2] --->
 [mapped(relative,[from:0,to:1,pos:relative, txt:_, lex:_,gender:G,
                    case:C, number:Nb, function:_]),
              % positions 0 to 1 - it must be the very first word

 mapped(noun,[from:1,to:2|FS]),                          % immediately followed by a noun
              % this is likely to prove too strict - we may use a more relaxed
                    % type of adjacency (2 to 3 as well as 1 to 2, for instance)
 constraint([lex:Lex,class:common,sem:Sem,
            number:Nb,gender:G,case:C],FS),             % case number and gender agreement : the usual agreement triplet

 map(np,[pathlist:[p(0,2)],hp:[p(1,2)],index:i(p(1,2)),distance:[0],
           cat:np,sem:Sem,class:common,lextype:full,      % lextype:full is meant to prevent further modification
           number:Nb,person:3,gender:G, type:core,lex:Lex,
           case:C,w:1,constituent_structure:[relative_adj,head:FS]])].


% Adjectives as Nouns : people (boni etc...)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% only adj bearing the n:yes or n:hpl feature can fit here
% the feature should be assigned on the basis of what we find in classical texts

[core,noun1] --->
[mapped(adj,[from:A,to:B|FS]),
 constraint([txt:Text, n:Pot,case:C, number:Number, gender:Gender, lex:Lex, degree:pos],FS),   % plural
 (Pot=yes;
  (Pot=hpl,Number=pl)), % can always be read as np or can be read as np if plural and human
 (Gender=masc;Gender=fem),

  map(noun,[from:A,to:B,pos:noun,origin:adj,txt:Text,lex:Lex,case:C,gender:Gender,class:common,ab:no,number:Number,sem:[hum],constituent_structure:Lex]) ].


% Adjectives as Nouns : abstract (bonum, uilia, etc...)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% here too a restricted class of adjectives seems to fit the part;
% they should bear the n:yes or the n:n feature

[core,noun2] --->
[mapped(adj,[from:A,to:B|FS]),
 constraint([n:Pot, case:C, number:Nb, gender:neuter, txt:Text,lex:Lex],FS),  % neuter
 (Pot=yes;Pot=n),                      % can always be read as np
                           % or can be read as np if neuter, singular or plural

 map(noun,[from:A,to:B,pos:noun,origin:adj,txt:Text,lex:Lex,case:C,gender:neuter,class:common,ab:no,number:Nb,sem:[abstract,thing],constituent_structure:Lex]) ].


% MWUs
%%%%%%

% MULTIPLE WORD UNITS are a very important component of the lexicon of any language
% The lexicographical ressources for Latin may not be fully adequate yet
% the mwu's must not only be registered but their degree of syntactic frozenness and their
% reluctance to open themselves to lexical variation must be carefully investigated and
% properly recorded - the task is particularly hard in the case of dead languages,
% but also made easier by the fact that we may very well deem ourselves satisfied if we
% can account for what is found in text (not, of course, by means of the totally adhoc
% listing of elements)


% Example compound entries : RES PUBLICA and RES NOVAE

% we can increase the weight (3 instead of 1+1)
% if we wish to privilege the compound reading as against the adj-noun pair
% we need to register the weight when filling in the arg slot of the arg-bearer

% one should not wonder at the mix between lexis and grammar here
% all non-frozen mwus, be they np compounds or more elaborate structures,
% need to have an entry in the grammar, at exactly the right level,
% to account for the amount of variation they are ready to accept
% while retaining their mwu reading (the whole problem in a nutshell...)
% see my paper 'VERBA' available at https://archibaldmichiels.hcommons.org/


% there should be (hundreds of) thousands of such entries
% no joke !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

% RES PUBLICA
[core,noun3] --->
[mapped(noun,[from:A, to:B|FSnoun]),
 mapped(adj,[from:B, to:C|FSadj]),    % not publica res

 constraint([number:sing,gender:fem,case:Case,lex:res],FSnoun),
 constraint([number:sing,gender:fem,case:Case,lex:publicus],FSadj),
  map(np,[pathlist:[p(A,B),p(B,C)],hp:[p(A,B)],index:i(p(A,B)),distance:[0],cat:np,class:common,sem:[abstract,hum],
          number:sing,person:3,gender:fem,type:core,lex:res_publica,lextype:full,
          case:Case,w:3,constituent_structure:res_publica]) ].

 % also to be mapped as a noun because it can be modified by an adj: 'sententia aliena a re publica nostra'.

% map(noun,[from:A,to:C,pos:noun,lex:res_publica,class:common,sem:[abstract,hum],txt:res_publica,
 %           number:sing,gender:fem,case:Case,constituent_structure:res_publica])
% ].



% RES NOVAE (revolution)
% cupiditate regni adductus novis rebus studebat (Caesar, De Bello Gallico, 1.9.3)
[core,np3] --->
[mapped(noun,[from:A, to:B|FSnoun]),
 mapped(adj,[from:X, to:Y|FSadj]),
 constraint([number:pl,gender:fem,case:Case,lex:res],FSnoun),  % plural needed, of course
 constraint([number:pl,gender:fem,case:Case,lex:nouus],FSadj),
 adjacent([p(A,B)],[p(X,Y)]), % adjacency required res nouae or nouae res
 append([p(A,B)],[p(X,Y)],Path),
 msort(Path, Sorted),
 map(np,[pathlist:Sorted,hp:[p(A,B)],index:i(p(A,B)),distance:[0],cat:np,class:common,sem:[abstract],
          number:pl,person:3,gender:fem,type:core,lex:res_novae,lextype:full,
          case:Case,w:3,constituent_structure:res_nouae])].


% VIA SACRA, SACRA VIA
% on account of the two word orders, we only require adjacency

[core,np4] --->
[mapped(noun,[from:X, to:Y|FSnoun]),
 mapped(adj,[from:A, to:B|FSadj]),
 constraint([number:sing,gender:fem,case:Case,lex:uia],FSnoun),    % sing and fem, of course
 constraint([number:sing,gender:fem,case:Case,lex:sacer],FSadj),
 adjacent([p(A,B)],[p(X,Y)]),
 append([p(A,B)],[p(X,Y)],Path),
 msort(Path, Sorted),
 map(np,[pathlist:Sorted,hp:[p(X,Y)],index:i(p(X,Y)),distance:[0],cat:np,class:common,sem:[loc],
          number:sing,person:3,gender:fem,type:core,lex:sacra_uia,lextype:full,
          case:Case,w:3,constituent_structure:uia_sacra])].



% Much more open: various cases of appositions (but not FREE appositions - the word order is a good clue
% to the restricted character of such groupings

% URBS ROMA etc.
[core,np5] --->
[mapped(noun,[from:A, to:B|FSnoun1]),
 mapped(noun,[from:B, to:C|FSnoun2]),

 constraint([number:sing,case:Case,lex:urbs],FSnoun1),        % urbs in the sing

 constraint([number:sing,gender:fem,case:Case,sem:[city],lex:Lex],FSnoun2),     % any city
                                                                                % we can use the sem feature
                    % remember that the value of the sem feature
                    % should always be a list

 map(np,[pathlist:[p(A,B),p(B,C)],hp:[p(A,B)],index:i(p(A,B)),distance:[0],cat:np,class:common,sem:[city],
          number:sing,person:3,gender:fem,type:core,lex:Lex,lextype:full,
          case:Case,w:2,constituent_structure:Lex])].

% in the lex we do not bother with the word urbs



% WITH INTERROGATIVE ADJ
%%%%%%%%%%%%%%%%%%%%%%%%%

[core,np6] --->
[mapped(noun,[from:B, to:C|FSnoun]),
 mapped(adj,[from:A, to:B|FSadj]),

 constraint([number:Nb,gender:Gender,case:Case,class:Class,sem:Sem,lex:LexNoun],FSnoun),
 constraint([number:Nb,gender:Gender,case:Case,type:int,lex:_],FSadj),                % interrogative

 map(np,[pathlist:[p(A,B),p(B,C)],hp:[p(B,C)],index:i(p(B,C)),distance:[0],cat:np,class:Class,sem:Sem,
          number:Nb,person:3,gender:Gender,type:int,lex:LexNoun,lextype:full,
          case:Case,w:4,constituent_structure:[head:FSnoun,adjp:FSadj]])].

% needs heavy weight to compete, as it carries the burden of signalling a wh_question
% and wh-forms have a knack of doing all sorts of things
% (relatives without antecedent, 'relatif de liaison', ...)


% WITH POSSESSIVE
%%%%%%%%%%%%%%%%%

[core,np7] --->
[mapped(noun,[from:B1, to:C|FSnoun]),
 mapped(adj,[from:A, to:B|FSadj]),
 X is B1-1,
 (B1=B;B1=X),                   % only one intervening element 'sua quisquam sententia'    ALP 193
 constraint([number:Nb,gender:Gender,case:Case,class:Class,sem:Sem,lex:LexNoun],FSnoun),
 constraint([number:Nb,gender:Gender,case:Case,type:poss,lex:_],FSadj),                % possessive

 map(np,[pathlist:[p(A,B),p(B1,C)],hp:[p(B1,C)],index:i(p(B1,C)),distance:[0],cat:np,class:Class,sem:Sem,
          number:Nb,person:3,gender:Gender,type:core,lex:LexNoun,lextype:full,
          case:Case,w:1,constituent_structure:[head:FSnoun,adjp:FSadj]])].



% STANDARD
%%%%%%%%%%

% with adjective phrase - the np cannot be core on account of the args that the adjective can bear,
% e.g. gerunds with adj such as cupidus



% adj phrase following the noun
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

[core,np8] --->
[mapped(noun,[from:A, to:B|FSnoun]),
 mapped(adjp,FSadj),

 constraint([pathlist:Padj],FSadj),
 msort(Padj,Padjsorted),                          % ALP 194
 extremity(Padjsorted,Ext),
 Ext > B,                             % here the adj phrase follows the noun

 constraint([number:Nb,gender:Gender,case:Case1,class:Class,sem:Sem,lex:LexNoun],FSnoun),

 constraint([number:Nb,gender:GenderAdj,case:Case2,lex:LexAdj,type:Type,w:W],FSadj),

 funify([gender:GenderAdj],[gender:Gender],_),

% compatibility voc and nom
 case_out(Case1,Case2,CaseOut),


 Type \= int,         % interrogative adjectives dealt with separately - they need heavy weight


 append([p(A,B)],Padjsorted,Pnp),  % ALP 194

 distance([p(A,B)],Padjsorted,Distance),    % the distance between the noun and the path of the adjective
                                      % determines the straining factor as well as helping to decide
              % whether noun and adj DO belong together

 ifthen(LexAdj=is, Distance=0),    % is/ea/id adjacent - this requirement is too strong

 \+member(LexAdj,[tot,aliquot]),    % a lot of work necessary to complete list

 msort(Pnp, Sorted),
 \+dup(Sorted),

 Distance < 6 ,        % 5 is thus the maximum distance between adj and noun

 % note diff with adj preceding noun : justified ???

 % we still have to exclude the occurrence, within the gap, of nouns to which the adjective could be
 % attached with priority, because they agree in the well-mapped agreement triplet
 ifthen(Distance=5, relaxedadjacent5_cgn([p(A,B)],Padj,CaseOut,Gender,Nb)),
                               % five in between, neither of them a noun with relevant triplet
 ifthen(Distance=4, relaxedadjacent4_cgn([p(A,B)],Padj,CaseOut,Gender,Nb)),
                               % four in between, neither of them a noun with relevant triplet

 ifthen(Distance=3, relaxedadjacent3_cgn([p(A,B)],Padj,CaseOut,Gender,Nb)),
                               % three in between, neither of them a noun with relevant triplet
 ifthen(Distance=2, relaxedadjacent2_cgn([p(A,B)],Padj,CaseOut,Gender,Nb)),
                               % two in between, neither of them a noun with relevant triplet
 ifthen(Distance=1, relaxedadjacent1_cgn([p(A,B)],Padj,CaseOut,Gender,Nb)),
                               % one in between, not a noun with same [gender,number,case] triplet

  % Weight is W+1,
  myplus(W,1,Weight),
  append([pathlist:[p(A,B)]],FSnoun,FSnounp),         % so that the noun should appear when the pathlists get pretty-printed
  map(np,[pathlist:Sorted,hp:[p(A,B)],index:i(p(A,B)),distance:[Distance],      % distance is recorded as straining factor
            cat:np,class:Class,sem:Sem,
            number:Nb,person:3,gender:Gender,type:core,lex:LexNoun,lextype:full,
            case:CaseOut,w:Weight,
            constituent_structure:[head:FSnounp,adjp:FSadj]])].



% adj phrase preceding the noun
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% cf above - here the adjective precedes

[core,np9] --->
[mapped(noun,[from:A, to:B|FSnoun]),
 mapped(adjp,FSadj),
 constraint([pathlist:Padj],FSadj),
 msort(Padj,Padjsorted),                        % ALP 194
 extremity(Padjsorted,Ext),
 Ext =< A,
  % the adj phrase precedes the noun



 constraint([number:Nb,gender:Gender,case:Case1,class:Class,sem:Sem,lex:LexNoun],FSnoun),

 constraint([number:Nb,gender:GenderAdj,case:Case2,lex:LexAdj,type:Type,w:W1],FSadj),

 funify([gender:GenderAdj],[gender:Gender],_),


  % compatibility voc and nom - preference for voc: pater optime nom+voc -> voc
 case_out(Case1,Case2,CaseOut),


 Type \= int,           % interrogative adjectives dealt with separately

 append([p(A,B)],Padjsorted,Pnp),



 distance(Padjsorted,[p(A,B)],Distance),                       % ALP 194
 ifthen(member(LexAdj,[is,tot,aliquot]), Distance=0),
 ifthen(Type=poss,Distance < 2),
% ifthen(Type=tool, Distance < 2),    % *haec* mihi si dederis *commoda* miles ero... hence the remming
                                      % nullum ........................... officium Tac Hist III XVII 1
 msort(Pnp, Sorted),
  \+dup(Sorted),
  Distance < 10 ,

 % we still have to exclude the occurrence, within the gap, of nouns to which the adjective could be
 % attached with priority, because they agree in the well-mapped agreement triplet

    ifthen(Distance=9, relaxedadjacent9_cgn(Padj,[p(A,B)],CaseOut,Gender,Nb)),
                               % nine in between, neither of them a noun with relevant triplet


   ifthen(Distance=8, relaxedadjacent8_cgn(Padj,[p(A,B)],CaseOut,Gender,Nb)),
                               % eight in between, neither of them a noun with relevant triplet


  ifthen(Distance=7, relaxedadjacent7_cgn(Padj,[p(A,B)],CaseOut,Gender,Nb)),
                               % seven in between, neither of them a noun with relevant triplet

 ifthen(Distance=6, relaxedadjacent6_cgn(Padj,[p(A,B)],CaseOut,Gender,Nb)),
                               % six in between, neither of them a noun with relevant triplet

 ifthen(Distance=5, relaxedadjacent5_cgn(Padj,[p(A,B)],CaseOut,Gender,Nb)),
                               % five in between, neither of them a noun with relevant triplet
 ifthen(Distance=4, relaxedadjacent4_cgn(Padj,[p(A,B)],CaseOut,Gender,Nb)),
                               % four in between, neither of them a noun with relevant triplet

 ifthen(Distance=3, relaxedadjacent3_cgn(Padj,[p(A,B)],CaseOut,Gender,Nb)),
                               % three in between, neither of them a noun with relevant triplet
 ifthen(Distance=2, relaxedadjacent2_cgn(Padj,[p(A,B)],CaseOut,Gender,Nb)),
                               % two in between, neither of them a noun with relevant triplet
 ifthen(Distance=1, relaxedadjacent1_cgn(Padj,[p(A,B)],CaseOut,Gender,Nb)),
                               % one in between, not a noun with same [gender,number,case] triplet

  % Weight is W1+1,
  myplus(W1,1,Weight),
  append([pathlist:[p(A,B)]],FSnoun,FSnounp), % so that the noun should appear when the pathlists get pretty-printed
  map(np,[pathlist:Sorted,hp:[p(A,B)],index:i(p(A,B)),distance:[Distance],
            cat:np,class:Class,sem:Sem,
            number:Nb,person:3,gender:Gender,type:core,lex:LexNoun,lextype:full,
            case:CaseOut,w:Weight,constituent_structure:[head:FSnounp,adjp:FSadj]])].




% with participle clause (mostly following the noun they get attached to)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% rex recubans sub tegmine
% vir imperator futurus
% rex amatus a regina



% status of other participle clauses (outside of absolute ablatives) still to be settled  !!!!!!!!!
% we need to have such clauses attached to the unexpressed subject of a verb
% quite a difficult exercise
% 'lego librum recubans sub tegmine fagi''
% where 'recubans' is linked to the subject of 'lego', a dummy np to be created for such a need
% but overgeneration is likely to be a problem

% on contextual subject:
% this type of subject is the only one to sport 'source:context_retrievable'
% the info should be used for the association of participle clauses with subjects,
% ***once the arg structure has been explored in finite clauses***



% note that the clauses are attached to nps, not nouns

[finite,np10] --->
[mapped(np,FSnp),
 mapped(participle_clause,FSpp),
                               % the latter are built around the participle
             % and deal with the args that the underlying verb features
                               % present, past and future participles are involved

 constraint([pathlist:PLnp,hp:HPnp,index:Index,distance:[Distance],w:Weightnp,
             number:Nb,gender:Gender,case:Case1,class:Class,person:Person,
             sem:Sem,lex:LexNoun],FSnp),


 constraint([pathlist:PLPC, number:Nb,gender:Gender,case:Case2,distance:[DisPart],type:Type_pp,lex:_,w:W],FSpp),
                % all these features are duly recorded in the participle_clause

 msort(PLnp,PLnpsorted),                            % ALP 194
 msort(PLPC, PLPCsorted),                           % ALP 194

 distance(PLnpsorted,PLPCsorted,DisCl),             % ALP 194
% DisCl < 3,                            % ad hoc cf. quem omnes urbes expulsum a vobis ad se vocabunt Cic Pro Milone 38 104  ALP 193
 myplus(Distance,DisPart,GD),

 % compatibility voc and nom
 case_out(Case1,Case2,CaseOut),

%  constraint([pathlist:PLpp],FSpp), % remmed ALP 194 this is a dup with PLPC

 append(PLnpsorted,PLPCsorted,PLbignp),

    % extremity(PLpp,Extpp),       % extremity of clause
          % length(PLpp,Length),
                % extremity(PLnp,Extnp),       % extremity of np
                % ifthen(Length > 1, Extpp > Extnp),          % the participle clause, if longer than one word, follows the noun ???????
                % relaxadjacent(PLnp,PLpp),    % a type of adjacency that allows for one element to interrupt the sequence
                              % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%
                %  adjacent(PLnp,PLpp),

msort(PLbignp, Sorted),
% quasicontiguous2(Sorted),      % SEE quem omnes urbes expulsum a vobis ad se vocabunt Cic Pro Milone 38 104  *** ALP 193 ***
contiguous(Sorted),              % difficult and dangerous to do without : accensum desiderio reginae vertisse iter (accensum iter ??? !!!)
 \+dup(Sorted),
 % Weight is W+Weightnp+1,     % we try to give them a good weight by adding the little bonus of 1
 myplus(W,Weightnp,T1),
 myplus(T1,1,Weight),
 map(np,[pathlist:Sorted,hp:HPnp,index:Index,distance:[GD],
           cat:np,class:pc2np,sem:Sem,
           number:Nb,person:Person,gender:Gender,type:core,lex:LexNoun,lextype:full,   % person to be left open or read from np
           case:CaseOut,w:Weight,type_pp:Type_pp,
           constituent_structure:[head:FSnp,participle_clause:FSpp]]) ].



% arg-bearing nouns
%%%%%%%%%%%%%%%%%%%

% cannot be assembled before the 'finite' run on account of args

% lexarg(studium,
  %     arg:[type:pp,constraints:[prep:in, case:acc]]).

% the arg-bearing noun is retrieved from within the NP it is included in


[finite,np11] --->
[mapped(np,Fnp),
 constraint([pathlist:PL1,hp:HL1,distance:[Dist1],sem:Sem,index:Index,
            number:Nb,person:3,gender:G,case:C,class:common,lex:Lex], Fnp),
 lexarg(Lex,arg:Arg),
       % connecting with the arglist via lexarg/2

 qm(arg:Arg,arg:Tree,PathlistArg,[DistanceArg],w:Wtot),
 msort(PathlistArg,ArgSorted),
 msort(PL1,PLSorted),

 % if the arg is a prep phrase it follows the arg-bearer immediately, or with a one-word gap
 % but 'mira inter exercitum imperatoremque diversitas'

  /* ifthen( constraint([type:pp],Arg) ,
         ( (adjacent_lr(PLSorted,ArgSorted) ; relaxadjacent_lr(PLSorted,ArgSorted)) )
       ),
  */

 ( (distance(PLSorted,ArgSorted,0),Malus is 0)  ;
   (distance(PLSorted,ArgSorted,1), Malus is 2) ;
   (distance(PLSorted,ArgSorted,2),Malus is 3 )
 ),
  % args before or after adj, but some type of relaxed adjacency is called for


%  Dis is Dist1+DistanceArg+Malus,               % they come from two sources
  myplus(Dist1,DistanceArg,Di),       % myplus guards against uninstantiated variables, which throw up an error
                                      % if standard 'is' is used
  myplus(Di,Malus,Dis),
  append(PLSorted,ArgSorted,Path),
  msort(Path,Sorted),
  \+ dup(Sorted),
  % W is Wtot+2,
  myplus(Wtot,2,W),         % important bonus for arg info as opposed to adjuncts
  map(np,[pathlist:Sorted,hp:HL1,index:Index,distance:[Dis],
           cat:np,sem:Sem,class:common,lextype:full,              % 'lextype:full' means we do not want to expand it further
           number:Nb,person:3,gender:G, type:full,lex:Lex,
           case:C,w:W,constituent_structure:[Fnp,arg:Tree]])].



% names
%%%%%%%

[core,np12] --->
[mapped(noun,[from:A,to:B|FS1]),
 constraint([class:proper,number:Nb,gender:G, case:C,lex:LexNoun,    % proper as class value
              sem:Sem],FS1),
 map(np,[pathlist:[p(A,B)],hp:[p(A,B)],index:i(p(A,B)),distance:[0],
           cat:np,sem:Sem,class:proper,lex:LexNoun,lextype:full,
           number:Nb,person:3,gender:G,type:core,case:C,w:1,constituent_structure:LexNoun])].



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% addresses
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% names
%%%%%%%

% lex(arria, noun, [pos:noun, txt:arria, lex:arria, case:nom, gender:fem, class:proper, ab:no, number:sing, sem:[hum]]).

[core,address1] --->
[ mapped(noun,[from:A,to:B|FS1]),

  ( (A=0, Adpath1=[]) ;
    (mapped(punct,[from:AA,to:A, lex:comma|_]), Adpath1=[p(AA,A)])
  ),                  % comma before or first word of the sentence

 constraint([class:proper,number:Nb,gender:G, case:Case,lex:LexNoun,    % proper as class value
              sem:Sem],FS1),
 (Case=nom;Case=voc),

  recorded(fin,fin(Fin),_),

 (
   (B=Fin, Adpath2=[]) ;
   (mapped(punct,[from:B,to:C, lex:comma|_]), Adpath2=[p(B,C)])
  ),                   % comma after or last word of the sentence

 append(Adpath1,[p(A,B)],Path1),
 append(Path1,Adpath2,Path2),



 map(address,[pathlist:Path2,hp:[p(A,B)],index:i(p(A,B)),distance:[0],
           cat:address,sem:Sem,class:proper,lex:LexNoun,lextype:full,
           number:Nb,person:P,gender:G,type:core,case:voc,w:2, constituent_structure:[address:LexNoun]])].



% specific np heads
%%%%%%%%%%%%%%%%%%%

[core,address2] --->
[ mapped(np,FSNP),
 constraint([pathlist:Path,hp:HP,index:Index,number:Number,gender:G, case:Case,lex:LexNoun,
              sem:Sem, distance:Dis,class:Class, constituent_structure:CSTR],FSNP),

(Class=common;Class=proper),

 start(Path,Start),
 extremity(Path,End),

 ( (Start=0, Adpath1=[]) ;
   (mapped(punct,[from:AA,to:Start, lex:comma|_]), Adpath1=[p(AA,Start)])
 ),               % comma before or first word of the sentence

 ifthen(Class=common, member(LexNoun,[amicus, deus, filia, iudex, liber,
                 maritus, mater, miles, pater, puer,
                 senex, uir])),
                % list to be filled in on the basis of textual occurrences
 (Case=nom;Case=voc),

  recorded(fin,fin(Fin),_),
 (
   (End=Fin, Adpath2=[]) ;
   (mapped(punct,[from:End,to:Next, lex:comma|_]), Adpath2=[p(End,Next)])
  ),                 % comma after or last word of the sentence


 append(Adpath1,Path,Path1),
 append(Path1,Adpath2,Path2),

 ifthenelse(Case=voc, Weight=90, Weight=40),

 msort(Path2,Sorted),
 contiguous(Sorted),
 map(address,[pathlist:Sorted,hp:HP,index:Index,distance:Dis,
           cat:address,sem:Sem,class:common,lex:LexNoun,lextype:full,
           number:Number,person:2,gender:G,type:core,case:voc,w:Weight, constituent_structure:[address:CSTR]])].



% nouns out of adj
%%%%%%%%%%%%%%%%%%

% map(noun,[from:A,to:B,pos:noun,origin:adj,txt:Text,lex:Lex,case:C,gender:Gender,class:common,ab:no,number:Number,sem:[hum]]) ].

[core,address3] --->
[ mapped(noun,[from:A,to:B|FS1]),
%
    ( (A=0, Adpath1=[]) ;
    (mapped(punct,[from:AA,to:A, lex:comma|_]), Adpath1=[p(AA,A)])
  ),                  % comma before or first word of the sentence

 constraint([origin:adj,number:Nb,gender:G, case:Case,lex:LexNoun],FS1),   % origin: adj

 (Case=nom;Case=voc),

  recorded(fin,fin(Fin),_),

 (
   (B=Fin, Adpath2=[]) ;
   (mapped(punct,[from:B,to:C, lex:comma|_]), Adpath2=[p(B,C)])
  ),                  % comma after or last word of the sentence

 append(Adpath1,[p(A,B)],Path1),
 append(Path1,Adpath2,Path2),

 map(address,[pathlist:Path2,hp:[p(A,B)],index:i(p(A,B)),distance:[0],
           cat:address,sem:[hum],class:common,lex:LexNoun,lextype:full,
           number:Nb,person:2,gender:G,type:core,case:voc,w:12, constituent_structure:[address:LexNoun]])].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%














% PRONOUNS
%%%%%%%%%%


% interrogative pronouns
%%%%%%%%%%%%%%%%%%%%%%%%

[core,np13] --->
[mapped(print,[from:A,to:B|FS1]),   % PRINT: interrogative pronouns as a lexical class
 constraint([number:Nb,gender:G, case:C,sem:Sem,lex:Lex],FS1),
 map(np,[pathlist:[p(A,B)],hp:[p(A,B)],index:i(p(A,B)),distance:[0],
           cat:np,sem:Sem,class:int,lex:Lex,
           number:Nb,person:3,gender:G,type:int,case:C,w:1,constituent_structure:Lex])].                         % we register the interrogative nature
                           % as type and class value : overkill ?


% demonstrative pronouns
%%%%%%%%%%%%%%%%%%%%%%%%

[core,np14] --->
[mapped(prdem,[from:A,to:B|FS1]),              % PRDEM : ditto for demonstrative pronouns
 constraint([number:Nb,gender:G, case:C,lex:Lex],FS1),
 map(np,[pathlist:[p(A,B)],hp:[p(A,B)],index:i(p(A,B)),distance:[0],
           cat:np,sem:_,class:dem,lex:Lex,            % class dem
           number:Nb,person:3,gender:G,type:core,case:C,w:1,constituent_structure:Lex])].

% indefinite pronouns
%%%%%%%%%%%%%%%%%%%%%

[core,np15] --->
[mapped(prindef,[from:A,to:B|FS1]),            % PRINDEF : guess what...
 constraint([number:Nb,gender:G, case:C,sem:Sem,lex:Lex],FS1),
 map(np,[pathlist:[p(A,B)],hp:[p(A,B)],index:i(p(A,B)),distance:[0],
           cat:np,sem:Sem,class:indef,lex:Lex,            % class indef
           number:Nb,person:3,gender:G,type:core,case:C,w:1,constituent_structure:Lex])].





% nihil humani, nil novi
%%%%%%%%%%%%%%%%%%%%%%%%

[core,np16] --->

[(Typepro=prdem;Typepro=prindef;Typepro=print),     % demonstrative, indefinite or interrogative pronoun
 mapped(Typepro,[from:A,to:B|FS1]),
 constraint([number:sing,gender:neuter, case:C,sem:Sem,lex:Lex],FS1),
 mapped(adjp,FSadj),
 constraint([pathlist:Padj,number:sing,gender:neuter,w:Weight,case:gen,lex:LexAdj],FSadj),
 adj(LexAdj,_,1,_,_,_,_),           % 1 indicates second declension adj
 adjacent([p(A,B)],Padj),
 append([p(A,B)],Padj,Path),
 myplus(Weight,1,Weight1),
 ifthenelse(Typepro=print,Type=int,Type=core),
 map(np,[pathlist:Path,hp:[p(A,B)],index:i(p(A,B)),distance:[0],
           cat:np,sem:Sem,lex:Lex,class:Typepro,type:Type,            % class inherited from pronoun
           number:sing,person:3,gender:neuter,case:C,w:Weight1,constituent_structure:[FS1,FSadj]])].

% quid praemii
%%%%%%%%%%%%%%

[core,np17] --->

[(Typepro=prdem;Typepro=prindef;Typepro=print),      % demonstrative, indefinite or interrogative pronoun
 mapped(Typepro,[from:A,to:B|FS1]),
 constraint([number:sing,gender:neuter, case:C,lex:Lex],FS1),
 mapped(np,FSnp),
 constraint([pathlist:Pnp,number:N,gender:G,w:Weight,sem:Sem,case:gen],FSnp),
 adjacent([p(A,B)],Pnp),
 append([p(A,B)],Pnp,Path),
 myplus(Weight,1,Weight1),
 ifthenelse(Typepro=print,Type=int,Type=core),
 map(np,[pathlist:Path,hp:[p(A,B)],index:i(p(A,B)),distance:[0],
           cat:np,sem:Sem,lex:Lex,class:Typepro,type:Type,            % class inherited from pronoun
           number:N,person:3,gender:G,case:C,w:Weight1,constituent_structure:[Lex,FSnp]])].


% pronoun plural + gen plural + HUM
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% why have we got this rule ? ALP 193


[core,np18] --->

[(Typepro=prdem;Typepro=prindef;Typepro=print;Typepro=prpers),     % demonstrative, indefinite, interrogative or personal pronoun
 mapped(Typepro,[from:A,to:B|FS1]),
 constraint([number:Numberpro,gender:or([masc,fem]), case:C,lex:Lex],FS1),
 mapped(np,FSnp),
 constraint([pathlist:Pnp,number:pl,gender:or([masc,fem]),w:Weight,sem:[hum],case:gen],FSnp),
 adjacent([p(A,B)],Pnp),
 precedes([p(A,B)],Pnp),
 append([p(A,B)],Pnp,Path),
 myplus(Weight,1,Weight1),
 ifthenelse(Typepro=print,Type=int,Type=core),
 map(np,[pathlist:Path,hp:[p(A,B)],index:i(p(A,B)),distance:[0],
           cat:np,sem:[hum],lex:Lex,class:Typepro,type:Type,            % class inherited from pronoun
           number:Numberpro,person:3,gender:or([masc,fem]),case:C,w:Weight1,constituent_structure:[Lex,FSnp]])].




% nihil alienum a te, quid facilius
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

[core,np19] --->
[(Typepro=prdem;Typepro=prindef;Typepro=print),      % demonstrative, indefinite or interrogative pronoun
 mapped(Typepro,[from:A,to:B|FS1]),
 constraint([number:sing,gender:neuter, case:C,sem:Sem,lex:Lex],FS1),
 mapped(adjp,FSadj),
 constraint([pathlist:Padj,number:sing,gender:neuter,w:Weight,case:C],FSadj),
 adjacent([p(A,B)],Padj),
 append([p(A,B)],Padj,Path),
 myplus(Weight,1,Weight1),
 ifthenelse(Typepro=print,Type=int,Type=core),
 map(np,[pathlist:Path,hp:[p(A,B)],index:i(p(A,B)),distance:[0],
           cat:np,sem:Sem,lex:Lex,class:Typepro,type:Type,            % class inherited from pronoun
           number:sing,person:3,gender:neuter,type:core,case:C,w:Weight1,constituent_structure:[FS1,FSadj]])].






% personal pronouns
%%%%%%%%%%%%%%%%%%%

[core,np20] --->
[mapped(prpers,[from:A,to:B|FS1]),       %  PRPERS
 constraint([number:Nb,gender:G, case:C,person:Person,sem:Sem,lex:Lex],FS1),
 Lex\=pp3refl,                          % reflexives are worth special attention
 map(np,[pathlist:[p(A,B)],hp:[p(A,B)],index:i(p(A,B)),distance:[0],
           cat:np,sem:Sem,class:proper,lex:Lex,           % class proper
                      % as if they were proper names
           number:Nb,person:Person,gender:G,type:core,case:C,w:1,constituent_structure:Lex])].




% reinforced by solus or totus
% to be revised !!!!
% just to be able to deal with the splendid line : elige cui dicas : tu mihi sola places
%                                        splendid on account of the unstressed (there lies the genius !)
%          contrast between 'elige' and 'sola'....

[core,np21] --->
[mapped(prpers,[from:A,to:B|FS1]),
 mapped(adjp,FSadj),
 constraint([pathlist:PLadj,number:Nb,gender:G,case:C,lex:or([solus,totus])],FSadj),      % solus and totus only : shows how ad hoc the thing is
 constraint([number:Nb,gender:G, case:C,person:Person,sem:Sem,lex:Lex],FS1),
 Lex\=pp3refl,
 append([p(A,B)],PLadj,PL),
 msort(PL,Sorted),
 map(np,[pathlist:Sorted,hp:[p(A,B)],index:i(p(A,B)),distance:[0],
           cat:np,sem:Sem,class:proper,lex:Lex,
           number:Nb,person:Person,gender:G,type:core,case:C,w:1,constituent_structure:[head:FS1,adjp:FSadj]])].




% THIRD PERSON REFLEXIVE PRONOUNS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 3rd person reflexive pronouns are given a variable as index -
% the index will be instantiated with a value inside or outside the clause
% Scit rex se epistulam Marco misisse / Scit rex se reginam amare :
% think of the possible references for SE


% both non-emphatic and emphatic (se / se ipsum)


% emphatic

[core,np22] --->
[mapped(prpers,[from:A,to:B|FS1]),
 constraint([case:C,sem:Sem, lex:pp3refl],FS1),               % the lex value catches the reflexive pronouns
                      % of the third person
 mapped(adjp,FSadj),
       constraint([pathlist:[p(B,X)],number:Nbi,gender:Gi,case:C,lex:ipse],FSadj),  % 'ipse' follows : 'se ipsum' not * ipsum se

 map(np,[pathlist:[p(A,B),p(B,X)],hp:[p(A,B)],index:_,distance:[0],         % the index will be instantiated when the parse is ready for output
           cat:np,sem:Sem,class:proper,lex:pp3refl,emphasis:yes,
           number:Nbi,person:3,gender:Gi,type:core,case:C,w:0.5,constituent_structure:pp3refl])].


% non emphatic

[core,np23] --->
[mapped(prpers,[from:A,to:B|FS1]),
 constraint([case:C,sem:Sem, lex:pp3refl],FS1),     % the lex value catches the reflexive pronouns
                      % of the third person
 map(np,[pathlist:[p(A,B)],hp:[p(A,B)],index:_,distance:[0],         % the index will be instantiated when the parse is ready for output
           cat:np,sem:Sem,class:proper,lex:pp3refl,emphasis:no,
           number:Number,person:3,gender:Gender,type:core,case:C,w:0.4,constituent_structure:pp3refl])].






% so-called 'relatif de liaison'
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% must always be in front, in positions 0 to 1

[core,np24] --->
[mapped(relative,[from:0,to:1,pos:relative, txt:_, lex:_,gender:Gender,
                    case:Case, number:Number, function:_]),

map(np,[pathlist:[p(0,1)],hp:[p(0,1)],index:i(p(0,1)),distance:[0],
           cat:np,sem:_,class:proper,lex:relatif_de_liaison,
           number:Number,person:3,gender:Gender,type:core,w:1,case:Case,constituent_structure:linking_relative])].






















% GENITIVES IN NPS
%%%%%%%%%%%%%%%%%%


% core np + genitive np = full np
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% the two heads (of the genitive phrase and the NP it is attached to) must be adjacent
% (within a two-word flexibility range, without any noun in between)
% tu patulae recubans sub tegmine fagi : OK
% rationes bonas bonae deae : OK
% the head cannot be a proper name : * marcus pulchrae deae

% we must wait for the finite run on account of the fact that the genitive np might contain a rel clause


[finite,np25] --->
[mapped(np,Fnp1), % the head
 mapped(np,Fnp2), % the noun cplt

 Fnp1 \= Fnp2,  % not the same NP

 constraint([pathlist:PL1,hp:[p(H1,H2)],distance:[Distnp1],sem:Sem,w:W1,index:i(Inp1),
            number:Nb,person:3,gender:G,case:C,class:common,lex:LexNoun], Fnp1),
 constraint([pathlist:PL2, hp:[p(G1,G2)],distance:[Distnp2],w:W2,case:gen,index:i(Inp2)],Fnp2),  % case must be genitive

ifthenelse((H2=G1;G2=H1),SpecialBonus is 1, SpecialBonus is 0), % ALP 193 bonus for adjacency of the two np heads ALP 193 ; either order
								% vestimenta maris vs maris deo

% distancev([Inp1],[Inp2],DistIndex),  % ALP193
% DistIndex < 3,                       % ALP193                        admissibility condition !


 % below : conditions on admissibility
 % TOO SEVERE ? Very probably.... introduced in alp190 on account of time
 % necessary to parse 'Ausus sum in ecclesia tua concupiscere fructus mortis et agere negotium
 % procurandi eos'

 msort(PL1,PL1sorted),   % ALP 194
 msort(PL2, PL2sorted),  % ALP 194

 distancev(PL1sorted,PL2sorted,Strain),                  Strain < 2,   % distance between paths    one word max !!!

 distancev([p(H1,H2)],[p(G1,G2)],Strain2),   Strain2 < 3,   % distance between heads    two words max !!!


append(PL1,PL2,PL),
 msort(PL, Sorted),
 \+dup(Sorted),


   % relaxedadjacentX_n allows for an interruption by X words, none of them a noun

 ifthenelse( % (H2=G1;G2=H1),
             adjacent(PL1,PL2),

              Malus is 0,
              ifthenelse(relaxedadjacent1_n(H1,H2,G1,G2,n),
                         Malus is 2,
                         ifthenelse(relaxedadjacent2_n(H1,H2,G1,G2,n),
                                    Malus is 3,
                                    ifthenelse(contiguous(Sorted),
                                               Malus is 4,
                                               true)))),

 nonvar(Malus),     % one of the four conditions above must have fired !!!!!!!!!!!!!!!!!!!!!


 ifthenelse(contiguous(Sorted),Weight=3,Weight=1), % NOT an error - contiguousness is both a redeeming factor in case of distance
                 % AND a bonus factor - we work with both distance and weight


 myplus(Weight,SpecialBonus,FW),   % ALP 193
 myplus(Distnp1,Distnp2,T1),
 myplus(T1,Malus,Distance),
 % Distance is Distnp1 + Distnp2 + Malus,       % the Malus increases the straining factor

 map(np,[pathlist:Sorted, hp:[p(H1,H2)],index:i([p(H1,H2)]),distance:[Distance],
           sem:Sem,number:Nb, person:3,cat:np,lex:LexNoun,lextype:full,
           gender:G, type:full,case:C,class:common,w:FW,                     % FW : ALP193
           constituent_structure:[head:Fnp1,noun_cplt:Fnp2]])].











% APPOSITION
%%%%%%%%%%%%%


% Noun noun
%%%%%%%%%%%

% noun + np more appropriate but we would have to impose restrictions
% to be revised

[core,np26] --->
[mapped(noun,[from:A, to:B|FSnoun1]),
 mapped(noun,[from:B, to:C|FSnoun2]),

 constraint([number:N,case:Case,gender:G,sem:Sem1,lex:Lex1],FSnoun1),

 constraint([number:N,case:Case,gender:G,sem:Sem2,lex:Lex2],FSnoun2),

 \+constraint([origin:adj],FSnoun1),
 \+constraint([origin:adj],FSnoun2),
          % they cannot be read as adj

 intersection(Sem1,Sem2,[_|_]),   % check that they have sth in common

 map(np,[pathlist:[p(A,B),p(B,C)],hp:[p(A,B)],index:i(p(A,B)),distance:[0],cat:np,class:common,sem:Sem1,
          number:N,person:3,gender:G,type:core,lex:Lex1,lextype:full,
          case:Case,w:(0.5), constituent_structure:[apposition_pair,head:Lex1,head:Lex2]])].

 % low weight to leave room for noun+adjective pair when adj is readable as noun


% Noun NP
%%%%%%%%%

% the noun is a proper noun (Iugurtha rex Numidarum)

[finite,np27] --->
[mapped(noun,[from:A, to:B|FSnoun]),
 mapped(np,FSNP),

 constraint([class:proper,number:N,case:Case,gender:G,sem:Sem1,lex:Lex1],FSnoun),

 constraint([pathlist:Path,number:N,case:Case,gender:G,class:common,
             sem:Sem2,lex:Lex2, distance:Distance,w:Weight,constituent_structure:C_str],FSNP),

  \+constraint([origin:adj],FSNP),

 intersection(Sem1,Sem2,[_|_]),      % check that they have sth in common TOO WEAK ???
             % better insist on identity ???

 append([p(A,B)],Path,NPath),
 msort(NPath,Sorted),
 contiguous(Sorted),

 map(np,[pathlist:Sorted,hp:[p(A,B)],index:i(p(A,B)),distance:Distance,cat:np,class:common,sem:Sem1,
          number:N,person:3,gender:G,type:core,lex:Lex1,lextype:full,
          case:Case,w:Weight, constituent_structure:[apposition_pair,head:Lex1,apposition:C_str]])].


% NP NP
%%%%%%%


[finite,np27a] --->
[mapped(np,FSNP1),
 mapped(np,FSNP2),

 FSNP1 \= FSNP2,

  \+constraint([origin:adj],FSNP1),
   \+constraint([origin:adj],FSNP2),

 constraint([pathlist:Path1,hp:HP,index:Index,number:N,case:Case,gender:G1,class:common,
             sem:Sem1,lex:Lex1, distance:[Distance1],w:Weight1,constituent_structure:C_str1],FSNP1),

 constraint([pathlist:Path2,number:N,case:Case,gender:G2,class:common,
             sem:Sem2,lex:Lex2, distance:[Distance2],w:Weight2,constituent_structure:C_str2],FSNP2),

 Lex1 \= dummy_np, Lex2 \= dummy_np,    % dummies excluded ALP 196

 extremity(Path1,End),
 start(Path2,End),

 append(Path1,Path2,Path),
 msort(Path,Sorted),
 contiguous(Sorted),

 myplus(Distance1,Distance2,Distance),

 map(np,[pathlist:Sorted,hp:HP,index:Index,distance:[Distance],
         cat:np,class:common,sem:Sem1,
          number:N,person:3,gender:G1,type:core,lex:Lex1,lextype:full,
          case:Case,w:Weight1, constituent_structure:[apposition_pair,head:C_str1,apposition:C_str2]])].





% NP abstract + Pred in apposition
% mihi solacium est quod bona teneo
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



[finite,np28] --->
[mapped(np,HeadNP), % the head NP


 constraint([pathlist:PL1,hp:HL1,distance:[Distnp1],sem:[abstract],
            number:Nb,person:3,gender:G,case:C,lex:LexNoun], HeadNP),
 member(LexNoun,[solacium, defensio]),   % list to be established on the basis of textual data

 mapped(pred,ApposedPred), % the Pred in apposition

 ( constraint([type:finite,class:s,gap:[],argbound:no,
               pathlist:PL2, distance:[Distpred],mood:indicative, w:Wsub,constituent_structure:[quod,_]],ApposedPred);
   constraint([type:finite,class:s,gap:[],argbound:no,
               pathlist:PL2, distance:[Distpred],mood:subjunctive, w:Wsub,constituent_structure:[ut,_]],ApposedPred)
 ),



precedes(HL1,PL2), % apposition follows

append(PL1,PL2,PL),
msort(PL, Sorted),
\+dup(Sorted),
 myplus(Distnp1,Distpred,Distance),
% myplus(T1,Malus,Distance),
Weight is Wsub+5,

map(np,[pathlist:Sorted, hp:HL1,index:i(HL1),distance:[Distance],
           sem:[abstract],number:Nb, person:3,cat:np,lex:LexNoun,lextype:full,
           gender:G, type:full,case:C,class:common,w:Weight,
           constituent_structure:[head:HeadNP,apposition:ApposedPred]])].



% id .... ut/quod ....

% id habeo solacium quod bona teneo
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

[finite,np29] --->
[mapped(prpers,[from:A,to:B|FS1]),
 constraint([txt:or([id,eo]),case:C],FS1), % id or eo, nothing else

 mapped(pred,ApposedPred), % the Pred in apposition

  ( constraint([type:finite,class:s,gap:[],argbound:no,
               pathlist:PL2, distance:[Distpred],mood:indicative, w:Wsub,constituent_structure:[quod,_]],ApposedPred);
   constraint([type:finite,class:s,gap:[],argbound:no,
               pathlist:PL2, distance:[Distpred],mood:subjunctive, w:Wsub,constituent_structure:[ut,_]],ApposedPred)
  ),

precedes([p(A,B)],PL2), % apposition follows

append([p(A,B)],PL2,PL),
msort(PL, Sorted),
\+dup(Sorted),


map(np,[pathlist:Sorted, hp:[p(A,B)],index:i([p(A,B)]),distance:[Distpred],
           sem:[abstract],number:sing, person:3,cat:np,lex:id,lextype:full,
           gender:neuter, type:full,case:C,class:common,w:Wsub,
           constituent_structure:[head:id,apposition:ApposedPred]])].



















% NP WITH RELATIVE CLAUSE
%%%%%%%%%%%%%%%%%%%%%%%%%

[finite,np30] --->

[ % the NP
 mapped(np,FS1),
 mapped(relative_clause,FS2),

 constraint([pathlist:PL1,hp:HL1,distance:[Distnp1],w:Wnp],FS1),         % w ALP 193
 constraint([cat:np,index:Index,number:Nbnp,gender:Gnp,person:Pnp,
             sem:SemNP,lex:Lex, case:Case],FS1),


 ifthenelse(constraint([constituent_structure:C_str1],FS1),Head=C_str1,Head=Lex),

 % the relative clause

 constraint([number:Nb,gender:G,person:P,pathlist:PL2,distance:[Distrel], constraints:Constraints,w:Wrel],FS2), % w ALP 193

% ALP 193:
% bonus for contiguity HEAD of NP and relative pronoun:
% rex laudat reginam quae laudat ancillam quae deleverat urbem
% reginam quae and  ancillam quae rather than attachment of quae deleverat urbem to reginam quae laudat ancillam

 start(PL2, PL2start),        % start of rel clause, i.e. relative prounoun
 extremity(HL1,EndHeadNP),    % end of NP HEAD (end of whole NP: no good)
 ifthenelse(PL2start=EndHeadNP, SuperBonus is 5, Superbonus is 0),         % contiguity leads to heavy bonus



constraint([number:Nbnp],[number:Nb]),
constraint([gender:Gnp],[gender:G]),
constraint([person:Pnp],[person:P]),
    %  we apply the constraints
    % the constraint on Person will as a matter of fact only concern relatives in the nominative in subject position
    % otherwise the person slot in the relative will have been passed as an uninstantiated variable

% funify([number:Nb],[number:Nbnp],_), funify([gender:G],[gender:Gnp],_), funify([person:Pnp],[person:P],_),
      % funify safest ?????


cleanc(Constraints,CC),        % the Constraints should not include case:Case (to be removed - other constraints to be kept)
ifthen(CC\=[],constraint(CC,FS1)),

 ifthenelse( constraint([gap:[]],FS2), % relatives without gap : amo reginam [cuius librum reges legunt].
              true,     % THEN we skip the index sharing
              constraint([index:Index],FS2)
             ), % ELSE index sharing with the NP - essential to link relative and antecedent


 msort(PL1,PLnpSorted),

 extremity(PLnpSorted,X), % relaxed contiguity test np and rel clause

 Xp is X+1,
 Xpp is X+2,
 Xppp is X+3,
 Xpppp is X+4,
 Xppppp is X+5,

( (start(PL2,X), Mali is 0);
  (start(PL2,Xp), Mali is 0.5);
  (start(PL2,Xpp), Mali is 1);
  (start(PL2,Xppp), Mali is 1.5);
  (start(PL2,Xpppp), Mali is 2);
  (start(PL2,Xppppp), Mali is 2.5)   ),

nonvar(Mali),                                      % ALP 193: prevents severe non-contiguity

 append(PLnpSorted,PL2,PL),
 msort(PL, Sorted),
 \+dup(Sorted),
 quasicontiguous2(Sorted),           % strict contiguity not applicable on account of possible non-contiguity in the NP constituents
                                      % although at first sight the restriction looks reasonable... but:
                                      % 'imperatores timeo qui a pace abhorrent'
 % Distance is Distnp1+Distrel,
 % Weight is Wnp+Wrel ,                % ALP 193

 myplus(Distnp1,Distrel,DistanceA),
 myplus(DistanceA, Mali, Distance),
 myplus(Wnp,Wrel,Weight),
 myplus(Weight,SuperBonus, Wsup),     % ALP 193

 map(np,[pathlist:Sorted, hp:HL1,distance:[Distance],
           cat:np,type:full,class:common,lextype:full,
           index:Index,number:Nbnp,gender:Gnp,sem:SemNP,person:Pnp,case:Case,
           lex:Lex,w:Wsup,constituent_structure:[head:Head,rel_clause:FS2]])].  % Wsup : ALP 193





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% code below to be revised:

% rel clause precedes antecedent ; antecedent has hic... is... ipse... idem....
% as adj or boils down to one of these pronouns

% pronouns : prpers or prdem accessible through pos

% adjectives : lex value within the constituent_structure of the adjp within the np
% this lex value to be one of member(LexAdj,[hic,(is),idem,iste,ille,ipse])

% qui a pace abhorrent eos timeo
% qui a pace abhorrent eos imperatores timeo

% end of relative clause not too far from beginning of np :
% use the same requirement as above : one word in prose, two in poetry
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


[finite,np31] --->

[ % the NP
 mapped(np,FS1),
 mapped(relative_clause,FS2),

 constraint([pathlist:PL1,hp:HL1,distance:[Distnp1]],FS1),
 constraint([cat:np,index:Index,number:Nbnp,gender:Gnp,
             sem:SemNP,lex:Lex, case:Case],FS1),

  ifthenelse(constraint([constituent_structure:C_str1],FS1),Head=C_str1,Head=Lex),

 % the relative clause

 constraint([number:Nb,gender:G,pathlist:PL2,distance:[Distrel], constraints:Constraints,w:W],FS2),

 constraint([number:Nbnp],[number:Nb]), constraint([gender:Gnp],[gender:G]),

 % constraint on person does not apply here, or so it seems
 % it it should prove necessary, copy from above, where the relative follows

 constraint([index:Index],FS2), % index sharing with the NP - essential to link relative and antecedent

 cleanc(Constraints,CC),        % the Constraints should not include case:Case (to be removed - other constraints to be kept)
 ifthen(CC\=[],constraint(CC,FS1)),   % apply the constraints, e.g. semantic constraints passed on to the antecedent noun

 msort(PL1,PLnpSorted),
 msort(PL2,PLrelSorted),




 extremity(PLrelSorted,X), % contiguity test np and rel clause
 Xplus is X+1,
 Xplusplus is X+2,


 % we need a general penalty for preceding, plus specific penalties for distance !!!!!


   (start(PLnpSorted,X), Mali is 1;
    start(PLnpSorted,Xplus), Mali is 2;
    start(PLnpSorted,Xplusplus), Mali is 3),


 append(PLnpSorted,PLrelSorted,PL),
 msort(PL, Sorted),
  \+dup(Sorted),
          % contiguous(Sorted),
 % Distance is Distnp1+Distrel,
 % Weight is W+1,
 myplus(Distnp1,Distrel,DistanceA),
 myplus(DistanceA, Mali, Distance),
 myplus(W,1,Weight),
 map(np,[pathlist:Sorted, hp:HL1,distance:[Distance],
           cat:np,type:full,class:common,lextype:full,
           index:Index,number:Nbnp,gender:Gnp,sem:SemNP,person:Person,case:Case,
           lex:Lex,w:Weight,constituent_structure:[head:Head,rel_clause:FS2]])].







% CLEANC
%%%%%%%%%

% doing away with the case constraint, which concerns the relative pronoun, not the antecedent !

cleanc([],[]).

cleanc([case:_|OtherC],OtherC1) :- !,
                                  cleanc(OtherC,OtherC1).

cleanc([H|T],[H|T1]) :-           cleanc(T,T1).




% with dummy np derivable from relative clause
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% restricted to subject, direct object and indirect object:
% qui amat reginam amat ancillam
% amo quem amat regina

% case sharing does not seem to be required:
% 'do aquam cui ancilla dat pecuniam' but also:

%                                      elige cui dicas        : tu mihi sola places
%                                      haec tibi non tenues   veniet delapsa per auras
%              Ovid, Ars Amatoria, I, 42-43

% let's pay tribute :

%	déca				choisis à qui dire          : toi seule me plais
%       alexandrin                      la vois-tu tout d'en haut   te tomber dans les bras ?

% case sharing would be ensured by the use of a single variable Case in the rel clause constraints
% and the np being built - a very reasonable thing to do, but goodbye Ovid - can't bear the thought ;-)


[finite,np32] --->

[ mapped(relative_clause,FS2),
  constraint([number:Nb,gender:G,case:Case_in_Rel,pathlist:PL2,distance:[Distrel],gap:Gap,
             constraints:Constraints,w:W],FS2),
  

  start(PL2,Y),
  Yminus is Y-1,
  Gap \= [],         % exclude ubi and cum-relatives  which feature no gap

  
  

 % we have a relative clause only, NO NP; therefore
 % we check that the relative is not (immediately) preceded by a noun which could serve as antecedent

   ifthenelse( (mapped(noun,[from:Here,to:Y|Noun]), constraint([number:Nb,gender:G],Noun)),
              fail,true),
   ifthenelse( (mapped(noun,[from:Here,to:Yminus|Noun]), constraint([number:Nb,gender:G],Noun)),
              fail,true),


   constraint([case:Case_in_Rel],[case:or([nom,acc,dat,abl])]),    % restrictions on case
                % ablative should not be excluded, in fact
                                                               % quibus utitur civitas illa

   constraint([index:dummy_np],FS2),             % index sharing with the NP to be built

    
    % reasonable constraint ALP 196:
  
  constraint([case:Case_NP],[case:or([nom,acc,abl,Case_in_Rel])]),

   % abl abs : caesis qui restiterant Tac Ann

 
   cleanc(Constraints,CC),        % the Constraints should not include case:Case (to be removed - other constraints to be kept)
   ifthen(CC\=[],constraint(CC,[cat:np,type:full,class:common,index:_,
                                number:Nb,gender:G,sem:SemNP,person:3])),
   % register the constraints, e.g. semantic constraints passed on to the antecedent noun
   

   % mapping the NP:
   map(np,[pathlist:PL2,hp:[],distance:[Distrel],
             cat:np,type:full,class:common,
             index:dummy_np,number:Nb,gender:G,sem:SemNP,person:3,case:Case_NP,     % lex value enables the parser
                      % to spot this fabricated NP
             lex:dummy_np,w:W,
             constituent_structure:[head:[lex:dummy_np, number:Nb,gender:G,person:3,index:_,
                                    constraints_to_be_met:Constraints],         % specified for the sake of the reader of the parse tree
                    rel_clause:FS2]])].


% SUNT QUI etc
%%%%%%%%%%%%%%

[finite,np32a] --->

 [mapped(v,[from:0, to:1|FSverb]),
  constraint([type:finite, lex:esse,tense:Tense, mood:indicative,number:Nb,person:3],FSverb),
  mapped(np,[pathlist:PL2,hp:[],distance:[Distrel],
             cat:np,type:full,class:common,
             index:dummy_np,number:Nb,gender:G,sem:SemNP,person:3,case:Case_NP,     % lex value enables the parser
                      % to spot this fabricated NP
             lex:dummy_np,w:W,
             constituent_structure:[head:[lex:dummy_np, number:Nb,gender:G,person:3,index:_,
                          constraints_to_be_met:Constraints],         % specified for the sake of the reader of the parse tree
                    rel_clause:FS2]]),
 start(PL2,1),
 append([p(0,1)],PL2,PL),
 myplus(W,10,Weight),
 map(pred,[cat:pred,
              type:finite,
              pathlist:PL,
              distance:[Distrel],
              illocutionary_force:statement,
              class:m,
              number:sing,                      % nber, gender, and person of the CLAUSE, not its subject
              person:3,
              gender:neuter,
              mood:indicative,
              tense:Tense,
              polarity:pos,
              argbound:no,
              gap:[],
              w:Weight,

              flagint:_,
              constituent_structure:[illocutionary_force:existential,vg:FSverb|FS2]])].

























%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PPS  Prepositional Phrases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% restriction : the preposition precedes the ***head*** of the np
% magno sub tegmine OK
% sub deae bonae tegmine OK

% all pps must wait for the 'finite' run on account of the np they include


% prep precedes NP
%%%%%%%%%%%%%%%%%%

% the prep sports 'pre' as value for the 'type' feature

% lex(in,prep,[lex:in,pos:prep, requires:acc,gerund:no,type:pre]).

% CORE OR FINITE ?
% finite if we want 'vacuos ab ira atque misericordia',
% i.e. a conjoined np within a pp
% test sentence : Omnes homines qui de rebus dubiis consultant ab ira atque misericordia vacuos esse decet.
% TEST VERIFIED

% IN CORE
%%%%%%%%%

[core,pp1a] --->
[mapped(prep,[from:A, to:B|Fprep]),
 mapped(np,Fnp),

 constraint([sem:Sem,lex:LexNoun, index:Inp,w:W],Fnp),

 constraint([requires:Case, lex:Lexprep,type:pre],Fprep),  % case constraint on np

 constraint([hp:[p(Begin,End)],pathlist:P,distance:[Dist],case:Case],Fnp), % it is the HEAD that we worry about 'magna cum cura'
 constraint([type:Type],Fnp),

 Begin < B+4,


  ifthen(Begin < B, Malus is 1),
  ifthen(Begin=B,Malus is 0),
  ifthen(Begin is B+1,Malus is 3),
  ifthen(Begin is B+2, Malus is 5),
  ifthen(Begin is B+3, Malus is 6),


  ifthen(Begin < B,( member(p(B,_),P) , End=A)) ,   % End=A possible in poetry 'rebus in arduis'
                                                    % we require a bit of the NP after the prep

  ifthenelse(End=A, Nmalus is Malus+3, Nmalus=Malus),


 append([p(A,B)],P,PP),
 msort(PP, Sorted),
 Weight is W-Nmalus,
 NewDis is Dist+Malus,

 map(pp,[pathlist:Sorted,hp:[p(Begin,End)],distance:[NewDis],index:Inp,
           case:Case,prep:Lexprep, sem:Sem,lex:LexNoun,
           w:Weight,
           type:Type,
              % the type is the NP type, not the prep's
              % it will be available for checks on the NP within the PP
              % for instance in adjuncts
              % (Type registers whether the NP is interrogative)
           cat:pp,constituent_structure:[prep:Lexprep,head:Fnp]])].




% IDEM IN FINITE RUN
%%%%%%%%%%%%%%%%%%%%


[finite,pp1b] --->
[mapped(prep,[from:A, to:B|Fprep]),
 mapped(np,Fnp),

 constraint([sem:Sem,lex:LexNoun, index:Inp,w:W],Fnp),

 constraint([requires:Case, lex:Lexprep,type:pre],Fprep),  % case constraint on np

 constraint([hp:[p(Begin,End)],pathlist:P,distance:[Dist],case:Case],Fnp), % it is the HEAD that we worry about 'magna cum cura'
 constraint([type:Type],Fnp),

 Begin < B+3,

    ifthen(Begin < B, Malus is 5),
  ifthen(Begin=B,Malus is 0),
  ifthen(Begin is B+1,Malus is 3),
  ifthen(Begin is B+2, Malus is 5),
%   ifthen(Begin is B+3, Malus is 6),


   ifthen(Begin < B,( member(p(B,_),P) , End=A)) ,   % End=A possible in poetry 'rebus in arduis'
                                                    % we require a bit of the NP after the prep

  ifthenelse(End=A, Nmalus is Malus+10, Nmalus=Malus),

 append([p(A,B)],P,PP),
 msort(PP, Sorted),
 Weight is W-Nmalus,
  NewDis is Dist+Malus,

 map(pp,[pathlist:Sorted,hp:[p(Begin,End)],distance:[NewDis],index:Inp,
           case:Case,prep:Lexprep, sem:Sem,lex:LexNoun,
           w:Weight,
           type:Type,
              % the type is the NP type, not the prep's
              % it will be available for checks on the NP within the PP
              % for instance in adjuncts
              % (Type registers whether the NP is interrogative)
           cat:pp,constituent_structure:[prep:Lexprep,head:Fnp]])].





% prep can follow NP : causa
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% the 'post' value need not be assigned to the prep
% it can be satisfied by feature unification with an or-value which has 'post' as a component
% as in the case of 'causa' (qv.)

% lex(causa,prep,[lex:causa,pos:prep,requires:gen,gerund:yes,type:or([pre,post])]).


[finite,pp2] --->
[mapped(prep,[from:A, to:B|Fprep]),
 mapped(np,Fnp),
 constraint([sem:Sem,lex:LexNoun, index:Inp,w:W],Fnp),
 constraint([requires:Case, lex:Lexprep,type:post],Fprep),
 constraint([hp:[p(Begin,End)],pathlist:P,distance:Dist,case:Case,index:Inp],Fnp),
 extremity(P,A),            % preposition follows NP
 append(P,[p(A,B)],PP),
 msort(PP, Sorted),
 map(pp,[pathlist:Sorted,hp:[p(Begin,End)],distance:Dist,index:Inp,
           case:Case,prep:Lexprep, sem:Sem,lex:LexNoun,
           w:W,
           cat:pp,constituent_structure:[prep:Lexprep,head:Fnp]])].

% we do not bother about interrogatives - they are not likely to occur within an NP attached to causa - or are they?





% special case : urbis capiendae causa : gerund with post-NP causa
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

[finite,pp3] --->
[mapped(prep,[from:A, to:B|Fprep]),
 constraint([type:post,requires:Case,lex:Lexprep],Fprep),

 mapped(pred,Pred),   % gerunds (and gerunds substituted by gerundives) are PREDS

 constraint([pathlist:PathlistPred,distance:Dist,w:W, type:gerund, case:gen],Pred),  % case is recorded in such preds
                         % and must be genitive here
 extremity(PathlistPred,A),    % prep follows
 append(PathlistPred,[p(A,B)],PP),
 msort(PP, Sorted),
 map(pp,[pathlist:Sorted,hp:[p(A,B)],distance:Dist,index:_,
           case:Case,prep:Lexprep, sem:_,lex:Lexprep,
           w:W,type:gerund,   % the type of the pp is relevant in arg structure specification
           cat:pp,constituent_structure:[prep:Lexprep,head:Pred]])].






% preps with gerund(ive) : in, ad, pre-NP causa
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

[finite,pp4] --->
[mapped(prep,[from:A, to:B|Fprep]),
 constraint([requires:Case, lex:Lexprep, gerund:yes, type:pre],Fprep),  % prep precedes

 mapped(pred,Pred),
 constraint([pathlist:PathlistPred,distance:Dist,w:W, type:gerund, case:Case],Pred),  % case as required by prep

% start(PathlistPred,B),      % preD follows - careful : augendam ad invidiam (Tacitus)
 append([p(A,B)],PathlistPred,PP),
 msort(PP, Sorted),
 contiguous(Sorted),
 myplus(W,10,Wtot), % bonus for pred as opposed to noun+adj nexus
 map(pp,[pathlist:Sorted,hp:[p(A,B)],distance:Dist,index:_,
           case:Case,prep:Lexprep, sem:_,lex:Lexprep,
           w:Wtot,type:gerund,
           cat:pp,constituent_structure:[prep:Lexprep,head:Pred]])].




















% ADJUNCTS
%%%%%%%%%%%

% These structures are housed in the np part of the grammar because they are often built out of an np.
% But what is of interest is the role they play in clause extensions, i.e. as structures belonging to the clause
% without being part of the arg structure of a predicate

% Note that PPs also cover a good deal of the ground assignable to adjuncts
% we should try to avoid the double-reading of pps as both pps AND adjuncts


% TIME DURATION
%%%%%%%%%%%%%%%%

% Tres annos regnavit.
% Quantos annos regnavit ?

[core,adjunct1] --->
[mapped(noun,[from:X, to:Y|FSnoun]),
 mapped(adj,[from:A, to:B|FSadj]),
 ifthen(constraint([type:int],FSadj), Type=int),  % we use the type feature here to register the
              % declarative or interrogative nature of the structure
                                                  % Type is left as an uninstantiated variable in declaratives

 constraint([number:Nb,gender:Gender,case:acc,sem:[time],lex:Lexnoun],FSnoun),  % a time noun + accusative case
 constraint([number:Nb,gender:Gender,case:acc, lex:LexAdj],FSadj),              % agreement check
 adjacent([p(A,B)],[p(X,Y)]),
 append([p(A,B)],[p(X,Y)],Path),
 msort(Path, Sorted),

 map(adjunct,[pathlist:Sorted,hp:[p(X,Y)],distance:[0],
               cat:np,class:adjunct,value:time,         % the 'value' feature can be used in clause expansions
               number:Nb,person:3,gender:Gender,lex:Lexnoun,lextype:full,
               type:Type,
               case:acc,w:2,
               constituent_structure:[head:[lex:Lexnoun, sem:time_duration,cat:np,number:Nb,gender:Gender,case:acc,index:i(p(X,Y))]],
                             adj:LexAdj])].


% Quamdiu regnavit?

[core,adjunct2] --->
[ mapped(adv,[from:A, to:B|FSadv]),
  constraint([type:int,sem:duration, lex:LexAdv],FSadv),      % both interrogative and duration-oriented

  map(adjunct,[pathlist:[p(A,B)],hp:[p(A,B)],distance:[0],
                cat:advp,class:adjunct,value:time,
                lex:LexAdv,lextype:full,type:int,             % we know this is an interrogative adjunct
                w:2,constituent_structure:[lex:LexAdv, sem:time_duration,cat:advp]])].


% prep ab in adjuncts : to be narrowed down to relevant cases
% ab initio, a principio

[core,adjunct3] --->
[mapped(pp,FSpp),
 constraint([pathlist:PL,distance:Dist,
           case:abl,prep:ab,lex:or([principium,initium]),
           constituent_structure:C_str], FSpp),

map(adjunct,[pathlist:PL,distance:Dist,class:adjunct,sem:time_origin,value:time,type:_,
                w:2,constituent_structure:C_str])].



% TIME WHEN
%%%%%%%%%%%%

% Hac hora veniet.  Aestivo.........tempore : no adjacency required !!!

[core,adjunct4] --->
[mapped(noun,[from:X, to:Y|FSnoun]),
 mapped(adj,[from:A, to:B|FSadj]),
 constraint([number:Nb,gender:Gender,case:abl,sem:[time],lex:Lexnoun],FSnoun),  % time noun + ablative
 constraint([number:Nb,gender:Gender,case:abl, lex:LexAdj],FSadj),
 ifthen(constraint([type:int],FSadj), Type=int),
 % adjacent([p(A,B)],[p(X,Y)]),

 distance([p(A,B)],[p(X,Y)],Distance),    % the distance between the noun and the path of the adjective
                                      % determines the straining factor as well as helping to decide
              % whether noun and adj DO belong together


 Distance < 4,                      % 3 is thus the maximum distance
            % between adj and noun

 % we still have to exclude the occurrence, within the gap, of nouns to which the adjective could be
 % attached with priority, because they agree in the well-mapped agreement triplet

 ifthen(Distance=3, relaxedadjacent3_cgn([p(X,Y)],[p(A,B)],abl,Gender,Nb)),
                               % three in between, neither of them a noun with relevant triplet
 ifthen(Distance=2, relaxedadjacent2_cgn([p(X,Y)],[p(A,B)],abl,Gender,Nb)),
                               % two in between, neither of them a noun with relevant triplet
 ifthen(Distance=1, relaxedadjacent1_cgn([p(X,Y)],[p(A,B)],abl,Gender,Nb)),
                               % one in between, not a noun with same [gender,number,case] triplet



 append([p(A,B)],[p(X,Y)],Path),
 msort(Path, Sorted),
 map(adjunct,[pathlist:Sorted,hp:[p(X,Y)],distance:[Distance],                             % CORR 188
                cat:np,class:adjunct,value:time,
                number:Nb,person:3,gender:Gender,lex:Lexnoun,lextype:full,type:Type,
                case:abl,w:2,
                constituent_structure:[head:[lex:Lexnoun, sem:time_when,cat:np,number:Nb,gender:Gender,case:acc,index:i(p(X,Y))]],
                                 adj:LexAdj])].


% Quando veniet ?

[core,adjunct5] --->
[ mapped(adv,[from:A, to:B|FSadv]),
  constraint([type:int,sem:when, lex:LexAdv],FSadv),     % interrogative and time-when
  map(adjunct,[pathlist:[p(A,B)],hp:[p(A,B)],distance:[0],
                 cat:advp,class:adjunct,value:time,
                 lex:LexAdv,lextype:full,type:int,w:2,
                 constituent_structure:[lex:LexAdv, sem:time_when,cat:advp]])].


% Domi militiaeque diripere domos nolebat.

% Phrases with the right value for the 'value' feature
% e.g. string(phrase,[domi,militaeque],[lex:domi_militaeque,w:1, value:time]).

[core,adjunct_time_phrase] --->
[mapped(phrase,FSphrase),
 constraint([value:time, pathlist:PL],FSphrase),
 map(adjunct,[pathlist:PL,distance:[0],class:adjunct,value:time,type:time,
                w:2,constituent_structure:FSphrase])].



% PLACE ADJUNCTS
%%%%%%%%%%%%%%%%

% Locatives : sum Romae.

[core,adjunct6] --->
[mapped(noun,[from:X, to:Y|FSnoun]),
 constraint([number:Nb,gender:Gender,case:gen,sem:[city],lex:Lexnoun],FSnoun),
                                           % genitive as locative for city names

map(adjunct,[pathlist:[p(X,Y)],hp:[p(X,Y)],distance:[0],
               cat:np,class:adjunct,value:place,
               number:Nb,person:3,gender:Gender,lex:Lexnoun,lextype:full,type:place,
               case:gen,w:2,
               constituent_structure:[head:[lex:Lexnoun, sem:location,cat:np,number:Nb,gender:Gender,case:gen,index:i(p(X,Y))]]])].

% Ubi sunt ?

[core,adjunct7] --->
[ mapped(adv,[from:A, to:B|FSadv]),
  constraint([type:int,sem:position, lex:LexAdv],FSadv),
  map(adjunct,[pathlist:[p(A,B)],hp:[p(A,B)],distance:[0],
                 cat:advp,class:adjunct,value:place,
                 lex:LexAdv,lextype:full,type:int,w:2,
                 constituent_structure:[lex:LexAdv, sem:location,cat:advp]])].

% Sunt domi.

[core,adjunct8] --->
[mapped(phrase,FSphrase),
 constraint([value:loc, pathlist:PL],FSphrase),
 map(adjunct,[pathlist:PL,distance:[0],class:adjunct,value:place,type:place,
                w:2,constituent_structure:FSphrase])].

/*

Covered by PP

% Sunt in Italia

[finite,adjunct9] --->
[mapped(pp,FSpp),
 constraint([pathlist:PL,distance:Dist,
           prep:or([in,ex,ab,per,inter,intra,sub,trans,apud,circum]),sem:Sem,
           constituent_structure:C_str], FSpp),
(member(loc,Sem); member(city,Sem)), % semantic pair associated with locations

map(adjunct,[pathlist:PL,distance:Dist,class:adjunct,sem:place,value:place,type:_,
                w:2,constituent_structure:C_str])].

*/

% PURPOSE AND REASON
%%%%%%%%%%%%%%%%%%%%

% Misit legatum ad milites hortandos.
% pps belong to the finite pass


% registered as adjunct because ad+np is NOT included in prep phrases recorded as prep phrase adjuncts

[finite,adjunct10] --->
[mapped(pp,FSpp),
 constraint([pathlist:PL,distance:Dist,
           case:acc,prep:ad,
           type:gerund,                  % this is the prepositional phrase treatment
                                         % it might duplicate a pred treatment
           constituent_structure:C_str], FSpp),

map(adjunct,[pathlist:PL,distance:Dist,class:adjunct,value:purpose,type:purpose,
                w:2,constituent_structure:C_str])].



% genitive gerundives (without 'causa')  (and gerunds???)
%%%%%%%%%%%%%%%%%%%%%

% 'Pacis petendae oratores ad consulem miserunt' (T.L) /
% 'Germanicus Aegyptum proficiscitur cognoscendae antiquitatis' (Tacitus)
% the procedure is costly as it detaches genitive gerundives from their anchor points
% their very existence is debatable (see Benveniste)

[finite,adjunct11] --->
[mapped(pred,FSpred),
 constraint([pathlist:PL,distance:Dist,
           local_case:gen, w:WeightPred,
           type:gerund,subtype:gerundive,               %  there are also subordinate clauses of purpose dealt with on their own  !
           constituent_structure:C_str], FSpred),     %  here we deal only with gerunds

 NW is WeightPred - 2,

 map(adjunct,[pathlist:PL,distance:Dist,class:adjunct,value:purpose,type:purpose,
                w:NW,constituent_structure:C_str])].

  % since we have a pred, we copy over the pred's weight (but somewhat downgraded) rather than assign weight 2 without further ado



% Cur misit epistulam ?

[core,adjunct12] --->
[ mapped(adv,[from:A, to:B|FSadv]),
  constraint([type:int,sem:reason, lex:LexAdv],FSadv),
  map(adjunct,[pathlist:[p(A,B)],hp:[p(A,B)],distance:[0],cat:advp,class:adjunct,value:purpose,
          lex:LexAdv,lextype:full,type:int,
          w:2,constituent_structure:[lex:LexAdv, sem:reason,cat:advp]])].


% MANNER AND MEANS
%%%%%%%%%%%%%%%%%%

% Libros legendo licet insanire.
% needs to wait for the finite pass on account of the pred

[finite,adjunct13] --->
[mapped(pred,FSpred),
 constraint([cat:pred,type:gerund,local_case:abl,mood:gerund,
             number:sing,person:3,gender:neuter,
             pathlist:PL, distance:Dist,w:WeightPred,
             constituent_structure:C_str], FSpred),

map(adjunct,[pathlist:PL,distance:Dist,
               class:adjunct,value:manner_means,
                type:manner_means,
                w:WeightPred,constituent_structure:C_str])].
% the weight accumulated in the pred is copied over rather than assign standard weight for adjuncts, to wit 2


% Quomodo vicit hostes ?

[core,adjunct14] --->
[ mapped(adv,[from:A, to:B|FSadv]),
  constraint([type:int,sem:manner_means, lex:LexAdv],FSadv),
  map(adjunct,[pathlist:[p(A,B)],hp:[p(A,B)],distance:[0],
                 cat:advp,class:adjunct,value:manner_means,
                 lex:LexAdv,lextype:full,type:int,w:2,
                 constituent_structure:[lex:LexAdv, sem:manner_means,cat:advp]])].


% dealt with as adverbial phrase

/*
% grauiter angi

%  advp,[cat:advp,pathlist:[p(A,B)],distance:[0],type:Type,lex:Lex,sem:Sem,w:1])].

[core,adjunct15] --->
[ mapped(advp,[from:A, to:B|FSadvp]),
  constraint([type:vpbound,sem:manner_means, lex:LexAdv],FSadvp),
  map(adjunct,[pathlist:[p(A,B)],hp:[p(A,B)],distance:[0],
                 cat:advp,class:adjunct,value:manner_means,
                 lex:LexAdv,lextype:full,type:_,w:2,
                 constituent_structure:[lex:LexAdv, sem:manner_means,cat:advp]])].
*/

% 'de integro'
% as an example of frozen mwu

[core,adjunct16] --->
[mapped(phrase,FSphrase),
 constraint([value:manner_means, pathlist:PL],FSphrase),
 map(adjunct,[pathlist:PL,distance:[0],class:adjunct,value:manner_means,type:manner_means,
                w:2,constituent_structure:FSphrase])].



% Various types of ablative nps - we check the feature on the np lex : ab:loc / ab:time / ab:mm (manner-means)
% make it tighter before applying



[finite,adjunct17] --->

[mapped(np,NP),
 constraint([pathlist:PL,lextype:full,type:Type,distance:Dist,
           case:abl,lex:Lex,constituent_structure:C_str, w:Weight],NP),
% Weight <3, % cannot be heavy if built as an adjunct
            % heavier weights are best parsed as aa if appropriate
            % but see the parses of 'Missis ad Vitellium litteris auxilium postulat.' (Tacitus Hist III 41 1)
 lex(Lex,noun,FSnoun),
 constraint([ab:Ab],FSnoun),
 ab(Lex),                         % on the basis of textual data
 (Ab=loc;Ab=time;Ab=mm),
 ifthen(Ab=loc,About=place),
 ifthen(Ab=time,About=time),
 ifthen(Ab=mm, About=manner_means),
 ifthen(Lex=urbs,PL=[One,Two|R]),    % not 'urbe' alone tota or media urbe are OK
 ifthen(Ab=time,PL=[One,Two|R]),     % single time words to be entered as time adverbs  % ALP 194
 map(adjunct,[pathlist:PL,distance:Dist,class:adjunct,value:About,type:Type,w:0.5,constituent_structure:C_str])]. % 0.5 ALP 197






















%%%%%%%%%%%%
% VERB GROUP
%%%%%%%%%%%%

% we keep track of polarity (also for clausal constraints - e.g. non pili facere)
% we only take 'non' as a vg negation (an oversimplification)
% the other negations are deemed to be clause-type-bound, i.e. can affect any clause constituent


% positive polarity
%

% we copy the info from the verb form
% to the verb group

% lex(amabunt, v, [pos:v, class:tr_cod, type:finite, lex:amare, voice:act, txt:amabunt,
%                  tense:future, kind:std, mood:indicative, number:pl, person:3]).


% standard, one-word units
%%%%%%%%%%%%%%%%%%%%%%%%%%

[verb,vg1] --->
[ mapped(v,[from:B, to:C|FS1]),
  ifthen(constraint([gender:Gender],FS1),G=Gender), % we have a gender, we record it
                      % if we don't, we leave the G var a free var
                % this prevents failure in the case of a gender check
   map(vgpos,[cat:vg,pathlist:[p(B,C)],hp:[p(B,C)], gender:G,w:0|FS1])].

  % computing weight is necessary to prioritize verb groups with esse as compared with esse + predicative
  % passive voice, for instance: amatus est





% two-word units
%%%%%%%%%%%%%%%%%


% aux + verb
%%%%%%%%%%%%

 [verb,vg2] --->
[ mapped(v,[from:X, to:Y|FSaux]),
  constraint([lex:LexAux,tense:Tense,type:Type,
              mood:Mood,number:Nb,person:P],FSaux),
  aux(LexAux,value:Value),

  mapped(v,[from:C, to:D|FSverb]),
   % adjacent([p(X,Y)],[p(C,D)]),      % both orders : possunt scribere, scribere possunt
                                     % also possunt carmina....... scribere
                                     % scribere carmina possunt
  append([p(X,Y)],[p(C,D)],Path),
   % msort(Path, Sorted),             % put the aux in front of path, whatever its position

  constraint([lex:LexVerb,mood:infinitive,voice:Voice],FSverb),  % making sure we have an infinitive

  map(vgpos,[cat:vg,aux:LexAux,value:Value,type:Type,pathlist:Path,hp:[p(C,D)],lex:LexVerb,
               person:P,mood:Mood,tense:Tense, voice:Voice,           % active voice
               number:Nb,gender:G,w:3])].




% deponents : active voice
%%%%%%%%%%%%%%%%%%%%%%%%%%

% hortatus sum eram ero sim essem
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% lex(sumus, v, [pos:v, class:v_esse, type:finite, lex:esse, txt:sumus,
%                tense:present, kind:std, mood:indicative, number:pl, person:1]).

% lex(hortatus, p_p, [pos:p_p, txt:hortatus, case:nom, gender:masc, number:sing,
%     lex:hortari, class:tr_cod, type:p_p, kind:dep, mood:participle, person:3]).


[verb,vg3] --->
[ mapped(v,[from:X, to:Y|FSverb]),
  constraint([type:finite, lex:esse,tense:Tense,
              mood:Mood,number:Nb,person:P],FSverb),
  mapped(p_p,[from:C, to:D|FSpp]),
  adjacent([p(X,Y)],[p(C,D)]),      % both orders : sum hortatus, hortatus sum
  append([p(X,Y)],[p(C,D)],Path),
  msort(Path, Sorted),
  constraint([case:nom,number:Nb,gender:G,lex:Lex,kind:dep],FSpp),  % making sure we have a deponent verb
  ifthen(Tense=present,Tenseout=perfect),
  ifthen(Tense=imperfect,Tenseout=pluperfect),
  ifthen(Tense=future,Tenseout=future_perfect),

  map(vgpos,[cat:vg,type:finite,pathlist:Sorted,hp:[p(C,D)],lex:Lex,
               person:P,mood:Mood,tense:Tenseout, voice:act,           % active voice
               number:Nb,gender:G,w:5])].


% with 'est' or 'sunt' understood
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

[verb,vg4] --->
[ mapped(p_p,[from:C, to:D|FSpp]),
  constraint([case:nom,number:Nb,gender:G,lex:Lex,kind:dep],FSpp),   % dep as opposed to std (standard versus deponent)
  map(vgpos,[cat:vg,type:finite,pathlist:[p(C,D)],hp:[p(C,D)],lex:Lex,
            person:3,mood:indicative,tense:perfect, voice:act,number:Nb,gender:G,w:3])].   % perfect tense


% secutum esse
%%%%%%%%%%%%%%

[verb,vg5] --->
[ mapped(v,[from:X, to:Y|FSverb]),
  constraint([txt:esse],FSverb),
  mapped(p_p,[from:C, to:D|FSpp]),
  adjacent([p(X,Y)],[p(C,D)]),
  append([p(X,Y)],[p(C,D)],Path),
  msort(Path, Sorted),
  constraint([case:acc,number:Nb,gender:G,lex:Lex,kind:dep],FSpp),   % the -UM etc forms

  map(vgpos,[cat:vg,type:nonfinite,pathlist:Sorted,hp:[p(C,D)],lex:Lex,
               person:_,mood:infinitive,tense:perfect,
               voice:act,number:Nb,gender:G,w:3])].   % active voice


% true passives
%%%%%%%%%%%%%%%

% amatus sum eram ero sim essem

[verb,vg6] --->
[ mapped(v,[from:X, to:Y|FSverb]),
  constraint([type:finite, lex:esse,txt:Text,tense:Tense, mood:Mood,number:Nb,person:P],FSverb),
  ifthen(Text=es,Mood=indicative),  % reject 'es' as imperative
  mapped(p_p,[from:C, to:D|FSpp]),
  adjacent([p(X,Y)],[p(C,D)]),
  append([p(X,Y)],[p(C,D)],Path),
  msort(Path, Sorted),
  constraint([case:nom,number:Nb,gender:G,lex:Lex,kind:std],FSpp),   % std as opposed to dep (standard versus deponent)
  ifthen(Tense=present,Tenseout=perfect),
  ifthen(Tense=imperfect,Tenseout=pluperfect),
  ifthen(Tense=future,Tenseout=future_perfect),

  map(vgpos,[cat:vg,type:finite,pathlist:Sorted,hp:[p(C,D)],lex:Lex,
            person:P,mood:Mood,tense:Tenseout, voice:pass,number:Nb,gender:G,w:20])].   % passive voice


% with 'est' or 'sunt' understood
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

[verb,vg7] --->
[ mapped(p_p,[from:C, to:D|FSpp]),
  constraint([case:nom,number:Nb,gender:G,lex:Lex,kind:std],FSpp),   % std as opposed to dep (standard versus deponent)
  map(vgpos,[cat:vg,type:finite,pathlist:[p(C,D)],hp:[p(C,D)],lex:Lex,
            person:3,mood:indicative,tense:perfect, voice:pass,number:Nb,gender:G,w:5])].   % passive voice


% amatum esse
%%%%%%%%%%%%%%

[verb,vg8] --->
[ mapped(v,[from:X, to:Y|FSverb]),
  constraint([txt:esse],FSverb),
  mapped(p_p,[from:C, to:D|FSpp]),
  adjacent([p(X,Y)],[p(C,D)]),
  append([p(X,Y)],[p(C,D)],Path),
  msort(Path, Sorted),
  constraint([case:acc,number:Nb,gender:G,lex:Lex,kind:std],FSpp),

  map(vgpos,[cat:vg,type:nonfinite,pathlist:Sorted,hp:[p(C,D)],lex:Lex,
            person:_,mood:infinitive,tense:perfect, voice:pass,number:Nb,gender:G,w:5])].   % passive voice


% with 'esse' understood
%%%%%%%%%%%%%%%%%%%%%%%%

[verb,vg9] --->
[ mapped(p_p,[from:C, to:D|FSpp]),
  constraint([case:acc,number:Nb,gender:G,lex:Lex,kind:std],FSpp),

  map(vgpos,[cat:vg,type:nonfinite,pathlist:[p(C,D)],hp:[p(C,D)],lex:Lex,
            person:3,mood:infinitive,tense:perfect, voice:pass,number:Nb,gender:G,w:5])].   % passive voice


% middle voice
%%%%%%%%%%%%%%

% we introduce middle voice to account for passive forms of intransitive verbs
% and a special use of the passive with transitives

% pugnatum est
%%%%%%%%%%%%%%

[verb,vg10] --->
[ mapped(v,[from:X, to:Y|FSverb]),
  constraint([lex:esse,type:Type, person:3, number:sing, tense:Tense,mood:Mood],FSverb),       % esse
  mapped(v,[from:C, to:D|Supine]),          % supine verb form
  adjacent([p(X,Y)],[p(C,D)]),
  append([p(X,Y)],[p(C,D)],Path),
  msort(Path, Sorted),
  constraint([type:supine,lex:Lex,kind:std],Supine),
 %   constraint([txt:coeptum],Supine) ),                % does not apply to deponent verbs except coepi
  ifthen(Tense=present,Tenseout=perfect),
  ifthen(Tense=imperfect,Tenseout=pluperfect),
  ifthen(Tense=future,Tenseout=future_perfect),

  map(vgpos,[cat:vg,type:Type,pathlist:Sorted,hp:[p(C,D)],lex:Lex,
               person:3,mood:Mood,tense:Tenseout,
               voice:middle,number:sing,gender:neuter,w:3])].           % middle voice


% pugnatum
%%%%%%%%%%

% understood: (est)

[verb,vg11] --->
[ mapped(v,[from:A, to:B|Supine]),
          % supine verb form

   constraint([type:supine,lex:Lex,kind:std],Supine),
  %  constraint([txt:coeptum],Supine) ),                % does not apply to deponent verbs except coepi

  map(vgpos,[cat:vg,type:finite,pathlist:[p(A,B)],hp:[p(A,B)],lex:Lex,
               person:3,mood:indicative,tense:perfect,
               voice:middle,number:sing,gender:neuter,w:3])].           % middle voice



% with gerund
%%%%%%%%%%%%%

% insaniendum est (also with dep : hortandum est)

[verb,vg12] --->
[ mapped(v,[from:X, to:Y|FSverb]),
  constraint([lex:esse,type:Type, person:3, number:sing,tense:Tense,mood:Mood],FSverb),       % esse
  mapped(v,[from:C, to:D|Gerund]),          % gerund
  adjacent([p(X,Y)],[p(C,D)]),
  append([p(X,Y)],[p(C,D)],Path),
  msort(Path, Sorted),
  constraint([case:acc,type:gerund,lex:Lex,kind:_],Gerund),          % accusative form of the gerund
  map(vgpos,[cat:vg,type:Type,pathlist:Sorted,hp:[p(C,D)],lex:Lex,
               person:3,mood:Mood,tense:Tense,
               voice:middle,number:sing,gender:neuter,                  % middle voice
               value:obligation,w:3])].                                 % semantic force : obligation



% infinitive future : hortaturum esse, amaturum esse
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% active voice : dealt with the same way for both deponent and standard verbs

[verb,vg13] --->
[ mapped(v,[from:X, to:Y|FSverb]),
  constraint([txt:esse],FSverb),        % esse
  mapped(p_f,[from:C, to:D|FSpf]),       % future participle
  adjacent([p(X,Y)],[p(C,D)]),
  append([p(X,Y)],[p(C,D)],Path),
  msort(Path, Sorted),
  constraint([case:acc,number:Nb,gender:G,lex:Lex],FSpf),  % accusative

  map(vgpos,[cat:vg,type:nonfinite,pathlist:Sorted,hp:[p(C,D)],lex:Lex,
               person:_,mood:infinitive,tense:future,
               voice:active,number:Nb,gender:G,w:15])].                 % heavy weight to counteract interpretation as a participial clause

% periphrastic future
%%%%%%%%%%%%%%%%%%%%%

% lex(sum, v, [pos:v, class:v_esse, type:finite, lex:esse, voice:act, txt:sum, tense:present, kind:std, mood:indicative, number:sing, person:1]).

[verb,vg14] --->
[ mapped(v,[from:X, to:Y|FSverb]),
  constraint([type:finite,number:Nb,lex:esse, mood:Mood,person:Person],FSverb),        % esse: finite forms : sum etc
  mapped(p_f,[from:C, to:D|FSpf]),       % future participle
  adjacent([p(X,Y)],[p(C,D)]),
  append([p(X,Y)],[p(C,D)],Path),
  msort(Path, Sorted),
  constraint([case:nom,number:Nb,gender:G,lex:Lex],FSpf),  % nominative

  map(vgpos,[cat:vg,type:finite,pathlist:Sorted,hp:[p(C,D)],lex:Lex,
               person:Person,mood:Mood,tense:periphrastic_future,
               voice:active,number:Nb,gender:G,w:10])].


% with esse understood
%%%%%%%%%%%%%%%%%%%%%%

[verb,vg15] --->
[ mapped(p_f,[from:B, to:C|FSpf]),
  constraint([case:acc,number:Nb,gender:G,lex:Lex],FSpf),

  map(vgpos,[cat:vg,type:nonfinite,pathlist:[p(B,C)],hp:[p(B,C)],
               lex:Lex,
               person:_,mood:infinitive,tense:future,
               voice:active,number:Nb,gender:G,w:15])].                % same heavy weight, the ESSE being very frequently omitted


% FORE is entered as such in the lexicon:
% lex(fore, v,
%      [pos:v, class:v_esse, type:nonfinite, lex:esse, voice:act, txt:fore,
%       tense:future, kind:std, mood:infinitive, number:_, person:_]).

% and will be turned into a verb group by the general procedure described at the top of this section


% future passive
%%%%%%%%%%%%%%%%

% same procedure for iri


[verb,vg16] --->
[ mapped(v,[from:X, to:Y|FSverb]),
  constraint([txt:iri],FSverb),        % iri
  mapped(p_p,[from:C, to:D|FSpp]),       % past participle
  constraint([case:acc,number:sing,gender:neuter,lex:Lex],FSpp),  % accusative neuter sg (amatum)

  adjacent([p(X,Y)],[p(C,D)]),
  append([p(X,Y)],[p(C,D)],Path),
  msort(Path, Sorted),

  map(vgpos,[cat:vg,type:nonfinite,pathlist:Sorted,hp:[p(C,D)],lex:Lex,
               person:_,mood:infinitive,tense:future,
               voice:pass,number:_,gender:_,w:3])].



% subjunctive future
%%%%%%%%%%%%%%%%%%%%

% periphrastic / used in indirect questions
% ... quid dicturus sit /esset

[verb,vg17] --->
[ mapped(v,[from:X, to:Y|FSverb]),
  constraint([lex:esse, number:Nb, person:Person,mood:subjunctive, tense:or([present,imperfect])],FSverb),
  mapped(p_f,[from:C, to:D|FSpf]),
  adjacent([p(X,Y)],[p(C,D)]),
  append([p(X,Y)],[p(C,D)],Path),
  msort(Path, Sorted),
  constraint([case:nom,number:Nb,gender:G,lex:Lex],FSpf),

  map(vgpos,[cat:vg,type:finite,pathlist:Sorted,hp:[p(C,D)],lex:Lex,
            person:Person,mood:subjunctive,tense:future, voice:active,number:Nb,gender:G,w:3])].


% gerundive with est or sunt understood
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

[verb,vg18] --->
[ mapped(gdiv,[from:C,to:D|FS]),
        % gdiv gerundives (NOT gerund)
 constraint([case:nom, number:N, gender:G,lex:Lex],FS),

map(vgpos,[cat:vg,type:finite,pathlist:[p(C,D)],hp:[p(C,D)],lex:Lex,
            person:3,mood:indicative,tense:present, voice:active,number:N,gender:G,w:1]) ].

% gerundive with esse understood
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

[verb,vg19] --->
[ mapped(gdiv,[from:C,to:D|FS]),
        % gdiv gerundives (NOT gerund)
 constraint([case:acc, number:N, gender:G,lex:Lex],FS),

map(vgpos,[cat:vg,type:nonfinite,pathlist:[p(C,D)],hp:[p(C,D)],lex:Lex,origin:gerund,
            person:3,mood:infinitive,tense:present, voice:active,number:N,gender:G,w:0])].



% FULL VERB GROUPS
%%%%%%%%%%%%%%%%%%

[verb, vg20] --->
[ mapped(vgpos,FS1),
  append([polarity:pos], FS1, FSVG),
  map(vg,FSVG)].

% to each vgpos group we assign the polarity:pos feature pair and turn it into a full verb group (vg)



% negative polarity
%%%%%%%%%%%%%%%%%%%

% neg necessarily in front of verb
% this amounts to an oversimplification (one more...)

[verb, vg21] --->
[ mapped(neg,[from:A, to:B|Fneg]),
  constraint([lex:or([non,nec,haud])],Fneg),
  mapped(vgpos,FS1),
  pick(pathlist:Path,FS1,FSnew),
  pick(w:Weight,FSnew,FSnew1),
  ifthenelse(start(Path,B),Penalty=0,Penalty=1),                 % negation immediately precedes verb or aux : no penalty
  append([p(A,B)],Path,PL),      % adding the negation to the path
  msort(PL,PLsorted),
    %  contiguous(PLsorted),  % non debes ..... vocare: contiguity cannot be enforced
  \+ dup(PLsorted),
  WeightNew is Weight-Penalty,

  append([polarity:neg, w:WeightNew,pathlist:PLsorted],FSnew1,FSVG), % the vgpos does not yet bear any polarity feature
   map(vg,FSVG)].



















%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CLAUSE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% We start from the verb group in which we isolate the main verb.
% We fill in all the non-optional arguments of its argument list, taking all the relevant constraints
% into account.
% We also allow for pp adjuncts and other type of adjuncts which do not belong to the arglist,
% but can be attached to the clause as a whole: 'rex reginae aquam dat in templo magno'.

% The non-finite clauses are built first, but do not have a pre-emptive right on the constituents -
% the distribution of constituents as between main and complement clause allows for ambiguity.




%%%%%%%%%%%%
% nonfinite
%%%%%%%%%%%%

% (scio) regem epistulas legere / regem credere reginam se laudavisse.
%

[finite,pred1] --->
[ mapped(flags,active(nonfinite)),  % we use this flag to jump the bits concerning non-finite clauses
                                   % if we do not have any nonfinite form in the input string
   mapped(vg,FSverb),
  constraint([type:nonfinite,mood:Mood,voice:Voice,tense:Tense,        % nonfinite verb form
              pathlist:PathlistVerb,lex:Clex, w:WVerb],FSverb),

  \+member(origin:gerund,FSverb), % exclude gerunds : mittendos (esse) censuerant patres mittendos legatos
        % patres is not subject of mittendos

  ifthen(constraint([gender:Gendersubj],FSverb),true),                  % if we have gender in the verb form, it must agree with the one
                  % specified in the subject slot used in arg filling
  ifthen(constraint([number:Numbersubj],FSverb),true),                  % if we have number in the verb form, it must agree with the one
                  % specified in the subject slot used in arg filling


  ifthenelse(constraint([polarity:neg],FSverb), Pol=neg, Pol=pos),      % retrieving polarity

  lexarg(Clex,arglist:ArgList),           % connection with the args via lexarg
  pick(ws(Lex,_,clause:Clause_Constraints,mwuw:MW,args:Args), ArgList,_),   % picking a word sense to see if it is appropriate ...
                  % remember that in general there will be more than one
                  % ws (i.e. word sense) for a given arg-bearer






ifthenelse(Voice=pass,              % PASSIVE
              % selecting the object to remove it from the arglist and turn it into a subject

                (pick(object:ObjectSpecs,Args,Args1),  % note that there must be an object if a passive was produced !!
                 pick(oblig:_, ObjectSpecs,OS1),
                 pick(constraints:Oconstraints,OS1,OS2),
                 pick(case:_,Oconstraints,Oconstraints1),
                 append([case:acc],Oconstraints1,NewOconstraints), % subjects are acc in nonfinite clauses !!
                 append([constraints:NewOconstraints],OS2,NOS),
                 append([oblig:yes],NOS,NewObjectSpecs),           % there must be a subject if a passive is used !!!


              % turning the subject into an optional (a+) abl pp arg
                pick(subject:SubjectSpecs,Args1,Args2),
                pick(constraints:Sconstraints,SubjectSpecs,_),

                ifthenelse( Sconstraints=[],
                                   % no constraint on subj: both types of agent are OK  IF-CLAUSE
                        ( NewArg=[type:pp,oblig:no,constraints:[prep:ab,sem:[hum]]] ;        % THEN-CLAUSE note the OR operator (;)
                      NewArg=[type:np,oblig:no,constraints:[case:abl]])  ,
                                                                                     % ELSE-CLAUSE:
                 % ab+hum vs simple abl for non-hum

                 ifthenelse(   constraint([sem:[hum]],Sconstraints),   % IF2-clause     % there are constraints: we act accordingly
                                    NewArg=[type:pp,oblig:no,constraints:[prep:ab,sem:[hum]]],    % THEN2-clause
                                    NewArg=[type:np,oblig:no,constraints:[case:abl]])),           % ELSE2-clause


                % make_arg(Sconstraints,NewArg),
                 append([agent:NewArg],Args2,At),
                 append([subject:NewObjectSpecs],At,Argstomatch) ),
             Argstomatch=Args),                                           % ACTIVE : leave the args as they are in the arg specs


  match_list(Argstomatch,TreeArgs,PathlistArgs, DistanceArgs,                 % attempting to match the arglist
             sujet([number:Numbersubj,gender:Gendersubj,person:_]),


             nonfinite,gap:Gap,w:Weight,Int,PathlistVerb),           % the nonfinite flag is important to determine the case of the subject
                                                                     % the pathlist of the verb is passed so that distance from the arg to the argbearer
                     % can be taken into account : visum est mihi tibi scribere

% we keep track of the fact that we are dealing with a nonfinite clause (accusative subject !!!)
% the weight is meant to reflect the nber and nature of the arguments satisfied - the more the better


 % we have explained above why we once decided to rem the partial expansions -
 % the attachment point of adjuncts is too often undecidable so that too many solutions are generated
 % we need a sophisticated pragmatic discourse-oriented module to deal adequately with this overgeneration

  full_expansions(FE),
  expandkernel(FE,ExpandTrees,ExpandPaths,ExpandDistances,w:WExpand, Int, partial),  % here too weight is computed
                      % and the interrogative can hide in the adjuncts
  ifthenelse(WExpand \= 0, WExpandN is WExpand+1, WExpandN=WExpand),
  append(PathlistVerb,PathlistArgs,CPathlist),
  flatten(CPathlist,CFlat),
  flatten(ExpandPaths,EFlat),
  adjacent(EFlat,CFlat),                                    % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%
  append(ExpandPaths,CPathlist,AllPaths),
  myappend(DistanceArgs,ExpandDistances,Distances),
  flatten(Distances, FlatDist),
  sum(FlatDist, Distance),
  flatten(AllPaths, Flat),
  msort(Flat, Sorted),
  \+dup(Sorted),
  ifthenelse(contiguous(Sorted),BonusContiguous=3, BonusContiguous=0),  % bonus for strict contiguity


  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%         % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%
   quasicontiguous2(Sorted),  % insisting on straightforward contiguity is too strict
                  %  Me tabula sacer.... EXPENSIVE
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


  append(TreeArgs,ExpandTrees,AllTrees),
  insort(AllTrees, ST),                                          % standard order imposed on args IN THE PARSE TREE
                 % NOT in the string, of course




   clause_constraints(Clause_Constraints,FSverb),   % they concern polarity (so far...)

            % prioritize normal subject-object order (following a suggestion made by Dominique Longrée, ULg)

  ifthenelse(constraint([subject:[hp:Pathsubj]],ST),  % we have a non-gapped subject, we record its head path
       true,              % and abstain from doing anything else
             Pathsubj=[p(0,0)]),          % otherwise the subject is higher up and therefore necessarily precedes

  ifthenelse(constraint([object:[hp:Pathobj]],ST),                % IF-CLAUSE we have an object
             ifthenelse(precedes(Pathsubj,Pathobj), NW is Weight+1, NW=Weight),         % THEN-CLAUSE
             NW=Weight),                % ELSE-CLAUSE

  ifthenelse(constraint([i_object:[hp:Pathiobj1]],ST),          %  we have an indirect object (first type: dat)
             ifthenelse(precedes(Pathsubj,Pathiobj1), NW1 is NW+1, NW1=NW),
             NW1=NW),
  ifthenelse(constraint([object_i:[hp:Pathiobj2]],ST),          %  we have an indirect object (second type: acc)
             ifthenelse(precedes(Pathsubj,Pathiobj2), NW2 is NW1+1, NW2=NW1),           % as with doceo, for instance
             NW2=NW1),


   ifthen( (constraint([object:[gender:GENDERinNP,number:NUMBERinNP]],ST),
            constraint([object_cplt:[cat:adjp,gender:GENDERinADJ,number:NUMBERinADJ]],ST)
            ),
             (funify([gender:GENDERinADJ], [gender:GENDERinNP],_),
             funify([number:NUMBERinADJ],[number:NUMBERinNP],_))
         ),

     % if the object cplt is an adj, it must agree in gender and number with the object


   ifthen( (constraint([object:[gender:GENDERinNP,number:NUMBERinNP]],ST),
            constraint([object_cplt:[cat:np,gender:GENDERinNP1,number:NUMBERinNP1]],ST)
            ),
             (funify([gender:GENDERinNP], [gender:GENDERinNP1],_),
             funify([number:NUMBERinNP],[number:NUMBERinNP1],_))
         ),

     % if the object cplt is an np, it must agree in gender and number with the object
     % probably too strict




   ifthen( (constraint([subject:[gender:GENDERinNP1,number:NUMBERinNP1]],ST),
            constraint([predicative:[cat:adjp,case:acc,gender:GENDERinADJ1,number:NUMBERinADJ1]],ST)
            ),
            (constraint([gender:GENDERinADJ1], [gender:GENDERinNP1]),
             constraint([number:NUMBERinADJ1],[number:NUMBERinNP1]))

         ),
    % predicative adj : triple agreement


    ifthen( (constraint([subject:SUBJECT],ST),
            constraint([predicative:[cat:np,case:CASEPRED]],ST)
            ),
            CASEPRED=acc
         ),
    % predicative np : case agreement



  ifthenelse(var(Int), IntFlag=no, IntFlag=wh_question),       % the int flag on an arg indicates a wh_question

  % Wtot is NW2+WExpand+WVerb+MW,
        % summing the weights : for the args, the expansions and the verb group
  myplus(NW2,WExpandN,T1),
  myplus(T1,WVerb,T2),
  myplus(T2,MW,Wtot),
  myplus(Wtot,BonusContiguous,Wtottot),
  append([selected_reading:Lex],FSverb,FSverbfull),
  map(pred,[cat:pred,type:nonfinite,           % type of the whole pred is nonfinite
              mood:Mood,tense:Tense,class:m,
              pathlist:Sorted,distance:[Distance],
              number:sing,gender:neuter,case:or([nom,acc]),     % of little relevance for pred, but necessary on account of
                 % subject role of the pred with ESSE
              person:3,polarity:Pol,                             % idem for person
              argbound:no,
              gap:Gap,
              w:Wtottot,
              flagint:IntFlag,
              constituent_structure:[vg:FSverbfull|ST]])].










% (scis) vincere
%%%%%%%%%%%%%%%%%

[finite,pred2] --->
[ mapped(flags,active(nonfinite)),
    mapped(vg,FSverb),
  constraint([type:nonfinite,mood:Mood,tense:Tense,voice:Voice,lex:Clex,pathlist:PathlistVerb, w:WVerb],FSverb),
  ifthenelse(constraint([polarity:neg],FSverb), Pol=neg, Pol=pos),
  lexarg(Clex,arglist:ArgList),
  pick(ws(Lex,_,clause:Clause_Constraints,mwuw:MW,args:Args), ArgList,_),

ifthenelse(Voice=pass,

              % selecting the object to remove it from the arglist
                ( pick(object:_,Args,Args1),  % note that there must be an object if a passive was produced !!

               % turning the subject into an optional (a+) abl pp arg
                pick(subject:SubjectSpecs,Args1,Args2),
                pick(constraints:Sconstraints,SubjectSpecs,_),

                % make_arg(Sconstraints,NewArg),

           ifthenelse( Sconstraints=[],
                                   % no constraint on subj: both types of agent are OK  IF-CLAUSE
                        ( NewArg=[type:pp,oblig:no,constraints:[prep:ab,sem:[hum]]] ;        % THEN-CLAUSE
                      NewArg=[type:np,oblig:no,constraints:[case:abl]])  ,
                                                                                     % ELSE-CLAUSE:
                 ifthenelse(   constraint([sem:[hum]],Sconstraints),   % IF2-clause     % there are constraints: we act accordingly
                                    NewArg=[type:pp,oblig:no,constraints:[prep:ab,sem:[hum]]],    % THEN2-clause
                                     NewArg=[type:np,oblig:no,constraints:[case:abl]])),          % ELSE2-clause



                 append([agent:NewArg],Args2,At),
                 append([subject:_],At,Argstomatch) ), % adding dummy subject for match_list to drop in mode nonfinite_i !!!


                 Argstomatch=Args),

   match_list(Argstomatch,TreeArgs,PathlistArgs, DistanceArgs,
               sujet([number:_,gender:_,person:_]),
              nonfinite_i,gap:Gap,w:Weight,Int,PathlistVerb),

             % nonfinite_i specifies that no subject should be looked for
             % contrast 'scis vincere' et 'scis te vicisse'


   ifthenelse( Sconstraints=[],
               ifthenelse(member(sem:Sem,Sconstraints),
                                 Semclause=Sem,
                                 Semclause=_), % sem:[hum] really matters

               Semclause=_),


  full_expansions(FE),
  expandkernel(FE,ExpandTrees,ExpandPaths,ExpandDistances,w:WExpand, Int, partial),  % here too weight is computed
                      % and the interrogative can hide in the adjuncts
  ifthenelse(WExpand \= 0, WExpandN is WExpand+1, WExpandN=WExpand),
  append(PathlistVerb,PathlistArgs,CPathlist),
  flatten(CPathlist,CFlat),
  flatten(ExpandPaths,EFlat),
  adjacent(EFlat,CFlat),                              % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%
  append(ExpandPaths,CPathlist,AllPaths),
  myappend(DistanceArgs,ExpandDistances,Distances),
  flatten(Distances, FlatDist),
  sum(FlatDist, Distance),
  flatten(AllPaths, Flat),
  msort(Flat, Sorted),
  \+dup(Sorted),
  append(TreeArgs,ExpandTrees,AllTrees),
  insort(AllTrees, ST),
   clause_constraints(Clause_Constraints,FSverb),
  % Wtot is Weight+WExpand+WVerb+MW,
  myplus(Weight,WExpandN,T1),
  myplus(T1,WVerb,T2),
  myplus(T2,MW,Wtot),

  ifthenelse( (constraint([object:[hp:Pathobject]],ST), constraint([object_cplt:[hp:Pathobjectcplt]],ST)),
                             % IF-CLAUSE we have both obj and obj cplt
           ifthenelse(precedes(Pathobject,Pathobjectcplt), NW is Wtot+2, NW=Wtot),         % THEN-CLAUSE
          NW=Wtot),               % ELSE-CLAUSE



   ifthenelse(var(Int), IntFlag=no, IntFlag=wh_question),
   append([selected_reading:Lex],FSverb,FSverbfull),

  map(pred,[cat:pred,type:nonfinite_i,
              mood:Mood, tense:Tense,class:m,
              pathlist:Sorted, distance:[Distance],
              number:_,gender:_,person:_, case:or([nom,acc]), % requirements as subject of ESSE
                                                              % necessary for nominativus cum nonfinite
              sem:Semclause,  % Semclause attempts to catch requirements on the absent subject
              polarity:Pol,
              gap:Gap,w:NW,
              flagint:IntFlag,
              constituent_structure:[vg:FSverbfull|ST]])].







% rogatum auxilium : supine
%%%%%%%%%%%%%%%%%%%%%%%%%%%

[finite,pred3] --->
[  mapped(vg,FSverb),
  constraint([mood:supine,lex:Clex,pathlist:PathlistVerb, w:WVerb],FSverb),  % needs a supine
  lexarg(Clex,arglist:ArgList),
  pick(ws(Lex,_,clause:Clause_Constraints,mwuw:MW,args:Args), ArgList,_),

  match_list(Args,TreeArgs,PathlistArgs, DistanceArgs,
               sujet([number:_,gender:_,person:_]),
              nonfinite_i,gap:[],w:Weight,_,PathlistVerb),

             % nonfinite_i specifies that no subject should be looked for


  full_expansions(FE),
  expandkernel(FE,ExpandTrees,ExpandPaths,ExpandDistances,w:WExpand, _, partial),

  append(PathlistVerb,PathlistArgs,APaths),
  append(ExpandPaths,APaths,AllPaths),
  myappend(DistanceArgs,ExpandDistances,Distances),
  flatten(Distances, FlatDist),
  sum(FlatDist, Distance),
  flatten(AllPaths, Flat),
  msort(Flat, Sorted),
  contiguous(Sorted),                                   % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%
   \+dup(Sorted),
  append(TreeArgs,ExpandTrees,AllTrees),
  insort(AllTrees, ST),
  clause_constraints(Clause_Constraints,FSverb),
  % Wtot is Weight+WExpand+WVerb+MW,
  myplus(Weight,WExpand,T1),
  myplus(T1,WVerb,T2),
  myplus(T2,MW,Wtot),
  myplus(Wtot,5,Wtotal), % bonus for predicate
  append([selected_reading:Lex],FSverb,FSverbfull),
  map(pred,[cat:pred,type:nonfinite_i,
              mood:supine, tense:present,class:m,
              pathlist:Sorted, distance:[Distance],
              number:sing,gender:neuter,case:or([nom,acc]),
              person:3,
              gap:[],w:Wtotal,
              constituent_structure:[vg:FSverbfull|ST]])].






% urbem capiendi (causa) / procurandi eos
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* PROCURARE
lexarg(procurare,
       arglist:[ws(procuro_procure,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).

lex(procurandi, gdiv, [pos:gdiv, txt:procurandi, case:nom, gender:masc, number:pl, lex:procurare,
                                                           class:tr_cod, type:gdiv, kind:std, mood:gerund, person:3]).

lex(procurandi, gdiv, [pos:gdiv, txt:procurandi, case:gen, gender:neuter, number:sing, lex:procurare,
                                                           class:tr_cod, type:gdiv, kind:std, mood:gerund, person:3]).

lex(procurandi, v, [pos:v, class:tr_cod, type:gerund, lex:procurare, txt:procurandi, kind:std, mood:gerund, person:3, case:gen]).


*/

% we have a gerund (not a gerundive) - the args get satisfied the usual way

% PROBLEMS with contiguity checks but they need to be maintained

[finite,pred4] --->
[
  mapped(v,[from:A, to:B|FSverb]),
  constraint([type:gerund,case:Case,lex:Clex],FSverb),      % true gerund form of the verb

  lexarg(Clex,arglist:ArgList),
  pick(ws(Lex,_,clause:_,mwuw:MW,args:Args), ArgList,_),
  match_list(Args,TreeArgs,PathlistArgs, DistanceArgs,sujet(_), nonfinite_i,gap:[],w:Weight,Int,[p(A,B)]),

     % nonfinite_i specifies that no subject should be looked for;
     % contrast 'urbem capiendi' et 'caesar cepit urbem'

  append([p(A,B)],PathlistArgs,CPathlist),
  flatten(CPathlist,Flat),
  flatten(DistanceArgs,Flatdis),
   msort(Flat, Sorted),
   \+dup(Sorted),
   % contiguous(Sorted),                                % see below contiguity test
   sum(Flatdis,Distance),
    append([selected_reading:Lex],FSverb,FSverbfull),
   % TW is Weight+MW,
   myplus(Weight,MW,TW),
    full_expansions(FE),
  expandkernel(FE,ExpandTrees,ExpandPaths,ExpandDistances,w:WExpand, Int, partial),  % here too weight is computed
                      % and the interrogative can hide in the adjuncts
  ifthenelse(WExpand \= 0, WExpandN is WExpand+1, WExpandN=WExpand),

  flatten(ExpandPaths,EFlat),

  append(EFlat,Sorted,AllPaths),
  flatten(ExpandDistances,ExDisFlat),
   sum(ExDisFlat, ExDistance),
   TotDis is Distance + ExDistance,
  flatten(AllPaths, AllFlat),
  msort(AllFlat, NSorted),
  \+dup(NSorted),
 quasicontiguous(NSorted),                                  % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%
  append(TreeArgs,ExpandTrees,AllTrees),
  insort(AllTrees, ST),
  myplus(TW,WExpandN,TotWeight),
   map(pred,[cat:pred,type:gerund,      % type:gerund is how we specify them in args and adjuncts
               case:_,
               mood:gerund,
               local_case:Case,                         % case is used for subject agreement,
    class:m,          % local_case to store the case of the gerund
               number:sing,person:3,
               gender:neuter,
               pathlist:NSorted, distance:[TotDis],
               gap:[],w:TotWeight,constituent_structure:[vg:FSverbfull|ST]])].








% urbis capiendae (causa)
%%%%%%%%%%%%%%%%%%%%%%%%%


% PROBLEMS with contiguity checks: Caesari urbs ERAT capienda / Mihi colenda EST virtus
% we have to change the analysis of such strings - we opt for gerundive as adj
% and we keep the contiguity checks


/*
Augustinus : "agere negotium procurandi fructus mortis":

procurandi gerund gen *    *     + fructus acc masc pl + gen sg mortis //
procurandi gdiv   gen masc sg    + fructus gen masc sg + gen sg mortis

Lexical entries:
lex(procurandi, gdiv, [pos:gdiv, txt:procurandi, case:gen,
                       gender:masc, number:sing, lex:procurare, class:tr_cod, type:gdiv, kind:std, mood:gerund, person:3]).
lex(procurandi, gdiv, [pos:gdiv, txt:procurandi, case:gen,
                       gender:neuter, number:sing, lex:procurare, class:tr_cod, type:gdiv, kind:std, mood:gerund, person:3]).
lex(procurandi, gdiv, [pos:gdiv, txt:procurandi, case:nom,
                       gender:masc, number:pl, lex:procurare, class:tr_cod, type:gdiv, kind:std, mood:gerund, person:3]).

lex(procurandi, v, [pos:v, class:tr_cod, type:gerund, lex:procurare, txt:procurandi, kind:std, mood:gerund, person:3, case:gen]).
*/






% we have a gerundive
% the verb must be transitive and the object satisfied by an np
% with the same [case,gender,number] as that of the gerundive

[finite,pred5] --->
[ mapped(gdiv,[from:A, to:B|FSverb]),                 % gerundives are gdiv
  constraint([type:gdiv,
              case:Case,gender:Gender, number:Number,  % agreement triplet
              lex:Clex],FSverb),
  lexarg(Clex,arglist:ArgList),

  pick(ws(Lex,_,clause:_,mwuw:MW,args:Args), ArgList,_),
  pick(object:ObjectSpecs,Args,Args_temp),
                  % removing the arg with its Specs
  pick(constraints:Constraints, ObjectSpecs,ObjectSpecs_temp),    % removing the Specs Constraints
  pick(case:acc,Constraints,Constraints_temp),        % removing the case constraint from the Constraints
  Case \= nom,  % nominative banned here
  append([case:Case,gender:Gender,number:Number],Constraints_temp, New_Constraints),      % rebuilding the Constraints
  append([constraints:New_Constraints],ObjectSpecs_temp,New_ObjectSpecs),       % rebuilding the Specs
  append([object:New_ObjectSpecs],Args_temp,NewArgs),             % rebuilding the Args


  match_list(NewArgs,TreeArgs,PathlistArgs, DistanceArgs,sujet(_), nonfinite_i,gap:[],w:Weight,Int,[p(A,B)]),

             % nonfinite_i specifies that no subject should be looked for
             % contrast 'urbis capiendae' et 'caesar cepit urbem'

  PathlistArgs \= [],  % we must have at least one arg filled,
                       % the gerundive adj cannot be used on its own as a gerund substitute

  append([p(A,B)],PathlistArgs,CPathlist),
  flatten(CPathlist,Flat),
   msort(Flat, Sorted),
   flatten(DistanceArgs,Flatdis),
   \+dup(Sorted),
    sum(Flatdis,Distance),

   full_expansions(FE),
  expandkernel(FE,ExpandTrees,ExpandPaths,ExpandDistances,w:WExpand, Int, partial),  % here too weight is computed
                      % and the interrogative can hide in the adjuncts
  ifthenelse(WExpand \= 0, WExpandN is WExpand+1, WExpandN=WExpand),

  flatten(ExpandPaths,EFlat),
  % adjacent(EFlat,Sorted),
  append(EFlat,Sorted,AllPaths),
  flatten(ExpandDistances,ExDisFlat),
   sum(ExDisFlat, ExDistance),
   TotDis is Distance + ExDistance,

  flatten(AllPaths, AllFlat),
  msort(AllFlat, NSorted),
  \+dup(NSorted),
 quasicontiguous(NSorted),                       % NEEDS TO BE MAINTAINED        % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%
  append(TreeArgs,ExpandTrees,AllTrees),
  insort(AllTrees, ST),



   myplus(Weight,MW,T1),
   myplus(T1,2,NewWeight),
   % NewWeight is Weight+MW+2,                                % giving priority to gerundive construction as opposed to simple
               % noun+adj nexus (in case gerundives are also registered as adj)

    myplus(NewWeight,WExpandN,TotW),
    append([selected_reading:Lex],FSverb,FSverbfull),
   map(pred,[cat:pred,type:gerund,subtype:gerundive,      % type:gerund is how we specify them in args and adjuncts
               case:Case,mood:gerund,class:m,
               local_case:Case,  % case is used to determine subject agreement, local_case to retrieve the case of the gerund
               number:sing,person:3,gender:neuter,
               pathlist:NSorted, distance:[TotDis],
               gap:[],w:TotW,constituent_structure:[vg:FSverbfull|ST]])].















%%%%%%%%%%%%%%%%%%%%%
% ABLATIVE ABSOLUTE
%%%%%%%%%%%%%%%%%%%%%

% type CICERONE CONSULE
%%%%%%%%%%%%%%%%%%%%%%%

[finite,aa1] --->
[ mapped(np,FSnp1),
  mapped(np,FSnp2),
  FSnp1 \= FSnp2,
  constraint([pathlist:PL1,case:abl, class:Class,number:Number],FSnp1),
  (Class=proper;Class=common),

  constraint([pathlist:PL2,case:abl, number:Number,
              lex:or([consul, dux, rex, imperator])], FSnp2),
                      % best dealt with as a list, even if a long one;
                      % toy list here !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                      % the key quality seems to be built-in non-permanence
  adjacent(PL1,PL2),
  append(PL1,PL2,PL),
  map(aa,[    pathlist:PL,
              distance:[0],
              w:0,
              constituent_structure:[FSnp1,FSnp2]])].


% type HAMILCARE UIUO
%%%%%%%%%%%%%%%%%%%%%

[finite,aa2] --->
[ mapped(np,FSnp1),
  mapped(adjp,FSadj),
  constraint([pathlist:PL1,case:abl, class:Class,number:Number, gender:Gender],FSnp1),
  (Class=proper;Class=common),

  constraint([pathlist:PL2,case:abl, number:Number, gender:Gender,
              lex:or([uiuus,mortuus,grauis, ignarus])], FSadj),
                      % the list to be established on the basis of genuine examples !!!!!!!!!!!!!!!!!!!!!
                      % best dealt as a list, even if a long one;
                      % toy list here !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                      % the key quality seems to be *****  built-in non-permanence  *******
  adjacent(PL1,PL2),
  append(PL1,PL2,PL),
  map(aa,[  pathlist:PL, distance:[0],
              w:0,constituent_structure:[FSnp1,FSadj]])].




% type URBE CAPTA
%%%%%%%%%%%%%%%%%%

% alternate way of viewing ablative absolute
% aa also parsable as ablative np containing participle clause
% see below

[finite,aa3] --->
[  mapped(np,FSnp),
   constraint([number:Number, gender:Gender, case:abl, pathlist:Pathlist, distance:[Distance]],FSnp),

   mapped(p_p,[from:X,to:Y|FSpp]),  % a past participle

   constraint([case:abl,gender:Gender, number:Number,lex:Clex],FSpp), % agreement checks case gender number
   lexarg(Clex,arglist:ArgList),

                 % selecting the object constraints:
   pick(ws(Lex,_,clause:_,mwuw:MW,args:Args), ArgList,_),
   pick(object:ObjectSpecs,Args,Args1),            % note that there must be an object if a pp was produced !!
   pick(constraints:Constraints, ObjectSpecs,_),
   pick(case:acc,Constraints,NPConstraints),       % deleting the case constraint on the object
   constraint(NPConstraints,FSnp),                 % applying the object constraints on the ablative NP

   % turning the subject into an optional (a+) abl pp arg

   pick(subject:SubjectSpecs,Args1,Args2),
   pick(constraints:Sconstraints,SubjectSpecs,_),
   % make_arg(Sconstraints,NewArg),

  ifthenelse( Sconstraints=[],                 % no constraint on subj: both types of agent are OK  IF-CLAUSE
             ( NewArg=[type:pp,oblig:no,constraints:[prep:ab,sem:[hum]]] ;        % THEN-CLAUSE
              NewArg=[type:np,oblig:no,constraints:[case:abl]])  ,

                                                                                   % ELSE-CLAUSE:
              ifthenelse(   constraint([sem:[hum]],Sconstraints),   % IF2-clause     % there are constraints: we act accordingly
                            NewArg=[type:pp,oblig:no,constraints:[prep:ab,sem:[hum]]],    % THEN2-clause
                            NewArg=[type:np,oblig:no,constraints:[case:abl]])),          % ELSE2-clause


   append([agent:NewArg],Args2,Args3),

   expand(Args3,ExpandTrees,ExpandPaths,ExpandDistances,w:Weight),    % using expand should prove sufficient
   flatten(ExpandPaths,Flat),
   msort(Flat,Sorted),
   ifthen(Sorted\=[],adjacent([p(X,Y)],Sorted)),  % args before or after pp, but contiguous
   \+dup(Sorted),
   % contiguous(Sorted),    % too strict ? --> see below

   insort(ExpandTrees,ST),
   flatten(ExpandDistances,ED),
   sum(ED,DistExpand),
   append([p(X,Y)],Sorted,Path),
   % NewDis is Distance+DistExpand,
   myplus(Distance,DistExpand,NewDis),

   append(Path,Pathlist,CPathlist),
   flatten(CPathlist,Flat2),
   msort(Flat2, Sorted2),


 full_expansions(FE),
  expandkernel(FE,ExpandT,ExpandP,ExpandD,w:WExpand, _, partial),  % here too weight is computed
                      % and the interrogative can hide in the adjuncts
  ifthenelse(WExpand \= 0, WExpandN is WExpand+6, WExpandN=WExpand),

  flatten(ExpandP,EFlat),
  adjacent(EFlat,Sorted2),
  append(EFlat,Sorted2,AllPaths),

  flatten(ExpandD, FlatD),
  sum(FlatD, DistanceD),
  myplus(DistanceD,NewDis,NDistance),

  flatten(AllPaths, FlatAll),
  msort(FlatAll, NSorted),
  \+dup(NSorted),
  contiguous(NSorted),                                     % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%

  append(ST,ExpandT,Trees),
  insort(Trees, NST),


   extremity(NSorted,End),
   ifthenelse( mapped(punct,[from:End,to:Endpunct, lex:comma|_]),
             Adtopath=[p(End,Endpunct)],
             Adtopath=[]),            % comma after ?
   append(NSorted,Adtopath,Path_aa1),



% comma after and aa begins at zero
   ifthenelse( (mapped(punct,[from:End,to:Endpunct, lex:comma|_]), start(NSorted,0) ),
            Bonus1=3,
            Bonus1=0),




   start(NSorted,Start),

   ifthenelse( mapped(punct,[from:BeforeStart,to:Start, lex:comma|_]),
             Adtopath2=[p(BeforeStart,Start)],
             Adtopath2=[]),           % comma before ?
   append(Adtopath2,Path_aa1,Path_aa),



% comma before and aa stops at Fin
    recorded(fin,fin(Fin),_),
   ifthenelse( (mapped(punct,[from:BeforeStart,to:Start, lex:comma|_]), extremity(NSorted,Fin)),
                 Bonus2=3,
                 Bonus2=0),



   % TotWeight is Weight+MW,
   myplus(Weight,MW,TotWeight),
   myplus(TotWeight,WExpandN,TW),
   myplus(TW,Bonus1,TW1),
   myplus(TW1,Bonus2,TW2),
   myplus(TW2,2,TW3),                              % ALP 197
   map(aa,[  pathlist:Path_aa,
               distance:[NDistance],
               w:TW3,                              % ALP 197
               constituent_structure:[lex:Lex,FSpp,NST,object:FSnp]])].


% type DICTIS DICENDIS
%%%%%%%%%%%%%%%%%%%%%%%

% dictis dicendis

[finite,aa4a] --->
[ mapped(p_p,[from:A,to:B|FSpp]),   % the past participle
  constraint([case:abl,gender:neuter, number:pl,lex:Clex],FSpp),  % neuter plural on top of ablative case
  lexarg(Clex,arglist:ArgList),
  pick(ws(Lex,_,clause:_,mwuw:MW,args:_), ArgList,_),  % getting at the Lex
  not(atom_concat(mwu,_,Lex)),            % Lex is not a multi-word unit

  mapped(gdiv,[from:B,to:C|FSgdiv]),                                  % the gerundive
  constraint([number:pl, gender:neuter, case:abl, lex:Clex],FSgdiv), % agreement checks
   ifthenelse( mapped(punct,[from:C,to:D, lex:comma|_]),
             Adtopath=[p(C,D)],
             Adtopath=[]),            % comma ?
   append([p(A,B),p(B,C)],Adtopath,Path_aa),
   map(aa,[   pathlist:Path_aa,
              distance:[0],
              w:MW,
              constituent_structure:[lex:Lex,pp:FSpp,gdiv:FSgdiv]])].


% dicendis dictis
%%%%%%%%%%%%%%%%%

[finite,aa4b] --->
[ mapped(p_p,[from:B,to:C|FSpp]),
  constraint([case:abl,gender:neuter, number:pl,lex:Clex],FSpp),
  lexarg(Clex,arglist:ArgList),
  pick(ws(Lex,_,clause:_,mwuw:MW,args:_), ArgList,_),  % getting at the Lex
  not(atom_concat(mwu,_,Lex)),
  mapped(gdiv,[from:A,to:B|FSgdiv]),
  constraint([number:pl, gender:neuter, case:abl, lex:Clex],FSgdiv),
  ifthenelse( mapped(punct,[from:C,to:D, lex:comma|_]),
             Adtopath=[p(C,D)],
             Adtopath=[]),            % comma ?
   append([p(A,B),p(B,C)],Adtopath,Path_aa),

  map(aa,[    pathlist:Path_aa,
              distance:[0],
              w:MW,
              constituent_structure:[lex:Lex,pp:FSpp,gdiv:FSgdiv]])].





% VIEWING ABL ABS AS [NP+PARTICIPLE CLAUSE] IN THE ABLATIVE CASE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% type REGINA SUB TEGMINE FAGI RECUBANTE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% does not apply to aa built around a past participle
% whose structure should be delved in
% to retrieve verb-object relations


 [finite,aa5] --->
 [mapped(np,FSNP),
  constraint([case:abl,pathlist:Sorted,hp:HPnp,index:Index,distance:Distance,
           cat:np,class:Class,sem:Sem,type_pp:Type_pp,
           number:Nb,person:Person,gender:Gender,type:core,lex:LexNoun,lextype:full,   % person to be left open or read from np
           w:Weight,
           constituent_structure:[head:FSnp,participle_clause:FSpp]], FSNP),
   Type_pp \= ppcentered,
   extremity(Sorted,End),
   ifthenelse( mapped(punct,[from:End,to:Y, lex:comma|_]),
             Adtopath=[p(End,Y)],
             Adtopath=[]),            % comma after ?

   append(Sorted,Adtopath,Path_aa1),

   start(Sorted,Start),
   ifthenelse( mapped(punct,[from:BeforeStart,to:Start, lex:comma|_]),
             Adtopath2=[p(BeforeStart,Start)],
             Adtopath2=[]),           % comma before ?
   append(Adtopath2,Path_aa1,Path_aa),

   NewWeight is Weight+2,
                              % AA weightier to ensure that adjuncts stay in the aa

   map(aa,[pathlist:Path_aa,
           distance:[Distance],
           w:NewWeight,
           constituent_structure:[head:FSnp,participle_clause:FSpp]])].












%%%%%%%%%%%%%%%%%%%%%
% NOMINAL PREDS
%%%%%%%%%%%%%%%%%%%%%




% type Plenum exiliis mare
%%%%%%%%%%%%%%%%%%%%%%%%%%

[finite,nompred1] --->
[ ifthen((member(V,[v,p_f,p_p,gdiv]), mapped(V,_)),fail),

  mapped(np,FSnp),
  mapped(adjp,FSadj),
  constraint([pathlist:PL1,case:nom, class:Class,number:Number, gender:Gender,w:WN],FSnp),
  (Class=proper;Class=common),

  constraint([pathlist:PL2,case:nom, number:Number, gender:Gender,w:WADJ], FSadj),
  ifthenelse(precedes(PL2,PL1), Bonus=1,Bonus=0),
  myplus(WN,WADJ,W),
  myplus(W,Bonus,Weight),

   full_expansions(FE),
  expandkernel(FE,ExpandT,ExpandP,_,_, _,_),
  flatten(ExpandP,EFlat),
  adjacent(EFlat,PL2),
  append(EFlat,PL2,PLADJ),
  msort(PLADJ,Sorted),

  adjacent(PL1,Sorted),
  append(PL1,Sorted,AllPaths),

  map(pred,[cat:pred,argbound:no,class:m,type:finite,gap:[],  pathlist:AllPaths, distance:[0],
              w:Weight,constituent_structure:[nominal_pred,subject:FSnp,predicative:FSadj,adjunct:ExpandT]])].


% type Deus illuminatio mea
%%%%%%%%%%%%%%%%%%%%%%%%%%%%



[finite,nompred2] --->
[ ifthen((member(V,[v,p_f,p_p,gdiv]), mapped(V,_)),fail),

  mapped(np,FSnp1),
  mapped(np,FSnp2),

  FSnp1 \= FSnp2,

  constraint([pathlist:PL1,case:nom, w:WN1],FSnp1),

  constraint([pathlist:PL2,case:nom,w:WN2], FSnp2),
  precedes(PL1,PL2),

  myplus(WN1,WN2,Weight),

  append(PL1,PL2,Path),
  msort(Path,Sorted),

  map(pred,[cat:pred,argbound:no,class:m,type:finite,gap:[],  pathlist:Sorted, distance:[0],
              w:Weight,constituent_structure:[nominal_pred,subject:FSnp1,predicative:FSnp2]])].


% type In Britannico exercitu nihil irarum
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


[finite,nompred3] --->
[ ifthen((member(V,[v,p_f,p_p,gdiv]), mapped(V,_)),fail),

  mapped(np,FSnp),
  mapped(pp,FSpp),

  constraint([pathlist:PL1,case:nom, w:WN],FSnp),

  constraint([pathlist:PL2,w:WPP], FSpp),

  myplus(WN,WPP,Weight),

  append(PL1,PL2,Path),
  msort(Path,Sorted),

  map(pred,[cat:pred,argbound:no,class:m,type:finite,gap:[],  pathlist:Sorted, distance:[0],
              w:Weight,constituent_structure:[nominal_pred,subject:FSnp,predicative:FSpp]])].


% exclamations (accusative : me miserum etc.)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

[finite,exclamation1] --->

[ ifthen((member(V,[v,p_f,p_p,gdiv]), mapped(V,_)),fail),
 mapped(np,FSnp),
 constraint([number:N,gender:G,
             case:acc,            % accusative only
             pathlist:Pathlistnp, w:WNP,distance:[Distnp]],FSnp),
 mapped(adjp,FSadjp),
 constraint([pathlist:Pathlistadjp,distance:[Distadjp],
             case:acc,number:N, gender:G,w:WADJP],FSadjp),

 append(Pathlistnp,Pathlistadjp,Pathlist),
 msort(Pathlist,Sorted),

 start(Sorted,0),
 recorded(fin,fin(Fin)),
 extremity(Sorted,Fin),

 Dis is Distnp+Distadjp,
 Weight is WNP+WADJP,

 map(pred,[cat:pred,argbound:no,class:m,type:finite,gap:[],  pathlist:Sorted, distance:[Dis],
              w:Weight,constituent_structure:[exclamation, subject:FSnp,predicative:FSadjp]]) ].



% Virgilian exclamation : fortunatos nimium sua si bona norint agricolas...
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

[finite,exclamation2] --->

[
 mapped(np,FSnp),
 constraint([number:N,gender:G,
             case:acc,            % accusative only
             pathlist:Pathlistnp, w:WNP,distance:[Distnp]],FSnp),

 mapped(adjp,FSadjp),
 constraint([pathlist:Pathlistadjp,distance:[Distadjp],
             case:acc,number:N, gender:G,w:WADJP],FSadjp),

 mapped(pred,FSSub),
 constraint([type:finite,class:s,pathlist:Pathlistfreesub, distance:[Distfreesub],gap:[],argbound:no,
               w:WFS,constituent_structure:C_str_FreeSub], FSSub),


 append(Pathlistadjp,Pathlistnp,PL1),
 append(PL1,Pathlistfreesub,PL),
 msort(PL,Sorted),

 start(Sorted,0),
 recorded(fin,fin(Fin)),
 extremity(Sorted,Fin),

 Dis is Distfreesub+Distnp+Distadjp,
 Weight is WFS+WNP+WADJP,

 map(pred,[cat:pred,argbound:no,class:m,type:finite,gap:[],  pathlist:Sorted, distance:[Dis],
              w:Weight,constituent_structure:[exclamation, subject:FSnp,predicative:FSadjp,C_str_FreeSub]])].


















%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% finite clauses of various types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% neg imp  person
%%%%%%%%%%%%%%%%%%

% ne hoc feceris

[finite, pred6] --->
[mapped(neg,[from:A, to:B|Neg]),
constraint([type:imp],Neg),    % the neg must be of the right type, i.e. for imperatives (ne)
mapped(pred,FSpred),
constraint([type:finite,class:m,        % finite
            pathlist:PL,distance:Distance,
            number:N,vgperson:2,
            person:_,                                   % second person but person here applies to the whole clause;
                                                        % we need a new feature (vgperson)
            mood:subjunctive,                           % subjunctive
            tense:or([perfect,present]),        % perfect (present in poetry 'ne volgo narres...' Hor,Ep,1,xiii, 16)
            argbound:no,
            gap:[],w:W,
            add:Add,          % the finite preds feature a feature... that tells whether
              % they come naked of with sth added in front

            flagint:no,         % no interrogative
            constituent_structure:Cstr],FSpred),
start(PL,B),
append([p(A,B)],PL,PLtot),
myplus(W,2,WB),

map(pred,[cat:pred,type:finite,pathlist:PLtot,distance:Distance,
            illocutionary_force:command,
            number:N,person:3, mood:subjunctive,class:m,
            tense:perfect,argbound:no,
            polarity:neg,
            gap:[],w:WB,
            add:Add,
            flagint:no,
            constituent_structure:[illocutionary_force:prohibition,Cstr]])].  % we mention the illocutionary force in the parse tree


% neg imp 1 person
%%%%%%%%%%%%%%%%%%

% ne hoc faciamus

[finite, pred7] --->
[mapped(neg,[from:A, to:B|Neg]),
constraint([type:imp],Neg),
mapped(pred,FSpred),
constraint([type:finite,class:m,pathlist:PL,distance:Distance,number:N,vgperson:1, % person ought to be 1 - see above
            mood:subjunctive,
            tense:present,
            argbound:no,
            gap:[],w:W,add:Add,flagint:no,constituent_structure:Cstr],FSpred),
start(PL,B),
append([p(A,B)],PL,PLtot),
myplus(W,2,WB),
map(pred,[cat:pred,type:finite,pathlist:PLtot,distance:Distance,
            illocutionary_force:command,
            number:N,person:3,class:m,
            mood:subjunctive,
            tense:present,
            argbound:no,
            polarity:neg,
            gap:[],w:WB,add:Add,
            flagint:no,
            constituent_structure:[illocutionary_force:prohibition,Cstr]])].


% wishes
%%%%%%%%

% utinam + subj present or perfect

[finite, pred8] --->
[mapped(part,[from:A, to:B|Utinam]),
constraint([lex:utinam],Utinam),     % only utinam !!!

mapped(pred,FSpred),
constraint([type:finite,class:m,pathlist:PL,distance:Distance,number:N,person:Person,
            mood:subjunctive,
            tense:Tense,
            argbound:no,
            polarity:Pol,
            gap:[],w:W,add:Add,
            flagint:no,
            constituent_structure:Cstr],FSpred),

constraint([tense:or([present,perfect])],[tense:Tense]),       % not how the constraint is checked
                     % in order not to damage the Tense value
                     % assigned to the predicate

start(PL,B),
append([p(A,B)],PL,PLtot),
map(pred,[cat:pred,type:finite,pathlist:PLtot,distance:Distance,
            illocutionary_force:wish,
            number:N,person:Person, mood:subjunctive,class:m,
            tense:Tense,argbound:no,polarity:Pol,
            gap:[],w:W,add:Add,flagint:no,
            constituent_structure:[illocutionary_force:wish,Cstr]])].


% regrets
%%%%%%%%%

% utinam + imperfect or pluperfect subjunctive


[finite, pred9] --->
[mapped(part,[from:A, to:B|Utinam]),
constraint([lex:utinam],Utinam),
mapped(pred,FSpred),
constraint([type:finite,class:m,pathlist:PL,distance:Distance,number:N,person:Person,
            mood:subjunctive,
            tense:Tense,
            argbound:no,polarity:Pol,
            gap:[],w:W,
            add:Add,
            flagint:no,
            constituent_structure:Cstr],FSpred),

constraint([tense:or([imperfect, pluperfect])],[tense:Tense]),

start(PL,B),
append([p(A,B)],PL,PLtot),

map(pred,[cat:pred,type:finite,
            pathlist:PLtot,distance:Distance,
            illocutionary_force:regret,class:m,
            number:N,person:Person,
            mood:subjunctive,
            tense:Tense,
            argbound:no,polarity:Pol,
            gap:[],w:W,
            add:Add,
            flagint:no,
            constituent_structure:[illocutionary_force:regret,Cstr]])].


















%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% BASIC DECLARATIVE CLAUSES

% start here to get a flavour of the structural part of the parsing process
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Rex scribit epistulas
%%%%%%%%%%%%%%%%%%%%%%%



[finite,predbase] --->
[
   mapped(vg,FSverb),
   constraint([number:Nsubj,
              person:Psubj,
              type:finite,
              voice:Voice,
              mood:Mood,
              tense:Tense,
              pathlist:PathlistVerb,
              lex:Clex,
              w:WVerb],FSverb),

   ifthenelse(constraint([polarity:neg],FSverb), Pol=neg, Pol=pos), % polarity is read off from the verb group


   Voice \= middle,                             % middle voice is given a specific treatment of the args
                  % and gets its own clause pattern

   lexarg(Clex,arglist:ArgList),
   pick(ws(Lex,_,clause:Clause_Constraints,mwuw:MW,args:Args), ArgList,_),

   ifthen(constraint([person:PersonC],Clause_Constraints),                       % type of possible constraint NOT USED
          constraint([person:PersonC],[person:Psubj])),

   ifthenelse(Voice=pass,

              % selecting the object to remove it from the arglist and turn it into a subject
                (pick(object:ObjectSpecs,Args,Args1),  % note that there must be an object if a passive was produced !!
                 pick(oblig:_, ObjectSpecs,OS1),
                 pick(constraints:Oconstraints,OS1,OS2),
                 pick(case:_,Oconstraints,Oconstraints1),
                 append([case:nom],Oconstraints1,NewOconstraints),
                 append([constraints:NewOconstraints],OS2,NOS),
                 append([oblig:yes],NOS,NewObjectSpecs),


              % turning the subject into an optional (a+) abl pp arg
                pick(subject:SubjectSpecs,Args1,Args2),
                pick(constraints:Sconstraints,SubjectSpecs,_),

                 % make_arg(Sconstraints,NewArg),

               ifthenelse( Sconstraints=[],
                                   % no constraint on subj: both types of agent are OK  IF-CLAUSE
                        ( NewArg=[type:pp,oblig:no,constraints:[prep:ab,sem:[hum]]] ;        % THEN-CLAUSE
                      NewArg=[type:np,oblig:no,constraints:[case:abl]])  ,
                                                                                     % ELSE-CLAUSE:
                 ifthenelse(   constraint([sem:[hum]],Sconstraints),   % IF2-clause     % there are constraints: we act accordingly
                                    NewArg=[type:pp,oblig:no,constraints:[prep:ab,sem:[hum]]],    % THEN2-clause
                                    NewArg=[type:np,oblig:no,constraints:[case:abl]])),          % ELSE2-clause


                 append([agent:NewArg],Args2,At),
                 append([subject:NewObjectSpecs],At,Argstomatch) ),
               Argstomatch=Args),                                    % active voice : leave arglist as it is



  match_list(Argstomatch,TreeArgs,PathlistArgs,DistanceArgs,
              sujet([number:Nsubj,gender:_,person:Psubj]),
              finite,
              gap:Gap,
              w:Weight,
              Int,
              PathlistVerb),



 full_expansions(FE,finite),
   expandkernel(FE,ExpandTrees,ExpandPaths,ExpandDistances,w:WExpand, Int, partial),  % here too weight is computed
                      % and the interrogative can hide in the adjuncts
  append(PathlistVerb,PathlistArgs,CPathlist),
  flatten(CPathlist,CFlat),



   flatten(ExpandPaths,EFlat),
   adjacent(EFlat,CFlat),
   append(ExpandPaths,CPathlist,AllPaths),
   flatten(AllPaths,AP),


/*
  recorded(fin,fin(Fin),_),

   % in the present implementation, the question mark is not returned
   % the reason is that we cannot rely on an element introduced by modern editors
   % therefore AP=APF, always

   ifthenelse(mapped(punct,[from:Lastbutone,to:Fin,lex:question_mark|_]),
               (append([p(Lastbutone,Fin)],APF,AP), Int=int),
                AP=APF),

*/



   myappend(DistanceArgs,ExpandDistances,Distances),
   flatten(Distances, FlatDist),
   sum(FlatDist, Distance),

  flatten(AP, Flat),
  msort(Flat, NSorted),
   \+dup(NSorted),

  % flatten(OtherPL,OtherPLflat),         % see above
  %  msort(OtherPLflat,OtherPLflatsorted),

  % contiguous(OtherPLflatsorted),


 start(NSorted,Start),

 ifthenelse(       mapped(punct,[from:Z,to:Start,lex:colon|_]),           % we have a colon in front of the finite clause
                   (Add=colon, append([p(Z,Start)],NSorted,NSortedF)),     % we add the colon to the path and register the property
                   (Add=no, NSortedF=NSorted)                              % we keep the old values
             ),


                               % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%
 ifthenelse( Add=colon,                                        % the colon indicates that the clause is a bit of direct speech
                                         contiguous(NSorted), quasicontiguous4(NSorted)),


 ifthen(Add=colon,Gap=[]), % no gap in piece of direct speech used as arg


  append(TreeArgs,ExpandTrees,AllTrees),
  insort(AllTrees, ST),




  % the proper treatment of gaps in cplt clauses involves percolating them up:

    ifthen((constraint([object:OBJ],ST),constraint([cat:pred,gap:Gapobj],OBJ)), % IF-clause
        ifthen(Gapobj\=[],Gap=Gapobj)),  % THEN-clause

/*
  % idem for weight:

  ifthenelse(  (constraint([object:OBJ],ST),constraint([cat:pred,w:LocalWeight],OBJ)), % IF-clause
                                           TotalWeight is LocalWeight+Weight,         % THEN-clause
                                          TotalWeight=Weight),            % ELSE-clause

% more doubtful : it's only ONE arg, and should not be weightier than the others - or should it????
*/


   % priority should be given to the subject-predicative order : parsimonia(subject) est scientia (predicative)
                % vitandi sumptus supervacuos

  ifthenelse(constraint([subject:[hp:Pathsubj]],ST),  % we have a non-gapped subject, we record its head path
       true,              % and abstain from doing anything else
             Pathsubj=[p(0,0)]),          % otherwise the subject is higher up and therefore necessarily precedes

 % in case nom and acc coalesce on both subject and object : artes studia conciliant

  ifthenelse(constraint([object:[hp:Pathobj]],ST),                % IF-CLAUSE we have an object
             ifthenelse(precedes(Pathsubj,Pathobj), NW is Weight+1, NW=Weight),         % THEN-CLAUSE
             NW=Weight),                % ELSE-CLAUSE


  ifthenelse(constraint([predicative:[hp:Pathpredicative]],ST),           % IF-CLAUSE we have a predicative
             ifthenelse(precedes(Pathsubj,Pathpredicative), NW1 is NW+1, NW1=NW),     % THEN-CLAUSE
             NW1=NW),               % ELSE-CLAUSE

  ifthenelse( (constraint([object:[hp:Pathobject]],ST), constraint([object_cplt:[hp:Pathobjectcplt]],ST)),
                              % IF-CLAUSE we have both obj and obj cplt
             ifthenelse(precedes(Pathobject,Pathobjectcplt), NW2 is NW1+2, NW2=NW1),         % THEN-CLAUSE
             NW2=NW1),                % ELSE-CLAUSE



   ifthen( (constraint([object:[gender:GENDERinNP,number:NUMBERinNP]],ST),
            constraint([object_cplt:[cat:adjp,gender:GENDERinADJ,number:NUMBERinADJ]],ST)
            ),
             (funify([gender:GENDERinADJ], [gender:GENDERinNP],_),
             funify([number:NUMBERinADJ],[number:NUMBERinNP],_))
         ),

     % if the object cplt is an adj, it must agree in gender and number with the object


ifthen( (constraint([object:[gender:GENDERinNP,number:NUMBERinNP]],ST),
            constraint([object_cplt:[cat:np,gender:GENDERinNP1,number:NUMBERinNP1]],ST)
            ),
             (funify([gender:GENDERinNP], [gender:GENDERinNP1],_),
             funify([number:NUMBERinNP],[number:NUMBERinNP1],_))
         ),

     % if the object cplt is an np, it must agree in gender and number with the object
     % probably too strict







  ifthen( (constraint([subject:[gender:GENDERinNP1,number:NUMBERinNP1]],ST),
            constraint([predicative:[cat:adjp,case:nom,gender:GENDERinADJ1,number:NUMBERinADJ1]],ST)
            ),
            (funify([gender:GENDERinADJ1], [gender:GENDERinNP1],_),
             funify([number:NUMBERinADJ1],[number:NUMBERinNP1],_))

         ),

  % predicative adj : triple agreement


   ifthen( (constraint([subject:SUBJECT],ST),
            constraint([predicative:[cat:np,case:CASEPRED]],ST)
            ),
            CASEPRED=nom
         ),
  % predicative np : case agreement



  % applying the clausal constraints (polarity):
  ifthen(Clause_Constraints=[[polarity:neg]], clause_constraints(Clause_Constraints,FSverb)),

  ifthenelse(var(Int),Force=statement,Force=question),  % the check on the uninstantiated character of Int doesn't seem very reliable...

   % applying the clausal constraints (non-affirmative context)
   ifthen(Clause_Constraints=[[illocutionary_force:question]],Force=question),

   ifthenelse(contains_interrogative(NSorted),Interrogative=yes,Interrogative=no),  % the path is inspected to see
										    % if it contains an interrogative word

    % Wtot is NW2+WExpand+WVerb+MW,
   myplus(NW2,(round(WExpand/4)),T1),
   myplus(T1,WVerb,T2),
   myplus(T2,MW,Wtot),

  append([selected_reading:Lex],FSverb,FSverbfull),
  ifthenelse(Interrogative=yes, Flagint=or([yes_no_question,wh_question]), Flagint=no),


  ParseTot=[constituent_structure:[_|ST]],  % ALP 194  bind_se at clause level, and not only when parse is ready to be printed

   ifthen(bind_se(ParseTot), true),  % ALP 194 : here we perform se-binding on the clauses as soon as they are mapped
                                     % the general bind_se on the parses ready to be printed
                                     % embodies the propagation of se_binding on embedded non-finite args 

   % PROBLEM with finite clauses that are argbound - we should be able to undo the work carried out by se_binding at clause level

  map(pred,[cat:pred,
              type:finite,
              pathlist:NSortedF,
              distance:[Distance],
              illocutionary_force:Force,
              class:m,
              number:sing,                      % nber, gender, and person of the CLAUSE, not its subject
              person:3,vgperson:Psubj, % needed in imperative clauses of the first and second persons
              gender:neuter,
              mood:Mood,
              tense:Tense,
              polarity:Pol,
              argbound:no,
              gap:Gap,
              w:Wtot,
              add:Add,
              checkint:Interrogative, % this is the test that seems to work... needs to be carried over to other clauses, i.e. non-finite ones?
              flagint:Flagint,
              constituent_structure:[illocutionary_force:Force,distance:[Distance],vg:FSverbfull|ST]])]. % ST is the sorted list of arg fillers










% scribit epistulas
%%%%%%%%%%%%%%%%%%%

% subject to be found elsewhere ; works as a building block in conjoined structures


[finite,pred10] --->
[ mapped(vg,FSverb),
  constraint([number:Nsubj,person:Psubj,type:finite,voice:Voice,
              mood:Mood, tense:Tense,pathlist:PathlistVerb,lex:Clex, w:WVerb],FSverb),
  ifthenelse(constraint([polarity:neg],FSverb), Pol=neg, Pol=pos),
  Voice \= middle,
  lexarg(Clex,arglist:ArgList),
  pick(ws(Lex,_,clause:Clause_Constraints,mwuw:MW,args:Args), ArgList,_),
  pick(subject:SubjectSpecs,Args,Args1),
  pick(constraints:Sconstraints,SubjectSpecs,_), % we need access to the constraints on the subject for two purposes
            % a) passivization b) making sure that there is no LEXICAL constraint on the subject
            % since it is not available in this type of incomplete, subjectless clause
  ifthenelse(Voice=pass,
              % selecting the object to remove it from the arglist
                (pick(object:_,Args1,Args2),  % note that there must be an object if a passive was produced !!


              % turning the subject into an optional (a+) abl pp arg

              %  make_arg(Sconstraints,NewArg),

                ifthenelse( Sconstraints=[],
                                   % no constraint on subj: both types of agent are OK  IF-CLAUSE
                        ( NewArg=[type:pp,oblig:no,constraints:[prep:ab,sem:[hum]]] ;        % THEN-CLAUSE
                      NewArg=[type:np,oblig:no,constraints:[case:abl]])  ,
                                                                                     % ELSE-CLAUSE:
                 ifthenelse(   constraint([sem:[hum]],Sconstraints),   % IF2-clause     % there are constraints: we act accordingly
                                    NewArg=[type:pp,oblig:no,constraints:[prep:ab,sem:[hum]]],    % THEN2-clause
                                    NewArg=[type:np,oblig:no,constraints:[case:abl]])),          % ELSE2-clause


                 append([agent:NewArg],Args2,At),
                 append([subject:_],At,Argstomatch) ),  % dummy subject to be dropped by match_list in mode finite_i !!!

               Argstomatch=Args),

  % we should look for constraints on the subject - if there is a lexical constraint we need the subject in the clause and cannot drop it
  % we do not build the non-finite incomplete clause

            \+member([lex:_],Sconstraints),

  match_list(Argstomatch,TreeArgs,PathlistArgs,DistanceArgs,
              sujet([number:Nsubj,gender:_,person:Psubj]),

              finite_i,     % indicates that we should not look for a subject !!!!!!!!!!!!!!!!!!!

              gap:Gap,
              w:Weight,
              Int,
              PathlistVerb),



 full_expansions(FE),
  expandkernel(FE,ExpandTrees,ExpandPaths,ExpandDistances,w:WExpand, Int, full),  % here too weight is computed
                      % and the interrogative can hide in the adjuncts
  append(PathlistVerb,PathlistArgs,CPathlist),
  flatten(CPathlist,CFlat),
  flatten(ExpandPaths,EFlat),
  adjacent(EFlat,CFlat),
  append(EFlat,CFlat,AllPaths),
  myappend(DistanceArgs,ExpandDistances,Distances),
  flatten(Distances, FlatDist),
  sum(FlatDist, Distance),
  flatten(AllPaths, Flat),
  msort(Flat, Sorted),
  \+dup(Sorted),



  append(TreeArgs,ExpandTrees,AllTrees),
  contiguous(Sorted),                                    % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%
  insort(AllTrees, ST),
  clause_constraints(Clause_Constraints,FSverb),
  % Wtot is Weight+WVerb+MW+WExpand
  myplus(Weight,WVerb,T1),
  myplus(T1,WExpand,T2),
  myplus(T2,MW,Wtot),
  append([selected_reading:Lex],FSverb,FSverbfull),

  map(pred,[cat:pred,
              type:finite_i,          % records its subjectless nature
              pathlist:Sorted,distance:[Distance],
              number:Nsubj,person:Psubj,gender:_,
              class:m,
              mood:Mood, tense:Tense,
              polarity:Pol,
              gap:Gap,
              w:Wtot,
              constituent_structure:[vg:FSverbfull|ST]])].









%%%%%%%%%%%%%%%
% MIDDLE VOICE
%%%%%%%%%%%%%%%

% Middle Voice in this parser is a mere technical term - nothing to do with middle voice as diathesis
% it is meant to account for 'impersonal passives', which are not really passives either

% insanitur etc...

[finite,pred11] --->
[ mapped(vg,FSverb),
  ifthenelse(constraint([polarity:neg],FSverb), Pol=neg, Pol=pos),
  constraint([number:sing,
              person:3,
              type:finite,
              voice:middle,    % conditions are strict : third person sg middle voice

              mood:Mood,
              tense:Tense,
              pathlist:PathlistVerb,
              lex:Clex,
              w:WVerb],FSverb),
  lexarg(Clex,arglist:ArgList),

  pick(ws(Lex,_,clause:Clause_Constraints,mwuw:MW,args:Args), ArgList,_),

  ifthenelse( (pick(object:OSpecs,Args,Args1), constraint([type:np],OSpecs)),
                                                     % NP as objects excluded with middles
             Nargs=Args1, Nargs=Args),

  match_list(Nargs,TreeArgs,PathlistArgs,DistanceArgs,
              sujet([number:sing,gender:neuter,person:3]),
                     finite_i,        % no specified subject : do not look for one...
                     gap:[],
                     w:Weight, Int,PathlistVerb),

  full_expansions(FE),
  expandkernel(FE,ExpandTrees,ExpandPaths,ExpandDistances,w:WExpand, Int, full),
  append(PathlistVerb,PathlistArgs,CPathlist),
    flatten(CPathlist,CFlat),
  flatten(ExpandPaths,EFlat),
  adjacent(EFlat,CFlat),
  append(ExpandPaths,CPathlist,AllPaths),
  myappend(DistanceArgs,ExpandDistances,Distances),
  flatten(Distances, FlatDist),
  sum(FlatDist, Distance),
  flatten(AllPaths, Flat),
  msort(Flat, Sorted),
   \+dup(Sorted),
  contiguous(Sorted),                                     % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%
  append(TreeArgs,ExpandTrees,AllTrees),
  insort(AllTrees, ST),
  clause_constraints(Clause_Constraints,FSverb),
  % Wtot is Weight+WExpand+WVerb+MW,
  myplus(Weight,(WExpand/4),T1),
  myplus(T1,WVerb,T2),
  myplus(T2,MW,Wtot),
  append([selected_reading:Lex],FSverb,FSverbfull),

  map(pred,[cat:pred,
              type:finite,class:m,
              pathlist:Sorted,distance:[Distance],
              number:sing,person:3,mood:Mood, tense:Tense,
              voice:middle,gender:_,
              argbound:no,
              illocutionary_force:statement,
              gap:[],
              w:Wtot,
              polarity:Pol,
              constituent_structure:[vg:FSverbfull|ST]])].








% NOMINATIVUS CUM INFINITIVO
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Rex seruam Marci amare dicitur
%

[finite,pred12] --->
[ mapped(vg,FSverb),
  ifthenelse(constraint([polarity:neg],FSverb), Pol=neg, Pol=pos),

  constraint([number:Nsubj,person:Psubj,
              type:finite,
              voice:pass,
              mood:indicative,     % (over)restricted to finite, pass, and indicative (subjunctive OK ?)
              tense:Tense,
              pathlist:PathlistVerb,
              lex:Lex,
              w:WVerb],FSverb),

 constraint([tense:or([present,imperfect, future])],[tense:Tense]),  % restrictions on tense, too

 constraint([lex:or([credere,dicere,existimare,
                     ferre,iudicare,
                     negare, nuntiare, putare,scribere,
                     tradere,uidere])],[lex:Lex]),   % restriction on the verb
                 % here too we are using a list
                 % should be incremented as vocabulary coverage increases
                 % but a list remains the best tool
 mapped(np,FSnp),
 constraint([number:Nsubj, person:Psubj, gender:Gsubj,
             case:nom,            % we are in finite clauses
             pathlist:Pathlistnp, w:WNP,distance:[Distnp]],FSnp),

 mapped(pred,FSpred),

 constraint([type:nonfinite_i,        % second part : the pred assigned to the subject
             mood:infinitive,       % infinitive without its own subject
             pathlist:Pathlistpred, distance:[Distpred],
             number:Nsubj,gender:Gsubj,case:nom,person:Psubj,
             gap:[],w:Wpred,
             constituent_structure:C_str_pred], FSpred),

 append([PathlistVerb,Pathlistnp,Pathlistpred],PL),
 \+dup(PL),
 msort(PL,Sorted),

 % Distance is Distnp+Distpred,
 % Weight is WNP+Wpred+WVerb+1,
 myplus(Distnp,Distpred,Distance),
 myplus(WNP,Wpred,T1),
 myplus(T1,WVerb,T2),
 myplus(T2,1,Weight),
 map(pred,[cat:pred,type:finite,
             pathlist:Sorted,class:m,
             distance:[Distance],gender:_,
             number:Nsubj,person:Psubj,   % real values here, not as in other preds
            % where the values are filled in so that the clause can serve as subject
            % primarily of ESSE : /  number:sing, person:3, gender:neuter /
             mood:indicative,

             tense:Tense,polarity:Pol,
             illocutionary_force:statement,
             argbound:no,
             gap:[],
             w:Weight,
             add:no,           % just the clause
             constituent_structure:[vg:FSverb,
                    % subject:C_str_NP,
                    pred:[subject:FSnp,pred:C_str_pred]]]) ].  % quite a specific tree !
















%%%%%%%%%%%%%%%%%%%%%%
% SUBORDINATE AS ARG
%%%%%%%%%%%%%%%%%%%%%%


% lex(ut, sub,        [lex:ut, pos:sub, argbound:yes, mood:subjunctive]).
% lex(ne, sub,        [lex:ne, pos:sub, argbound:yes, mood:subjunctive]).

[finite,pred13] --->
[ mapped(sub,[from:A,to:B|Fsubordinator]),                 % we have a subordinator
 mapped(pred,Fp1),                                         % and a clause

 constraint([argbound:yes, mood:Mood, lex:Lex],Fsubordinator),    % the subordinator is arg-bound
                                                                  % and can specify mood
 constraint([type:finite,class:m,
             pathlist:PL1,distance:Distance,                      % we have a finite clause with the right mood
             gap:[],             % and gapless
             mood:Mood,
             tense:Tense,
             w:Weight,
             constituent_structure:Head],Fp1),



 append([p(A,B)], PL1,NPL1),                     % add subordinator to subordinate clause
 msort(NPL1,Sorted),
 quasicontiguous4(Sorted),                       % room for one or two words to stand out        % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%
  \+dup(Sorted),

 ifthenelse(contiguous(Sorted),BonusC=3,BonusC=0), % bonus for strict contiguousness

 % the pred we build should bear all the features that an arg expects,
 % including number, gender, case and index
 % weight is increased because it is an argument
 myplus(Weight,3,NW),
 myplus(NW,BonusC,NWW),

 map(pred,[cat:pred,type:finite,
             pathlist:Sorted,distance:Distance,
             gap:[],class:m,
             number:_,person:_,gender:_,case:_,
             mood:Mood,
             tense:Tense,
             w:NWW,
             subordinator:Lex,                 % we keep track of which subordinator is used
             argbound:yes,                     % and declare the clause arg-bound, not free-standing
             constituent_structure:Head])].

                                                    % ALP 194 Lex taken out of constituent_structure
                                                    % Head instead of NewHead






% YES-NO QUESTION AS ARG
%%%%%%%%%%%%%%%%%%%%%%%%%

% lex(num,part,[lex:num,type:int,value:open_orientation,clausetype:sub]). % in indirect questions

[finite,pred14] --->
[ mapped(part,[from:A,to:B|Fint]),
  constraint([type:int, clausetype:sub], Fint),     % interrogative particle for subordinate clause

 mapped(pred,Fp1),

 constraint([type:finite,class:m,
             pathlist:PL1,distance:Distance,
             gap:[],
             mood:subjunctive,                     % subjunctive mood in indirect yes-no questions
             tense:Tense,
             w:Weight,
             flagint:no,                            % the clause does not feature an interrogative word (case then of a wh-question)
             constituent_structure:Head],Fp1),
 append([p(A,B)], PL1,NPL1),                     % add interrogative particle to subordinate clause
 msort(NPL1,Sorted),
 contiguous(Sorted),                               % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%
  \+dup(Sorted),
 % TotalWeight is Weight+3,                        % a bonus for such constructions + contiguity bonus
 myplus(Weight,6,TotalWeight),
 % the pred should bear all the features that an arg expects,
 % including number, gender, case and index

 map(pred,[cat:pred,type:finite,
             pathlist:Sorted,distance:Distance,
             gap:[],class:m,
             number:sing,person:3,gender:neuter,case:_,
             mood:subjunctive,
             tense:Tense,
             w:TotalWeight,
             argbound:yes,
             flagint:yes_no_question,    % flagint records the type of question (yes-no here, wh-based elsewhere)
             checkint:yes,
             constituent_structure:Head])].






% FREE YES-NO QUESTION
%%%%%%%%%%%%%%%%%%%%%%%

% i.e. not as subordinate clauses

% lex(ne_int, part,   [lex:ne_int,  type:int, value:open_orientation,     clausetype:_]).
% lex(num,    part,   [lex:num,     type:int, value:negative_orientation, clausetype:main]). % in direct questions
% lex(nonne,  part,   [lex:num,     type:int, value:positive_orientation, clausetype:_]). % in direct questions

% ne_int is produced on the basis of enclitic ne


[finite,pred15] --->
[ mapped(part,[from:A,to:B|Fint]),
  constraint([type:int, clausetype:main, value:Value], Fint),
                                         % the type of interrogative particle we find in main clauses

 mapped(pred,Fp1),
 constraint([type:finite,class:m,
             pathlist:PL1,distance:Distance, gap:[],
             mood:Mood, tense:Tense,w:Weight,
             % flagint:no,
             constituent_structure:Head],Fp1),
 (Mood=indicative; Mood=subjunctive),            % no imperative, for instance
                                              % the subjunctive carries various values
             % and should be recorded in the parse


 append([p(A,B)], PL1,NPL1),                     % add interrogative particle
 msort(NPL1,Sorted),
 contiguous(Sorted),                             % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%
  \+dup(Sorted),
 % TotalWeight is Weight+3,
  myplus(Weight,3,TotalWeight),
 map(pred,[cat:pred,type:finite,
             pathlist:Sorted,distance:Distance, gap:[],
             illocutionary_force:question,class:m,
             number:sing,person:3,gender:neuter,polarity:Value,
             argbound:no,
             mood:Mood,       % the various discourse values associated with subjunctive mood
            % can be specified later, on the basis of more than just syntactic parsing
             tense:Tense,w:TotalWeight,
             flagint:yes_no_question,
             constituent_structure:[yes_no_question,Value,Head]])].





% WITH FREE SUBORDINATE CLAUSE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% the free subordinate clauses can be associated with various types of main clauses
% it is on the basis of subordinator, mood and tense that we can retrieve semantic values and discourse functions


% lex(si, sub,        [lex:si, pos:sub, argbound:no, mood:_]).
% lex(nisi, sub,      [lex:nisi, pos:sub, argbound:no, mood:_]).
% lex(ni, sub,        [lex:nisi, pos:sub, argbound:no, mood:_]).
% lex(cum, sub,       [lex:cum, pos:sub, argbound:no, mood:_]).
% lex(dum, sub,       [lex:dum, pos:sub, argbound:no, mood:_]).
% lex(ut,sub,   [lex:ut,       pos:sub, argbound:no,   mood:indicative, value:comparison]).
% lex(uti,sub,    [lex:uti,       pos:sub, argbound:no,   mood:indicative, value:comparison]).
% lex(ut, sub,        [lex:ut, pos:sub, argbound:no, mood:subjunctive, value:or([purpose,consequence])]).
% lex(ne, sub,        [lex:ne, pos:sub, argbound:no, mood:subjunctive, value:purpose]).
% lex(quoniam, sub,   [lex:quoniam, pos:sub, argbound:no, mood:_]).
% lex(quia, sub,      [lex:quia, pos:sub, argbound:no, mood:_]).
% lex(quod, sub,      [lex:quod, pos:sub, argbound:no, mood:_]).




% MAIN CLAUSE AND UNBOUND SUB CLAUSE TOGETHER
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

[finite,pred16] --->
[
 mapped(pred,FSub),
 mapped(pred,FMain),
 FSub \= FMain,       %  2 clauses

 constraint([class:s,type:finite,
             pathlist:PLSub,distance:[Distp1], gap:[],argbound:no,
             mood:MoodSub,
             tense:TenseSub,subordinator:Fsubordinator,
             w:W1,constituent_structure:Head1],FSub),



 constraint([type:finite,class:m,
             pathlist:PLMain,distance:[Distp2], gap:[],argbound:no,
             mood:MoodMain,
             tense:TenseMain,
             w:W2,constituent_structure:Head2],FMain),

( (start(PLMain,StartMain),extremity(PLSub,StartMain)) ;
  (extremity(PLMain,StartSub), start(PLSub,StartSub)) ;
  (recorded(fin,fin(Fin),_),start(PLMain,0),extremity(PLMain,Fin))
),

 append(PLSub, PLMain,PL),
 msort(PL,Sorted),
 \+dup(Sorted),
 quasicontiguous(Sorted),                       % still room for one or two words out
 myplus(Distp1,Distp2,Distance),
 myplus(W1,W2,Weight),

% Clause semantics should be developed further
% we can use the nature of the subordinator as well as both mood and tense in both clauses


 ifthen( (constraint([lex:si],Fsubordinator),     % OPEN condition
          MoodMain=indicative,
          MoodSub=indicative),
          Clausesem=open),

 ifthen( (constraint([lex:si],Fsubordinator),                   % POTENTIALIS
          MoodSub=subjunctive,
          (TenseSub=perfect;TenseSub=present),
          MoodMain=subjunctive,
          TenseMain=present),
          Clausesem=potentialis),

 ifthen( (constraint([lex:si],Fsubordinator),     % IRREALIS (present)
          MoodSub=subjunctive,
          TenseSub=imperfect,
          MoodMain=subjunctive,
          TenseMain=imperfect),
          Clausesem=irrealis_present),

 ifthen( (constraint([lex:si],Fsubordinator),     % IRREALIS (past)
          MoodSub=subjunctive,
          (TenseSub=pluperfect;TenseSub=imperfect),
          MoodMain=subjunctive,
          TenseMain=pluperfect),
          Clausesem=irrealis_past),

 ifthen( (constraint([lex:or([cum,dum])],Fsubordinator),           % CUM DUM for time full stop
          MoodSub=indicative,
          MoodMain=indicative),
          Clausesem=modality_free_time_clause),

 ifthen( (constraint([lex:or([cum,dum])],Fsubordinator),      % CUM DUM for time+ and so on...
          MoodSub=subjunctive,
          MoodMain=indicative),
          Clausesem=modality_laden_time_clause),

 map(pred,[cat:pred,type:finite,
             pathlist:Sorted,distance:[Distance],
             gap:[],class:m,
             argbound:no,
             mood:MoodMain,
             tense:TenseMain,
             w:Weight,
             constituent_structure:[clause_semantics:Clausesem, subordinator:Fsubordinator,subordinate_clause:Head1, main_clause:Head2]])].




% UNBOUND SUBORDINATE CLAUSE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%



[finite,pred17] --->

[mapped(sub,[from:StartSubordinator,to:EndSubordinator|Fsubordinator]),        % subordinatOR
 constraint([argbound:no,lex:LexSub,mood:MoodSub],Fsubordinator),

 mapped(pred,SubClause),
 constraint([type:finite,class:m,pathlist:PLSub,distance:Dis, gap:[],

             mood:MoodSub, tense:TenseSub,w:W1,constituent_structure:HeadSub],SubClause),


 start(PLSub,StartSubClause), %  possible displacement of
                                % the subject and other constituents out of the subordinate
        % 'servi mei si me metuerent... Cicero, Cat. I

 MinusOne is StartSubordinator - 1,
 MinusTwo is StartSubordinator - 2,
 MinusThree is StartSubordinator - 3,

 ( (StartSubClause=EndSubordinator, Bonus=2);
   (StartSubClause=MinusOne, Bonus=0);
   (StartSubClause=MinusTwo, Bonus=(-1));
   (StartSubClause=MinusThree, Bonus=(-2))
 ),

 extremity(PLSub,EndSub),
 append([p(StartSubordinator,EndSubordinator)], PLSub,NPLSub),                     % add subordinator to subordinate clause
 msort(NPLSub, NPLSubSorted),

 start(NPLSubSorted,StartFullSubClause),


 ifthenelse(mapped(punct,[from:Here,to:StartSubordinator, lex:comma|_]),           % bonus for comma in front of SUBORDINATOR
            BonusSub is 2, BonusSub is 0),


 ifthenelse(mapped(punct,[from:StP1,to:StartFullSubClause, lex:comma|_]), % in front of Subordinate Clause, not necessarily
                  % in front of SUBORDINATOR ..., Ithacam ut videret, ....
            append([p(StP1,StartFullSubClause)],NPLSubSorted,NPLSub1),
            NPLSub1=NPLSub),

 ifthenelse(mapped(punct,[from:EndSub,to:EndP2, lex:comma|_]),  % at the end of SubClause
            append([p(EndSub,EndP2)],NPLSub1,NPLSub2),
            NPLSub2=NPLSub1),

 myplus(W1,Bonus,W),
 myplus(W,BonusSub,Wtot),
msort(NPLSub2,NPL1sorted),
 contiguous(NPL1sorted),
 map(pred,[cat:pred,type:finite,class:s,
             pathlist:NPL1sorted,distance:Dis,
             gap:[],number:sing,person:3,gender:neuter,case:nom,
             mood:MoodSub, tense:TenseSub,
             w:Wtot,subordinator:Fsubordinator,
             argbound:no,
             constituent_structure:[LexSub,HeadSub]])].





















% ELLIPSIS
%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% rex patriam amat, regina regem. (Finite + comma + arglist2)
% mater insanit, filius, pater.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


/* proposal to have only comma, no coordinator:

% ( (mapped(punct,[from:EndMain,to:StartEllipted, lex:comma|_]), CC=comma) ;
  % (mapped(coord,[from:EndMain,to:StartEllipted, lex:Coord|_]), CC=Coord)),
  % it's better to have both coordinator and comma as delimiters
  % but coordination is very expensive and double work is entailed
  % TO BE REVISED

 mapped(punct,[from:EndMain,to:StartEllipted, lex:comma|_]),
 CC=comma,
*/

% Main clause precedes
%%%%%%%%%%%%%%%%%%%%%%

[finite,ellipsis1] --->

[ % ( (mapped(punct,[from:EndMain,to:StartEllipted, lex:comma|_]), CC=comma) ;
  % (mapped(coord,[from:EndMain,to:StartEllipted, lex:Coord|_]), CC=Coord)),


mapped(punct,[from:EndMain,to:StartEllipted, lex:comma|_]),
CC=comma,


 mapped(pred,Main),

constraint([type:finite,class:m,pathlist:PLMain,distance:[DistMain], gap:[],
             number:NMain,person:PMain,polarity:Pol,
             mood:MoodMain, tense:TenseMain,w:WMain,constituent_structure:[illocutionary_force:Force,vg:FSverb|ST]],Main),
start(PLMain,0),
extremity(PLMain,EndMain),
append([p(EndMain,StartEllipted)],PLMain,NPLMain),
constraint([number:Nsubj,
              person:Psubj,
              voice:Voice,
              lex:Clex],FSverb),
lexarg(Clex,arglist:ArgList),
pick(ws(_,_,clause:_,mwuw:_,args:Args), ArgList,_),

   ifthenelse(Voice=pass,

              % selecting the object to remove it from the arglist and turn it into a subject
                (pick(object:ObjectSpecs,Args,Args1),  % note that there must be an object if a passive was produced !!
                 pick(oblig:_, ObjectSpecs,OS1),
                 pick(constraints:Oconstraints,OS1,OS2),
                 pick(case:_,Oconstraints,Oconstraints1),
                 append([case:nom],Oconstraints1,NewOconstraints),
                 append([constraints:NewOconstraints],OS2,NOS),
                 append([oblig:yes],NOS,NewObjectSpecs),


              % turning the subject into an optional (a+) abl pp arg
                pick(subject:SubjectSpecs,Args1,Args2),
                pick(constraints:Sconstraints,SubjectSpecs,_),

              %   make_arg(Sconstraints,NewArg),

                ifthenelse( Sconstraints=[],
                                   % no constraint on subj: both types of agent are OK  IF-CLAUSE
                        ( NewArg=[type:pp,oblig:no,constraints:[prep:ab,sem:[hum]]] ;        % THEN-CLAUSE
                      NewArg=[type:np,oblig:no,constraints:[case:abl]])  ,
                                                                                     % ELSE-CLAUSE:
                 ifthenelse(   constraint([sem:[hum]],Sconstraints),   % IF2-clause     % there are constraints: we act accordingly
                                    NewArg=[type:pp,oblig:no,constraints:[prep:ab,sem:[hum]]],    % THEN2-clause
                                    NewArg=[type:np,oblig:no,constraints:[case:abl]])),          % ELSE2-clause


                 append([agent:NewArg],Args2,At),
                 append([subject:NewObjectSpecs],At,Argstomatch) ),
               Argstomatch=Args),                                    % active voice : leave arglist as it is


ifthenelse( Clex=esse,            % esse (and other predicatives such as videri etc)
                % need number and person to be passed on
                % *sum consul, iugurtha rex is out, at least for this grammar
            match_list(Argstomatch,TreeArgs,PathlistArgs,DistanceArgs,
                      sujet([number:Nsubj,gender:_,person:Psubj]),
                      finite,
                      gap:[],
                      w:WeightArgs,
                      Int,
                      [p(EndMain,StartEllipted)]), % use comma to compute arg bearer substituted position

             match_list(Argstomatch,TreeArgs,PathlistArgs,DistanceArgs,  % others need a free hand so that number and person can change from
                  % one clause to the next
                  % Contemnunt novitatem meam, ego [contemno] illorum ignaviam.
                      sujet([number:_,gender:_,person:_]),
                      finite,
                      gap:[],
                      w:WeightArgs,
                      Int,[p(EndMain,StartEllipted)])
           ),


full_expansions(FE),
   expandkernel(FE,ExpandTrees,ExpandPaths,ExpandDistances,w:WeightExpands, Int, partial),  % here too weight is computed
                      % and the interrogative can hide in the adjuncts

% PATH
flatten(PathlistArgs,FlatArgs),
flatten(ExpandPaths,FlatExpands),
msort(FlatArgs,ArgsSorted),
msort(FlatExpands,ExpandsSorted),
adjacent(ArgsSorted,ExpandsSorted),
append(ArgsSorted,ExpandsSorted,EP),
flatten(EP,EPflat),
msort(EPflat,EPsorted),
contiguous(EPsorted),
 \+dup(EPsorted),
start(EPsorted,StartEllipted),
append(NPLMain,EPsorted,PP),
msort(PP,PPsorted),
contiguous(PPsorted),

% DISTANCES
myappend(DistanceArgs,ExpandDistances,Distances),
flatten(Distances, FlatDist),
sum(FlatDist, DistanceSum),
myplus(DistanceSum,DistMain,DistanceAll),

% WEIGHTS
 myplus(WeightArgs,(round(WeightExpands/4)),Wpartial),
 myplus(WMain,Wpartial,Wtot),

% ARG TREES
append(TreeArgs,ExpandTrees,AllTrees),
insort(AllTrees, NST),


map(pred,[cat:pred,
              type:finite,class:m,
              pathlist:PPsorted,
              distance:[DistanceAll],
              illocutionary_force:Force,
              number:NMain,                      % nber, gender, and person of the CLAUSE, not its subject
              person:PMain,
              gender:neuter,
              mood:MoodMain,
              tense:TenseMain,
              polarity:Pol,
              argbound:no,
              gap:[],
              w:Wtot,
              add:_,
              flagint:_,
              constituent_structure:[illocutionary_force:Force,vg:FSverb,arglist:ST,linker:CC,arglist:NST]]) ].




% Main clause follows
%%%%%%%%%%%%%%%%%%%%%

% main follows : only comma admitted as separator, Main begins at comma and ends at sentence end
% ego illorum ignaviam, illi novitatem meam contemnunt.

[finite,ellipsis2] --->

[ mapped(punct,[from:Here,to:BeginMain, lex:comma|_]),
 CC=comma,

 recorded(fin,fin(Fin)),
 mapped(pred,Main),

constraint([type:finite,class:m,pathlist:PLMain,distance:[DistMain], gap:[],
             number:NMain,person:PMain,polarity:Pol,
             mood:MoodMain, tense:TenseMain,w:WMain,constituent_structure:[illocutionary_force:Force,vg:FSverb|ST]],Main),
start(PLMain,BeginMain),
extremity(PLMain,Fin),
append([p(Here,BeginMain)],PLMain,NPLMain),

constraint([number:Nsubj,
              person:Psubj,
              voice:Voice,
              lex:Clex],FSverb),
lexarg(Clex,arglist:ArgList),
pick(ws(_,_,clause:_,mwuw:_,args:Args), ArgList,_),

   ifthenelse(Voice=pass,

              % selecting the object to remove it from the arglist and turn it into a subject
                (pick(object:ObjectSpecs,Args,Args1),  % note that there must be an object if a passive was produced !!
                 pick(oblig:_, ObjectSpecs,OS1),
                 pick(constraints:Oconstraints,OS1,OS2),
                 pick(case:_,Oconstraints,Oconstraints1),
                 append([case:nom],Oconstraints1,NewOconstraints),
                 append([constraints:NewOconstraints],OS2,NOS),
                 append([oblig:yes],NOS,NewObjectSpecs),


              % turning the subject into an optional (a+) abl pp arg
                pick(subject:SubjectSpecs,Args1,Args2),
                pick(constraints:Sconstraints,SubjectSpecs,_),

              %   make_arg(Sconstraints,NewArg),

              ifthenelse( Sconstraints=[],
                                   % no constraint on subj: both types of agent are OK  IF-CLAUSE
                        ( NewArg=[type:pp,oblig:no,constraints:[prep:ab,sem:[hum]]] ;        % THEN-CLAUSE
                      NewArg=[type:np,oblig:no,constraints:[case:abl]])  ,
                                                                                     % ELSE-CLAUSE:
                 ifthenelse(   constraint([sem:[hum]],Sconstraints),   % IF2-clause     % there are constraints: we act accordingly
                                    NewArg=[type:pp,oblig:no,constraints:[prep:ab,sem:[hum]]],    % THEN2-clause
                                    NewArg=[type:np,oblig:no,constraints:[case:abl]])),          % ELSE2-clause


                 append([agent:NewArg],Args2,At),
                 append([subject:NewObjectSpecs],At,Argstomatch) ),
               Argstomatch=Args),                                    % active voice : leave arglist as it is


ifthenelse( Clex=esse,            % esse (and other predicatives such as videri etc)
                % need number and person to be passed on
                % *sum consul, iugurtha rex is out, at least for this grammar
            match_list(Argstomatch,TreeArgs,PathlistArgs,DistanceArgs,
                      sujet([number:Nsubj,gender:_,person:Psubj]),
                      finite,
                      gap:[],
                      w:WeightArgs,
                      Int,[p(Here,BeginMain)]),

             match_list(Argstomatch,TreeArgs,PathlistArgs,DistanceArgs,  % others need a free hand so that number and person can change from
                  % one clause to the next
                  % Contemnunt novitatem meam, ego [contemno] illorum ignaviam.
                      sujet([number:_,gender:_,person:_]),
                      finite,
                      gap:[],
                      w:WeightArgs,
                      Int,
                      [p(Here,BeginMain)])
           ),


full_expansions(FE),
   expandkernel(FE,ExpandTrees,ExpandPaths,ExpandDistances,w:WeightExpands, Int, partial),  % here too weight is computed
                      % and the interrogative can hide in the adjuncts

% PATH
flatten(PathlistArgs,FlatArgs),
flatten(ExpandPaths,FlatExpands),
msort(FlatArgs,ArgsSorted),
msort(FlatExpands,ExpandsSorted),
adjacent(ArgsSorted,ExpandsSorted),
append(ArgsSorted,ExpandsSorted,EP),
flatten(EP,EPflat),
msort(EPflat,EPsorted),
contiguous(EPsorted),
 \+dup(EPsorted),
start(EPsorted,StartEllipted),
append(NPLMain,EPsorted,PP),
msort(PP,PPsorted),
contiguous(PPsorted),

% DISTANCES
myappend(DistanceArgs,ExpandDistances,Distances),
flatten(Distances, FlatDist),
sum(FlatDist, DistanceSum),
myplus(DistanceSum,DistMain,DistanceAll),

% WEIGHTS
 myplus(WeightArgs,(round(WeightExpands/4)),Wpartial),
 myplus(WMain,Wpartial,Wtot),

% ARG TREES
append(TreeArgs,ExpandTrees,AllTrees),
insort(AllTrees, NST),


map(pred,[cat:pred,
              type:finite,class:m,
              pathlist:PPsorted,
              distance:[DistanceAll],
              illocutionary_force:Force,
              number:NMain,                      % nber, gender, and person of the CLAUSE, not its subject
              person:PMain,
              gender:neuter,
              mood:MoodMain,
              tense:TenseMain,
              polarity:Pol,
              argbound:no,
              gap:[],
              w:Wtot,
              add:_,
              flagint:_,
              constituent_structure:[illocutionary_force:Force,vg:FSverb,arglist:ST,linker:CC,arglist:NST]]) ].





% regem a regina amari, reginam autem a marco. (NONFinite + comma + arglist2)
% regem a regina amari et reginam a marco                 + coord

% matres insanire, filios, patres.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* cf above:
 mapped(punct,[from:EndMain,to:StartEllipted, lex:comma|_]),
 CC=comma,
*/

% test sentence : [0/ausus,1/est,2/augustinus,3/in,4/ecclesia,5/tua,6/concupiscere,7/fructus,
%  8/mortis,9/et,10/agere,11/negotium,12/procurandi,13/eos,endpos(14)]
% cputime :  106.3125 for comma only
% as opposed to: cputime :  513.3125 for both comma and coordinator
% the second conjunct can be parsed as second conjunct OR as new arglist if coord is maintained by the side of comma

[finite,ellipsis3] --->

[  mapped(flags,active(nonfinite)),

   mapped(punct,[from:EndMain,to:StartEllipted, lex:comma|_]),
 CC=comma,


/*
 ( (mapped(punct,[from:EndMain,to:StartEllipted, lex:comma|_]), CC=comma) ;
  (mapped(coord,[from:EndMain,to:StartEllipted, lex:Coord|_]), CC=Coord)),
*/

 mapped(pred,Main),
 constraint([type:nonfinite,pathlist:PLMain,distance:[DistMain], gap:[],
             number:NMain,person:PMain,polarity:Pol,
             mood:MoodMain, tense:TenseMain,w:WMain,constituent_structure:[vg:FSverb|ST]],Main),

extremity(PLMain,EndMain),
append([p(EndMain,StartEllipted)],PLMain,NPLMain),

constraint([ type:nonfinite,
                voice:Voice,
                lex:Clex],FSverb),

lexarg(Clex,arglist:ArgList),
pick(ws(_,_,clause:_,mwuw:_,args:Args), ArgList,_),

   ifthenelse(Voice=pass,

              % selecting the object to remove it from the arglist and turn it into a subject
                (pick(object:ObjectSpecs,Args,Args1),  % note that there must be an object if a passive was produced !!
                 pick(oblig:_, ObjectSpecs,OS1),
                 pick(constraints:Oconstraints,OS1,OS2),
                 pick(case:_,Oconstraints,Oconstraints1),
                 append([case:acc],Oconstraints1,NewOconstraints),
                 append([constraints:NewOconstraints],OS2,NOS),
                 append([oblig:yes],NOS,NewObjectSpecs),


              % turning the subject into an optional (a+) abl pp arg
                pick(subject:SubjectSpecs,Args1,Args2),
                pick(constraints:Sconstraints,SubjectSpecs,_),

               %  make_arg(Sconstraints,NewArg),

               ifthenelse( Sconstraints=[],
                                   % no constraint on subj: both types of agent are OK  IF-CLAUSE
                        ( NewArg=[type:pp,oblig:no,constraints:[prep:ab,sem:[hum]]] ;        % THEN-CLAUSE
                      NewArg=[type:np,oblig:no,constraints:[case:abl]])  ,
                                                                                     % ELSE-CLAUSE:
                 ifthenelse(   constraint([sem:[hum]],Sconstraints),   % IF2-clause     % there are constraints: we act accordingly
                                    NewArg=[type:pp,oblig:no,constraints:[prep:ab,sem:[hum]]],    % THEN2-clause
                                    NewArg=[type:np,oblig:no,constraints:[case:abl]])),          % ELSE2-clause


                 append([agent:NewArg],Args2,At),
                 append([subject:NewObjectSpecs],At,Argstomatch) ),
               Argstomatch=Args),                                    % active voice : leave arglist as it is




 match_list(Argstomatch,TreeArgs,PathlistArgs,DistanceArgs,
              sujet([number:_,gender:_,person:_]),
              nonfinite,
              gap:[],
              w:WeightArgs,
              Int,
              [p(EndMain,StartEllipted)]),

 full_expansions(FE),
   expandkernel(FE,ExpandTrees,ExpandPaths,ExpandDistances,w:WeightExpands, Int, partial),  % here too weight is computed
                      % and the interrogative can hide in the adjuncts

% PATH

flatten(PathlistArgs,FlatArgs),
flatten(ExpandPaths,FlatExpands),
msort(FlatArgs,ArgsSorted),
msort(FlatExpands,ExpandsSorted),
adjacent(ArgsSorted,ExpandsSorted),
append(ArgsSorted,ExpandsSorted,EP),
flatten(EP,EPflat),
msort(EPflat,EPsorted),
contiguous(EPsorted),
 \+dup(EPsorted),
start(EPsorted,StartEllipted),
append(NPLMain,EPsorted,PP),
msort(PP,PPsorted),
contiguous(PPsorted),

% DISTANCES
myappend(DistanceArgs,ExpandDistances,Distances),
flatten(Distances, FlatDist),
sum(FlatDist, DistanceSum),
myplus(DistanceSum,DistMain,DistanceAll),



% ARG TREES
append(TreeArgs,ExpandTrees,AllTrees),
insort(AllTrees, NST),


 ifthenelse(constraint([subject:[hp:Pathsubj]],NST),  % we have a non-gapped subject, we record its head path
       true,              % and abstain from doing anything else
             Pathsubj=[p(0,0)]),          % otherwise the subject is higher up and therefore necessarily precedes

  ifthenelse(constraint([object:[hp:Pathobj]],NST),               % IF-CLAUSE we have an object
             ifthenelse(precedes(Pathsubj,Pathobj), NW is WeightArgs+1, NW=WeightArgs),         % THEN-CLAUSE
             NW=WeightArgs),                % ELSE-CLAUSE

  ifthenelse(constraint([i_object:[hp:Pathiobj1]],NST),         %  we have an indirect object (first type: dat)
             ifthenelse(precedes(Pathsubj,Pathiobj1), NW1 is NW+1, NW1=NW),
             NW1=NW),
  ifthenelse(constraint([object_i:[hp:Pathiobj2]],NST),         %  we have an indirect object (second type: acc)
             ifthenelse(precedes(Pathsubj,Pathiobj2), NW2 is NW1+1, NW2=NW1),           % as with doceo, for instance
             NW2=NW1),

% WEIGHTS
 myplus(NW2,(round(WeightExpands/4)),Wpartial),
 myplus(WMain,Wpartial,Wtot),
 map(pred,[cat:pred,
              type:nonfinite,class:m,
              mood:MoodMain,
              tense:TenseMain,
              pathlist:PPsorted,
              distance:[DistanceAll],
              number:NMain,
              gender:neuter,
              case:or([nom,acc]),                       % nber, gender, and person of the CLAUSE, not its subject
              person:PMain,
              polarity:Pol,
              argbound:no,
              gap:[],
              w:Wtot,
              add:_,
              flagint:_,
              constituent_structure:[vg:FSverb,arglist:ST,linker:CC,arglist:NST]]) ].




















%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% RELATIVE CLAUSES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Recall that the function of the np is independent from its function in the relative clause:
% "liber quem  rex legit ..." : liber is subject in the main clause and quem is object in the relative
% The index is shared; it reports the positions spanned by the np.
% A relative clause is an S displaying a [gap:Gap] feature corresponding to the antecedent:
% same category (np, pp) and shared index
% The gap site can specify any type of constraints on the constituent structure of the antecedent NP;
% this power is necessary to deal with mwus where the deletion site
% can point to an NP that must be lexically described,
% not just in terms of features such as number and broad semantic category


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% with a relative pronoun filling an np slot
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% "(vir) qui  epistulas ad Marcum misit" ; "(librum) quem ancilla legit"


% subject, object, i_object, object_i ('doceo' type verbs)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

[finite,rel_clause_1] --->
[ (Function=subject; Function=object; Function=i_object;Function=object_i;Function=adjunct),

 mapped(relative,[from:X,to:Y|FS1]),    % a relative pronoun

 ifthenelse( mapped(punct,[from:AA,to:X, lex:comma|_]), % comma ?
             (Adtopath=[p(AA,X)],Rel_type=non_restrictive),     % YES
             (Adtopath=[], Rel_type=restrictive)            ), % NO

 constraint([number:Nb, case:Case,gender:Gender, function:Functions],FS1),

 mapped(pred,FS2),                                % a clause

 constraint([type:finite,class:m,  vgperson:Person,                      % relative clauses are finite
             mood:Mood, tense:Tense,pathlist:Pathlist,distance:Distance,w:Weight],FS2),

 msort(Pathlist,Sorted),
 start(Sorted,Y),
 contiguous(Sorted),    % the relative clause cannot bind structures outside of itself
                                % this restriction is important
        % strict contiguity seems called for      % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%


 constraint([gap:GAPARG,constituent_structure:C_str],FS2),      % there MUST be a gap in the clause

 nonvar(GAPARG),        % !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                                                % without such a test the danger is that the following test
            % is no test at all, but simply results in unification of the GAPARG variable
            % with whatever comes its way

 GAPARG=[gap:[type:np,                          % GAP specifies type
              index:Index,                      % opens a place for the Index of the antecedent to fill
              function:Function,                 % function must be compatible with relative pronoun
              Function:[e:Index],                % info for the parse tree - e (empty, trace) followed by the Index
              constraints:Constraints           % we put the constraints to be checked on the antecedent in this box
                           ]],


  member(Function,Functions),                   % remember that the acc rel pronouns must bear the 'subject' function as well as the
                 % object one

  ifthenelse( (Function=subject,Case=nom), PersonRel=Person, PersonRel=_),  % check on person only if subject in nominative (not acc !)

  append([p(X,Y)],Pathlist,Pnew),   % appending the relative pronoun to the path
  append(Adtopath,Pnew,PL),
  \+dup(PL),
  map(relative_clause,[pathlist:PL,
                         distance:Distance,
                         gap:GAPARG,              % gap info carried by the clause
                         index:Index,
                         number:Nb,
                         gender:Gender,
                         case:Case,
                         person:PersonRel,
                         reltype:Rel_type,
                         type:finite,
                         mood:Mood,
                         tense:Tense,
                         constraints:Constraints,
                         w:Weight,
                         constituent_structure:C_str]) ].




% With IPSE as GAP filler 'qui viderunt ipsi omnia'
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


[finite,rel_clause_1a] --->
[ (Function=subject; Function=object; Function=i_object;Function=object_i;Function=adjunct),

 mapped(relative,[from:X,to:Y|FS1]),    % a relative pronoun

 ifthenelse( mapped(punct,[from:AA,to:X, lex:comma|_]), % comma ?
             (Adtopath=[p(AA,X)],Rel_type=non_restrictive),     % YES
             (Adtopath=[], Rel_type=restrictive)            ), % NO

 constraint([number:Nb, case:Case,gender:Gender, function:Functions],FS1),

 mapped(pred,FS2),                                % a clause

 constraint([type:finite,class:m, vgperson:Person,                          % relative clauses are finite
             mood:Mood, tense:Tense,pathlist:Pathlist,distance:Distance,w:Weight],FS2),

 msort(Pathlist,Sorted),
 start(Sorted,Y),
 contiguous(Sorted),    % the relative clause cannot bind structures outside of itself
                                % this restriction is important
        % strict contiguity seems called for      % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%


 constraint([constituent_structure:C_str],FS2),      % getting at the args
 member(Function,Functions),
 ifthenelse((Function=subject,Case=nom), PersonRel=Person, PersonRel=_),  % check on person only if subject
 constraint([Function:FSF],C_str),
 constraint([lex:pr_ipse,number:Nb,case:Case,gender:Gender],FSF),

  append([p(X,Y)],Pathlist,Pnew),   % appending the relative pronoun to the path
  append(Adtopath,Pnew,PL),
  \+dup(PL),
  map(relative_clause,[pathlist:PL,
                         distance:Distance,

                         gap:[nogap],
                         index:Index,
                         number:Nb,
                         gender:Gender,
                         case:Case,
                         person:PersonRel,
                         reltype:Rel_type,
                         type:finite,
                         mood:Mood,
                         tense:Tense,
                         constraints:[],
                         w:Weight,
                         constituent_structure:C_str]) ].




% prep+rel filling a pp slot
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% "(vir) ad quem epistulam misisti" [pp-gap here]

[finite,rel_clause_2] --->
 [mapped(prep,[from:A, to:B|FS1]),    % we have a prep
 constraint([lex:Prep, requires:Required],FS1), % and the requirement it sets on the case of the np it governs

 % the case is checked on the relative pronoun:
 mapped(relative,[from:B, to:C|FS2]),
 constraint([number:Nb,gender:Gender],FS2),
 constraint([case:Required,function:Functions],FS2),
 ifthenelse( mapped(punct,[from:AA,to:A, lex:comma|_]), % comma ?
             (Adtopath=[p(AA,A)],Rel_type=non_restrictive),     % YES
             (Adtopath=[], Rel_type=restrictive)            ), % NO

 mapped(pred,FS3),
 constraint([type:finite,class:m, mood:Mood, vgperson:Person, tense:Tense,pathlist:Pathlist,distance:Distance,w:Weight],FS3),
 msort(Pathlist,Sorted),
 start(Sorted,C),
 contiguous(Sorted),                    % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%
 append([p(A,B), p(B,C)],Sorted,PL1),
 append(Adtopath,PL1,PL),
  \+dup(PL),
 constraint([gap:GAPARG,constituent_structure:C_str],FS3),
 nonvar(GAPARG),
 GAPARG=[gap:[type:pp,
               index:Index,
               function:prep_cplt,
               prep_cplt:[e:Index],
               constraints:Constraints
                           ]],

 member(prep_cplt,Functions),  % this is the function that should be associated with prep+np pairs
                               % make sure that this is true or increase the possibilities here !!!!!!!!!!

 select_ppconstraints(Constraints,PPConstraints,OtherConstraints),

 % this predicates divides the constraints into two classes : the ones that need to be satisfied here
 % locally, and the ones that must wait for the antecedent to be available

 % the OtherConstraints concern the NP and are projected to the relative clause constraints

 constraint(PPConstraints,[prep:Prep]), % the prep constraints are satisfied here

 map(relative_clause,[pathlist:PL,
                        distance:Distance,
                         gap:GAPARG,
                         index:Index,
                         number:Nb,
                         gender:Gender,
                         person:PersonRel,
                         case:Required,
                         reltype:Rel_type,
                         type:finite,
       mood:Mood, tense:Tense,
                         constraints:OtherConstraints,
                         w:Weight,
                         constituent_structure:C_str]) ].



% AGENT
%%%%%%%

% "studia in eos a quibus provinciae contemnuntur [pp-gap here]"

[finite,rel_clause_3] --->
 [mapped(prep,[from:A, to:B|FS1]),    % we have a prep
 constraint([lex:ab],FS1), % and the requirement it sets on the case of the np it governs

 % the case is checked on the relative pronoun:
 mapped(relative,[from:B, to:C|FS2]),
 constraint([number:Nb,gender:Gender],FS2),
 constraint([case:abl,function:[agent]],FS2),
 ifthenelse( mapped(punct,[from:AA,to:A, lex:comma|_]), % comma ?
             (Adtopath=[p(AA,A)],Rel_type=non_restrictive),     % YES
             (Adtopath=[], Rel_type=restrictive)            ), % NO

 mapped(pred,FS3),
 constraint([type:finite,class:m, vgperson:Person, mood:Mood, tense:Tense,pathlist:Pathlist,distance:Distance,w:Weight],FS3),
 msort(Pathlist,Sorted),
 start(Sorted,C),
 contiguous(Sorted),                    % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%
 append([p(A,B), p(B,C)],Sorted,PL1),
 append(Adtopath,PL1,PL),
  \+dup(PL),
 constraint([gap:GAPARG,constituent_structure:C_str],FS3),
 nonvar(GAPARG),
 GAPARG=[gap:[type:pp,
               index:Index,
               function:agent,
               agent:[e:Index],
               constraints:Constraints
                           ]],

 select_ppconstraints(Constraints,PPConstraints,OtherConstraints),

 % this predicates divides the constraints into two classes : the ones that need to be satisfied here
 % locally, and the ones that must wait for the antecedent to be available

 % the OtherConstraints concern the NP and are projected to the relative clause constraints

 constraint(PPConstraints,[prep:_]), % the prep constraints are satisfied here

 map(relative_clause,[pathlist:PL,
                        distance:Distance,
                         gap:GAPARG,
                         index:Index,
                         number:Nb,
                         gender:Gender,
                         person:PersonRel,
                         case:_,
                         reltype:Rel_type,
                         type:finite,
       mood:Mood, tense:Tense,
                         constraints:OtherConstraints,
                         w:Weight,
                         constituent_structure:C_str]) ].




%  UBI OR CUM : specific constraints on antecedent, no gap to be filled
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Relative UBI
%%%%%%%%%%%%%%

% simplification, as here the relative cannot fill an arg position within the relative clause

[finite,rel_clause_4] --->
[ifthenelse( mapped(punct,[from:AA,to:A, lex:comma|_]), % comma ?
             (Adtopath=[p(AA,A)],Rel_type=non_restrictive),     % YES
             (Adtopath=[], Rel_type=restrictive)            ), % NO
 mapped(adv,[from:A, to:B|FS2]),
 constraint([lex:ubi],FS2),         % UBI only here

 mapped(pred,FS3),
 constraint([type:finite, class:m,mood:Mood, tense:Tense,pathlist:Pathlist,
             distance:Distance,w:Weight,gap:[],constituent_structure:C_str],FS3),

 start(Pathlist,B),
 append(Adtopath,[p(A,B)],HeadPath),
 append(HeadPath,Pathlist,PL),
  \+dup(PL),
 msort(PL,PLsorted),                                 % ALP 194
 contiguous(PLsorted),                                      % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%

 map(relative_clause,[pathlist:PL,
                        distance:Distance,
                         gap:[],           % such relative clauses exhibit NO gap
                         index:_,
                         number:_,
                         gender:_,
                         case:_,           % and have no restrictions to impose on the antecedent
                         person:_,
                         type:finite,
                         reltype:Rel_type,
       mood:Mood,
                         tense:Tense,
                         constraints:[sem:[loc]],  % except a semantic one (must be a place of some sort;
               % this is checked by imposing the loc value
                         w:Weight,
                         constituent_structure:C_str]) ].





% relative [in quo in qua in quibus] as [ubi] equivalent - check on gender and number of the antecedent np
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

[finite,rel_clause_5] --->
[ifthenelse( mapped(punct,[from:AA,to:A, lex:comma|_]), % comma ?
             (Adtopath=[p(AA,A)],Rel_type=non_restrictive),     % YES
             (Adtopath=[], Rel_type=restrictive)            ), % NO

 mapped(prep,[from:A, to:B|FS1]),   % we have a prep
 constraint([lex:in],FS1), % it is IN
 % the ablative case is checked on the relative pronoun:
 mapped(relative,[from:B, to:C|FS2]),
 constraint([number:Nb,gender:Gender,case:abl],FS2),

 mapped(pred,FS3),
 constraint([type:finite, class:m,mood:Mood, tense:Tense,pathlist:Pathlist,
             distance:Distance,w:Weight,gap:[],constituent_structure:C_str],FS3),

 start(Pathlist,C),
 append(Adtopath,[p(A,B),p(B,C)],HeadPath),
 append(HeadPath,Pathlist,PL),
  \+dup(PL),
 msort(PL,PLsorted),          % ALP 194
 contiguous(PLsorted),                                      % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%

 map(relative_clause,[pathlist:PL,
                        distance:Distance,
                         gap:[],           % such relative clauses exhibit NO gap
                         index:_,
                         number:Nb,
                         gender:Gender,
                         case:_,           % and have no restrictions to impose on the antecedent except gender and number
                         person:_,
                         type:finite,
                         reltype:Rel_type,
       mood:Mood,
                         tense:Tense,
                         constraints:[sem:[loc]],  % and a semantic one (must be a place of some sort;
               % this is checked by imposing the loc value
                         w:Weight,
                         constituent_structure:C_str]) ].




% Relative CUM
%%%%%%%%%%%%%%

% Memini temporis cum ancilla mea tuos libros in horto legeret.


[finite,rel_clause_6] --->
[ifthenelse( mapped(punct,[from:AA,to:A, lex:comma|_]), % comma ?
             (Adtopath=[p(AA,A)],Rel_type=non_restrictive),     % YES
             (Adtopath=[], Rel_type=restrictive)            ), % NO

 mapped(prep,[from:A, to:B|FS2]), % of course it's NOT a prep - we are interested in the form only
 constraint([lex:cum],FS2),                    % CUM only

 mapped(pred,FS3),
 constraint([type:finite, class:m,mood:Mood, tense:Tense,
            pathlist:Pathlist,distance:Distance,w:Weight,gap:[],constituent_structure:C_str],FS3),
 start(Pathlist,B),
 append(Adtopath,[p(A,B)],HeadPath),
 append(HeadPath,Pathlist,PL),
  \+dup(PL),
 msort(PL,PLsorted),    % ALP 194
 contiguous(PLsorted),                                 % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%
 map(relative_clause,[pathlist:PL,
                        distance:Distance,
                         gap:[],
                         index:_,
                         number:_,
                         gender:_,
                         case:_,
                         person:_,
                         type:finite,
                         reltype:Rel_type,
       mood:Mood,
                         tense:Tense,
                         constraints:[sem:[time]],    % a time-related antecedent is needed
                         w:Weight,
                         constituent_structure:C_str]) ].





% noun cplt
%%%%%%%%%%%

% amo reginam [ [cuius librum] legi]

% here we have an np containing the relative pronoun


[finite,rel_clause_7] --->
[mapped(relative,[from:A, to:B|FSrelative]),     % the relative pronoun must be genitive : cuius quorum quarum
 mapped(np,NP),
 constraint([pathlist:PLnoun,distance:[Distancenoun],
             % type:core,         % we insist on the np being core
             index:Index,
             constituent_structure:Cstrnoun],NP),

 msort(PLnoun,Sortednoun),
 start(Sortednoun,B),                   % the np follows the relative pronoun : cuius  -> librum

 extremity(Sortednoun,Startrelative),  % the pred of the relative clause follows the NP: cuius librum -> legi



 constraint([number:Nb,gender:Gender,case:gen],FSrelative), % relative shares gender and number with antecedent

 ifthenelse( mapped(punct,[from:AA,to:A, lex:comma|_]), % comma ?
             (Adtopath=[p(AA,A)],Rel_type=non_restrictive),     % YES
             (Adtopath=[], Rel_type=restrictive)            ), % NO

 mapped(pred,FS3),      % the pred of the relative clause : legi [GAP]

 constraint([type:finite, class:m,mood:Mood, tense:Tense,
             pathlist:Pathlist,distance:[Distancepred],
             w:Weight],FS3),

 msort(Pathlist,Sorted),
 start(Sorted,Startrelative),            % the pred follows the noun
 contiguous(Sorted),                                                     % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%

 constraint([gap:GAPARG,constituent_structure:C_str],FS3),
 nonvar(GAPARG),
 GAPARG=[gap:[type:np,      % the gap is the whole np
               index:Index,
               function:Function,
               Function:[e:Index],
               constraints:Constraints
                           ]],

 % the Constraints concern the NP above, not the antecedent !!!!

 constraint(Constraints,NP),

 append([p(A,B)],Sortednoun,PLnp),
 append(Adtopath,PLnp,PLnp1),
 append(PLnp1,Sorted,PL),
  \+dup(PL),
 Distance is Distancenoun+Distancepred,

 map(relative_clause,[pathlist:PL,
                        distance:[Distance],
                        gap:[],       % the gap is already filled
                        index:Index,
                        number:Nb,                     % checks on number and gender are the only checks to be performed
                        gender:Gender,
                        case:_,
                        person:_,
                        type:finite,
                         reltype:Rel_type,
      mood:Mood, tense:Tense,
                        constraints:[],     % no constraint to be imposed on the antecedent
                        w:Weight,
                        constituent_structure:[head:Cstrnoun, rel_clause:C_str]]),


% as dummy np : non scribit cuius carmina nemo legit (Martial)

map(np,[pathlist:PL,hp:[],distance:[Distance],
             cat:np,type:full,class:common,
             index:dummy_np,number:sing,gender:masc,sem:[hum],person:3,case:_,     % lex value enables the parser
                      % to spot this fabricated NP
             lex:dummy_np,w:(Weight/3), % low weight
             constituent_structure:[head:[lex:dummy_np, number:sing,gender:masc,person:3,index:_,
                          rel_clause:[head:Cstrnoun, rel_clause:C_str]]]])].





% with participle clause turned into np as subject
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


 /* map(np,[pathlist:NSorted,hp:[p(A,B)],
           index:i(p(A,B)),distance:[NDistance],
           cat:np,class:_,sem:[hum],
           number:Nb,person:Person,gender:G, type:clausal,lex:Lex,lextype:full,
           case:C,w:WW,
           constituent_structure:[present_participle,Lex,NST]]))
*/

% we would have to look inside the NST to set the index correctly

[finite,rel_clause_8] --->

[ mapped(relative,[from:A, to:B|FSrelative]),     % the relative pronoun opens the subject np
                                                 % [quem] amans
 mapped(np,NP),
 constraint([pathlist:PLnoun,distance:[Distancenoun],
             type:clausal, case:nom,          % clausal indicates a participle clause read as a noun phrase
             index:_,
             constituent_structure:Cstrnoun],NP),

 msort(PLnoun,Sortednoun),
 start(Sortednoun,A),
 extremity(Sortednoun,Ex),



 constraint([number:Nb,gender:Gender],FSrelative), % relative shares gender and number with antecedent

 ifthenelse( mapped(punct,[from:AA,to:A, lex:comma|_]), % comma ?
             (Adtopath=[p(AA,A)],Rel_type=non_restrictive),     % YES
             (Adtopath=[], Rel_type=restrictive)            ), % NO

 mapped(pred,FS3),      % the pred of the relative clause : quem amans [insanit]

 constraint([type:finite, class:m,mood:Mood, tense:Tense,
             pathlist:Pathlist,distance:[Distancepred],
             w:Weight],FS3),

 msort(Pathlist,Sorted),
 start(Sorted,Ex),            % the pred follows the subject np
 contiguous(Sorted),                                                     % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%

 constraint([gap:GAPARG,constituent_structure:C_str],FS3),
 nonvar(GAPARG),
 GAPARG=[gap:[type:np,      % the gap is the whole np
               index:IndexAnt,
               function:subject,
               subject:[e:IndexAnt],
               constraints:Constraints
                           ]],

 % the Constraints concern the NP above, not the antecedent !!!!

 constraint(Constraints,NP),


 append(Adtopath,PLnoun,PLnp),
 append(PLnp,Sorted,PL),
  \+dup(PL),
 Distance is Distancenoun+Distancepred,

 map(relative_clause,[pathlist:PL,
                        distance:[Distance],
                        gap:[],       % the gap is already filled
                        index:IndexAnt,
                        number:Nb,                     % checks on number and gender are the only checks to be performed
                        gender:Gender,
                        case:_,
                        person:_,
                        type:finite,
                         reltype:Rel_type,
      mood:Mood, tense:Tense,
                        constraints:[],     % no constraint to be imposed on the antecedent
                        w:Weight,
                        constituent_structure:[head:Cstrnoun, rel_clause:C_str]]) ].




% with np subject with attached participle clause
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
map(np,[pathlist:Sorted,hp:HPnp,index:Index,distance:Distance,
           cat:np,class:Class,sem:Sem,
           number:Nb,person:Person,gender:Gender,type:core,lex:LexNoun,lextype:full,   % person to be left open or read from np
           case:Case,w:Weight,
           constituent_structure:[head:LexNoun,participle_clause:FSpp]])].

*/

% we would have to look inside the FSpp in order to set the index
% more precisely inside its argument tree - see above

[finite,rel_clause_9] --->

[mapped(relative,[from:A, to:_|FSrelative]),     % the relative pronoun opens the subject np
                                                 % [quem] amans rex
 mapped(np,NP),
 constraint([pathlist:PLnoun,distance:[Distancenoun],
             type:core, case:nom,
             index:_,
             constituent_structure:[head:LexNoun,participle_clause:FSpp]],NP), % constituent_structure value indicates we have a np+participle clause
                          % quem amans rex

 msort(PLnoun,Sortednoun),
 start(Sortednoun,A),
 extremity(Sortednoun,Ex),



 constraint([number:Nb,gender:Gender],FSrelative), % relative shares gender and number with antecedent

 ifthenelse( mapped(punct,[from:AA,to:A, lex:comma|_]), % comma ?
             (Adtopath=[p(AA,A)],Rel_type=non_restrictive),     % YES
             (Adtopath=[], Rel_type=restrictive)            ), % NO

 mapped(pred,FS3),      % the pred of the relative clause : quem amans rex [insanit]

 constraint([type:finite, class:m,mood:Mood, tense:Tense,
             pathlist:Pathlist,distance:[Distancepred],
             w:Weight],FS3),

 msort(Pathlist,Sorted),
 start(Sorted,Ex),            % the pred follows the subject np
 contiguous(Sorted),                                                     % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%

 constraint([gap:GAPARG,constituent_structure:C_str],FS3),
 nonvar(GAPARG),
 GAPARG=[gap:[type:np,      % the gap is the whole np
               index:IndexAnt,
               function:subject,
               subject:[e:IndexAnt],
               constraints:Constraints
                           ]],

 % the Constraints concern the NP above, not the antecedent !!!!

 constraint(Constraints,NP),


 append(Adtopath,PLnoun,PLnp),
 append(PLnp,Sorted,PL),
  \+dup(PL),
 Distance is Distancenoun+Distancepred,

 map(relative_clause,[pathlist:PL,
                        distance:[Distance],
                        gap:[],       % the gap is already filled
                        index:IndexAnt,
                        number:Nb,                     % checks on number and gender are the only checks to be performed
                        gender:Gender,
                        case:_,
                        person:_,
                        type:finite,
                         reltype:Rel_type,
      mood:Mood, tense:Tense,
                        constraints:[],     % no constraint to be imposed on the antecedent
                        w:Weight,
                        constituent_structure:[head:[LexNoun,participle_clause:FSpp], rel_clause:C_str]]) ].











% PP constraints
%%%%%%%%%%%%%%%%

select_ppconstraints([],[],[]) :- !.             % end of recursion

select_ppconstraints(Constraints,[prep:Prep],OtherC) :-
    pick(prep:Prep,Constraints,OtherC),
                !.

% the pp constraint has one form, namely restriction on the lex of the prep


select_ppconstraints(Constraints,[],Constraints).




% myappend
%%%%%%%%%%

% useful for gaps, which are most of the time uninstantiated variables or empty lists

 myappend([],[],[]) :- !.
 myappend([],X,X) :- nonvar(X),!.
 myappend(X,[],X) :- nonvar(X),!.
 myappend(X,Y,[]) :- var(X),
                     var(Y), !.
 myappend(X,Y,X) :- var(Y),!.
 myappend(X,Y,Y) :- var(X),!.
 myappend([Head|L1],L2,[Head|L3]) :- myappend(L1,L2,L3).




% myplus
%%%%%%%%

% gets rid of vars on the fly
% recall that 'is' does not like uninstantiated vars !!!

myplus(A,B,0) :- var(A),
                 var(B), !.
myplus(A,B,A) :- var(B), !.
myplus(A,B,B) :- var(A), !.
myplus(A,B,Sum) :- Sum is A+B.























%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% COORDINATION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% ADJ PHRASES
%%%%%%%%%%%%%

% finite run seems necessary ...


[finite,c1] --->
[mapped(coord,[from:X,to:Y|Fcoord]),
 constraint([lex:Coord],Fcoord),

 mapped(adjp,Fadj1),
 mapped(adjp,Fadj2),
Fadj1 \= Fadj2,

 constraint([pathlist:Path1],Fadj1),
 constraint([pathlist:Path2],Fadj2),

 Xminus1 is X-1,

 extremity(Path1,X),  % ALP 197

% ( extremity(Path1,X); extremity(Path1,Xminus1) ), % multas res et magnas (Cic, Pro Murena VIII 20)



                     % the coordinator sits in between the 2 adj phrases -
                     % this condition amounts to an oversimplification

 Yplus is Y+1,

( start(Path2,Y) ; start(Path2,Yplus)),

 constraint([hp:HL1,distance:[Dis1],w:Weight1,case:Case, number:N, gender:G,type:Type,lex:Lex1],Fadj1),
 constraint([hp:HL2,distance:[Dis2],w:Weight2,case:Case, number:N, gender:G,type:_],Fadj2),
      % we insist on the usual coherence triple (Case, gender, number)


 append(Path1,[p(X,Y)],Path1new),  % add coordinator to the path
 append(Path1new,Path2,PL),
 msort(PL, Sorted),
  \+dup(Sorted),
 append(HL1,HL2,HLraw),
 msort(HLraw, HL),
 % Distance is Dis1+Dis2,
 myplus(Dis1,Dis2,Distance),
 % Weight is Weight1+Weight2+WeightCoord,
 myplus(Weight1,Weight2,Weight),
 % myplus(WeightT,WeightCoord,Weight),
 map(adjp,[cat:adjp,pathlist:Sorted,hp:HL,distance:[Distance],
             case:Case,number:N, gender:G,type:Type,lex:Lex1,w:Weight,
             constituent_structure:[head:Fadj1,coord:Coord,head:Fadj2]])
 % map(flags,nocoord2)
].




% coordinated adjs and participial clauses
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
    map(participle_clause,[pathlist:NSorted,hp:[p(A,B)],distance:[NDistance],
           cat:ppclause,w:WW,type:fpcentered,sem:[hum],
           number:Nb,gender:G, lex:Lex,
           case:C,constituent_structure:[future_participle,Lex,NST]]),


 */



[finite,c1a] --->
[mapped(coord,[from:X,to:Y|Fcoord]),
 constraint([lex:Coord],Fcoord),

 mapped(adjp,Fadj1),
 mapped(participle_clause,Fpp),


 constraint([pathlist:Path1],Fadj1),
 constraint([pathlist:Path2],Fpp),

%  Xminus1 is X-1,

 ( extremity(Path1,X); extremity(Path2,X) ),



                     % the coordinator sits in between the 2 phrases -
                     % this condition amounts to an oversimplification

%  Yplus is Y+1,

( start(Path1,Y) ; start(Path2,Y)),

 constraint([hp:HL1,distance:[Dis1],w:Weight1,case:Case, number:N, gender:G,type:Type,lex:Lex1],Fadj1),
 constraint([hp:HL2,distance:[Dis2],w:Weight2,case:Case, number:N, gender:G,lex:Lex2],Fpp),
      % we insist on the usual coherence triple (Case, gender, number)


 append(Path1,[p(X,Y)],Path1new),  % add coordinator to the path
 append(Path1new,Path2,PL),
 msort(PL, Sorted),
  \+dup(Sorted),
 append(HL1,HL2,HLraw),
 msort(HLraw, HL),
 % Distance is Dis1+Dis2,
 myplus(Dis1,Dis2,Distance),
 % Weight is Weight1+Weight2+WeightCoord,
 myplus(Weight1,Weight2,Weight),
 % myplus(WeightT,WeightCoord,Weight),
 map(adjp,[cat:adjp,pathlist:Sorted,hp:HL,distance:[Distance],
             case:Case,number:N, gender:G,type:Type,lex:Lex1_Lex2,w:Weight,
             constituent_structure:[head:Fadj1,coord:Coord,head:Fpp]])
 % map(flags,nocoord2)
].











% coordinated gerundives (inadequate)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Urbs est mihi capienda delendaque.

% here is an example of what the lexicon yields for gerundives:

% lex(delenda,
%       gdiv,
%       [pos:gdiv, txt:delenda, case:abl, gender:fem, number:sing, lex:delere,
%        class:tr_cod, type:gdiv, kind:std, mood:gerund, person:3]).


% we take the required extra info from the first conjunct

[coord,c2] --->
[
 mapped(coord,[from:B,to:C|_]),
 mapped(gdiv,[from:A,to:B |Fgdiv1]),
 mapped(gdiv,[from:C, to:D|Fgdiv2]),
 Fgdiv1 \= Fgdiv2,

 constraint([case:Case, number:N, gender:G,lex:Lex1, class:Class, kind:Kind],Fgdiv1),
 constraint([case:Case, number:N, gender:G,lex:Lex2],Fgdiv2),
 atomic_list_concat([Lex1,Lex2],' et ',TwoLex),                 % building the compound lex out of the two verbs

 map(gdiv,[from:A, to:D, case:Case,number:N, gender:G,
             lex:Lex1,compound_lex:TwoLex, class:Class,type:gdiv,kind:Kind,mood:gerund,person:3,
             constituent_structure:[head:Lex1,coord:TwoLex,head:Lex2]]),


 map(gerundive,[cat:gerundive,pathlist:[p(A,B),p(B,C),p(C,D)],distance:[0], hp:[p(A,B)],
                  case:Case,number:N, gender:G,
                  lex:Lex1,compound_lex:TwoLex, class:Class,
                  type:gdiv,kind:Kind,mood:gerund,person:3,w:2,
                  constituent_structure:[head:Lex1,coord:TwoLex,head:Lex2]]),
map(flags,nocoord2)].

 % we map two different boxes:
 % 'gdiv' is used by gerundive predications (urbs est mihi capienda et delenda),
 % 'gerundive' is adjective-like and is used in predicative argument construction (karthago capienda delendaque est)


 % problem: the second lex does not appear in the tree
 % even if we put it as value of the compound_lex feature in the info box
 % we need to keep a clean Lex1 to connect with the arglist
 % and it is this lex that appears in the info box associated with the gerund-type predicate
 % if we do not put it as value of the coord feature, the compound_lex is properly built but remains hidden...


%%%%%%%%%%%%%%%%%%%%
% ADVERBIAL PHRASES
%%%%%%%%%%%%%%%%%%%%

[coord,c3] --->
[mapped(coord,[from:X,to:Y|Fcoord]),
  constraint([lex:Coord],Fcoord),
 mapped(advp,Fadv1),
 mapped(advp,Fadv2),
 Fadv1 \= Fadv2,
 constraint([pathlist:Path1],Fadv1),
 constraint([pathlist:Path2],Fadv2),
 extremity(Path1,X), % the coordinator sits in between the 2 adv phrases -
                     % this condition amounts to an oversimplification
 start(Path2,Y),


 constraint([distance:[Dis1],w:Weight1,sem:Sem,type:Type,lex:Lex1],Fadv1),
 constraint([distance:[Dis2],w:Weight2,type:_,lex:Lex2],Fadv2),


 append(Path1,[p(X,Y)],PL1new),  % add coordinator to the path
 start(Path1,PL1Start),

 ifthenelse(mapped(coord,[from:AAA,to:PL1Start|Fcoord]),
             % coordination in front of the first conjunct 'et hic et nunc'
             % should be the same coordinator as the one in between the 2 conjuncts
            append([p(AAA,PL1Start)],PL1new, PL1new2), % append it to the path
            PL1new2=PL1new),

 append(PL1new2,Path2,PL),
 msort(PL, Sorted),
  \+dup(Sorted),
 myplus(Dis1,Dis2,Distance),
 myplus(Weight1,Weight2,Weight),

 myplus(1,Weight,WeightTot),        % bonus for cohesion due to coordination
 map(advp,[cat:advp,pathlist:Sorted,distance:[Distance],
             type:Type,lex:Lex1,w:WeightTot,sem:Sem,
             constituent_structure:[head:Lex1,coord:Coord,head:Lex2]]),
map(flags,nocoord2)].


% ADJUNCTS
%%%%%%%%%%%

[coord,c4] --->
[mapped(coord,[from:X,to:Y|Fcoord]),
  constraint([lex:Coord],Fcoord),
 mapped(adjunct,Fadjunct1),
 mapped(adjunct,Fadjunct2),
 Fadjunct1 \= Fadjunct2,
  constraint([pathlist:Path1],Fadjunct1),
   constraint([pathlist:Path2],Fadjunct2),
  extremity(Path1,X), % the coordinator sits in between the 2 adjuncts -
                     % this condition amounts to an oversimplification
 start(Path2,Y),


 constraint([distance:[Dis1],w:Weight1,value:Value,constituent_structure:C_str1],Fadjunct1),
 constraint([distance:[Dis2],w:Weight2,value:_,constituent_structure:C_str2],Fadjunct2),



 append(Path1,[p(X,Y)],PL1new),  % add coordinator to the path
 start(Path1,PL1Start),

 ifthenelse(mapped(coord,[from:AAA,to:PL1Start|Fcoord]),
             % coordination in front of the first conjunct 'et in Roma et per annos'
             % should be the same coordinator as the one in between the 2 conjuncts
            append([p(AAA,PL1Start)],PL1new, PL1new2), % append it to the path
            PL1new2=PL1new),

 append(PL1new2,Path2,PL),
 msort(PL, Sorted),
  \+dup(Sorted),
 myplus(Dis1,Dis2,Distance),
 myplus(Weight1,Weight2,Weight),
 myplus(6,Weight,WeightTot),        % bonus for cohesion due to coordination
 map(adjunct,[pathlist:Sorted,distance:[Distance],
             class:adjunct,value:Value,w:WeightTot,type:_,
             constituent_structure:[head:C_str1,coord:Coord,head:C_str2]]),
map(flags,nocoord2)].




% NPS
%%%%%%

% a proper treatment should wait for the 'finite' run
% on account of rel clauses within one or more of the conjoined nps


[finite,c5] --->
[mapped(coord,[from:X,to:Y|Fcoord]),     % making sure we have a coordinator
 constraint([lex:Coord],Fcoord),

 mapped(np,Fnp1),
 mapped(np,Fnp2),
 Fnp1 \= Fnp2,

 constraint([pathlist:PL1], Fnp1),
 constraint([pathlist:PL2], Fnp2),

 msort(PL1,PL1Sorted),
 msort(PL2,PL2Sorted),

 start(PL1Sorted,PL1Start),
 start(PL2Sorted,Y),
 extremity(PL1Sorted,EndPL1),

 ifthen(Coord \= comma,(EndPL1 = X; EndPL1 < X)),              % ALP 192 end of first conjunct shouldn't be later than begin of coord
                                                               % can't be applied to comma on account of the role it plays in ellipsis

 Malus is Y-EndPL1,
 ifthen(Coord \= comma,Malus < 3),                              %%% ALP192 restriction on distance in coordinated nps

 constraint([hp:HL1,distance:[Distnp1],sem:Sem,number:NUMBER1,person:3,gender:G1,case:Case,lex:Lexnp1,w:W1], Fnp1),

 ifthenelse(constraint([type:int],Fnp1), Type=int,Type=full),  %  interrogative ? (useful for spotting wh-questions)

 constraint([hp:HL2,distance:[Distnp2],case:Case,gender:G2,w:W2,lex:Lexnp2],Fnp2),
                % only CASE needs to be shared

 Lexnp2 \= dummy_np,  % not acceptable as second member of a conjoined np - prioritize the reading
                      % where a full relative clause is coordinated with another, the two
                      % sharing an antecedent

 Lexnp1 \= relatif_de_liaison,
 Lexnp2 \= relatif_de_liaison,

 ifthen( G1=G2,Gout=G1),
 ifthen((G1=masc;G2=masc),Gout=masc),

 append(PL1Sorted,[p(X,Y)],PL1new),  % add coordinator to the path

 ifthenelse( mapped(coord,[from:AAA,to:PL1Start|Fcoord]),
             % coordination in front of the first conjunct 'et pater et mater'
             % should be the same coordinator as the one in between the 2 conjuncts
            append([p(AAA,PL1Start)],PL1new, PL1new2), % append it to the path
            PL1new2=PL1new),

% length(PL1,PL1L),                                                 % ALP 197
% length(PL2,PL2L),                                                 % ALP 197
%  ifthenelse(PL1L=PL2L, BonusBalance=1, BonusBalance=0),            % ALP 197

 append(PL1new2,PL2,PL),
 msort(PL, Sorted),
  \+dup(Sorted),
 append(HL1,HL2,HLraw),
 msort(HLraw, HL),

 % Distance is Distnp1+Distnp2+Malus
 myplus(Distnp1,Distnp2,DistanceT),
 myplus(DistanceT,Malus,Distance),

 % Weight is W1+W2,
 myplus(W1,W2,Weight),
 % myplus(Weight,BonusBalance,FWeight),                             % ALP 197

ifthenelse(contiguous(Sorted),NUMBER=pl,NUMBER=NUMBER1),
       % if the two are together, plural; otherwise, number of the first conjunct
       % pater et mater amant filiam
       % pater amat filiam et mater


ifthenelse( Case=gen,

            (map(np,[pathlist:Sorted, hp:HL1,index:i(HL),distance:[Distance],    % two hp features                      
           sem:Sem,class:common,lex:Lexnp1,lextype:full,type:core,
           number:NUMBER, person:3, gender:Gout,case:gen,w:Weight,coord:yes,            
           constituent_structure:[head:Fnp1,coord:Coord, head:Fnp2]]),

           map(np,[pathlist:Sorted, hp:HL2,index:i(HL),distance:[Distance],             % ALP 197 : HL2 as alternative HeadPath for the measure of
           sem:Sem,class:common,lex:Lexnp1,lextype:full,type:core,           % distance between head NPs and  genitive cplts (right or left)
           number:NUMBER, person:3, gender:Gout,case:gen,w:Weight,coord:yes,  % ALP 197
           constituent_structure:[head:Fnp1,coord:Coord, head:Fnp2]])
            ),


           map(np,[pathlist:Sorted, hp:HL,index:i(HL),distance:[Distance],                   
           sem:Sem,class:common,lex:Lexnp1,lextype:full,type:core,
           number:NUMBER, person:3, gender:Gout,case:Case,w:Weight,coord:yes,            
           constituent_structure:[head:Fnp1,coord:Coord, head:Fnp2]])
           ),



map(flags,nocoord2)].



% number should not be registered as plural (domus et hortus...)
% and gender is best left open as well (sometimes neuter
% plural for two non-neuter nps - anyway, the two conjuncts
% do not necessarily share gender...)
% SUCH A POLICY WOULD BE DISASTROUS IN GENERATION !!!
% and even in parsing leads to spurious parses being produced

% perhaps more advisable to relax the requirements with 'esse' only...




% NON x SED Y non miles sed imperator
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

[coord,c6] --->
[mapped(coord,[from:X,to:Y|Fcoord]),     % making sure we have a coordinator
  constraint([lex:Coord],Fcoord),
  mapped(neg,[from:A,to:B|Fneg]),

 constraint([lex:sed],Fcoord),
 constraint([lex:non],Fneg),
 mapped(np,Fnp1),
 mapped(np,Fnp2),
 Fnp1 \= Fnp2,
 constraint([pathlist:PL1], Fnp1),
 constraint([pathlist:PL2], Fnp2),
 start(PL1,B),
 extremity(PL1,X),               % the coordinator sits in between the 2 NPs - this condition amounts to an oversimplification
 start(PL2,Y),

constraint([hp:HL1,distance:[Distnp1],sem:Sem,
            gender:_,case:Case,lex:Lexnp1,w:W1], Fnp1),

 ifthenelse(constraint([constituent_structure:C_str1],Fnp1),Headinfo1=C_str1,Headinfo1=Lexnp1),
                                            % we have to take into account nps which do not have a constituent_structure feature

 ifthenelse(constraint([type:int],Fnp1), Type=int,Type=full),  %  interrogative ? (useful for spotting wh-questions)

 constraint([hp:HL2,number:Nb2,distance:[Distnp2],case:Case,w:W2,lex:Lexnp2],Fnp2),
                % only CASE needs to be shared
 ifthenelse(constraint([constituent_structure:C_str2],Fnp2),Headinfo2=C_str2,Headinfo2=Lexnp2),
 Lexnp2 \= dummy_np,  % not acceptable as second member of a conjoined np - prioritize the reading
                      % where a full relative clause is coordinated with another, the two
                      % sharing an antecedent

 append(PL1,[p(X,Y)],PL1more),  % add coordinator to the path
 append([p(A,B)],PL1more,PL1new),
 append(PL1new,PL2,PL),
 msort(PL, Sorted),
  \+dup(Sorted),
 append(HL1,HL2,HLraw),
 msort(HLraw, HL),
 % Distance is Distnp1+Distnp2,
 myplus(Distnp1,Distnp2,Distance),
 % Weight is W1+W2,
 myplus(W1,W2,Weight),
 myplus(Weight,2,Weight2),  % bonus for cohesion of the non/sed

map(np,[pathlist:Sorted, hp:HL,index:i(HL),distance:[Distance],
           sem:Sem,class:common,lex:Lexnp1,lextype:full,type:Type,
           number:Nb2, person:3, gender:_,case:Case,w:Weight2,coord:yes,
           constituent_structure:[neg:Fneg,head:Headinfo1,coord:Fcoord, head:Headinfo2]]),
map(flags,nocoord2)].






% we still need to account for the adjectives that can be associated with coordinate nps:

% COORD NPS with adj
%%%%%%%%%%%%%%%%%%%%

[finite,c7] --->
[mapped(np,Fnp),
 mapped(adjp,Fadj),

 constraint([coord:yes,
             pathlist:PLNP,
             hp:HLNP,
             distance:[Distnp],
             sem:Sem,index:Index,
             class:common,
             lextype:full,
             type:Type,
             number:pl,person:3,
             gender:Gender,case:Case,
             lex:LexNoun,w:WNP,
          % making sure we have a coordinated np
             constituent_structure:HeadNP], Fnp),

  constraint([pathlist:PLADJ, hp:_,distance:[Distadj],
             case:Case,number:pl,gender:Gender,                % gender gets projected from the adj,
                                                               % as it is left open in the coordinated np
             w:WADJ,lex:_],Fadj),
                %  CASE and NUMBER need to be shared
                            % good grounds for insisting on a plural

  adjacent(PLNP, PLADJ),   % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%

  append(PLNP,PLADJ,PL),
   msort(PL, Sorted),
  \+dup(Sorted),
  % Distance is Distnp+Distadj,
  % Weight is WNP+WADJ,
  myplus(Distnp,Distadj,Distance),
  myplus(WNP,WADJ,Weight),
  map(np,[pathlist:Sorted, hp:HLNP,index:Index,distance:[Distance],
           sem:Sem,class:common,lex:LexNoun,lextype:full,type:Type,
           number:pl, person:3, gender:Gender,case:Case,w:Weight,coord:yes,
           constituent_structure:[np,HeadNP,adjp:Fadj]])].



% COORDINATED PERSONAL PRONOUNS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Person is not necessarily third : mihi ac tibi

[coord,c8] --->
[mapped(coord,[from:X,to:Y|Fcoord]),     % making sure we have a coordinator

 mapped(prpers,[from:A,to:X|FS1]),       %  PRPERS
 constraint([number:Nb1,gender:G1, case:C,person:Person1,sem:Sem,lex:Lex1],FS1),
 mapped(prpers,[from:Y,to:Z|FS2]),       %  PRPERS
 constraint([case:C,sem:Sem,lex:Lex2],FS2),

 constraint([lex:Coord],Fcoord),

 map(np,[pathlist:[p(A,X),p(X,Y),p(Y,Z)], hp:[p(A,X)],index:i([p(A,X)]),distance:[0],
           sem:Sem,class:proper,lex:Lex1,lextype:full,type:core,
           number:Nb1, person:Person1, gender:G1,case:C,w:1,coord:yes,
           constituent_structure:[head:Lex1,coord:Coord, head:Lex2]])].





% PPs
%%%%%%

% very sketchy, as are ALL coordinate structures in this parser !!!!!!!!!!!!!!!!!!!!!!

[coord,c9] --->
[mapped(coord,[from:X,to:Y|Fcoord]),  % we have a coordinator somewhere in the string
  constraint([lex:Coord],Fcoord),
 mapped(pp,Fpp1),
 mapped(pp,Fpp2),
 Fpp1 \= Fpp2,                % ought to be different
 constraint([pathlist:PL1],Fpp1),
 constraint([pathlist:PL2],Fpp2),
 extremity(PL1,X), % the coordinator sits in between the 2 PPs
 start(PL2,Y),

 constraint([case:Case, prep:Lexprep,hp:HL,distance:[Distpp1],
             sem:Sem,lex:LexNoun,w:W1,constituent_structure:Head1],Fpp1),
 constraint([distance:[Distpp2],w:W2,constituent_structure:Head2],Fpp2),
                                                                     % the pps need not share anything else
                     % than the fact that they are pps


 append(PL1,[p(X,Y)],PL1new),  % add coordinator to the path
 append(PL1new,PL2,PL),
 msort(PL, Sorted),
 % Distance is Distpp1+Distpp2,
 % Weight is W1+W2,
 myplus(Distpp1,Distpp2,Distance),
 myplus(W1,W2,Weight),

 map(pp,[pathlist:Sorted, hp:HL, distance:[Distance],index:_,
           cat:pp,case:Case,prep:Lexprep, sem:Sem,lex:LexNoun,      % we carry the info over from the FIRST pp
                    % obviously inadequate
                    % the other 'solution' is to leave the features
                    % with uninstantiated variables as values
                          % trouble is then on the way
           w:Weight,
           constituent_structure:[head:Head1,coord:Coord, head:Head2]]),
map(flags,nocoord2)].



% VERBS
%%%%%%%


[coord,c10] --->
[ mapped(coord,[from:B, to:C|Fcoord]),
   constraint([lex:Coord],Fcoord),
  mapped(v,[from:A, to:B|Fverb1]),
  mapped(v,[from:C, to:D|Fverb2]),


  constraint([class:Class,type:Type, voice:Voice,number:Nb, person:P, lex:Lex1, mood:Mood, tense:Tense],Fverb1),
  constraint([class:Class,type:Type,             number:Nb, person:P, lex:_],Fverb2),

  % a certain amount of compatibility is needed :
    % class (transitivity etc.),
                % type (finite , nonfinite),
                % number,
                % person

% we copy voice mood and tense from the first verb

  map(vgpos,[cat:vg,class:Class, voice:Voice,type:Type,pathlist:[p(A,B),p(B,C),p(C,D)],hp:[p(A,B),p(C,D)],
              number:Nb, person:P, mood:Mood, gender:_,tense:Tense,lex:Lex1, w:1,
              constituent_structure:[head:Fverb1,coord:Coord,head:Fverb2]]),
map(flags,nocoord2)].








%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CLAUSES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% PARTICIPLE CLAUSES
%%%%%%%%%%%%%%%%%%%%%

[coord2,c2_1] --->
[mapped(coord,[from:X,to:Y|Fcoord]),     % making sure we have a coordinator
  constraint([lex:Coord],Fcoord),

 mapped(participle_clause,Fpc1),
 mapped(participle_clause,Fpc2),
 Fpc1 \= Fpc2,
 constraint([pathlist:PL1], Fpc1),
 constraint([pathlist:PL2], Fpc2),
 Xminus1 is X-1,

 ifthenelse( (extremity(PL1,Xminus1), recorded(pos,position(Xminus1,X,First),_) ),  % leaving room for the first part of the correlative
                append(PL1,[p(Xminus1,X)],PL1n), % append first part
               (extremity(PL1,X), PL1n=PL1)       % first part elsewhere, or simply not the second member of a correlative pair
             ),

 start(PL2,Y),



constraint([hp:HL1,distance:[Dist1],number:N1,gender:G1,case:C1,w:W1, constituent_structure:C_str1, type:Type, lex:Lex], Fpc1),
constraint([ hp:HL2,distance:[Dist2],number:N1,gender:G1,case:C1,w:W2, constituent_structure:C_str2], Fpc2),



 start(PL1,PL1Start),

 append(PL1n,[p(X,Y)],PL1new),  % add coordinator to the path

 ifthenelse(mapped(coord,[from:AAA,to:PL1Start|Fcoord]),
             % coordination in front of the first conjunct 'et credens et amaturus'
             % should be the same coordinator as the one in between the 2 conjuncts
            append([p(AAA,PL1Start)],PL1new, PL1new2), % append it to the path
            PL1new2=PL1new),


 append(PL1new2,PL2,PL),
 msort(PL, Sorted),
  \+dup(Sorted),
 append(HL1,HL2,HLraw),
 msort(HLraw, HL),
 % Distance is Dist1+Dist2,
 myplus(Dist1,Dist2,Distance),
 % Weight is W1+W2,
 myplus(W1,W2,Weight),


map(participle_clause,[pathlist:Sorted, hp:HL,distance:[Distance],
           type:Type,cat:ppclause,lex:Lex,
           number:N1, gender:G1,case:C1,w:Weight,
           constituent_structure:[C_str1,coord:Coord, C_str2]])].





% GERUNDS
%%%%%%%%%

[coord2,c2_2] --->
[ mapped(coord,[from:X,to:Y|Fcoord]),     % making sure we have a coordinator
  constraint([lex:Coord],Fcoord),

 mapped(pred,FG1),
 mapped(pred,FG2),
 FG1 \= FG2,
 constraint([pathlist:PL1], FG1),
 constraint([pathlist:PL2], FG2),
  Xminus1 is X-1,

 ifthenelse( (extremity(PL1,Xminus1), recorded(pos,position(Xminus1,X,First),_) ),  % leaving room for the first part of the correlative
                append(PL1,[p(Xminus1,X)],PL1n), % append first part
               (extremity(PL1,X), PL1n=PL1)       % first part elsewhere, or simply not the second member of a correlative pair
             ),

 start(PL2,Y),


constraint([distance:[Dist1],local_case:Case,w:W1, constituent_structure:C_str1, type:gerund], FG1),
constraint([distance:[Dist2],local_case:Case,w:W2, constituent_structure:C_str2, type:gerund], FG2),


 start(PL1,PL1Start),

 append(PL1n,[p(X,Y)],PL1new),  % add coordinator to the path

 ifthenelse(mapped(coord,[from:AAA,to:PL1Start|Fcoord]),
             % coordination in front of the first conjunct 'et credens et amaturus'
             % should be the same coordinator as the one in between the 2 conjuncts
            append([p(AAA,PL1Start)],PL1new, PL1new2), % append it to the path
            PL1new2=PL1new),


 append(PL1new2,PL2,PL),
 msort(PL, Sorted),
  \+dup(Sorted),
 % Distance is Dist1+Dist2,
 myplus(Dist1,Dist2,Distance),
 % Weight is W1+W2,
 myplus(W1,W2,Weight),


map(pred,[cat:pred,type:gerund, class:m,    % type:gerund is how we specify them in args and adjuncts
               case:_,
               mood:gerund,
               local_case:Case,                         % case is used for subject agreement,
              % local_case to store the case of the gerund
               number:sing,person:3,
               gender:neuter,
               pathlist:Sorted, distance:[Distance],
               gap:[],w:Weight,constituent_structure:[C_str1,coord:Coord, C_str2]])].




% ABL ABS
%%%%%%%%%

[coord2,c2_3] --->
[mapped(coord,[from:X,to:Y|Fcoord]),     % making sure we have a coordinator
  constraint([lex:Coord],Fcoord),

 mapped(aa,FAA1),
 mapped(aa,FAA2),
 FAA1 \= FAA2,
 constraint([pathlist:PL1, distance:[Dist1],w:W1, constituent_structure:C_str1], FAA1),
 constraint([pathlist:PL2, distance:[Dist2],w:W2, constituent_structure:C_str2], FAA2),
  Xminus1 is X-1,

 ifthenelse( (extremity(PL1,Xminus1), recorded(pos,position(Xminus1,X,First),_) ),  % leaving room for the first part of the correlative
                append(PL1,[p(Xminus1,X)],PL1n), % append first part
               (extremity(PL1,X), PL1n=PL1)       % first part elsewhere, or simply not the second member of a correlative pair
             ),

 start(PL2,Y),

 start(PL1,PL1Start),

 append(PL1n,[p(X,Y)],PL1new),  % add coordinator to the path

 ifthenelse(mapped(coord,[from:AAA,to:PL1Start|Fcoord]),
             % coordination in front of the first conjunct 'et rege amato a regina et mutatis mutandis'
             % should be the same coordinator as the one in between the 2 conjuncts
            append([p(AAA,PL1Start)],PL1new, PL1new2), % append it to the path
            PL1new2=PL1new),


 append(PL1new2,PL2,PL),
 msort(PL, Sorted),
  \+dup(Sorted),
 % Distance is Dist1+Dist2,
 myplus(Dist1,Dist2,Distance),
 % Weight is W1+W2,
 myplus(W1,W2,Weight),

 map(aa,[      pathlist:Sorted,
               distance:[Distance],
               w:Weight,
               constituent_structure:[C_str1,coord:Coord, C_str2]])].






%%%%%%%%%%%%%%%%%%%%
% Coordinated preds
%%%%%%%%%%%%%%%%%%%%

%
% FULL PREDICATIONS
%

% NONFINITE
%%%%%%%%%%%

% (puto) regem epistulas scrivere et reginam librum regis legisse

[coord2,c2_4] --->
[mapped(coord,[from:X,to:Y|Fcoord]),
  constraint([lex:Coord],Fcoord),
 mapped(flags,active(nonfinite)),


 mapped(pred,Fp1),
 mapped(pred,Fp2),
 Fp1 \= Fp2,
 constraint([pathlist:PL1],Fp1),
 constraint([pathlist:PL2],Fp2),
 extremity(PL1,X), % the coordinator sits in between the 2 preds
 start(PL2,Y),


 constraint([type:nonfinite,mood:Mood,tense:Tense,
            distance:[Distp1],gap:Gap1,w:W1,constituent_structure:Head1],Fp1),
 constraint([type:nonfinite,mood:Mood,
             distance:[Distp2],gap:Gap2,w:W2,constituent_structure:Head2],Fp2),

 myappend(Gap1,Gap2,Gap),
 append(PL1,[p(X,Y)],PL1new),  % add coordinator to the path
 append(PL1new,PL2,PL),
 msort(PL, Sorted),
  \+dup(Sorted),
 contiguous(Sorted),                                   % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%
 % Distance is Distp1+Distp2,
 % Weight is W1+W2,
 myplus(Distp1,Distp2,Distance),
 myplus(W1,W2,Weight),

 map(pred,[pathlist:Sorted,distance:[Distance],
             type:nonfinite,
             cat:pred,
             mood:Mood,tense:Tense,
             class:m,
             number:sing,gender:neuter,
             case:or([nom,acc]),person:3,
             gap:Gap, w:Weight,
             constituent_structure:[head:Head1,coord:Coord, head:Head2]])].


% FINITE
%%%%%%%%

% rex scribit epistulas et regina legit librum

[coord2,c2_5] --->
[ mapped(coord,[from:X,to:Y|Fcoord]),
  constraint([lex:Coord],Fcoord),
 mapped(pred,Fp1),
 mapped(pred,Fp2),
 Fp1 \= Fp2,
 constraint([pathlist:PL1],Fp1),
  constraint([pathlist:PL2],Fp2),
  extremity(PL1,X), % the coordinator sits in between the 2 preds
 start(PL2,Y),


 constraint([type:finite,class:m,
             distance:[Distp1],
             mood:Mood, tense:Tense,number:Nsubj,person:Psubj,
             gap:Gap1,
             w:W1,
             constituent_structure:Head1],Fp1),
 constraint([type:finite,class:m,
             distance:[Distp2],
             gap:Gap2,
             w:W2,
             constituent_structure:Head2],Fp2),

 append(PL1,[p(X,Y)],PL1new),  % add coordinator to the path
 append(PL1new,PL2,PL),
 myappend(Gap1,Gap2,Gap),      % special type of append for gaps - tries to get rid of empty gaps
 msort(PL, Sorted),
  \+dup(Sorted),
 contiguous(Sorted),                                     % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%
% Distance is Distp1+Distp2,
% Weight is W1+W2,
   myplus(Distp1,Distp2,Distance),
  myplus(W1,W2,Weight),


 map(pred,[cat:pred,type:finite,
             pathlist:Sorted,distance:[Distance],
             argbound:no,
             mood:Mood, tense:Tense,
             class:m,
             number:Nsubj,person:Psubj,gender:_,
             gap:Gap,
             w:Weight,
             constituent_structure:[head:Head1,coord:Coord, head:Head2]])].


% SUBORDINATE CLAUSES
%%%%%%%%%%%%%%%%%%%%%

[coord2,c2_6] --->
[mapped(coord,[from:X,to:Y|Fcoord]),

 mapped(pred,Fp1),
 mapped(pred,Fp2),
 Fp1 \= Fp2,
 constraint([pathlist:PL1],Fp1),
  constraint([pathlist:PL2],Fp2),
 extremity(PL1,X), % the coordinator sits in between the 2 preds
 start(PL2,Y),
constraint([lex:Et],Fcoord),
constraint([type:finite,class:s,subordinator:Fs1,
             distance:[Distp1],
             mood:Mood, tense:Tense,
             w:W1,
             constituent_structure:Head1],Fp1),
 constraint([type:finite,class:s,subordinator:Fs2,
             distance:[Distp2],
             w:W2,
             constituent_structure:Head2],Fp2),
 constraint([lex:S1],Fs1),
  constraint([lex:S2],Fs2),


 append(PL1,[p(X,Y)],PL1new),  % add coordinator to the path
 append(PL1new,PL2,PL),
 flatten(PL, PLflat),
 msort(PLflat, Sorted),
  \+dup(Sorted),
 contiguous(Sorted),                                     % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%
 myplus(Distp1,Distp2,Distance),
 myplus(W1,W2,Weight),
 myplus(Weight,4,WeightWithBonus),
 map(pred,[cat:pred,type:finite,class:s,
             pathlist:Sorted,distance:[Distance],
             argbound:no,
             subordinator:Fs1,subordinator:Fs2,
             mood:Mood, tense:Tense,
             gap:[],
             w:WeightWithBonus,
             constituent_structure:[lex:S1,head:Head1, coord:Et, lex:S2,head:Head2]])].





%%%%%%%%%%%%%%%%%%%%%%%%
% PARTIAL PREDICATIONS
%%%%%%%%%%%%%%%%%%%%%%%%

% FINITE
%%%%%%%%

% subject + coordinated verb group
% rex scribit epistulas et legit librum
% -> rex scribit epistulas + legit librum
% find a pred and record person, number of the subject
% insert coord
% find a finite pred with no subject
% check subject features
% register the lot as a single clause

% this looks a bit ad hoc ;-)
% you bet !


[coord2,c2_7] --->
[mapped(coord,[from:X,to:Y|Fcoord]),  % we have a coordinator
  constraint([lex:Coord],Fcoord),
 mapped(pred,Fp1),
 mapped(pred,Fp2),
 Fp1 \= Fp2,
 constraint([pathlist:PL1],Fp1),
  constraint([pathlist:PL2],Fp2),
 extremity(PL1,X), % the coordinator sits in between the 2 preds
 start(PL2,Y),


 constraint([type:finite,class:m,
             mood:Mood,
             tense:Tense,
            distance:[Distp1],
             number:Nsubj,      % subject features have to be shared
             person:Psubj,
             constituent_structure:Head1,gap:Gap1,w:W1],Fp1),

 constraint([type:finite_i,      % subjectless
             mood:Mood,
            distance:[Distp2],
             number:Nsubj,                        % the subject is shared
             person:Psubj,
             constituent_structure:Head2,gap:Gap2,w:W2],Fp2),



 append(PL1,[p(X,Y)],PL1new),  % add coordinator to the path
 append(PL1new,PL2,PL),
 msort(PL, Sorted),
   \+dup(Sorted),
 contiguous(Sorted),                                         % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%
 % Distance is Distp1+Distp2,
 % Weight is W1+W2,
  myplus(Distp1,Distp2,Distance),
  myplus(W1,W2,Weight),

  myplus(Weight,2,WeightWithBonus),

 myappend(Gap1,Gap2,Gaps),    % adding up the gaps, i.e. getting rid of empty gaps

 map(pred,[cat:pred,type:finite,
             pathlist:Sorted,distance:[Distance],
             number:Nsubj,person:Psubj,
             gender:neuter,
             mood:Mood, tense:Tense,
             class:m,
             argbound:no,
             gap:Gaps,
             w:WeightWithBonus,
             constituent_structure:[head:Head1,coord:Coord, head:Head2]])].




% NONFINITE
%%%%%%%%%%%

% puto regem epistulas scribere et librum legere

% treatment along the same lines : regem epistulas scribere (full nonfinite clause)
% followed by partial, subject-less non finite clause (nonfinite_i):

% to be dealt with in finite pass, not in coord2
% decision on the basis of experience and experiments....


[finite,c2_8] --->
[ mapped(coord,[from:X,to:Y|Fcoord]),
   constraint([lex:Coord],Fcoord),
 mapped(flags,active(nonfinite)),    % both flags need to be on

 mapped(pred,Fp1),
 mapped(pred,Fp2),
 Fp1 \= Fp2,
  constraint([pathlist:PL1],Fp1),
  constraint([pathlist:PL2],Fp2),
  extremity(PL1,X), % the coordinator sits in between the 2 preds
 start(PL2,Y),



 constraint([type:nonfinite,
             mood:Mood, tense:Tense,gap:Gap1,
             distance:[Distp1],w:W1,
             constituent_structure:Head1],Fp1),

 constraint([type:nonfinite_i,
             mood:Mood,
             distance:[Distp2],
             gap:Gap2,w:W2,
             constituent_structure:Head2],Fp2),


 append(PL1,[p(X,Y)],PL1new),  % add coordinator to the path
 append(PL1new,PL2,PL),
 msort(PL, Sorted),
  \+dup(Sorted),
 contiguous(Sorted),                                          % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%
% Distance is Distp1+Distp2,
% Weight is W1+W2,
  myplus(Distp1,Distp2,Distance),
  myplus(W1,W2,Weight),

  myplus(Weight,2,WeightWithBonus),
 myappend(Gap1,Gap2,Gaps),     % special append for gaps
 map(pred,[cat:pred,
             type:nonfinite,
             mood:Mood, tense:Tense,
             class:m,
             pathlist:Sorted,distance:[Distance],
             number:sing,gender:neuter,case:or([nom,acc]),person:3, % necessary info as subject of ESSE
             gap:Gaps,w:WeightWithBonus,
             constituent_structure:[head:Head1,coord:Coord, head:Head2]])].


% obliuisci et tacere
%%%%%%%%%%%%%%%%%%%%%

% coupling of infinitive clauses
% to be dealt with in finite, too

% decision on the basis of experience and experiments....

[finite,c2_9] --->
[ mapped(coord,[from:X,to:Y|Fcoord]),
   constraint([lex:Coord],Fcoord),
 mapped(flags,active(nonfinite)),

 mapped(pred,Fp1),
 mapped(pred,Fp2),
 Fp1 \= Fp2,
  constraint([pathlist:PL1],Fp1),
  constraint([pathlist:PL2],Fp2),
 % extremity(PL1,X), % the coordinator sits in between the 2 preds
 start(PL2,Y),        % safer to specify begin of second pred after coord
      % Huic [ego] satis facere [cupio] vosque -> que vos adhibere arbitros


constraint([type:nonfinite_i,              % no subject for the infinitives
             mood:infinitive,
             distance:[Distp1],
             gap:[],        % the two clauses must be gap-free
             w:W1,constituent_structure:Head1],Fp1),

 constraint([type:nonfinite_i,
             mood:infinitive,
             distance:[Distp2],
             gap:[],
             w:W2,constituent_structure:Head2],Fp2),


 append(PL1,[p(X,Y)],PL1new),  % add coordinator to the path
 append(PL1new,PL2,PL),
 msort(PL, Sorted),
  \+dup(Sorted),
 laxquasicontiguous2(Sorted),     % two out not necessarily together
        % Huic [ego] satis facere [cupio] vosque adhibere arbitros
                                           % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%

 % Distance is Distp1+Distp2,
 % Weight is W1+W2,
 myplus(Distp1,Distp2,Distance),
  myplus(W1,W2,Weight),

  map(pred,[cat:pred,type:nonfinite_i,
             mood:infinitive,
             tense:_,
             class:m,
             pathlist:Sorted,distance:[Distance],
             number:_,gender:_,person:_,case:or([nom,acc]), % subject of ESSE

             gap:[],
             w:Weight,
             constituent_structure:[head:Head1,coord:Coord, head:Head2]])].




%%%%%%%%%%%%%%%%%%
% RELATIVE CLAUSES
%%%%%%%%%%%%%%%%%%

 % with relative pronouns at the start of each clause

 % vir [qui [np_e] librum legit] et [qui  [np_e] epistulas ad Marcum mittit]
 % librum [quem   amas [np_e]] et [quem regina legit [np_e]]
 % vir [qui [np_e] amat reginam] et [ad quem  rex epistulas longas mittit [pp_e]]

 % the other types of conjuncts concern the clause itself and are dealt with elsewhere,
 % in a **** VERY INADEQUATE FASHION **** !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

 % to be dealt with in finite pass, not in coord2

[finite,c2_10] --->
[ mapped(coord,[from:X, to:Y|FS2]),
   constraint([lex:Coord],FS2),


  mapped(relative_clause,FS1),
  mapped(relative_clause,FS3),
  FS1 \= FS3,
    constraint([pathlist:PL1],FS1),
  constraint([pathlist:PL2],FS3),
  extremity(PL1,X), % the coordinator sits in between the 2 preds
 start(PL2,Y),
  constraint([distance:[D1],
              number:Nb,          % number and gender must be shared
              gender:Gender,
              case:Case,          % not case
              gap:GAPARG1,
              mood:Mood,
              tense:Tense,
              index:Index,
              constraints:Co1,
              w:W1,
              constituent_structure:C_str1],FS1),

  constraint([distance:[D2],
              number:Nb,
              gender:Gender,
              gap:GAPARG2,
              index:Index,
              constraints:_,
              w:W2,
              constituent_structure:C_str2],FS3),

 GAPARG1=[gap:[type:_,                          % GAP specifies type
              index:Index,                      % opens a place for the Index of the antecedent to fill
              function:Function1,                 % function must be compatible with relative pronoun
              Function1:[e:Index],                % info for the parse tree - e (empty, trace) followed by the Index
              constraints:_
                           ]],

 GAPARG2=[gap:[type:_,
               index:Index,       % gap sharing allows proper indexing in the constituent_structure
               function:Function2,
               Function2:[e:Index],
               constraints:_
                           ]],

 append(PL1,[p(X,Y)],PLL),  % add coordinator to the path
 append(PLL,PL2,Sorted),
  \+dup(Sorted),
 contiguous(Sorted),                                        % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%
 % Distance is D1+D2,
  myplus(D1,D2,Distance),

 % Weight is W1+W2+3,  % bonus for greater cohesion - 2 relative clauses with the same antecedent
                     % rather than a second clause with antecedent understood

  myplus(W1,W2,T1),
  myplus(T1,3,Weight),
  map(relative_clause,[pathlist:Sorted,
                         distance:[Distance],
                         gap:GAPARG1,   % copied from the first rel clause
                         index:Index,
                         number:Nb,
                         gender:Gender,
                         case:Case,
                         person:Prel,
                         type:finite,
                         mood:Mood,
                         tense:Tense,
                         constraints:Co1,       % ditto
                         w:Weight,
                         constituent_structure:[rel_clause:C_str1, coord:Coord,
                                rel_clause:C_str2]]) ].


























%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PROCEDURES RELATING TO THE PATH
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% these procedures examine which parts of the string are covered by various elements
% they are meant to measure properties like adjacency, contiguousness and distance

% the path is a list of p(X,Y) structures, where X and Y stand for positions in the string,
% as computed when the string is entered in and processed, just after being delivered as a list of words
% by 'getsentence'


% FINDING A PATH THAT COVERS THE WHOLE SENTENCE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% all words must be used up
% no gap and the end of the sentence must be reached
% Begin and End are the extremities of the pathlist (0 and whatever fin(Fin) records)

 path(Begin,End,Pathlist) :-
  pick(p(Begin,Next),Pathlist,RPaths),
  path(Next,End,RPaths).

 path(E,E,[]).


% ADJACENCY
%%%%%%%%%%%

% see nps with genitive np as subconstituent for an example of the relevance of such a procedure

% strict
%

% one pair in the first path has an end which corresponds to the beginning of a pair
% belonging to the second path
% [p(3,5), p(2,3] and [p(5,6), p(6,8), p(8,9)] for instance

 adjacent(PL1,PL2) :- member(p(_,Y),PL1), member(p(Y,_),PL2),!.
 adjacent(PL1,PL2) :- member(p(_,Y),PL2), member(p(Y,_),PL1),!.
 adjacent([],_).
 adjacent(_,[]).


% left to right order in adjacency

 adjacent_lr(PL1,PL2) :- member(p(_,Y),PL1), member(p(Y,_),PL2).
 adjacent_lr([],_).
 adjacent_lr(_,[]).

 relaxadjacent_lr(PL1,PL2) :- member(p(_,Y),PL1), member(p(X,_),PL2), succ(Y,X).

/* the following algorithm is not to be used because of potential holes in path:
   rex recubat patulae sub tegmine fagi
   PL1 p(4,5) = tegmine
   PL2 p(2,3), p(5,6) = patulae fagi

adjacent(PL1,[p(Y,_)|_]) :- last(PL1, p(_,Y)).
adjacent([p(Y,_)|_],PL2) :- last(PL2, p(_,Y)).
relaxadjacent(PL1,[p(Y,_)|_]) :- last(PL1, p(_,X)), succ(X,Y).
relaxadjacent([p(X,_)|_],PL2) :- last(PL2, p(_,Y)), succ(Y,X).
*/


% relaxed adjacency
%%%%%%%%%%%%%%%%%%%

% a distance of 1 or 2 (in the case of relaxadjacent2) is allowed between the two corresponding pairs

 relaxadjacent(PL1,PL2) :- member(p(_,Y),PL1), member(p(X,_),PL2), succ(Y,X),!.
 relaxadjacent(PL1,PL2) :- member(p(_,Y),PL2), member(p(X,_),PL1), succ(Y,X).

 relaxadjacent2(PL1,PL2) :- member(p(_,A),PL1),
                              member(p(C,_),PL2),
                              succ(A,B),
                              succ(B,C),!.

 relaxadjacent2(PL1,PL2) :- member(p(_,A),PL2),
                              member(p(C,_),PL1),
                              succ(A,B),
                              succ(B,C).

 relaxadjacent3(PL1,PL2) :- member(p(_,A),PL2),
                              member(p(D,_),PL1),
                              succ(A,B),
                              succ(B,C),
                              succ(C,D).

relaxadjacent3(PL1,PL2) :- member(p(_,A),PL1),
                              member(p(D,_),PL2),
                              succ(A,B),
                              succ(B,C),
                              succ(C,D).

relaxadjacent4(PL1,PL2) :- member(p(_,A),PL2),
                              member(p(E,_),PL1),
                              succ(A,B),
                              succ(B,C),
                              succ(C,D),
                              succ(D,E).


relaxadjacent4(PL1,PL2) :- member(p(_,A),PL1),
                              member(p(E,_),PL2),
                              succ(A,B),
                              succ(B,C),
                              succ(C,D),
                              succ(D,E).

relaxadjacent5(PL1,PL2) :- member(p(_,A),PL2),
                              member(p(F,_),PL1),
                              succ(A,B),
                              succ(B,C),
                              succ(C,D),
                              succ(D,E),
                              succ(E,F).


relaxadjacent5(PL1,PL2) :- member(p(_,A),PL1),
                              member(p(F,_),PL2),
                              succ(A,B),
                              succ(B,C),
                              succ(C,D),
                              succ(D,E),
                              succ(E,F).




% relaxed adjacency with control on intervening elements
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% with respect to POS:noun
%

% sometimes we have to check that no noun occurs in an interval
% as when we wish to relate the 2 heads of nps linked by the cplt noun relation
% involving a genitive phrase
% Marci servas amicos
% Marci preferably linked with servas rather than amicos:
% putabas Marci servas amicos reginae amasse



% both directions left to right and right to left


relaxedadjacent1_n(H1,H2,G1,G2,n) :- succ(H2,G1),
                                                \+ mapped(noun,[from:H2,to:G1|_]),!.

relaxedadjacent1_n(H1,H2,G1,G2,n) :- succ(G2,H1),
                                                \+ mapped(noun,[from:G2,to:H1|_]).

relaxedadjacent2_n(H1,H2,G1,G2,n) :- succ(H2,X),
                                     succ(X,G1),
                                                 \+ mapped(noun,[from:H2,to:X|_]),
                                                 \+ mapped(noun,[from:X,to:G1|_]),!.

relaxedadjacent2_n(H1,H2,G1,G2,n) :- succ(G2,X),
                                     succ(X,H1),
                                                 \+ mapped(noun,[from:G2,to:X|_]),
                                                 \+ mapped(noun,[from:X,to:H1|_]).








% with respect to CASE GENDER and NUMBER
%

% when we try to relate adj and noun
% we are not likely to be allowed to jump a noun with all the right properties in terms of
% case gender and number:
% putabas malas servas amicas reginae fuisse
% malas is not likely to link with amicas by 'jumping' servas

relaxedadjacent1_cgn(PL1,PL2,Case,Gender,Nb) :- member(p(_,Y),PL1), member(p(X,_),PL2),
                                                succ(Y,X),
                                                mapped(noun,[from:Y,to:X|FSnoun]),
                                                constraint([case:Case,gender:Gender,number:Nb],FSnoun),
                                                !, fail.

relaxedadjacent1_cgn(_,_,_,_,_).

relaxedadjacent2_cgn(PL1,PL2,Case,Gender,Nb) :- member(p(_,A),PL1), member(p(C,_),PL2),
                                                succ(A,B),succ(B,C),
                                                ( (mapped(noun,[from:A,to:B|FSnoun1]),
                                                   constraint([case:Case,gender:Gender,number:Nb],FSnoun1)) ;
                                                   (mapped(noun,[from:B,to:C|FSnoun2]),
                                                   constraint([case:Case,gender:Gender,number:Nb],FSnoun2))),
                                                !, fail.

relaxedadjacent2_cgn(_,_,_,_,_).

relaxedadjacent3_cgn(PL1,PL2,Case,Gender,Nb) :- member(p(_,A),PL1), member(p(D,_),PL2),
                                                succ(A,B),succ(B,C),succ(C,D),
                                                ( (mapped(noun,[from:A,to:B|FSnoun1]),
                                                   constraint([case:Case,gender:Gender,number:Nb],FSnoun1)) ;
                                                          ( (mapped(noun,[from:B,to:C|FSnoun2]),
                                                             constraint([case:Case,gender:Gender,number:Nb],FSnoun2));
                                                             (mapped(noun,[from:C,to:D|FSnoun3]),
                                                             constraint([case:Case,gender:Gender,number:Nb],FSnoun3)))),
                                                !, fail.

relaxedadjacent3_cgn(_,_,_,_,_).


relaxedadjacent4_cgn(PL1,PL2,Case,Gender,Nb) :- member(p(_,A),PL1), member(p(E,_),PL2),
                                                succ(A,B),succ(B,C),succ(C,D),succ(D,E),
                                                ( (mapped(noun,[from:A,to:B|FSnoun1]),
                                                   constraint([case:Case,gender:Gender,number:Nb],FSnoun1)) ;
                                                          ( (mapped(noun,[from:B,to:C|FSnoun2]),
                                                             constraint([case:Case,gender:Gender,number:Nb],FSnoun2));
                                                             (mapped(noun,[from:C,to:D|FSnoun3]),
                                                             constraint([case:Case,gender:Gender,number:Nb],FSnoun3));
                                                             (mapped(noun,[from:D,to:E|FSnoun4]),
                                                             constraint([case:Case,gender:Gender,number:Nb],FSnoun4))


                                                 )),
                                                !, fail.

relaxedadjacent4_cgn(_,_,_,_,_).

relaxedadjacent5_cgn(PL1,PL2,Case,Gender,Nb) :- member(p(_,A),PL1), member(p(F,_),PL2),
                                                succ(A,B),succ(B,C),succ(C,D),succ(D,E),succ(E,F),
                                                ( (mapped(noun,[from:A,to:B|FSnoun1]),
                                                   constraint([case:Case,gender:Gender,number:Nb],FSnoun1)) ;
                                                          ( (mapped(noun,[from:B,to:C|FSnoun2]),
                                                             constraint([case:Case,gender:Gender,number:Nb],FSnoun2));
                                                             (mapped(noun,[from:C,to:D|FSnoun3]),
                                                             constraint([case:Case,gender:Gender,number:Nb],FSnoun3));
                                                             (mapped(noun,[from:D,to:E|FSnoun4]),
                                                             constraint([case:Case,gender:Gender,number:Nb],FSnoun4));
                                                             (mapped(noun,[from:E,to:F|FSnoun5]),
                                                             constraint([case:Case,gender:Gender,number:Nb],FSnoun5))


                                                 )),
                                                !, fail.

relaxedadjacent5_cgn(_,_,_,_,_).


relaxedadjacent6_cgn(PL1,PL2,Case,Gender,Nb) :- member(p(_,A),PL1), member(p(G,_),PL2),
                                                succ(A,B),succ(B,C),succ(C,D),succ(D,E),succ(E,F),succ(F,G),
                                                ( (mapped(noun,[from:A,to:B|FSnoun1]),
                                                   constraint([case:Case,gender:Gender,number:Nb],FSnoun1)) ;
                                                          ( (mapped(noun,[from:B,to:C|FSnoun2]),
                                                             constraint([case:Case,gender:Gender,number:Nb],FSnoun2));
                                                             (mapped(noun,[from:C,to:D|FSnoun3]),
                                                             constraint([case:Case,gender:Gender,number:Nb],FSnoun3));
                                                             (mapped(noun,[from:D,to:E|FSnoun4]),
                                                             constraint([case:Case,gender:Gender,number:Nb],FSnoun4));
                                                             (mapped(noun,[from:E,to:F|FSnoun5]),
                                                             constraint([case:Case,gender:Gender,number:Nb],FSnoun5));
                                                             (mapped(noun,[from:F,to:G|FSnoun6]),
                                                             constraint([case:Case,gender:Gender,number:Nb],FSnoun6))

                                                 )),
                                                !, fail.

relaxedadjacent6_cgn(_,_,_,_,_).

relaxedadjacent7_cgn(PL1,PL2,Case,Gender,Nb) :- member(p(_,A),PL1), member(p(H,_),PL2),
                                                succ(A,B),succ(B,C),succ(C,D),succ(D,E),succ(E,F),succ(F,G),succ(G,H),
                                                ( (mapped(noun,[from:A,to:B|FSnoun1]),
                                                   constraint([case:Case,gender:Gender,number:Nb],FSnoun1)) ;
                                                          ( (mapped(noun,[from:B,to:C|FSnoun2]),
                                                             constraint([case:Case,gender:Gender,number:Nb],FSnoun2));
                                                             (mapped(noun,[from:C,to:D|FSnoun3]),
                                                             constraint([case:Case,gender:Gender,number:Nb],FSnoun3));
                                                             (mapped(noun,[from:D,to:E|FSnoun4]),
                                                             constraint([case:Case,gender:Gender,number:Nb],FSnoun4));
                                                             (mapped(noun,[from:E,to:F|FSnoun5]),
                                                             constraint([case:Case,gender:Gender,number:Nb],FSnoun5));
                                                             (mapped(noun,[from:F,to:G|FSnoun6]),
                                                             constraint([case:Case,gender:Gender,number:Nb],FSnoun6));
                                                             (mapped(noun,[from:G,to:H|FSnoun7]),
                                                             constraint([case:Case,gender:Gender,number:Nb],FSnoun7))
                                                 )),
                                                !, fail.

relaxedadjacent7_cgn(_,_,_,_,_).

relaxedadjacent8_cgn(PL1,PL2,Case,Gender,Nb) :- member(p(_,A),PL1), member(p(I,_),PL2),
                                                succ(A,B),succ(B,C),succ(C,D),succ(D,E),succ(E,F),succ(F,G),succ(G,H),
                                                succ(H,I),
                                                ( (mapped(noun,[from:A,to:B|FSnoun1]),
                                                   constraint([case:Case,gender:Gender,number:Nb],FSnoun1)) ;
                                                          ( (mapped(noun,[from:B,to:C|FSnoun2]),
                                                             constraint([case:Case,gender:Gender,number:Nb],FSnoun2));
                                                             (mapped(noun,[from:C,to:D|FSnoun3]),
                                                             constraint([case:Case,gender:Gender,number:Nb],FSnoun3));
                                                             (mapped(noun,[from:D,to:E|FSnoun4]),
                                                             constraint([case:Case,gender:Gender,number:Nb],FSnoun4));
                                                             (mapped(noun,[from:E,to:F|FSnoun5]),
                                                             constraint([case:Case,gender:Gender,number:Nb],FSnoun5));
                                                             (mapped(noun,[from:F,to:G|FSnoun6]),
                                                             constraint([case:Case,gender:Gender,number:Nb],FSnoun6));
                                                             (mapped(noun,[from:G,to:H|FSnoun7]),
                                                             constraint([case:Case,gender:Gender,number:Nb],FSnoun7));
                                                             (mapped(noun,[from:H,to:I|FSnoun8]),
                                                             constraint([case:Case,gender:Gender,number:Nb],FSnoun8))                                           
                                                    )),
                                                !, fail.

relaxedadjacent8_cgn(_,_,_,_,_).

relaxedadjacent9_cgn(PL1,PL2,Case,Gender,Nb) :- member(p(_,A),PL1), member(p(J,_),PL2),
                                                succ(A,B),succ(B,C),succ(C,D),succ(D,E),succ(E,F),succ(F,G),succ(G,H),
                                                succ(H,I),succ(I,J),
                                                ( (mapped(noun,[from:A,to:B|FSnoun1]),
                                                   constraint([case:Case,gender:Gender,number:Nb],FSnoun1)) ;
                                                          ( (mapped(noun,[from:B,to:C|FSnoun2]),
                                                             constraint([case:Case,gender:Gender,number:Nb],FSnoun2));
                                                             (mapped(noun,[from:C,to:D|FSnoun3]),
                                                             constraint([case:Case,gender:Gender,number:Nb],FSnoun3));
                                                             (mapped(noun,[from:D,to:E|FSnoun4]),
                                                             constraint([case:Case,gender:Gender,number:Nb],FSnoun4));
                                                             (mapped(noun,[from:E,to:F|FSnoun5]),
                                                             constraint([case:Case,gender:Gender,number:Nb],FSnoun5));
                                                             (mapped(noun,[from:F,to:G|FSnoun6]),
                                                             constraint([case:Case,gender:Gender,number:Nb],FSnoun6));
                                                             (mapped(noun,[from:G,to:H|FSnoun7]),
                                                             constraint([case:Case,gender:Gender,number:Nb],FSnoun7));
                                                             (mapped(noun,[from:H,to:I|FSnoun8]),
                                                             constraint([case:Case,gender:Gender,number:Nb],FSnoun8));      
                                                             (mapped(noun,[from:I,to:J|FSnoun9]),
                                                             constraint([case:Case,gender:Gender,number:Nb],FSnoun9))                                       
                                                   
                                                 )),
                                                !, fail.

relaxedadjacent9_cgn(_,_,_,_,_).




% PATH CONTIGUITY
%%%%%%%%%%%%%%%%%

% the various elements follow each other without leaving a gap        % CONTIGUITY TEST IMPORTANT %%%%%%%%%%%%

contiguous([]).
contiguous([_]).

contiguous([p(_,Y),p(Y,Z)|Tail]) :- contiguous([p(Y,Z)|Tail]).


% in quasicontiguous we allow one or two or even three or even four elements to be out of place, i.e.
% out of the clause they belong to, provided they themselves are found together

quasicontiguous(L) :- contiguous(L), !. % Qui peut le plus...

quasicontiguous(L) :- pick(_,L,L1),
                      contiguous(L1),
                      !.      % ONE OUT

quasicontiguous2(L) :- quasicontiguous(L),
                       !.

quasicontiguous2(L) :- pick(A,L,L1),
                       pick(B,L1,L2),
                       contiguous([A,B]),
                       contiguous(L2),
                       !.     % TWO OUT IF TO BE FOUND TOGETHER

laxquasicontiguous2(L) :- pick(A,L,L1),
                          pick(B,L1,L2),
                          contiguous(L2),
                          !.      % allows the outsiders not to be together

% Horatius: me Romaene poemata censes scribere posse inter tot curas totque labores?
% Both 'me' and 'censes' are outsiders


quasicontiguous3(L) :- quasicontiguous2(L),
                       !.
quasicontiguous3(L) :- pick(A,L,L1),
                       pick(B,L1,L2),
                       pick(C,L2,L3),
                       contiguous([A,B,C]),
                       contiguous(L3),
                       !.
          % THREE OUT IF TO BE FOUND TOGETHER


quasicontiguous4(L) :- quasicontiguous3(L),
                       !.
quasicontiguous4(L) :- pick(A,L,L1),
                       pick(B,L1,L2),
                       pick(C,L2,L3),
                       pick(D,L3,L4),
                       contiguous([A,B,C,D]),
                       contiguous(L4),
                       !.
         % FOUR OUT IF TO BE FOUND TOGETHER




% DISTANCE BETWEEN TWO PATHS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% with respect to arg bearer

distancev(Path1,Path2,0) :-  adjacent(Path1,Path2), !.





distancev(Path1,Path2,1) :-  member(p(_,B),Path1),
                            succ(B,C),
                            member(p(C,_),Path2), !.

distancev(Path1,Path2,1) :-  member(p(_,B),Path2),
                            succ(B,C),
                            member(p(C,_),Path1), !.

distancev(Path1,Path2,2) :-  member(p(_,B),Path1),
                            succ(B,C),
                            succ(C,D),
                            member(p(D,_),Path2), !.

distancev(Path1,Path2,2) :-  member(p(_,B),Path2),
                            succ(B,C),
                            succ(C,D),
                            member(p(D,_),Path1), !.


distancev(Path1,Path2,3) :-  member(p(_,B),Path1),
                            succ(B,C),
                            succ(C,D),
                            succ(D,E),
                            member(p(E,_),Path2), !.


distancev(Path1,Path2,3) :-  member(p(_,B),Path2),
                            succ(B,C),
                            succ(C,D),
                            succ(D,E),
                            member(p(E,_),Path1), !.


distancev(Path1,Path2,4) :-  member(p(_,B),Path1),
                            succ(B,C),
                            succ(C,D),
                            succ(D,E),
                            succ(E,F),
                            member(p(F,_),Path2), !.

distancev(Path1,Path2,4) :-  member(p(_,B),Path2),
                            succ(B,C),
                            succ(C,D),
                            succ(D,E),
                            succ(E,F),
                            member(p(F,_),Path1), !.



distancev(Path1,Path2,5).    % 5 assigned to all cases of greater distance than 4




% the following code is appropriate only if there is no gap in the paths
% which cannot be guaranteed

% if distance is more than 4:
% we first determine the end points of the two paths
% we determine the order in which they appear
% and then the distance between extremity of the first one and start of the second

distance(Path1,Path2,Distance) :- extremity(Path1,Ext1),
                                  extremity(Path2,Ext2),
                                  start(Path1,St1),
                                  start(Path2, St2),
                                  ifthenelse(Ext1 =< St2,
                                             Distance is St2 - Ext1,
                                             Distance is St1 - Ext2).





% PATH GAPS
%%%%%%%%%%%

% Gap between two paths, g(Start,End)
% the positions between Start and End fall into the gap

gapinpath(Path1,Path2,g(Ext1,St2)) :-     extremity(Path1,Ext1),
            extremity(Path2,Ext2),
            start(Path1,St1),
                                          start(Path2, St2),
                                          Ext1 < St2.

gapinpath(Path1,Path2,g(Ext2,St1)) :-     extremity(Path1,Ext1),
            extremity(Path2,Ext2),
            start(Path1,St1),
                                          start(Path2, St2),
                                          Ext2 < St1.



% checks that a pathelement p(P1,P2) is in gap

ingap(p(P1,P2),g(Start,End)) :- P1 >= Start,
                                P2 =< End.



% extremity : last position in pathlist
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% we select the very last position registered, i.e. the second element of the p(X,Y) structure that ends the path

extremity(PathList, Ex):- last(PathList,p(_,Ex)).

% start : first position in a pathlist
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% a first element is easy to find by simple unification:
% we select the first element of the relevant p structure

start([p(Start,_)|_],Start).

% precedes(Path1,Path2)
%%%%%%%%%%%%%%%%%%%%%%%

precedes(P1,P2) :- extremity(P1,Extremity),
                   start(P2,Start),
                   Extremity =< Start.

















% SE-reference binding
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% We work on the parse when it's ready for output, exploring the tree structure via the C-string structure
% and binding the indices for SE, left uninstantiated up to that point


% TOP LEVEL

bind_se(ParseTot) :-

constraint([constituent_structure:[_|Parse]],ParseTot),


ifthen(
          constraint([subject:[cat:np,gender:GUP,number:NUP,index:IndexUP]],Parse), true),

ifthen(   constraint([object:OBJ1],Parse), true),
ifthen(   constraint([i_object:I_OBJ1],Parse), true),
ifthen(   constraint([prep_cplt:PREPCPLT1],Parse), true),
ifthen(   constraint([prep_phrase_adjunct_1:PREPADJ1],Parse), true),
ifthen(   constraint([prep_phrase_adjunct_2:PREPADJ2],Parse), true),
ifthen(   constraint([prep_phrase_adjunct_3:PREPADJ3],Parse), true),


ifthen(   (nonvar(I_OBJ1),constraint([lex:pp3refl],I_OBJ1)),
                                                constraint([gender:GUP,number:NUP,index:IndexUP],I_OBJ1)),
ifthen(   (nonvar(OBJ1),constraint([lex:pp3refl],OBJ1)),
                                                constraint([gender:GUP,number:NUP,index:IndexUP],OBJ1)),


ifthen(   (nonvar(PREPCPLT1),constraint([lex:pp3refl],PREPCPLT1)),
                                                constraint([constituent_structure:[head:[gender:GUP,number:NUP,index:IndexUP]]],PREPCPLT1)),


ifthen(   (nonvar(PREPADJ1),constraint([lex:pp3refl],PREPADJ1)),
                                                constraint([constituent_structure:[head:[gender:GUP,number:NUP,index:IndexUP]]],PREPADJ1)),

ifthen(   (nonvar(PREPADJ2),constraint([lex:pp3refl],PREPADJ2)),
                                                constraint([constituent_structure:[head:[gender:GUP,number:NUP,index:IndexUP]]],PREPADJ2)),
ifthen(   (nonvar(PREPADJ3),constraint([lex:pp3refl],PREPADJ3)),
                                                constraint([constituent_structure:[head:[gender:GUP,number:NUP,index:IndexUP]]],PREPADJ3)),


ifthen(    (nonvar(OBJ1),constraint([cat:pred],OBJ1)),
                                                constraint([constituent_structure:CSTROBJ1],OBJ1)),


% the first of the following constraints fails in the case of some nonfinite_i clauses such as "vincere scis" where the subject is missing
% since we use bind_se  for its side effects we can call it within an ifthen clause when we wish to apply it : ifthen(bind_se(X),true) will do the trick

ifthen( nonvar(CSTROBJ1),(constraint([subject:SUBJ2],CSTROBJ1);true)      ),
ifthen( nonvar(CSTROBJ1),(constraint([object:OBJ2],CSTROBJ1);true)        ),
ifthen( nonvar(CSTROBJ1),(constraint([i_object:I_OBJ2],CSTROBJ1);true)    ),

ifthen( nonvar(CSTROBJ1),(constraint([prep_cplt:PREPCPLT2],CSTROBJ1); true) ),

ifthen( nonvar(CSTROBJ1),(constraint([prep_phrase_adjunct_1:PREPADJ12],CSTROBJ1); true) ),
ifthen( nonvar(CSTROBJ1),(constraint([prep_phrase_adjunct_2:PREPADJ22],CSTROBJ1); true) ),
ifthen( nonvar(CSTROBJ1),(constraint([prep_phrase_adjunct_3:PREPADJ32],CSTROBJ1); true) ),


% ONE DOWN

% only the first choice fires since bind_se is called in an ifthen sequence
% unless, of course, the adjective ipse is used with se and provides info on nber and gender
% that allows us to choose: rex credit reginam se amare vs rex credit reginam se ipsam amare

% but otherwise we choose to promote the one-up binding : rex putat reginam se amare, se bound to rex and not to reginam


ifthen(   (nonvar(SUBJ2), constraint([lex:pp3refl],SUBJ2)),
                                                 constraint([index:IndexUP,number:NUP,gender:GUP],SUBJ2)),

ifthen(    (nonvar(SUBJ2), constraint([cat:np,lex:Lex2,sem:[hum]],SUBJ2), Lex2 \= pp3refl),
                                                  constraint([number:NMIDDLE,gender:GMIDDLE,index:IndexMIDDLE],SUBJ2)),

ifthen(   (nonvar(OBJ2),constraint([lex:pp3refl],OBJ2)),
                                                ( constraint([gender:GUP,number:NUP,index:IndexUP],OBJ2) ;
                                                  constraint([number:NMIDDLE,gender:GMIDDLE,index:IndexMIDDLE],OBJ2)
                                                 )),

ifthen(   (nonvar(I_OBJ2),constraint([lex:pp3refl],I_OBJ2)),
                                                ( constraint([gender:GUP,number:NUP,index:IndexUP],I_OBJ2) ;
                                                  constraint([number:NMIDDLE,gender:GMIDDLE,index:IndexMIDDLE],I_OBJ2)
                                                 )),
ifthen(   nonvar(PREPCPLT2),(
                             constraint([index:IndexUP,lex:pp3refl,constituent_structure:[head:[gender:GUP,number:NUP,index:IndexUP]]],PREPCPLT2) ;

                               constraint([index:IndexMIDDLE,lex:pp3refl,constituent_structure:[head:[number:NMIDDLE,gender:GMIDDLE,index:IndexMIDDLE]]],PREPCPLT2)
                              )
      ),

% prep_cplt has a more complex buildup, we have to enter into the Head belonging to the C_str (Constituent Structure)

ifthen(   nonvar(PREPADJ12),(
                             constraint([index:IndexUP,lex:pp3refl,constituent_structure:[head:[gender:GUP,number:NUP,index:IndexUP]]],PREPADJ12) ;

                               constraint([index:IndexMIDDLE,lex:pp3refl,constituent_structure:[head:[number:NMIDDLE,gender:GMIDDLE,index:IndexMIDDLE]]],PREPADJ12)
                              )
      ),

ifthen(   nonvar(PREPADJ22),(
                             constraint([index:IndexUP,lex:pp3refl,constituent_structure:[head:[gender:GUP,number:NUP,index:IndexUP]]],PREPADJ22) ;

                               constraint([index:IndexMIDDLE,lex:pp3refl,constituent_structure:[head:[number:NMIDDLE,gender:GMIDDLE,index:IndexMIDDLE]]],PREPADJ22)
                              )
      ),
ifthen(   nonvar(PREPADJ32),(
                             constraint([index:IndexUP,lex:pp3refl,constituent_structure:[head:[gender:GUP,number:NUP,index:IndexUP]]],PREPADJ32) ;

                               constraint([index:IndexMIDDLE,lex:pp3refl,constituent_structure:[head:[number:NMIDDLE,gender:GMIDDLE,index:IndexMIDDLE]]],PREPADJ32)
                              )
      ),



ifthen(    (nonvar(OBJ2),constraint([cat:pred],OBJ2)),
                                                (constraint([constituent_structure:CSTROBJ2],OBJ2),
                                                 constraint([subject:SUBJ3],CSTROBJ2))),

ifthen( (nonvar(CSTROBJ2),constraint([object:OBJ3],CSTROBJ2)),true),
ifthen( (nonvar(CSTROBJ2),constraint([i_object:I_OBJ3],CSTROBJ2)), true),
ifthen( (nonvar(CSTROBJ2),constraint([prep_cplt:PREPCPLT3],CSTROBJ2)), true),




% TWO DOWN

ifthen(   (nonvar(SUBJ3), constraint([lex:pp3refl],SUBJ3)),
                                                 (constraint([gender:GMIDDLE,number:NMIDDLE,index:IndexMIDDLE],SUBJ3);
                                                  constraint([number:NUP,gender:GUP,index:IndexUP],SUBJ3)
                                                  )),


ifthen(    (nonvar(SUBJ3), constraint([cat:np,lex:Lex3,sem:[hum]],SUBJ3),Lex3 \= pp3refl),
                                                  constraint([number:NDOWN,gender:GDOWN,index:IndexDOWN],SUBJ3)),

ifthen(   (nonvar(OBJ3),constraint([lex:pp3refl],OBJ3)),
                                                  (
                                                  constraint([gender:GMIDDLE,number:NMIDDLE,index:IndexMIDDLE],OBJ3);
                                                  constraint([number:NUP,gender:GUP,index:IndexUP],OBJ3) ;
                                                  constraint([number:NDOWN,gender:GDOWN,index:IndexDOWN],OBJ3)
                                                 )),
ifthen(   (nonvar(I_OBJ3),constraint([lex:pp3refl],I_OBJ3)),
                                                  (
                                                  constraint([gender:GMIDDLE,number:NMIDDLE,index:IndexMIDDLE],I_OBJ3);
                                                  constraint([number:NUP,gender:GUP,index:IndexUP],I_OBJ3) ;
                                                  constraint([number:NDOWN,gender:GDOWN,index:IndexDOWN],I_OBJ3)
                                                 )),
ifthen(   (nonvar(PREPCPLT3),constraint([lex:pp3refl,constituent_structure:[head:CSTRPP3]],PREPCPLT3)),
                                                  (
                                                  constraint([gender:GMIDDLE,number:NMIDDLE,index:IndexMIDDLE],CSTRPP3);
                                                 constraint([number:NUP,gender:GUP,index:IndexUP],CSTRPP3) ;
                                                  constraint([number:NDOWN,gender:GDOWN,index:IndexDOWN],CSTRPP3)
                                                 )),

% THREE DOWN


ifthen(    (nonvar(OBJ3),constraint([cat:pred],OBJ3)),
                                                (constraint([constituent_structure:CSTROBJ3],OBJ3),
                                                 constraint([subject:SUBJ4],CSTROBJ3))),

ifthen( (nonvar(CSTROBJ3),constraint([object:OBJ4],CSTROBJ3)),true),
ifthen( (nonvar(CSTROBJ3),constraint([i_object:I_OBJ4],CSTROBJ3)), true),
ifthen( (nonvar(CSTROBJ3),constraint([prep_cplt:PREPCPLT4],CSTROBJ3)), true),

ifthen(   (nonvar(SUBJ4), constraint([lex:pp3refl],SUBJ4)),
                                                 ( constraint([number:NMIDDLE,gender:GMIDDLE,index:IndexMIDDLE],SUBJ4) ;
                                                  constraint([gender:GUP,number:NUP,index:IndexUP],SUBJ4);
                                                  constraint([number:NDOWN,gender:GDOWN,index:IndexDOWN],SUBJ4)
                                                 )),

ifthen(   (nonvar(OBJ4),constraint([lex:pp3refl],OBJ4)),
                                                  ( constraint([number:NMIDDLE,gender:GMIDDLE,index:IndexMIDDLE],OBJ4) ;
                                                  constraint([gender:GUP,number:NUP,index:IndexUP],OBJ4);
                                                  constraint([number:NDOWN,gender:GDOWN,index:IndexDOWN],OBJ4)
                                                 )),
ifthen(   (nonvar(I_OBJ4),constraint([lex:pp3refl],I_OBJ4)),
                                                  ( constraint([number:NMIDDLE,gender:GMIDDLE,index:IndexMIDDLE],I_OBJ4) ;
                                                  constraint([gender:GUP,number:NUP,index:IndexUP],I_OBJ4);
                                                  constraint([number:NDOWN,gender:GDOWN,index:IndexDOWN],I_OBJ4)
                                                 )),
ifthen(   (nonvar(PREPCPLT4),constraint([lex:pp3refl,constituent_structure:[head:CSTRPP4]],PREPCPLT4)),
                                                  ( constraint([number:NMIDDLE,gender:GMIDDLE,index:IndexMIDDLE],CSTRPP4) ;
                                                  constraint([gender:GUP,number:NUP,index:IndexUP],CSTRPP4);
                                                  constraint([number:NDOWN,gender:GDOWN,index:IndexDOWN],CSTRPP4)
                                                 )).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




% SUM OF THE ELEMENTS IN A LIST
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% used to sum up the distances between members of the same constituent to work out the Straining factor
% the distances are integers
% myplus is used instead of 'is' to avoid problems with potentially uninstantiated vars

sum([],0).

sum([H|T],Sum):- sum(T,Tsum),
                 myplus(Tsum,H,Sum).





















%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DEALING WITH THE PREDICATE'S ARGLIST
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% QM Quick Match (for noun and adj args)

qm(arg:Specs,arg:FSSpecs,PathlistArg,DistanceArg,w:Wtot) :-

constraint([type:T],Specs),
constraint([constraints:Cs],Specs),
mapped(T,FSSpecs),
constraint(Cs,FSSpecs),
constraint([pathlist:PathlistArg, distance:DistanceArg,w:W],FSSpecs),
myplus(W,1,Wtot).




% INSTANTIATING THE ARGS ON THE ARGLIST


% partial (subjectless) arglists
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% (scis) uincere
%%%%%%%%%%%%%%%%

% nonfinite_i(ncomplete) is the value used to indicate that a subject should NOT be looked for

match_list(Args,                             % list of args to be satisfied
            ArgTrees,           % argtrees built by the process
            PathlistArgs,         % portions of the string covered by the args
            DistanceArgs,         % straining factor discovered in args
            sujet(SConstraints),              % constraints on the subject carried over for inspection if needed
            nonfinite_i,                      % indication on the nature and completeness
                                              % of the search with respect to the subject
            gap:Gap,                          % Gap remains a variable until an arg returns a gap
                                              % (i.e. is not found in the string)
            w:W,                              % cumulated weights of the args
            Int,
            PathlistVerb) :-                  % Int remains a variable until an interrogative element
                                              % is found in one of the args
                                              % PathlistVerb is used to measure the distance separating an arg from its arg bearer
                                              % 'visum est mihi tibi scribere' : associate mihi with visum and tibi with scribere

  pick(subject:_,Args,RemainingArgs),     % subject non instantiated - left out

  match_list(RemainingArgs,ArgTrees,PathlistArgs,DistanceArgs,sujet(SConstraints),
             nonfinite,gap:Gap,w:W,Int,PathlistVerb).
             %  the remaining args are looked for as in any nonfinite clause







% (rex legit librum et) scribit epistulas
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% finite_i parallel to nonfinite_i

match_list(Args,ArgTrees,PathlistArgs,DistanceArgs,
            sujet(SConstraints), finite_i,gap:Gap,w:W,Int,PathlistVerb) :-
  pick(subject:_,Args,RemainingArgs),    % subject non instantiated - left out
  match_list(RemainingArgs,ArgTrees,PathlistArgs,DistanceArgs,sujet(SConstraints),
             finite,gap:Gap,w:W, Int,PathlistVerb).
           % like in any finite clause




% ALL ARGS
%%%%%%%%%%

% rex scribit epistulas
%

match_list([Function:Specs|RemainingArgs],
            [ArgTree|ArgTrees],
            [PathlistArg|PathlistArgs],
            [DistanceArg|DistanceArgs],
            sujet(SConstraints),
            ClauseType,
            gap:Gap,
            w:BW,
            Int,
            PathlistVerb) :-

  ClauseType \= nonfinite_i,
  ClauseType \= finite_i,

  match(Function:Specs,ArgTree,SConstraints,PathlistArg,DistanceArg,ClauseType,gap:Gap1,w:W1,Int,PathlistVerb),
                                            % match it

  match_list(RemainingArgs,ArgTrees,PathlistArgs,DistanceArgs,sujet(SConstraints),
             ClauseType,gap:Gap2,w:W,Int,PathlistVerb),
                                            % match all the other args on the arglist
  % BW is W+W1,                               % sum the weights
  myplus(W,W1,BW),
  (Gap1=[] ; Gap2=[]),                      % only one gap per clause - either in this arg or in the other args on the arglist
  ifthenelse(Gap1=[], Gap=Gap2, Gap=Gap1).  % we register the gap found -if any-  as gap for the whole clause

  % in other words:
  % we cannot have two gaps; either the first arg has no gap, or the list of the other args displays no gap
  % this does not imply that there must be a gap; both gaps may return the empty list ([]) as value



% all arguments consumed or only optional args left unsatisfied

match_list([_:Specs|RemainingArgs],
            ArgTrees,
            PathlistArgs,
            DistanceArgs,
            sujet(SConstraints),
            ClauseType,
            gap:Gap,
            w:W,
            Int,PathlistVerb) :-
    constraint([oblig:no], Specs),
    match_list(RemainingArgs,
        ArgTrees,
        PathlistArgs,
        DistanceArgs,
        sujet(SConstraints),
        ClauseType,
        gap:Gap,
        w:W,
        Int,
                                PathlistVerb).






match_list([],[],[],[0],_,_,_,w:0,_,_).






% instantiating an arg of the arglist
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% SUBJECT
%%%%%%%%%

% finite : rex reginam amat
%

% nominative case required

match(subject:Specs,
        subject:OutSpecs,
        [number:Nsubj,gender:Gendersubj,person:Psubj], % information on the subject
                    % taken over from the verb
        PathlistArg,
        DistanceArg,
        finite,              % therefore subject in the nominative case
        gap:[],
        w:W,
        Int,
        PathlistVerb ) :-

constraint([type:T],Specs),   % looking at the nature of the required filler : pred, np, etc.
T \= dummy,                   % excluding spurious subjects,
                              % sometimes needed for coherence of the system (see impersonal verbs)
constraint([constraints:Cs],Specs),  % fishing out the constraints on the arg


mapped(T,FSSpecs),         % we have the type of phrase needed for the arg

constraint([number:NumberinSpecs,gender:GenderinSpecs,case:nom,person:Psubj],FSSpecs), % retrieving gender and number from the proposed subject
                  % and requiring nominative



funify([number:Nsubj,gender:Gendersubj],[number:NumberinSpecs,gender:GenderinSpecs],[number:NNsubj,gender:GGsubj]),
                                     % for cases where gender and number are specified as OR-sets
                                     % we project the needed gender and number before registering in the OutSpecs

pick(number:NumberinSpecs,FSSpecs,FSSpecs1), pick(gender:GenderinSpecs,FSSpecs1,FSSpecs2), append([number:NNsubj,gender:GGsubj],FSSpecs2,OutSpecs),
                                     % substituting gender and number with possibly tighter constraints



ifthen(constraint([type:int],OutSpecs),Int=int),  % we register the fact that it includes an interrogative element
             % ... if it does !


                                                 % the subject constraints are imposed -
                                                 % after all this is why they were carried over from the verb group
                                                 % down to here !

constraint(Cs,OutSpecs),                          % applying all other relevant constraints
constraint([pathlist:PathlistArg, distance:DistanceArg,w:W],OutSpecs).


% summing up:
% the process consists in projecting the constraints found in the Specs for the arg onto a mapped structure FSSpecs
% to see if it can act as arg-filler





% without explicit subject : amo reginam
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% the subject is declared context-retrievable - it must be able to meet
% the constraints set by the predicate on its subject, e.g. +HUM and such like
% this is *** NOT *** a gapped subject

match(subject:Specs,
        subject:[source:context_retrievable,   % we map up the info returned as arg filler
                 number:Nsubj,                 % info carried over from the verb group
                 gender:Gendersubj,
                 person:Psubj,
                 cat:np,
               %  sem:[hum],      % not necessarily so !!!!!! more interesting to keep track of the constraints set by the predicate
                 index:i(0,0),                 % conventionally assigned to context-retrievable subjects
                 constraints_to_be_met:Cs,     % carried over from the arg specs
                 distance:[0],
                 pathlist:[],
                 case:nom,      % we are in a finite clause
                 w:W],
        [number:Nsubj,gender:Gendersubj,person:Psubj],
        [],      % no pathlist
        [0],     % no straining factor
        finite,
        gap:[], % no gap
        w:W,
        _,
        PathlistVerb) :-

% we need a penalty for introducing a contextual subject - as there may be a true one around...

ifthen(Psubj=1,constraint([gender:Gendersubj],[gender:or([masc,fem])])),
ifthen(Psubj=2,constraint([gender:Gendersubj],[gender:or([masc,fem])])),
ifthenelse(Psubj=3,W=(-3),W=0),     % the penalty applies only to third person subjects

constraint([type:np],Specs),                     % the dummy subject must assume np status (not pred)
constraint([constraints:Cs],Specs),
constraint(Cs,[number:Nsubj,gender:Gendersubj,person:Psubj,case:nom,sem:_]),
not(constraint([lex:_],Cs)).  % we cannot supply a dummy subject if a definite lex is required !!!!

% this subject is the only one to spot 'source:context_retrievable'
% the info should be used for the association of participle clauses with subjects,
% once the arg structure has been explored in finite clauses



% nonfinite : regem reginam amare
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% accusative case required ; subject cannot be left out
% contrast : scis vincere <-> scis te vicisse

match(subject:Specs,
        subject:OutSpecs,
        [number:Nsubj,gender:Gendersubj,person:Psubj],
        PathlistArg,DistanceArg,
        nonfinite,
        gap:[],
        w:W,
        Int,
        PathlistVerb) :-
constraint([type:T],Specs),
T \= dummy,
constraint([constraints:Cs],Specs),
mapped(T,FSSpecs),
constraint([number:NumberinSpecs,gender:GenderinSpecs, case:acc,person:Psubj],FSSpecs), % retrieving gender and number from the proposed subject
                      % requiring acc


funify([number:Nsubj,gender:Gendersubj],[number:NumberinSpecs,gender:GenderinSpecs],[number:NNsubj,gender:GGsubj]),
                                     % in case gender and number are specified as OR-sets in the Specs
                                     % we project the needed gender and number before registering in the OutSpecs

pick(number:NumberinSpecs,FSSpecs,FSSpecs1), pick(gender:GenderinSpecs,FSSpecs1,FSSpecs2), append([number:NNsubj,gender:GGsubj],FSSpecs2,OutSpecs),
                                     % substituting gender and number with possibly tighter constraints



ifthen(constraint([type:int],OutSpecs),Int=int),
constraint([person:Psubj,case:acc],OutSpecs),
constraint(Cs,OutSpecs),
constraint([pathlist:PathlistArg, distance:DistanceArg,w:W],OutSpecs).




% dummy subject
%%%%%%%%%%%%%%%%

% see impersonal verbs - it would be too much of a bother to work out a new procedure whereby
% subjectless clauses are OK
% WHY ?????
% example :   eos peccatorum paenitet, where neither EOS nor PECCATORUM is parsed as a subject

match(subject:Specs,_,
        [number:_,gender:_,person:_],
        [],[0],_,gap:[],w:0,_,PathlistVerb) :-
constraint([type:dummy],Specs).



% gapped subject
%%%%%%%%%%%%%%%%

% in nonfinite clauses
%%%%%%%%%%%%%%%%%%%%%%

% the subject is the local one bearing the 'down' index :
% important info for interpretation of SE in such clauses

% regem quem // dicis reginam [subject-gap] amasse (in one of the two interpretations)

match(subject:Specs,
        subject:[index:Indexdown], % we register the index in the arg tree for the subject
        [number:Nsubj,gender:Gendersubj,person:Psubj],
      % index sharing
        [],
        [0],
        nonfinite, % important !!!
        gap:[gap:[type:Type,
                  index:Indexdown,  % index sharing via the gap structure
                  function:subject,
                  subject:[e:Indexdown],
                  constraints:GapConstraints]], % the gap constraints result from appending the constraints
                                                % found in the arg specs and the local constraints on the subject
                                                % derived from the verbal group
         w:1,
         _,
         PathlistVerb) :-  % the Int won't be instantiated here

 mapped(flags,active(gap)),   % we only look for gaps if there are structures to receive them (as in relative clauses)
 constraint([type:Type,constraints:Constraints],Specs),
 Type \= dummy,
 append([number:Nsubj,gender:Gendersubj,person:Psubj,case:acc,index:Indexdown],Constraints,GapConstraints).

% in finite clauses
%%%%%%%%%%%%%%%%%%%

% the subject is the one of the potentially main clause, i.e. the 'up' one
% rex qui // [subject-gap] amat reginam


match(subject:Specs,
        subject:[index:Indexup],
        [number:Nsubj,gender:Gendersubj,person:Psubj],
        [],
        [0],
        finite, % important !
        gap:[gap:[type:Type,
                  index:Indexup,
                  function:subject,
                  subject:[e:Indexup],
                  constraints:GapConstraints]],
        w:1,
        _,
        PathlistVerb) :-

 constraint([type:Type,constraints:Constraints],Specs),
 Type \= dummy,
 append([number:Nsubj,gender:Gendersubj,person:Psubj,case:nom,index:Indexup],Constraints,GapConstraints).

% remember that a gapped constituent simply puts its constraints in the Gap feature,
% to be satisfied when the pred is connected to the antecedent



%%%%%%%%%%%%%%%%%%%
% OTHER ARGS
%%%%%%%%%%%%%%%%%%%


% same treatment for a nber of args
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

match(To_be_matched:Specs,To_be_matched:OutSpecs,[number:Nsubj,gender:Gendersubj,person:_],
                          PathlistArg,DistanceArg,ClauseType,gap:[],w:Wout,Int,PathlistVerb) :-

      (To_be_matched=object;
      To_be_matched=agent;

      To_be_matched=i_object; % standard indirect objects

      To_be_matched=object_i; % indirect objects in the accusative case
                              % in double-accusative constructions (doceo-type verbs)

      To_be_matched=cplt; % this arg type is used when we do not really care to further specify the nature of the argument
                          % used with specific predicates such as some of the impersonal
      To_be_matched=adjunct;
      To_be_matched=arg   % a rather indefinite filler, such as cplt above
                          % perhaps we do not need both, but perhaps they can be used to reflect a distinction still to be decided on

      ),

constraint([type:T],Specs),
constraint([constraints:Cs],Specs),
mapped(T,FSSpecs),
ifthen(constraint([type:int],FSSpecs),Int=int),
constraint(Cs,FSSpecs),
ifthenelse(T = phrase,                                                 % IF
           (DistanceArgN=[0],OutSpecs=[]),                              % THEN discard phrase-related info, it gets included in the Lex
           (constraint([distance:DistanceArg],FSSpecs), Out=FSSpecs) % ELSE keep the info
          ),
constraint([pathlist:PathlistArg, w:W],Out),
distancev(PathlistVerb, PathlistArg,DistanceVA),  % DistanceVA should be recorded somewhere
                                                  % Aestivo serves ubi piscem tempore quaeris?
                                                  % 'serves' is separated from 'piscem'

                                                  % the relevant observation is not distance, however, but the nature of the inserted element
						  % 'ubi' is a whole constituent, not just any old word

myplus(W,1,Wtot),
Wout is Wtot-(DistanceVA/100),         % distance from arg bearer decreases weight assigned to arg (very slightly...)


binding_se(ClauseType,Nsubj,Gendersubj,Out,OutSpecs).



% if the string boasts a member of the SE family we have to undo the constraints on nber, gender and index set locally, i.e. when
% the clause was built : regina laudat se (SE is made to reflect nber and gender of regina)
% rex veretur ne regina se laudet (rex must be able to bind SE, gender masc and not fem, so we leave gender and number and index to be set when SE is bound


% we need an OR clause, so that NewHead remains unbound until the SE is met, and then stays bound
%

binding_se(finite,_,_,Out,OutSpecs):-

member(constituent_structure:CS,Out),
pick(constituent_structure:CS,Out,Out1),

% OBJECT
ifthenelse(mapped(flags,active(se)),  % IF1 CONDITION

                                    ifthenelse(member(object:ObjectIn,CS),   % IF 2 CONDITION  IF 1 THEN ACTION


                                                       (
                                                     pick(object:ObjectIn,CS,H1), % IF 2 ACTION
                                                             ifthenelse(  ( member(lex:pp3refl,ObjectIn) ,   % IF 3 CONDITION
                                                                            member(emphasis:no,ObjectIn)
                                                                           ),
 
                                                                                    ( pick(gender:GIN,ObjectIn, OI1),   % THEN 3 ACTION
                                                                                      pick(number:NIN,OI1,OI2),
                                                                                      pick(index:IndexIN,OI2,OI3),
                                                                                     append([gender:Gender,number:Number, index:Index],OI3,ObjectOut),
                                                                                      append(H1,[object:ObjectOut],NewCS),
                                                                                      append(Out1,[constituent_structure:NewCS],OutSpecs)),
                                                                                                                                                          
                                                                          OutSpecs=Out)),    % ELSE 3


                                         OutSpecs=Out), % ELSE 2

             OutSpecs=Out).  % ELSE 1



% same need to be done for other args within the clause, CS, developing new ifthenelse to be Or'ed to the one above for the object, cf what
% we find in top se-binding:

/* ifthen(   constraint([object:OBJ1],Parse), true),
ifthen(   constraint([i_object:I_OBJ1],Parse), true),
ifthen(   constraint([prep_cplt:PREPCPLT1],Parse), true),
ifthen(   constraint([prep_phrase_adjunct_1:PREPADJ1],Parse), true),
ifthen(   constraint([prep_phrase_adjunct_2:PREPADJ2],Parse), true),
ifthen(   constraint([prep_phrase_adjunct_3:PREPADJ3],Parse), true),
*/
					


binding_se(ClauseType,_,_,Out,Out) :- not(ClauseType = finite).




% PREPOSITIONAL CPLT
%%%%%%%%%%%%%%%%%%%%

match(prep_cplt:Specs,prep_cplt:Out,[number:Nsubj,gender:Gendersubj,person:_],
       PathlistArg,DistanceArg,ClauseType,gap:[],w:Wout, Int,PathlistVerb) :-
constraint([type:T],Specs),
constraint([constraints:Cs],Specs),
mapped(T,FSSpecs),
ifthen(constraint([type:int],FSSpecs),Int=int),
constraint(Cs,FSSpecs),
ifthenelse(T = phrase,                                                 % IF
           (DistanceArgN=0,OutSpecs=[]),                              % THEN discard phrase-related info, it gets included in the Lex
           (constraint([distance:DistanceArg],FSSpecs), Out=FSSpecs) % ELSE keep the info
          ),
constraint([pathlist:PathlistArg, w:W],Out),
distancev(PathlistVerb, PathlistArg,DistanceVA),
myplus(W,1,Wtot),
Wout is Wtot-(DistanceVA/100).



%
% add to the above match clause:
% binding_se(ClauseType,Nsubj,Gendersubj,Specs,Out).

                                                % BINDING SE MORE COMPLEX ON PREP CPLT
						%  pick(prep_cplt:PPIn,Head,H1),
						%  pick(constituent_structure:CSTR,PPIn, PP1),
						%  pick(head:CHead,CSTR,SSTR1),
						% ifthenelse to be carried out on CHead





% INT ADVERB 1
%%%%%%%%%%%%%%

% we regard an interrogative adverb (e.g. unde or quo) as being able to fill in this type of gap
% even without being a prepositional phrase
% but the semantics must fit

match(prep_cplt:Specs,prep_cplt:OutSpecs,[number:_,gender:_,person:_],
       PathlistArg,DistanceArg,_,gap:[],w:Wout,Int,PathlistVerb) :-
constraint([type:T],Specs),
constraint([constraints:Cs],Specs),

mapped(advp,FSSpecs),
constraint([type:int],FSSpecs),
Int=int,

 ( (constraint([case:acc],Cs),
    constraint([sem:direction_to],FSSpecs) ),                     % QUO
   (constraint([case:abl, prep:or([ex,ab])],Cs),
    constraint([sem:direction_from],FSSpecs) )                     % UNDE
 ),

% this OR construct is to be completed, but is restricted to ARGS, not clausal expansions


ifthenelse(T = phrase,                                                 % IF
           (DistanceArgN=0,OutSpecs=[]),                              % THEN discard phrase-related info, it gets included in the Lex
           (constraint([distance:DistanceArg],FSSpecs), OutSpecs=FSSpecs) % ELSE keep the info
          ),
constraint([pathlist:PathlistArg, w:W],FSSpecs),

distancev(PathlistVerb, PathlistArg,DistanceVA),

Wout is W-(DistanceVA/100).




% INT ADVERB 2
%%%%%%%%%%%%%%

match(cplt:Specs,cplt:OutSpecs,[number:_,gender:_,person:_],
       PathlistArg,DistanceArg,_,gap:[],w:Wout, Int,PathlistVerb) :-
constraint([type:T],Specs),
constraint([constraints:Cs],Specs),

mapped(advp,FSSpecs),
constraint([type:int],FSSpecs),
Int=int,

constraint([case:abl,sem:loc],Cs),
constraint([sem:position],FSSpecs),  % e.g. UBI

% to be extended as need arises


ifthenelse(T = phrase,                                                 % IF
           (DistanceArgN=0,OutSpecs=[]),                              % THEN discard phrase-related info, it gets included in the Lex
           (constraint([distance:DistanceArg],FSSpecs), OutSpecs=FSSpecs) % ELSE keep the info
          ),

constraint([pathlist:PathlistArg, w:W],FSSpecs),
distancev(PathlistVerb, PathlistArg,DistanceVA),

Wout is W-(DistanceVA/100).





% OBJECT CPLT ('attribut de l'objet direct')
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

match(object_cplt:Specs,object_cplt:OutSpecs,[number:_,gender:_,person:_],
        PathlistArg,DistanceArg,_,gap:[],w:W, Int,PathlistVerb) :-
constraint([type:T],Specs),
constraint([constraints:Cs],Specs),
mapped(T,FSSpecs),
ifthen(constraint([type:int],FSSpecs),Int=int),
constraint(Cs,FSSpecs),
constraint([pathlist:PathlistArg,w:Warg],FSSpecs),
ifthenelse(T=adjp,W is Warg-1,W=Warg),      % penalty : the adjective is better read as part of the object NP
ifthenelse(T = phrase,                                                 % IF
           (DistanceArg=[0],OutSpecs=[]),                              % THEN discard phrase-related info, it gets included in the Lex
           (constraint([distance:DistanceArg],FSSpecs), OutSpecs=FSSpecs) % ELSE keep the info
          ),
  % distancev(PathlistVerb, PathlistArg,DistanceVA),
  % myplus(DistanceArg,DistanceVA,DistanceArgN),
ifthen(T=adjp, (constraint([type:Type],FSSpecs),not(member(Type,[poss,tool])))),
ifthen(T=np,constraint([class:common],FSSpecs)).  % e.g. appellare Murenam saltatorem  -
                                                        % Murenam is object, saltatorem (a common noun) object_cplt



% PREDICATIVE (after 'sum')
%%%%%%%%%%%%%%%%%%%%%%%%%%%

match(predicative:Specs,predicative:OutSpecs,[number:Nsubj,gender:Gendersubj,person:_],
       PathlistArg,DistanceArg,_,gap:[],w:Wout, Int,PathlistVerb) :-

constraint([type:T],Specs),
constraint([constraints:Cs],Specs),
mapped(T,FSSpecs),
ifthenelse(constraint([type:int],FSSpecs),(Int=int, Bonus=3),Bonus=0), % needs heavy bonus for interrogatives, as they must compete
                                                                       % with (often spurious) relatives with dummy antecedents !!!!!
constraint(Cs,FSSpecs),
ifthenelse(T = phrase,                                                 % IF
           (DistanceArg=[0],OutSpecs=[]),                              % THEN discard phrase-related info, it gets included in the Lex
           (constraint([distance:DistanceArg],FSSpecs), OutSpecs=FSSpecs) % ELSE keep the info
          ),

ifthen((T=np;T=adjp;T=gerundive), constraint([number:Nsubj, case:or([nom,acc])],FSSpecs)),
               % Number constraint on predicatives seems jusfifiable; case is restricted to nominative and accusative
ifthen((T=adjp;T=gerundive), constraint([gender:Gendersubj],FSSpecs)),
               % Gender constraint on predicative adjectives seems jusfifiable

constraint([pathlist:PathlistArg,w:W],FSSpecs),
distancev(PathlistVerb, PathlistArg,DistanceVA),

% Wtot is W+Bonus
myplus(W,1,Wt),
myplus(Wt,Bonus,Wtot),

Wout is Wtot-(DistanceVA/100).




% GAPPED non-subject arg
%%%%%%%%%%%%%%%%%%%%%%%%%

% here a variable IS used for the function to be filled (a non-subject one, subjects being dealt with on their own)

match(F:Specs,F:[e:I],[number:_,gender:_,person:_],
           [],[0],
            _,
            gap: [gap:[type:Type,
                       index:I,
                       function:F,
                       F:[e:I],
                       constraints:Constraints]],
            w:1,
            _,PathlistVerb) :-
mapped(flags,active(gap)),
F \= subject,
constraint([type:Type,constraints:Constraints],Specs).













% CLAUSAL CONSTRAINTS
%%%%%%%%%%%%%%%%%%%%%

clause_constraints([Constraint|OtherC],ClauseF) :-
   constraint(Constraint,ClauseF),
   clause_constraints(OtherC,ClauseF).

clause_constraints([],_).
% out of recursion

% these Constraints are treated like any other
% i.e. by being imposed on the clause features

% they are meant to cover restrictions on the whole clause,
% e.g. concerning polarity (e.g. non pili/haud magni facere)














% EXTRA KERNEL STRUCTURES
%%%%%%%%%%%%%%%%%%%%%%%%%%

% mainly adjuncts and adjunct-like elements, not taking part in arg structure
% note that we assign a lighter weight to extra-kernel structures
% (the weight value (w:Wtot) gets reduced when the results of expandkernel are used)
% precisely on account of the fact that
% they do NOT take part in arg saturation

% the 'Nat' variable is always instantiated to 'full' or 'partial'
% in the present implementation it is immaterial
% all expansions being of the 'full' type


expandkernel([Function:Specs|Others], [Function:FSpecs|OtherTrees],
             [PathFirst|OtherPaths],[DistFirst|OtherDists],
             w:WT, Int, _Nat) :-                                % the Int arg remains an uninstantiated variable
                        % unless an interrogative element is spotted in the
                  % specifications of the filler
constraint([type:Type,constraints:Constraints], Specs),
mapped(Type,FSpecs),
ifthen(constraint([type:int],FSpecs),Int=int),
constraint(Constraints,FSpecs),
constraint([pathlist:PathFirst, distance:DistFirst,w:Weight], FSpecs),
expandkernel(Others,OtherTrees,OtherPaths, OtherDists,w:Ws, Int, _Nat),
myplus(Weight,Ws,WT).

% all are optional
expandkernel([_:_|Others], OtherTrees,OtherPaths,OtherDists,w:W, Int, _Nat) :-
expandkernel(Others,OtherTrees,OtherPaths, OtherDists,w:W, Int, _Nat).

% out of recursion
expandkernel([],[],[],[0],w:0, _, _).






% EXPAND (weights are standard - EXPAND is used with adj and noun arg-bearers)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% we do not expect these elements to instantiate an INT variable
% i.e. they are not supposed to contain interrogative elements
% * ars quae cognoscendi , * cupidus quae legendi / * quarum legendarum

expand([Function:Specs|Others],
       [Function:FSpecs|OtherTrees],
       [PathFirst|OtherPaths],
       [DistFirst|OtherDists],
       w:Wtot) :-
constraint([type:Type,constraints:Constraints], Specs),
mapped(Type,FSpecs),
constraint(Constraints,FSpecs),
constraint([pathlist:PathFirst, distance:DistFirst, w:Weight], FSpecs),
expand(Others,OtherTrees,OtherPaths, OtherDists,w:Ws),
% Wtot is Weight+Ws.
myplus(Weight,Ws,Wtot).

% optional elements
expand([_:Specs|Others], OtherTrees,OtherPaths,OtherDists,w:W) :-
constraint([oblig:no], Specs),
expand(Others,OtherTrees,OtherPaths, OtherDists,w:W).

% out of recursion
expand([],[],[],[0],w:0).



% make_arg(Sconstraints,NewArg)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% building the agent out of the subject

% PRESENTLY NOT USED !!!!!!!!!!!!!!!!!!!!!!!!!!

make_arg(_,[type:np,oblig:no,constraints:[case:abl]]).

make_arg(Constraints,[type:pp,oblig:no,constraints:[prep:ab,sem:[hum]]]) :- constraint([sem:[hum]],Constraints).



% case_out
%%%%%%%%%%

% compatibility vocative and nominative in adj noun pairs
% case_out(Case1,Case2,CaseOut)

case_out(nom,voc,voc) :- !.   % set vocative if either the adj or the noun is parsed as a voc
case_out(voc,nom,voc) :- !.

case_out(C,C,C).              % same case in both adj and noun



% pathgen
%%%%%%%%%%

% pathgen(Path,Distance)
% relationship between head NP and gen cplt

% PRESENTLY NOT USED !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

pathgen(Path,0) :- contiguous(Path), !.

pathgen(Path,2) :- hiatus(Path).


hiatus(Path) :-
                 start(Path,Start),
                 extremity(Path,End),
                 between(Start,End,X),
                 succ(X,Y),
                 \+member(p(X,Y),Path),
                 \+mapped(noun,[from:X,to:Y|_]),
                 append([p(X,Y)],Path,NewPath),
                 msort(NewPath,NewPathSorted),
                 contiguous(NewPathSorted).
















% ab(Lex)
%%%%%%%%%

% usable as head of ablative adjunct, mostly loc, time and manner-means

ab(aestas).
ab(annus).
ab(arbitrium).
ab(ars).
ab(aurum).
ab(bellum).
ab(castra).
ab(cauus).
ab(concordia).
ab(consilium).
ab(consuetudo).
ab(copiae).
ab(culpa).
ab(cura).
% ab(desiderium). % agent in: 'Fuerunt qui accensum desiderio reginae vertisse iter crederent.' Tac Hist II 2 1
ab(dies).
ab(diligentia).
ab(discordia).
ab(dolus).
ab(domus).
ab(epistula).
ab(equus).
ab(exercitus).
ab(experimentum).
ab(fama).
ab(flumen).
ab(fuga).
ab(gladius).
ab(hiems).
ab(iter).
ab(ius).
ab(laetitia).
ab(lex).
ab(lingua).
ab(litterae).
ab(locus).
ab(lux).
ab(manus).
ab(memoria).
ab(minae).
ab(modus).
ab(mos).
ab(nauis).
ab(nomen).
ab(nox).
ab(oratio).
ab(pecunia).
ab(periculum).
ab(proelium).
ab(promissum).
ab(pugna).
ab(ratio).
ab(risus).
ab(rus).
ab(sententia).
ab(silentium).
ab(studium).
ab(tectum).
ab(telum).
ab(tempus).
ab(timor).
ab(uestis).
ab(uox).
ab(urbs).
ab(uulnus).














% prefix_derive(First,Derived).
% from Collatinus
% credits to Yves Ouvrard

prefix_derive(absp,asp).
prefix_derive(adc,acc).
prefix_derive(adf,aff).
prefix_derive(adg,agg).
prefix_derive(adl,all).
prefix_derive(adn,ann).
prefix_derive(adp,app).
prefix_derive(adq,acq).
prefix_derive(adr,arr).
prefix_derive(adst,ast).
prefix_derive(adsc,asc).
prefix_derive(adsp,asp).
prefix_derive(ads,ass).
prefix_derive(adt,att).
prefix_derive(aps,abs).
prefix_derive(conb,comb).
prefix_derive(conl,coll).
prefix_derive(conm,comm).
prefix_derive(conp,comp).
prefix_derive(conr,corr).
prefix_derive(exs,ex).
prefix_derive(inb,imb).
prefix_derive(inl,ill).
prefix_derive(inm,imm).
prefix_derive(inp,imp).
prefix_derive(inr,irr).
prefix_derive(obc,occ).
prefix_derive(obf,off).
prefix_derive(obp,opp).
prefix_derive(obt,opt).
prefix_derive(ops,obs).
prefix_derive(subc,succ).
prefix_derive(subf,suff).
prefix_derive(subg,sugg).
prefix_derive(subm,summ).
prefix_derive(subp,supp).
prefix_derive(supt,subt).















%%%%%%%%
% OUTPUT
%%%%%%%%

% the constraint specifying the absence of duplicates in the path
% makes sure a constituent is not filling in more than a slot
% ************* WITHIN A GIVEN TREE ********************************
% it does **** NOT **** entail the rejection of ambiguity by the parser !



% no duplicates - failure of call to dup predicate:
%

dup(List) :- pick(El, List, Rest),
             pick(El, Rest, _).
                                                      % if we can pick an element twice,
                                                      % there is at least one duplicate



% the output procedure involves sorting of the parses according to their ratings
% it should be attempted only once
% and ***not*** restarted through imposed failure (the usual thing for runs to do in this grammar)





[output,results] --->
[recorded(out,Lists,_),
 mapped(fin, fin(Fin)),                 % sentence extremity
 statistics(cputime,TE),
 recorded(time,TB),
 TimeUsed is TE-TB,                    % computing time used by the parsing i.e.
                                       % the difference between time now and time then (when the S was read in)
 write('cputime : '), write(TimeUsed), nl,
 write(Lists,'cputime : '), write(Lists,TimeUsed), nl(Lists),
 recorded(alltime,TotTime,RefTime),
 NTotTime is TotTime+TimeUsed,
 erase(RefTime),     % forget the old total
 recorda(alltime,NTotTime),

 ifthen(recorded(stg,[type_of_structures,1]), % full sentences only
 setof(Rating-Parse,
             Pred^Pathlist^Distance^Weight^Fin^(mapped(pred,Pred),   % existential quantification to avoid multiple results


        constraint([cat:pred,argbound:no,class:m,pathlist:Pathlist,type:finite,distance:[Distance],gap:[],w:Weight,constituent_structure:Parse],Pred),


       %     constraint([cat:pred,argbound:no,class:m,pathlist:Pathlist,distance:[Distance],gap:[],w:Weight,constituent_structure:Parse],Pred),
                                                                        % the constraints enable us to show only complete parses,
                                                                        % i.e. parses for the sentence submitted
                                                                        % that make of it an independent gapless (finite) clause
                                                                        % the 'type:finite' constraint should be lifted in parsing bits of indirect discourse



                  % The weight is computed on the basis of arg saturation
                                                                        % the Distance holds the value for the Straining factor
                                    % measuring constituent inner non-contiguity

                  % the 'type:finite' requirement is to be dropped if we have to deal
                  % with ORATIO OBLIQUA : scire se illa esse uera (Caesar)

                                              \+dup(Pathlist),  % no word used twice within a single parse !!,
                                                path(0,Fin,Pathlist), % the path must cover all the words of the sentence !



                                                ifthen( mapped(flags,active(se)), ifthen(bind_se(Pred),true)), % we have a member of the SE family
                     % we apply binding without allowing failure
                    % we are only interested in the side-effects of the binding procedure

                                                Rating is -(Weight-Distance) % sign is inversed so that the weightiest appear first
                                                                              % Rating is only meaningful in the case of ambiguity -
                                                                              % it is a preference factor
                                                 ),
              ParseList)
),

 ifthen( recorded(stg,[type_of_structures,2]), % various types of syntagms including preds
          (   (C=relative_clause;C=participle_clause;C=free_sub;C=exclamation;C=adjunct;C=gerundive;c=gdiv;C=pp;C=np;C=advp;C=adjp;C=aa;C=pred),
              setof(Rating-Parse,
                            Pred^Pathlist^Distance^Weight^Fin^(mapped(C,Pred),   % existential quantification to avoid multiple results


            constraint([pathlist:Pathlist,distance:[Distance],w:Weight,constituent_structure:Parse],Pred),
                                                                        % the constraints enable us to show only complete parses,
                                                                        % i.e. parses for the string submitted


                  % The weight is computed on the basis of arg saturation
                                                                        % the Distance holds the value for the Straining factor
                                    % measuring constituent inner non-contiguity



                                                \+dup(Pathlist),  % no word used twice within a single parse !!,
                                                path(0,Fin,Pathlist), % the path must cover all the words of the sentence !

                                            %    ifthen( mapped(flags,active(se)), ifthen(bind_se(Pred),true)), % we have a member of the SE family
                     % we apply binding without allowing failure
                    % we are only interested in the side-effects of the binding procedure

                                                Rating is -(Weight-Distance) % sign is inversed so that the weightiest appear first
                                                                              % Rating is only meaningful in the case of ambiguity -
                                                                              % it is a preference factor
                                                                   ),
              ParseList)
              )
          ),

  keysort(ParseList,Sorted),
  printthem(Sorted,user),
  printthem(Sorted,Lists),
  nl,nl(Lists)].





% no parse
printthem([],Stream) :- nl(Stream), !.

% one parse           % one : print it
printthem([H],Stream) :- nl(Stream),
                           prpr2(H,0,Stream),
                           nl(Stream), nl(Stream),
                           !.





% two parses or more - print one, two or more depending on setting (0,1,2 or 3)

printthem([H1,H2|Tail],Stream) :-   ifthen(recorded(stg,[number_of_parses,0]),

                                  (nl(Stream),    % two or more : print only the first
                                  prpr2(H1,0,Stream),
                                  nl(Stream),
                                  !)),

                                   ifthen(recorded(stg,[number_of_parses,1]),

                                  (nl(Stream),    % two or more : print them both if the second is as good as the first
                                  prpr2(H1,0,Stream),
                                  nl(Stream),
                                  ifthen((H2=R-_,H1=R-_),prpr2(H2,0,Stream)),
                                  nl(Stream),
                                  !)),

                                   ifthen(recorded(stg,[number_of_parses,2]),
                                   (nl(Stream),   % two or more : print them both
                                  prpr2(H1,0,Stream),
                                  nl(Stream),
                                  prpr2(H2,0,Stream),
                                  nl(Stream),
                                  !)),
                                    ifthen(recorded(stg,[number_of_parses,3]),
                                   (nl(Stream),   % two or more : print them all
                                  prpr2(H1,0,Stream),
                                  nl(Stream),
                                  prpr2(H2,0,Stream),
                                  nl(Stream),
                                  printthemall(Tail,Stream))).


printthemall([H|T],Stream):-      prpr2(H,0,Stream),
                                  nl(Stream),
                                  printthemall(T,Stream).


printthemall([],_).






%%%%%%%%%%%%%%%%%%%
% DEBUGGING OUTPUTS
%%%%%%%%%%%%%%%%%%%

% The example queries show how we can obtain partial structures
% which would not necessarily make it up to the top level (the whole S)

% !!!! WARNING !!!
% the single pass restriction on the output should be removed - see metainterpreter well above

% example query : getting the nps
/*
 [output,nps] --->
[mapped(np,FS),
 not(recorded(parse,parse(FS),_)),
 recorded(out,Lists,_),
 nl,nl(Lists),
 prpr(FS,0,user),
 prpr(FS,0,Lists),
 write(Lists,FS),
 nl,nl(Lists),
 recorda(parse,parse(FS),_)].
*/

% getting the clauses
% cplt clauses included even if they do not make it up to the top level of the main clause
% ***************** FOR DEBUGGING PURPOSES  ********************************

/*
 [output,preds] --->
[mapped(pred,FS),
 not(recorded(parse,parse(FS),_)),
 recorded(out,Lists,_),
 nl,nl(Lists),
 prpr(FS,0,user),                  % writing pretty-printed parses onto the screen
 write(Lists,FS),                  % writing raw parses into the file
 nl,nl(Lists),
 recorda(parse,parse(FS),_)].
*/

/*
 [output,relative_clauses] --->
[mapped(relative_clause,FS),
 not(recorded(parse,parse(FS),_)),
 recorded(out,Lists,_),
 nl,nl(Lists),
 prpr(FS,0,user),
 write(Lists,FS),
 nl,nl(Lists),
 recorda(parse,parse(FS),_)].
*/







%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% INSORT : arg insertion sort
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

insort([H:V|T],S) :-
                   insort(T,L),
                   insert(H:V,L,S).

insort([],[]) :- !.

% INSERT predicate
%%%%%%%%%%%%%%%%%%

insert(F:V,[F1:V1|T],[F1:V1|L]) :-
                 before(F1,F),
                 !,
                 insert(F:V,T,L).

insert(F:V,L,[F:V|L]).


% BEFORE predicate
%%%%%%%%%%%%%%%%%%

before(F1,F2) :-
                      assoc(F1,Rank1),
                      assoc(F2,Rank2),
                      Rank1=<Rank2.


% ASSOC predicate
%%%%%%%%%%%%%%%%%

% assigns a canonical order in the returned parse

assoc(arglist,0).
assoc(subject,1).
assoc(agent,2).
assoc(predicative,3).
assoc(object,4).
assoc(i_object,5).
assoc(object_i,5).  % acc object playing the semantic role of an indirect object
                    % i_object and object_i are not suppposed to co-occur and can therefore
        % be assigned the same place in the canonical order
assoc(object_cplt,6).
assoc(prep_cplt,7).
assoc(cplt,8).
assoc(arg,9).
assoc(adjunct,10).
assoc(adverbial_adjunct,11).
assoc(prep_phrase_adjunct_1,12).
assoc(prep_phrase_adjunct_2,13).
assoc(prep_phrase_adjunct_3,14).
assoc(dative_of_interest,15).
assoc(ablative_absolute,16).
assoc(address,17).












%%%%%%%%%%%%%%%%%%%%%%%
% Clearing the database
%%%%%%%%%%%%%%%%%%%%%%%

% all these boxes could have been filled by either lexical or grammar clauses
% they must be emptied from one sentence to the next

% any time 'map' is enriched with a new structure as arg,
% that new structure should also be the target of the 'forgetting' process

clear :-
       eraseall(time),
       eraseall(pos),
       eraseall(fin),
       eraseall(unmapped),
       eraseall(flags),
       eraseall(parse),
       eraseall(noun),
       eraseall(prpers),
       eraseall(prindef),
       eraseall(prdem),
       eraseall(print),
       eraseall(relative),
       eraseall(relative_clause),
       eraseall(prep),
       eraseall(coord),
       eraseall(sub),
       eraseall(neg),
       eraseall(part),
       eraseall(punct),
       eraseall(pp),
       eraseall(adj),
       eraseall(adv),
       eraseall(advp),
       eraseall(gdiv),
       eraseall(p_p),
       eraseall(p_pr),
       eraseall(p_f),
       eraseall(adjp),
       eraseall(gerundive),
       eraseall(participle_clause),
       eraseall(v),
       eraseall(vg),
       eraseall(vgpos),
       eraseall(np),
       eraseall(phrase),
       eraseall(adjunct),
       eraseall(free_sub),
       eraseall(apposed_sub),
       eraseall(exclamation),
       eraseall(aa),
       eraseall(address),
       eraseall(pred).

 eraseall(X) :-
     recorded(X,_,Ref),
     erase(Ref),
     fail.

 eraseall(_).












%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Detach -QUE/-UE and -CUM where appropriate - + deals with nec (--> et non)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


    % end of recursion clause:

     deque([],[]).

     % NEC
     %

     deque([nec|T],[et,non|T1]) :- !, deque(T,T1).   % toy !!!!!

     % QUE
     %

     % !!!!!!!!!!!!!!!!!!!!!!!!!!!!! TOY !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

     deque([H|T],[H|T1]) :- member(H,[absque, abusque, aeque, antique, atque,
                                      bellique,  domique, marique, % included in this parser as parts of mwu string
                                      denique,
                                      extorque,
                                      iamque, inique, itaque,ideoque,
                                      linque,
                                      militiaeque,      % cf bellique
                                      namque, neque,
                                      oblique,
                                      peraeque, plerumque, pleraque, pleraeque, plerique,
                                      quandoque, quidque, quinque, quodque,
                                      quisque, quoque, quousque,
                                      relinque,
                                      simulatque,
                                      torque,
                                      ubique, utrubique, undique,usque,utique,
                                      utrimque]),
                              !,                 % list of words where -que should remain attached
                              deque(T,T1).

     deque([H|T],[H|T1]) :- atom_concat(_,cumque,H),  % all words ending in -cumque should be left alone
                            !,
                            deque(T,T1).

     deque([H|T],[H|T1]) :- atom_concat(Rel,que,H),  % ditto for those words made up of a relative followed by -que
                            lex(Rel,relative,_),!,
                            deque(T,T1).

     deque([H|T],[H|T1]) :- atom_concat(Lex,que,H),  % ditto for uterque, utrumque and friends
                            lex(Lex,adj,FS),
                            constraint([lex:uter],FS),
                            !,
                            deque(T,T1).

     deque([H|T],[H|T1]) :- atom_concat(Lex,que,H),
          % ditto for a form of UNUS followed by a relative (or quis or quid) and then -que
                            atom_concat(First,Second,Lex),
                                    % atom_concat 'suggests' string divisions until the right one is found
                            lex(First,adj,FS1),
                            constraint([lex:unus],FS1),
                            (lex(Second,relative,_); Second=quis; Second=quid),
                            !,
                            deque(T,T1).

     deque([Word|Tail],[que,Mot|Tail1]) :-  atom_concat(Mot,que,Word), !, deque(Tail, Tail1).
                                           % all other words ending in -que
                                           % note that we put the que in front (normal place for 'et')
             % e.g. populusque -> que populus





    % UE (-ve as in prouidendumue)

     deque([H|T],[H|T1]) :- member(H,[aue, adsidue, assidue, ambigue, angue, ablue, adnuue, amoue,
                                      breue, boue,
                                      caue, ciue, coargue, constitue, calue, captiue,
                                      diue, decliue, distingue, dilue,
                                      exigue, exsangue, extingue,
                                      faue, furtiue,
                                      graue, praegraue,
                                      ignaue, ingue, ioue, intempestiue, ingenue,
                                      leue, lue,
                                      minue, metue, moue, remoue,
                                      naue, neue, niue,
                                      oue,
                                      praue, promiscue, pingue, procliue, parue, praecipue,
                                      restitue, respue,
                                      sangue, suaue, saeue, strue, statue,siue, salue, solue, strenue,
                                      tempestiue,  tenue,
                                      ungue, uiue]),

                              !,                 % list of words where -ue should remain attached
                                                 % PARTIAL (from Lasla's 'Index inverse de la langue latine')
                              deque(T,T1).


      deque([Word|Tail],[ue,Mot|Tail1]) :-  atom_concat(Mot,ue,Word), !, deque(Tail, Tail1).

                               % all other words ending in -ue
                                           % note that we put the ue in front (normal place for 'uel')
             % e.g. populusue -> ue populus


   % CUM
     %

     deque([mecum|T],[cum_p,me|T1]):- !, deque(T,T1).
     deque([tecum|T],[cum_p,te|T1]):- !, deque(T,T1).
     deque([secum|T],[cum_p,se|T1]):- !, deque(T,T1).
     deque([nobiscum|T],[cum_p,nobis|T1]):- !, deque(T,T1).
     deque([uobiscum|T],[cum_p,uobis|T1]):- !, deque(T,T1).
     deque([quocum|T],[cum_p,quo|T1]):- !, deque(T,T1).
     deque([quacum|T],[cum_p,qua|T1]):- !, deque(T,T1).
     deque([quibuscum|T],[cum_p,quibus|T1]):- !, deque(T,T1).

     % DO NOTHING
     %

     deque([H|T],[H|T1]) :- deque(T,T1).
               % the word does not end in -que or -ue, neither is it food for the other processes described here

















%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  GENERAL PROCEDURES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PRETTY PRINTER
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% DECLARATIONS

% special features are printed as lists, with each subfeature on its own line

  special(constituent_structure).
  special(head).
  special(apposition).
  special(neg).
  special(object).
  special(agent).
  special(object_cplt).
  special(prep_cplt).
  special(cplt).
  special(arg).
  special(i_object).
  special(object_i).
  special(adjunct).
  special(adverbial_adjunct).
  special(prep_phrase_adjunct_1).
  special(prep_phrase_adjunct_2).
  special(prep_phrase_adjunct_3).
  special(dative_of_interest).
  special(linker).
  special(ablative_absolute).
  special(address).
  special(predicative).
  special(noun_cplt).
  special(comp_cplt).
  special(sup_cplt).
  special(rel_clause).
  special(subject).
  special(pred).
  special(exclamation).
  special(adjp).
  special(participle_clause).
  special(subordinate_clause).
  special(lower_clause).
  special(subordinator).
  special(main_clause).
  special(higher_clause).
  special(np).
  special(pp).
  special(gdiv).
  special(vg).
  special(string).
  special(free_sub).
  special(apposed_sub).
  special(arglist).
  special(address).



% The pretty-printing process begins here:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


 % weight printed in front of parse
  prpr2(Weight-X,I,Handle):- NewWeight is -Weight,
          % reverse before printing so that the highest (=least!!!) values appear on top
                            write(Handle,NewWeight),
                            write(Handle,'--->'),
                            nl(Handle),
                            write(Handle,X),
                            nl(Handle), nl(Handle),
                            prpr(X,I,Handle).

  % pretty-printing the parse
  prpr(X,_,_) :- var(X),!.   % we drop (i.e. do not print) uninstantiated variables and empty lists
  prpr([],_,_) :- !.
  prpr([H|T],I,Handle):-
                  !,J is I+4,prpr(H,J,Handle),prprx(T,J,Handle).   % we tab away from the left margin to increase indentation
                   % and thereby reveal structure
                                                                   % we use recursive prprx to print the tail of the list
% NOT PRINTED
%%%%%%%%%%%%%

  prpr(_:Value,_,_) :- var(Value),!.
          % features with uninstantiated values are not printed; neither is a variable index within an index structure : i()

  prpr(index:i(Value),_,_) :- var(Value),!.
  prpr(index:i(Indexup,Indexdown),_,_) :- var(Indexup), var(Indexdown), !.
  prpr(index:i(Indexup,Indexdown),I,Handle) :- var(Indexup),  !, prpr(index:Indexdown,I,Handle).
  prpr(index:i(Indexup,Indexdown),I,Handle) :- var(Indexdown),  !, prpr(index:Indexup, I, Handle).

  % - ditto for empty lists
  prpr(_:[],_,_) :- !.

% We choose not to pretty-print the following features - they can be studied in the raw parse, if necessary
%             mostly for debugging purposes

  prpr(class:_,_,_) :- !.
  prpr(constraints:_,_,_) :- !.
  prpr(txt:_,_,_) :- !.
  prpr(type:_,_,_) :- !.
  prpr(function:_,_,_) :- !.
  prpr(hp:_,_,_) :- !.
  prpr(distance:[0],_,_) :- !. % we do not bother printing non-discontinuity
  prpr(gap:_,_,_) :- !.
  prpr(w:_,_,_) :- !.
  prpr(lextype:_,_,_) :- !.
  prpr(kind:_,_,_) :- !.
  prpr(ab:_,_,_) :- !.
  prpr(flagint:_,_,_) :- !.
  prpr(argbound:_,_,_) :- !.
  prpr(origin:_,_,_) :- !.
  prpr(type_pp:_,_,_) :- !.

% PRINTED AS LISTS
%%%%%%%%%%%%%%%%%%

 prpr(pathlist:List,I,Handle) :- tab(Handle,I), prprxh(List,I,Handle),!.

 prpr(Special:V,I,Handle) :- special(Special),
                              !,
                              tab(Handle,I),
                              write(Handle,Special),
                              nl(Handle),
                              prpr(V,I,Handle).

   % special cases: printed as lists, i.e. with each element on its own line
   % following a header line made up of the name of the Special element

   % the other features just get printed in the standard way, i.e. written


% STANDARD
%%%%%%%%%%

  prpr(X,I,Handle):- tab(Handle,I),write(Handle,X),nl(Handle).


% we print each feature in turn, using recursive prprx

  prprx([],_,_):- !.
  prprx(X,_,_) :- var(X),!.
  prprx([H|T],I,Handle):- prpr(H,I,Handle),prprx(T,I,Handle).



% PATHLIST  e.g.  pathlist:[p(1,2),p(2,3),p(3,4)]
%%%%%%%%%%

  prprxh([],_,Handle):- nl(Handle),!.

  prprxh([H|T],I,Handle):- prprh(H,I,Handle),prprxh(T,I,Handle).

  prprh(p(A,B),I,Handle) :- recorded(pos,position(A,B,Word)),
                            B is A+1,
                            write(Handle,'* '),
                            write(Handle,Word), write(Handle,' * ').

  prprh(p(A,C),I,Handle) :- recorded(pos,position(A,B,Word1)),
                            B is A+1,
                            C is B+1,
                            recorded(pos,position(B,C,Word2)),
                            write(Handle,'* '),
                            write(Handle,Word1), write(Handle,' * '),
                            write(Handle,Word2), write(Handle,' * ').


















%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% GETS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* String to word list converter */
%

/* first arg of getsentence is the wordlist to be obtained from the string
   second arg is either user (keyboard input) or an input stream identifier computed by the system

   third arg is a list of sentence delimiters
   fourth arg is a list of word delimiters
   fifth arg is a list of punctuation signs to be returned as quoted atoms
   last arg but one is either caps (capitals preserved) or nocaps (capitals turned into small letters)
   last arg is last char read

   a licit call is for example:
   getsentence(Sentence, user, ".!?:;" , "\'" , "" ,nocaps,EOF).

   -----Sentence delimiters----
   1. No sentence delimiter is assumed; only eof forces termination of the procedure.
      This way a whole text can be read in at one go, and there is no need for list flattening

   2. Period (.), exclamation mark (!), question mark (?),
      as well as colon (:) and semi-colon (;),
      are typically declared sentence/chunk delimiters.
   3. Sentence delimiters are all ipso facto also word delimiters


   -----Word delimiters-----
   1. All sentence delimiters are word delimiters.
   2. Space and eol are word delimiters by right and should not be declared as such.
   3. Comma (,), quotes ("), parentheses ('()'), brackets ([]), curly brackets ({}) and slash (/)
      are typically declared as word delimiters


   -----Returned punctuation signs-----
   Only the punctuation signs specified in the fifth arg are returned as quoted atoms.
   They can be any of the following:

   Period           .
   Exclamation mark         !
   Question mark          ?
   Colon            :
   Semi-colon           ;
   Apostrophe           '
   Hyphen           -
   Comma            ,
   Quotes           "
   Parenthesis            ()
   Brackets           []
   Curly brackets         {}
   Slash            /


   Note that all the char lists are presented as double-quoted strings, i.e. Prolog sees them
   as CHARACTER CODES - they need to be converted to real chars before being returned
   this gets done with char_code(Char,Code)

*/


% THE WORDS ARE REPRESENTED AS ATOMS
%
% they should be written with writeq
% when the results are printed to file
%

getsentence(Wordlist,Handle,EndP) :-  % use defaults
                                 getsentence(Wordlist,Handle,
                                             ".!?",
                                             ".!?,;:/[]{}()",
                                             "",
                                             nocaps,EndP).


getsentence(Wordlist,Handle,SD, WD, PS,CapsFlag,EndP) :- get_code(Handle,Char),
                                                         getrest(Handle,Char,Wordlist,SD, WD, PS,CapsFlag,EndP).


/* we read a character in from the stream pointed to by Handle (get_code)
   we use this character as look-ahead character
   the end result is the Wordlist corresponding to the input string

   we return the atom end_of_file if we hit the eof character (-1)
   else we leave EOF as a variable (its status should therefore be examined using ==
   (!! not simple = !!)

   Example call:

     getthem(HandleIn,[Sentence|Var]) :-
                                         getsentence(Sentence,
                                                     HandleIn,
                                                     "",
                                                     ".?!\',\"-;:/{}()[]",
                                                     ".?!",
                                                     nocaps,
                                                     EOF),
                                         (EOF == end_of_file -> Var = []; getthem(HandleIn,Var)).



   Alternatively we can also use stop. as last line of a file,
   and interrupt the repeated getsentence process
   as soon as [stop,'.']  or [stop]  is returned, depending on whether
   period (.) is a returned puctuation sign */

%

/* end of sentence markers */
%---------------------------

/* eof 1 */
getrest(_,-1,[],_,_,_,_,end_of_file):- !.

getrest(_,47,[],_,_,_,_,slash):- !.
getrest(_,46,[],_,_,_,_,other):- !.
getrest(_,63,[],_,_,_,_,other):- !.


/* ! 2   */
getrest(_,Code,[Char],SD,_,PS,_,_):- memberchk(Code,SD),
                                                    memberchk(Code,PS),
                                                    char_code(Char,Code),!.

% we return the Char only if its code is a member of the PS list,
% the list of punctuation signs to be returned


getrest(_,Code,[],SD,_,_,_,_):-     memberchk(Code,SD),!.

% we have a sentence delimiter, but we do not return it in the word list


/* at the end of the sentence getrest simply succeeds,
   and thereby puts an end to the getsentence process too  */


/* end of word markers */
%-------------------------

/* eol       1 */
getrest(Handle,10,Wordlist,SD,WD,PS,CapsFlag,EOF) :-
              !,getsentence(Wordlist,Handle,SD,WD,PS,CapsFlag,EOF).

/* space     2 */
getrest(Handle,32,Wordlist,SD,WD,PS,CapsFlag,EOF) :-
             !,getsentence(Wordlist,Handle,SD,WD,PS,CapsFlag,EOF).

/* EOL and SPACE are always word end markers, and are never returned */


getrest(Handle,Code,[Char|Wordlist],SD,WD,PS,CapsFlag,EOF) :-   memberchk(Code,WD),
                                                                memberchk(Code,PS),
                                                                char_code(Char,Code),
                                                                !,
                                                                getsentence(Wordlist,Handle,SD,WD,PS,CapsFlag,EOF).

/* we return the word end marker */


getrest(Handle,Code,Wordlist,SD,WD,PS,CapsFlag,EOF) :-          memberchk(Code,WD),!,
                                                                getsentence(Wordlist,Handle,SD,WD,PS,CapsFlag,EOF).

/* we do not return the word end marker */


/* in these clauses getrest has come to the end of a word, and relaunches getsentence with the
   current Wordlist, to which new words will be added */

%

getrest(Handle,Letter,[Word|Wordlist],SD,WD,PS,CapsFlag,EOF):-
             getletters(Handle,Letter,Letters,Nextchar,SD,WD,CapsFlag,EOF),
             name(Word,Letters),
             getrest(Handle,Nextchar,Wordlist,SD,WD,PS,CapsFlag,EOF).

/* getletters collects the letters of a word in a list 'Letters'
   then the predicate name turns the list of letters into an atom representing the word read
   the word is put in front of the resulting wordlist
   getrest should get the other words of the string */



getletters(_,-1,[],-1,_,_,_,end_of_file):- !.    /* eof                             1 */
getletters(_,10,[],10,_,_,_,_):-!.                  /* eol                              2 */
getletters(_,32,[],32,_,_,_,_):-!.      /* space                            3 */

getletters(_,Code,[],Code,_,WD,_,_):- memberchk(Code,WD),!.
getletters(_,Code,[],Code,SD,_,_,_):- memberchk(Code,SD),!.


/* when we hit a word delimiter we get out of getletters, adding nothing to the letter list */

%

getletters(Handle,Let,Word,Nextchar,SD,WD,CapsFlag,EOF):-
                 transform(Let,LetList,CapsFlag),!,
                 get_code(Handle,Char),
                 getletters(Handle,Char,Letters,Nextchar,SD,WD,CapsFlag,EOF),
                 append(LetList,Letters,Word).

/* we examine the lookahead letter 'Let'
   we see if we need to keep it
   we keep it only if we can transform it

   transform operates vacuously on most chars
   but if CapsFlag is set to nocaps it turns capital letters into small letters
   transform fails for elements below ascii 32, i.e. non-printing chars

   we then add the resulting letter to the front of the letter list
   we get a fresh char with get_code
   we use this char as look-ahead char for the next getletters goal

   note that transform returns a list of letters, not a single char
   this enables us to transform linked oe into o+e at this stage */

%

getletters(Handle,_,Letters,Nextchar,SD,WD,CapsFlag,EOF):-
                 get_code(Handle,Char),
                 getletters(Handle,Char,Letters,Nextchar,SD,WD,CapsFlag,EOF).

/* here the 'letter' carried in Let is simply dropped */


/* checking and transforming the char */
%---------------------------------------

transform(118,[117],nocaps) :- !.  % v->u
transform(86,[85],caps) :- !.  % V->U
transform(86,[117],nocaps) :- !.  % V->u

transform(C,[C1],nocaps):-C>64,C<91,!,C1 is C+32.
transform(C,[C1],nocaps):-C>191,C<222,!,C1 is C+32.

% nocaps : small letters are capital letters+32;
% e.g. A is 65 and a is 97(=65+32)


transform(C,[C],_) :- C>31, C<513.

% chucks non-printing and widely foreign ;-) chars out
% C gets transformed into itself, i.e. kept,
% only if its ascii number is above 31 and below 513


















%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% FEATURE LIST UNIFICATION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
% feature unification in verification mode : CONSTRAINT
%

% adapted from GAL et al. "Prolog for NLP" Appendix 12 p.244-245

/* note that 'constraint' should not be called
   with a feature whose ***name*** is uninstantiated:

  !!!! constraint([Featurename:Featurevalue,sem:[thing]],
       [sem:[hum], gender:masc,Cat:N])

will succeed, instantiating Featurename to sem, and Featurevalue to [hum];
sem:[thing] will match Cat:N, and the whole predicate call will succeed,
in spite of the incompatibility between the two sem features */

% constraint : 2 args
%

constraint(F,F1) :- nonvar(F1),F=F1,!.
% identity of the two sets to be unified

constraint([],_) :-!.
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
% see below definition of SSET



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
%

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
%

constraint([F:Set1|Tail1],[F:Set2|Second]) :-
not(atomic(Set1)),
not(atomic(Set2)),
constraint(Set1,Set2),
constraint(Tail1,Second),!.

% set-valued features : we unify the values with constraint

constraint([F:V|_],[F:V2|_]) :-
   V\=V2,                        % test  not necessary because of all the cuts here above but included for the sake of logic
   !,
   fail.

% we have the same feature name, but a different value: we have to fail...


constraint([F1:V1|Tail1],[F2:V2|Tail2]) :-
   constraint([F1:V1],Tail2),
   constraint(Tail1,[F2:V2|Tail2]),!.

%
% in construction mode : FUNIFY : 3 args
%

% FUNIFY IS ARITY-3 CONSTRAINT !!!

% in construction mode we are building in arg3 the result of joining the conditions set by arg1 and arg2
% construction mode is used only in very specific cases, where we must wait before we can check the conditions set
% in the first two args
% whenever possible, we use constraint with arity 2

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
%

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
%

funify([F:Set1|Tail1],[F:Set2|Second],[F:ResF|NewTail]) :-
not(atomic(Set1)),
not(atomic(Set2)),
funify(Set1,Set2,ResF),
funify(Tail1,Second,NewTail),!.

% set-valued features : we unify the values with funify

funify([F:V|_],[F:V2|_],_) :-
   V \=V2,
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

%

exclude([H|T],V,T1) :- \+ member(H,V),!, exclude(T,V,T1).
exclude([H|T],V,T1) :- pick(H,V,V1), exclude(T,V1,T1).
exclude([],X,X).

% exclude eliminates a certain set from a Set
% case of : funify([one:except([a,b]), two:b],[one:or([b,c,a,d]), two:b],S).
% S = [one:or([c,d]), two:b]

%
% pick : non-deterministic selection in a list
%

% often called 'select'

pick(A,[A|B],B).
pick(A,[B|C],[B|D]) :- pick(A,C,D).




%%%  Semantic Features  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% we extend set membership to include inheritance

% the left semantic set expresses the restriction,
% the right semantic set the values the restriction is matched against

% predicates project restrictions on args
% adjectives project restrictions on nouns
% adverbs project restrictions on the phrase or clause they are housed in


sset([First|Others],Set) :- sfok(First,Set),
                            sset(Others,Set).
sset([],_).



sfok(Sem,Semlist) :- inlist(Sem,Semlist),
                     !.

sfok(Sem,Semlist) :- subclass(Hypo,Sem),
                     inlist(Hypo,Semlist),
                     !.

% we have sth more specific than what we are looking for
% we are looking for an animal and we've got a poodle
% recall that the left arg houses sem restrictions
% the right arg the sem potential of the object meant to satisfy these restrictions


inlist(Sem,[Sem|_]) :-       !.
inlist(Sem,[_|Remainder]) :- inlist(Sem,Remainder).


subclass(X,Y) :- ako(X,Y).
subclass(X,Y) :- ako(X,Z) ,
                 subclass(Z,Y).




% HIERARCHY
%%%%%%%%%%%%%


% TOP

ako(concrete,top).
ako(loc,top).
ako(time,top).
ako(abstract,top).



% CONCRETE

ako(alive,concrete).
ako(inert,concrete).



% LOC

ako(city,loc).
ako(country,loc).



% ABSTRACT

ako(quality,abstract).
ako(institution,abstract).



% ALIVE

ako(hum,alive).
ako(animal,alive).
ako(vegetal,alive).



% INERT

ako(thing,inert).
ako(artefact,inert).


% HUM

ako(male,hum).
ako(female,hum).



% ARTEFACT

ako(weapon,artefact).







%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SETS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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


% pushing an item on a list

push(Item,List,[Item|List]).











%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TEST GENERATION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% used only for special purposes

% generating strings to test parser robustness with respect to word order

% e.g. a call such as  genstring([dionysium, velim, salvere, iubeas]).
% (Marcus Tullius Cicero, Epistulae ad Atticum 4.15.10)
% will generate 4! (1x2x3x4=24) strings for the parser to try to parse, namely

/*
dionysium iubeas salvere velim .
dionysium iubeas velim salvere .
dionysium salvere iubeas velim .
dionysium salvere velim iubeas .
dionysium velim iubeas salvere .
dionysium velim salvere iubeas .
iubeas dionysium salvere velim .
iubeas dionysium velim salvere .
iubeas salvere dionysium velim .
iubeas salvere velim dionysium .
iubeas velim dionysium salvere .
iubeas velim salvere dionysium .
salvere dionysium iubeas velim .
salvere dionysium velim iubeas .
salvere iubeas dionysium velim .
salvere iubeas velim dionysium .
salvere velim dionysium iubeas .
salvere velim iubeas dionysium .
velim dionysium iubeas salvere .
velim dionysium salvere iubeas .
velim iubeas dionysium salvere .
velim iubeas salvere dionysium .
velim salvere dionysium iubeas .
velim salvere iubeas dionysium .
stop.
*/


genstring(List) :-
   protocola(strings),
   setof(Perm,
         permutation(List,Perm),     % permutation belongs to the list library
         Perms),
   writel(Perms),
   write('stop.'),
   nl,
   noprotocol.

writel([]).
writel([A|B]) :- writelist(A), writel(B).
writelist([]) :- write('.'),nl.
writelist([A|B]) :- write(A), tab(1), writelist(B).


perm([],[]).

perm(L,[El|LP]) :-
     sel(El,L,L1),
     perm(L1,LP).

sel(A,[A|B],B).
sel(A,[B|C],[B|D]) :- sel(A,C,D).

shuffle(L, [], L).

shuffle(L, [E|Rrest], S) :-
  append(L1, Lrest, L),
  shuffle(Lrest, Rrest, Srest),
  append(L1, [E|Srest], S).

/*
shuffle(A, [], A) :- !.
shuffle([], B, B) :- !.
shuffle([A|RestA], B, [A|Shuffled]) :-
shuffle(RestA, B, Shuffled).
shuffle(A, [B|RestB], [B|Shuffled]) :-
shuffle(A, RestB, Shuffled).

*/









%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PATTERN MATCHER
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Playground for the matcher:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

play :-          nl,
                 nl,
                 write('Key in your sentence or stop. to quit'),
                 nl,
                 getsentence(Sentence,     % list of words corresponding to string
                             user,         % where the string is to be found
                              ".?!",         % sentence delimiters : final dot / question mark / exclamation mark
                              ",:",         % word delimiters : comma / colon
                              ",:?",         % returned punctuation signs : comma / colon / question mark
                              nocaps,       % decapitalize or leave alone (nocaps - caps)
                              _),           % end-of-file capture

                 ifthen(Sentence=[stop],abort),
                 pair(Sentence,_,NS),
                 write(NS),
                 play.

% PAIR
%%%%%%

pair(St1,St2,Flat) :-
    pairing(Pat1,St2,Conditions),
               % at this stage St2 is still a mixture of words and variables
    match(Pat1,St1),
               % the match instantiates Pat1, filling the vars that are common to Pat1 and St2
    satisfy(Conditions),
    flatten(St2,Flat),!.

                     % we then check that the conditions specified in the third arg are satisfied
                     % using the cut means dropping the nonpaired string as well as allowing
                     % one transformation per string


pair(X,X,X).

satisfy([]).
satisfy([H|T]) :- call(H), satisfy(T).



% MATCH
%%%%%%%

match([],[]).
             % end of list traversal

match([Item|Items],[Word|Words]) :-
   nonvar(Item),
             % matching word against word
   Item=Word,
   match(Items,Words).


match([Var|Items],Words) :-
   var(Var), % Var covers an unknown list of words,
       % which can be empty
   append(SomeWords,WordsMatched,Words),
             % only arg3 is instantiated when append is first called
             % append is doing the real work here
             % i.e. proposing string divisions
             %  until the right one is found
   match(Items,WordsMatched),
             % matching the remaining items in the pattern
   Var=SomeWords.
             % instantiating Var

% DATA
%%%%%%

% the condition lists are empty here
% they can be filled with any Prolog goal on any of the variables used in the pairing - see example below


% time subordinators
pairing([X,post,Y,quam,Z],[X,Y,postquam,Z],[]).
pairing([X,prius,Y,quam,Z],[X,Y,priusquam,Z],[]).
pairing([X,ante,Y,quam,Z],[X,Y,antequam,Z],[]).


% correlatives

% MAGIS QUAM - Captain Archibald Haddock

pairing([X,magis,Y,quam,Z],[X,Y,magis_quam,Z],
                                             [member(Verb,Z),
                                              lex(Verb,v,_)]).

                                                       % Interfecto Vitellio bellum magis Y[desierat]Y quam pax coeperat ->
                                                       % Interfecto Vitellio bellum desierat magis_quam pax coeperat


pairing([X,magis,Y,quam,Z],[X,Y,magis_quam,Y,Z],[]).   % Y repeated to rebuild clause :
              % a qua magis Y[degeneravimus]Y quam ab eloquentia ->
              % a qua degeneravimus magis_quam degeneravimus ab eloquentia




pairing([X,tam,Y,quam,Z],[X,Y,tam_quam,Z],[]).

pairing([X,ita,Y,ut,Z],[X,Y,ita_ut,Z],[]).
pairing([X,sic,Y,ut,Z],[X,Y,ita_ut,Z],[]).
pairing([X,item,Y,ut,Z],[X,Y,ita_ut,Z],[]).

pairing([X,ut,Y,ita,Z],[X,ita_ut,Y,Z],[]).
pairing([X,ut,Y,sic,Z],[X,ita_ut,Y,Z],[]).
pairing([X,ut,Y,item,Z],[X,ita_ut,Y,Z],[]).
pairing([X,nusquam,Y,nisi,Z],[X,nusquam_nisi,Y,Z],[]).  % Captain AdHoc strikes again




% EXAMPLE WITH CONDITIONS ON MATCH
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*

We can turn a lexical treatment such as:

[lex,satis_facere] --->[recorded(pos,position(A,B,satis),_),
                          recorded(pos,position(B,C,Facere),_),
                          lex(Facere,v,FS),
                          pick(lex:facere,FS,FS1),
                          pick(txt:Txt,FS1,FS2),
                          atom_concat(satis,Facere,Satisfacere),
                          append([txt:Satisfacere,lex:satisfacere],FS2,FSNew),
                          map(v,[from:A,to:C | FSNew])].

into a pre-treatment at readtime such as:

pairing([X,satis,Facere],[X,Satisfacere,Z],[Facere=[Fac|Z],
                    lex(Fac,v,FS),
              constraint([lex:facere],FS),
              atom_concat(satis,Fac,Satisfacere)]).

*/












% addition version 187
%%%%%%%%%%%%%%%%%%%%%%

% purpose : to establish whether a clause is interrogative,
% i.e. contains an interrogative word
% the contains_interrogative clause is supposed to apply to the pathlist of the clause whose interrogative character is being investigated
% some verbs such as quaero need an interrogative complement clause

interrogative(Word) :- lex(Word,print,_).
                       % interrogative pronouns are interrogative... here only the POS needs to be retrieved

interrogative(Word) :- lex(Word,_,Features), member(type:int,Features).
                       % type:int is a feature assigned to interrogative words
			% here we need to explore the whole feature list


contains_interrogative(Pathlist) :- member(p(A,B), Pathlist),
                                    recorded(pos,position(A,B,Word)), % we can get at any word in the pathlist
                                    interrogative(Word).

                       % a pathlist contains an interrogative word if it has a couple of positions associated with an interrogative word


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% END OF PROG
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
