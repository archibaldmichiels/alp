%-----------------------------------------
% LEXDIS :  computing lexical distance  
%-----------------------------------------

% A. Michiels, University of Liège, Belgium
% amichiels@ulg.ac.be

% last update : 08/03/2012

% LDLIGHTADJ
% weighting : 100
% bonuses:
% 20 in def, 5 in ex


% check SWI Prolog settings  with the "statistics." command
% the global stack line should look sth like :
% Global stack :   134,217,728  ....
% :- set_prolog_flag(verbose_load,normal).
:- style_check(-singleton).
:- style_check(-discontiguous).
:- dynamic(kill/1).

% kill/1 is used to store the idnums of entries that are candidates for a merge
% if the merge is ok, the entries feeding the merge ought to be killed
% we can then use the kill(Idnum) clauses
% to retract the relevant entries from semdic
% to which we will previously have added the merged entries
% finally, we ought to list to file the new semdic

:- [lightdic,mt,roget,indic,coll,envir,pesi].

/* semdic : dictionary clauses derived from Cide, Cobuild, Ldoce
   and the WordNet Synsets and Synset Glosses

   mt : data base of RC/OH collocates - the pivotal property
   is copresence within the same collocate field

   roget : db of Roget's Thesaurus Categories (three levels)

   indic: data base of RC(Robert/Collins)/OH(Oxford/Hachette) indicators
   (in these two bilingual E-F/F-E dictionaries,
          only the E->F direction is explored)   

   coll : data base of RC/OH collocates -
   the pivotal element is the collocate bearer

   envir : data base of environments derived from RC/OH 'extended' lemmas
   i.e. including phrases and examples

   pesi : data base recording the lexical weight of lemmas

  Cide = Cambridge International Dictionary of English
  Cobuild = Cobuild dictionary of English, based on the Cobuild Corpus
  Ldoce = Longman Dictionary of Contemporary English
*/

go :-
     nl,nl,
     protocola(lexdis),
     write('Computing lexical distance...'),
     nl,
     write('A. Michiels, University of Liège'),nl,
     write('-----------------------------------------------------------------'),
     nl,
     nl,
     write('Input file?  [stdin. or filename.] --> '),
     read(Input),
     dwith(Input,HandleIn),
     write('Results file? [filename.] --> '),
     read(Output),
     concat(Output,'.lst',Outlist),
     !,
     start(Outlist,HandleIn,Input).


/* Input is from stdin(user's terminal) or from file */

dwith(stdin,_):- !.

dwith(FileIn,HandleIn):- 
                 open(FileIn,read,HandleIn).


% starting
%---------

start(Outlist,_,stdin) :-
     open(Outlist,write,Lists), 
     statistics(cputime,BT),
     recorda(time,BT),
     repeat,
     nl,
     nl,
     write('Please enter the pair of items as follows:
     [word1,pos:Pos1,word2,pos:Pos2,m:Mode]. or
     [word1,pos:Pos,word2,m:Mode].')
     ,nl,
     write('or [word1,pos:Pos1,word2,pos:Pos2,w:MW,m:Mode]. '),
     nl,
     write('(where Mode is either g(lobal:words) or l(ocal:wordsenses),
     pos1, pos2 and pos are either adj, adv, n or v
     or left as uninstantiated variables'),
     nl,
     write('and MW is an integer expressing the required
            minimum connectivity threshold'),nl,
     write('or t or t3 to get the top(or top 3)pairings only)'),
     nl,
     write('or show(word,pos,spec:data).
           where spec is either gw,lb,df or ex
           and data is a label, guideword or word'),nl,
     write('or show(Idnum).
            where Idnum is an identifier returned by the system'),
     nl,
     write('or finally nadamas. to exit --> '),
     (   recorded(weights,max(0),_)-> true; recorda(weights,max(0),_)),
       % we start with maximum weight (re)set to zero
     read(Query),
     nl(Lists),nl(Lists),write(Lists,'QUERY: '),
     write(Lists,Query),nl(Lists),nl(Lists),
     dealwith(Query,Lists),
     fail.
                 

start(Outlist,HandleIn,Input) :-
     Input \= stdin,
     open(Outlist,write,Lists),
     nl,
     statistics(cputime,BT),
     recorda(time,BT),
     repeat,
     (   recorded(weights,max(0),_)-> true; recorda(weights,max(0),_)),
     read(HandleIn,Query),nl,nl,
     write(Query),nl,nl,
     nl(Lists),nl(Lists),write(Lists,'QUERY: '),
     write(Lists,Query),nl(Lists),nl(Lists),
     dealwith(Query,Lists),
     fail.

%--------------------------------------------------------------------------

dealwith(nadamas,L) :-
     nl,
     recorded(time,BT),
     statistics(cputime,ET),
     TimeUsedR is ET-BT,
     
    TU10 is TimeUsedR*10,
        TUR is round(TU10),
        TimeUsed is TUR/10,
     
     
     nl(L), write(L,'cputime in sec : '), write(L,TimeUsed), nl(L),
     nl, write('cputime in sec : '), write(TimeUsed), nl,nl,
     write('End of input... Always glad to be able to help... Bye!'),nl,nl,
     noprotocol,
     close(L),
     open(tokill,write,Out),
     tell(Out),
     (listing(kill/1) -> true; true),  % we do not necessarily produce kill
                                       % clauses !
     tell(user),
     close(Out),
     abort.
%------------------------------------------------------


dealwith([Word1,pos:Pos,Word2],L) :-
     dealwith([Word1,pos:Pos,Word2,pos:Pos,w:none,m:g,adjust],L).

% no minimum weight specified : we set it to 'none',
% no mode specified : set to global
% + shared POS
%------------------------------------------------

dealwith([Word1,pos:Pos1,Word2,pos:Pos2],L) :-
     dealwith([Word1,pos:Pos1,Word2,pos:Pos2,w:none,m:g,adjust],L).

% we supply global mode and weight is set to 'none'
%--------------------------------------------------

dealwith([Word1,pos:Pos,Word2,m:Mode],L) :-
     dealwith([Word1,pos:Pos,Word2,pos:Pos,w:none,m:Mode,adjust],L).

% no minimum weight specified : we set it to 'none'
% + shared POS
%---------------------------------------------------

dealwith([Word1,pos:Pos1,Word2,pos:Pos2,m:Mode],L) :-
     dealwith([Word1,pos:Pos1,Word2,pos:Pos2,w:none,m:Mode,adjust],L).

% no minimum weight specified : we set it to 'none'
%---------------------------------------------------

dealwith([Word1,pos:Pos1,Word2,pos:Pos2,w:MW,m:Mode],L) :-
     dealwith([Word1,pos:Pos1,Word2,pos:Pos2,w:MW,m:Mode,adjust],L).

% specified MW
%---------------------------------------------------

dealwith(t(Target,Arrow1,Arrow2),L) :-
     dealwith(t(Target,v,Arrow1,n,Arrow2,n),L).

% triplets : ArgBearer,Arg1,Arg2 e.g. t(wear,tie,watch)
%------------------------------------------------------





% global mode : we relate lexical items not word senses
%-------------------------------------------------------

% here we are not interested in word senses specified by a given idnum
% idnum is allowed an 'existential' reading in the basic setof clauses
% the distance is measured as between words
% regrouping all their distinct wordsenses

% we collect the relevant data in all relevant semdic entries

/* example of a semdic entry:
mono(
  lem('epic'),
  ori('ci'),
  idnum('ci23101'),
  pos('adj'),
  lab([]),
  gw([]),
  deflex(['events','happen','period','involve','difficulty']),
  exlex(['journey','struggle']),
  def(' epic can also be used of events that happen over a long period
        and involve a lot of action and difficulty')
    ).
*/



dealwith([Word1,pos:Pos1,Word2,pos:Pos2,w:MinimumWeight,m:g,noadjust],Output) :-
    setof(data(Lablist1,Gwlist1,Deflex1,Exlex1),
          Def^Dic^Idnum^Lablist1^Gwlist1^Deflex1^Exlex1^(mono(lem(Word1),
             ori(Dic),
             idnum(Idnum),
             pos(Pos1),
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
             pos(Pos2),
             lab(Lablist2),
             gw(Gwlist2),
             deflex(Deflex2),
             exlex(Exlex2),
             def(Def)   )),
         DL2),
    
   collect(DL1,Lab1,Gw1,Def1,Ex1),
   collect(DL2,Lab2,Gw2,Def2,Ex2),

% we distribute the data just collected : labels, guidewords,
%                                     definition core items, example core items

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

   sumlist([WeightLabs,WeightGws,WeightDeflex,WeightExlex,
         Metaweight,Rogetweight,Indicweight,Collweight,Envirweight],GW),
 

  (MinimumWeight=none -> true; GW >= MinimumWeight),
  % reaching the threshold

   report(Output,global,GW,Word1,Pos1,
          Word2,Pos2,
          ResLabs,WeightLabs,
          ResGws,WeightGws,
          ResDeflex,WeightDeflex,
          ResExlex,WeightExlex,
          Metaweight,Rogetweight,
          Indicweight,Collweight,Envirweight,0). 


% ADJUSTING PROXIMITY ACCORDING TO LEXICAL WEIGHT 
%--------------------------------------------------

dealwith([Word1,pos:Pos1,Word2,pos:Pos2,w:MinimumWeight,m:g,adjust],Output) :-
    setof(data(Lablist1,Gwlist1,Deflex1,Exlex1),
          Def^Dic^Idnum^Lablist1^Gwlist1^Deflex1^Exlex1^(mono(lem(Word1),
             ori(Dic),
             idnum(Idnum),
             pos(Pos1),
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
             pos(Pos2),
             lab(Lablist2),
             gw(Gwlist2),
             deflex(Deflex2),
             exlex(Exlex2),
             def(Def)   )),
         DL2),
    
   collect(DL1,Lab1,Gw1,Def1,Ex1),
   collect(DL2,Lab2,Gw2,Def2,Ex2),

% we distribute the data just collected : labels, guidewords,
     %                               definition core items, example core items

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

   sumlist([WeightLabs,WeightGws,WeightDeflex,WeightExlex,
         Metaweight,Rogetweight,Indicweight,Collweight,Envirweight],GW),
 

  % adjusting weights according to lexical weight : remmed or remmable 
  % if no threshold is specified, neg weights also recorded
  
  adjustweight(Word1,Pos1,Word2,Pos2,Adjust),
  NW is GW /Adjust,
  (MinimumWeight=none -> true; NW >= MinimumWeight),
  report(Output,global,NW,Word1,Pos1,
          Word2,Pos2,
          ResLabs,WeightLabs,
          ResGws,WeightGws,
          ResDeflex,WeightDeflex,
          ResExlex,WeightExlex,
          Metaweight,Rogetweight,
          Indicweight,Collweight,Envirweight,Adjust).   




dealwith(friends(Word1,pos:Pos1,w:MinimumWeight),Output) :-
    setof(data(Lablist1,Gwlist1,Deflex1,Exlex1),
          Def^Dic^Idnum^Lablist1^Gwlist1^Deflex1^Exlex1^(mono(lem(Word1),
             ori(Dic),
             idnum(Idnum),
             pos(Pos1),
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
             pos(Pos2),
             lab(Lablist2),
             gw(Gwlist2),
             deflex(Deflex2),
             exlex(Exlex2),
             def(Def)   )),
         DL2),

   collect(DL1,Lab1,Gw1,Def1,Ex1),
   collect(DL2,Lab2,Gw2,Def2,Ex2),

% we distribute the data just collected : labels, guidewords,
     %                               definition core items, example core items

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

   sumlist([WeightLabs,WeightGws,WeightDeflex,WeightExlex,
         Metaweight,Rogetweight,Indicweight,Collweight,Envirweight],GW),


  % adjusting weights according to lexical weight : remmed or remmable
  % if no threshold is specified, neg weights also recorded

   (MinimumWeight=none -> true; GW >= MinimumWeight),

    Entry= f(Word2,Pos2,GW,Word1,Pos1),
    output_it(Entry,Output).




% local mode : we relate lexical items
% individuating word senses in our lexical data bases
%--------------------------------------------------------

% the Idnum field no longer receives an existential reading
% in the setof clauses
% items are paired Idnum to Idnum,
% which are supposed to be assigned to word senses


% we show only the top pairings ('w:t' option)

dealwith([Word1,pos:Pos1,Word2,pos:Pos2,w:t,m:l,_noadjust],Output) :-
    setof(data(Lablist1,Gwlist1,Deflex1,Exlex1),
          Def^Dic^Lablist1^Gwlist1^Deflex1^Exlex1^(mono(lem(Word1),
             ori(Dic),
             idnum(Idnum1),
             pos(Pos1),
             lab(Lablist1),
             gw(Gwlist1),
             deflex(Deflex1),
             exlex(Exlex1),
             def(Def) )),
           DL1) ,
   
    setof(data(Lablist2,Gwlist2,Deflex2,Exlex2),
         Def^Dic2^Lablist2^Gwlist2^Deflex2^Exlex2^(mono(lem(Word2),
             ori(Dic2),
             idnum(Idnum2),
             pos(Pos2),
             lab(Lablist2),
             gw(Gwlist2),
             deflex(Deflex2),
             exlex(Exlex2),
             def(Def) )),
         DL2) ,
    
   collect(DL1,Lab1,Gw1,Def1,Ex1),
   collect(DL2,Lab2,Gw2,Def2,Ex2),

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


   cw(def,Word1,Word2,D1,D2,ResDeflex,WeightDeflex),
   cw(ex,Word1,Word2,E1,E2,ResExlex,WeightExlex),
   
   sumlist([WeightLabs,WeightGws,WeightDeflex,WeightExlex],W),

% each time we get a new WS (Word Sense) pair
% we see if its weight is inferior to the recorded maximum
% if it is, we simply drop it and wait for the intrinsic failure
% built in the dealwith loop to provide other pairs
 
% if it comes up to the maximum but does not exceed it,
% we simply record it as a pair to remember

% if it exceeds the recorded max, we 'forget' the recorded max,
% and record the new value as max
% we also forget all the pairs we have recorded so far,
% since they are now dethroned by the pair exhibiting the new max
% we can then record the new winning pair and expect the intrinsic failure
% built in the dealwith loop to provide other candidates


   ( ( recorded(weights,max(Max),_),
     Max > W
     )  
     ->
     true
     ;
     (recorded(weights,max(Max),Ref),
      (   W=Max -> recorda(res, str([Output,local,W,Word1,Idnum1,Pos1,
                                     Word2,Idnum2,Pos2,ResLabs,WeightLabs,
                                     ResGws,WeightGws,ResDeflex,WeightDeflex,
                                     ResExlex,WeightExlex]),_)
                   ;
                   (   erase(Ref),
                   recorda(weights,max(W),_),
                   deleteref1(Max),
                   recorda(res, str([Output,local,W,Word1,Idnum1,Pos1,
                                     Word2,Idnum2,Pos2,
                                     ResLabs,WeightLabs,
                                     ResGws,WeightGws,
                                     ResDeflex,WeightDeflex,
                                     ResExlex,WeightExlex]),_)
                   )
     )
    )
   ).


% when all pairs have been examined and, if need be, recorded
% we can gather them all together with a setof clause on the recorded pairs 
% and report on the results
% we end up by cleaning, i.e. erasing all recorded information

dealwith([W1,pos:P1,W2,pos:P2,w:t,m:l,_noadjust],Output) :-
      setof( Args,
             recorded(res,str(Args),_),
             ListArgs),
      reportlist(ListArgs),
      eraseall(weights),
      eraseall(res).

reportlist([]).
reportlist([Args|MoreArgs]) :- report(Args), reportlist(MoreArgs).

%---------------------------------------------------------------------
% here a minimum weight is specified -
% we output all the pairs reaching the specified threshold

dealwith([Word1,pos:Pos1,Word2,pos:Pos2,w:MW,m:l,_noadjust],Output) :-
    MW \= t, MW \= m, MW \= t3,
    member(Pos1,[n,v,adj,adv]), 
    setof(data(Lablist1,Gwlist1,Deflex1,Exlex1),
          Def^Dic^Lablist1^Gwlist1^Deflex1^Exlex1^(mono(lem(Word1),
             ori(Dic),
             idnum(Idnum1),
             pos(Pos1),
             lab(Lablist1),
             gw(Gwlist1),
             deflex(Deflex1),
             exlex(Exlex1),
             def(Def) )),
           DL1) ,
   
    setof(data(Lablist2,Gwlist2,Deflex2,Exlex2),
         Def^Dic2^Lablist2^Gwlist2^Deflex2^Exlex2^(mono(lem(Word2),
             ori(Dic2),
             idnum(Idnum2),
             pos(Pos2),
             lab(Lablist2),
             gw(Gwlist2),
             deflex(Deflex2),
             exlex(Exlex2),
             def(Def) )),
         DL2) ,
    
   collect(DL1,Lab1,Gw1,Def1,Ex1),
   collect(DL2,Lab2,Gw2,Def2,Ex2),

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


   cw(def,Word1,Word2,D1,D2,ResDeflex,WeightDeflex),
   cw(ex,Word1,Word2,E1,E2,ResExlex,WeightExlex),
   
   sumlist([WeightLabs,WeightGws,WeightDeflex,WeightExlex],W),

   (MW=none -> true; W >= MW),

   report([Output,local,W,Word1,Idnum1,Pos1,
          Word2,Idnum2,Pos2,
          ResLabs,WeightLabs,
          ResGws,WeightGws,
          ResDeflex,WeightDeflex,
          ResExlex,WeightExlex]).


/* specifying an Idnum instead of a Pos for the first item */

dealwith([Word1,Idnum1,Word2,pos:Pos2,w:MW,m:l,_noadjust],Output) :-
    MW \= t, MW \= m, MW \=t3,
    Idnum1 \= pos:_,
       setof(data(Lablist1,Gwlist1,Deflex1,Exlex1),
          Def^Dic^Lablist1^Gwlist1^Deflex1^Exlex1^(mono(lem(Word1),
             ori(Dic),
             idnum(Idnum1),
             pos(Pos1),
             lab(Lablist1),
             gw(Gwlist1),
             deflex(Deflex1),
             exlex(Exlex1),
             def(Def) )),
           DL1) ,
   
    setof(data(Lablist2,Gwlist2,Deflex2,Exlex2),
         Def^Dic2^Lablist2^Gwlist2^Deflex2^Exlex2^(mono(lem(Word2),
             ori(Dic2),
             idnum(Idnum2),
             pos(Pos2),
             lab(Lablist2),
             gw(Gwlist2),
             deflex(Deflex2),
             exlex(Exlex2),
             def(Def) )),
         DL2) ,
   
   Word1 \= Word2, 
   collect(DL1,Lab1,Gw1,Def1,Ex1),
   collect(DL2,Lab2,Gw2,Def2,Ex2),

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


   cw(def,Word1,Word2,D1,D2,ResDeflex,WeightDeflex),
   cw(ex,Word1,Word2,E1,E2,ResExlex,WeightExlex),
   
   sumlist([WeightLabs,WeightGws,WeightDeflex,WeightExlex],W),

     (MW=none -> true; W >= MW),

   report([Output,local,W,Word1,Idnum1,Pos1,
          Word2,Idnum2,Pos2,
          ResLabs,WeightLabs,
          ResGws,WeightGws,
          ResDeflex,WeightDeflex,
          ResExlex,WeightExlex]).

%---------------------------------------------------------------------

% showing the relevant semdic entry

dealwith(show(Word,Pos,Spec:Data),L) :-
         show(Word,Pos,Spec:Data,L).

dealwith(show(Idnum),L) :-
         show(Idnum,L).

dealwith(show(Word,Pos),L) :-
         show(Word,Pos,L).

%-----------------------------------------------------------------------

collect([],[],[],[],[]).
collect([data(L,G,D,E)|MoreData],
         [L|MoreL], [G|MoreG],
         [D|MoreD],[E|MoreE]) :-
         collect(MoreData,MoreL,MoreG,MoreD,MoreE).

% redistributing the data into the 4 relevant lists:
% Labels, GuideWords, Words_in_def, Words_in_ex


% merge mode
%------------

collect([],[],[],[],[],[]).
collect([data(L,G,D,E,F)|MoreData],
         [L|MoreL], [G|MoreG],
         [D|MoreD],[E|MoreE],[F|MoreF]) :-
         collect(MoreData,MoreL,MoreG,MoreD,MoreE,MoreF).

% redistributing the data into the 5 relevant lists:
% Labels, GuideWords, Words_in_def, Words_in_ex, DefStrings


%------------------------------------------------------------

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

%-------------------------------------------------------------------------
       

cw(def,Word1,Word2,L1,L2,Res,Weight) :-
        Word1 \= Word2,
        (   is_set(L1) -> Set1=L1 ; list_to_set(L1,Set1)),
        (   is_set(L2) -> Set2=L2 ; list_to_set(L2,Set2)),
        (member(Word1,Set2) -> Bonus1=20 ; Bonus1=0 ),
            % bonus is granted when the words 'interdefine', i.e.
            % A is used in the definition of B, or vice-versa
        (member(Word2,Set1) -> Bonus2=20 ; Bonus2=0 ),
        intersection(Set1,Set2,Res),
        length(Res,Len),
        (Len <3 -> DefLen is Len*2 ; DefLen is Len*3),
        % we increase the weight if we believe
        % that the sharing exceeds a threshold
        % at which it could still be regarded as incidental (set to 2)
        sumlist([DefLen,Bonus1,Bonus2],Weight).         

cw(ex,Word1,Word2,L1,L2,Res,Weight) :-
         Word1 \= Word2,
        (   is_set(L1) -> Set1=L1 ; list_to_set(L1,Set1)),
        (   is_set(L2) -> Set2=L2 ; list_to_set(L2,Set2)),
        (member(Word1,Set2) -> Bonus1=5 ; Bonus1=0 ),
        (member(Word2,Set1) -> Bonus2=5 ; Bonus2=0 ),
        intersection(Set1,Set2,Res),
        length(Res,Len),
        sumlist([Len,Bonus1,Bonus2],Weight).  

% weighting for def and examples
% the weights are heavier in case of def
% we pay special attention to the presence of one of the words
% in the def or examples associated with the other

%------------------------------------------------------
% cw in merge mode : we are looking at a single lexeme
% its occurrence in def or ex is simply to be discarded

cw(def,Word,Word,L1,L2,Union,Weight) :-
        (   is_set(L1) -> Set1=L1 ; list_to_set(L1,Set1)),
        (   is_set(L2) -> Set2=L2 ; list_to_set(L2,Set2)),
        (pick(Word,Set1,SS1) -> NS1=SS1; NS1=Set1),
        (pick(Word,Set2,SS2) -> NS2=SS2; NS2=Set2),
        union(NS1,NS2,Union),
        intersection(NS1,NS2,Res),
        length(Res,Len),
        (Len <3 -> Weight is Len*2 ; Weight is Len*3).

cw(ex,Word,Word,L1,L2,Union,Weight) :-
        (   is_set(L1) -> Set1=L1 ; list_to_set(L1,Set1)),
        (   is_set(L2) -> Set2=L2 ; list_to_set(L2,Set2)),
        (pick(Word,Set1,SS1) -> NS1=SS1; NS1=Set1),
        (pick(Word,Set2,SS2) -> NS2=SS2; NS2=Set2),
        union(NS1,NS2,Union),
        intersection(NS1,NS2,Res),
        length(Res,Weight).


%-----------------------------------------------------------------------------

% working out decrease factor for heavy items
% adjustweight(Word1,Pos1,Word2,Pos2,Adjust)

% calls on pesi db, with clauses such as:
% w(fox, v, 189).


adjustweight(Word1,Pos1,Word2,Pos2,Adjust) :-
 (w(Word1,Pos1,Weight1) -> W1 is Weight1 ; W1 is 1),
 (w(Word2,Pos2,Weight2) -> W2 is Weight2 ; W2 is 1),
 Both is W1+W2,
 ( Both < 200 ->  Adjust is 1 ; Adjust is Both/200).


%---------------------------------------------------------------------------

% GLOBAL REPORT
%---------------

report(FO,global,WC,Word1,Pos1,
          Word2,Pos2,
          ResLabs,WeightLabs,
          ResGws,WeightGws,
          ResDeflex,WeightDeflex,
          ResExlex,WeightExlex,Metaweight,Rogetweight,
          Indicweight,Collweight,Envirweight,AdjustC) :-

          W10 is WC*10,
        WR is round(W10),
        W is WR/10,

          Adjust10 is AdjustC*10,
        AdjustR is round(Adjust10),
        Adjust is AdjustR/10,
        
          ( W > 0 ->
      (nlb(FO),writeb(FO,Word1),writeb(FO,' with POS='), writeb(FO,Pos1),
       writeb(FO,' is related to '), writeb(FO,Word2), writeb(FO,' with POS='),
       writeb(FO,Pos2), writeb(FO,' with weight='),
       writeb(FO,W),writeb(FO,' as follows: '), nlb(FO),
             (WeightLabs > 0 ->
                               (writeb(FO,'Shared Labels: '), 
                                writeb(FO,ResLabs), 
                                writeb(FO,' -> weight: '), 
                                writeb(FO,WeightLabs),nlb(FO)) ; true),
                  
              (WeightGws > 0 ->
                               (writeb(FO,'Shared Guide Words: '), 
                                writeb(FO,ResGws), 
                                writeb(FO,' -> weight: '), 
                                writeb(FO,WeightGws), nlb(FO)) ; true),

              (WeightDeflex  > 0 ->
                             (writeb(FO,'Shared words in definition: '),
                                 writeb(FO,ResDeflex), 
                                 writeb(FO,' -> weight: '), 
                                 writeb(FO,WeightDeflex), nlb(FO)) ; true),

              (WeightExlex > 0 ->
                                 (writeb(FO,'Shared words in examples: '), 
                                  writeb(FO,ResExlex), 
                                  writeb(FO,' -> weight: '), 
                                  writeb(FO,WeightExlex), nlb(FO)) ; true),
              (Metaweight > 0 ->
                                 (writeb(FO,'Cooccurrence in collocate lists '), 
                                  writeb(FO,' -> weight: '), 
                                  writeb(FO,Metaweight), nlb(FO)) ; true),
              (Rogetweight > 0 ->
                             (writeb(FO,'Cooccurrence in Roget\'s thesaurus '),
                                  writeb(FO,' -> weight: '), 
                                  writeb(FO,Rogetweight), nlb(FO)) ; true),        
              (Indicweight > 0 ->
                           (writeb(FO,'Cooccurrence in R/C-Oxf/Hach indic db '),
                                  writeb(FO,' -> weight: '), 
                                  writeb(FO,Indicweight), nlb(FO)) ; true),    
              (Collweight > 0 ->
                     (writeb(FO,'Cooccurrence in R/C-Oxf/Hach collocates db '),
                      writeb(FO,' -> weight: '),
                                  writeb(FO,Collweight), nlb(FO)) ; true), 
              (Envirweight > 0 ->
                 (writeb(FO,'Cooccurrence in R/C-Oxf/Hach extended lemma db '),
                                  writeb(FO,' -> weight: '), 
                                  writeb(FO,Envirweight), nlb(FO)) ; true),
              (Adjust > 1 ->
                                 (writeb(FO,'Penalty for Heavy Lexical Items '), 
                                  writeb(FO,' -> adjusted_weight= weight/'),
                                  writeb(FO,Adjust), nlb(FO)) ; true)
                 ) 
                 ;
           (
        nlb(FO), writeb(FO,Word1), writeb(FO,' with POS='), writeb(FO,Pos1),
        writeb(FO,' is ***not*** related to '),
        writeb(FO,Word2), writeb(FO,' with POS='),
        writeb(FO,Pos2), writeb(FO,' (Adjusted Weight = '),
        writeb(FO,W), writeb(FO,').')
           )         
           ).


% LOCAL REPORT
%-------------
        
report([FO,local,WC,Word1,Idnum1,Pos1,
          Word2,Idnum2,Pos2,
          ResLabs,WeightLabs,
          ResGws,WeightGws,
          ResDeflex,WeightDeflex,
          ResExlex,WeightExlex]) :-
   W10 is WC*10,
        WR is round(W10),
        W is WR/10,
        
          ( W > 0 ->
                 (show(Idnum1,FO),
                  writeb(FO,'is related to '),
                  nlb(FO),
                  show(Idnum2,FO),
                  writeb(FO,'with weight='),
                  writeb(FO,W),
                  writeb(FO,' as follows: '),
                  nlb(FO),
                  (WeightLabs > 0 ->
                               (writeb(FO,'Shared Labels: '),
                                writeb(FO,ResLabs),
                                writeb(FO,' -> weight: '),
                                writeb(FO,WeightLabs),nlb(FO)) ; true),

                  (WeightGws > 0 ->
                               (writeb(FO,'Shared Guide Words: '),
                                writeb(FO,ResGws),
                                writeb(FO,' -> weight: '),
                                writeb(FO,WeightGws), nlb(FO)) ; true),

                  (WeightDeflex  > 0 ->
                                (writeb(FO,'Shared words in definition: '),
                                 writeb(FO,ResDeflex),
                                 writeb(FO,' -> weight: '),
                                 writeb(FO,WeightDeflex), nlb(FO)) ; true),

                  (WeightExlex > 0 ->
                                 (writeb(FO,'Shared words in examples: '),
                                  writeb(FO,ResExlex),
                                  writeb(FO,' -> weight: '),
                                  writeb(FO,WeightExlex), nlb(FO)) ; true)
                             ;
           true)).

%------------------------------------------------------------------------

%------------------------------------------------------------------------

show(Word,Pos,gw:GW,L) :-
         mono(lem(Word),
             ori(Dic),
             idnum(Idnum),
             pos(Pos),
             lab(Lablist),
             gw(Gwlist),
             deflex(Deflex),
             exlex(Exlex),
             def(Def)),
         member(GW,Gwlist),
         writeb(L,Word),tabb(L,2), writeb(L,Pos), tabb(L,2),
         writeb(L,Gwlist),tabb(L,2),writeb(L,Lablist),tabb(L,2),
         writeb(L,Idnum), tabb(L,2),
         writeb(L,'Def: '), writeb(L,Def),
         nlb(L),nlb(L).
         

show(Word,Pos,lb:Lab,L) :-
         mono(lem(Word),
             ori(Dic),
             idnum(Idnum),
             pos(Pos),
             lab(Lablist),
             gw(Gwlist),
             deflex(Deflex),
             exlex(Exlex),
             def(Def)),
         member(Lab,Lablist),
         writeb(L,Word),tabb(L,2), writeb(L,Pos), tabb(L,2),
         writeb(L,Gwlist),tabb(L,2),writeb(L,Lablist),tabb(L,2),
          writeb(L,Idnum), tabb(L,2),
         writeb(L,'Def: '), writeb(L,Def),
         nlb(L),nlb(L).
         

show(Word,Pos,df:Defel,L) :-
         Defel \= and(Candi),
         Defel \= or(Candi),
         mono(lem(Word),
             ori(Dic),
             idnum(Idnum),
             pos(Pos),
             lab(Lablist),
             gw(Gwlist),
             deflex(Deflex),
             exlex(Exlex),
             def(Def)),
         member(Defel,Deflex),
         writeb(L,Word),tabb(L,2), writeb(L,Pos), tabb(L,2),
         writeb(L,Gwlist),tabb(L,2),writeb(L,Lablist),tabb(L,2),
         writeb(L,Idnum), tabb(L,2),writeb(L,Deflex),nlb(L),
         writeb(L,'Def: '), writeb(L,Def),
         nlb(L),nlb(L).

show(Word,Pos,df:and(Defellist),L) :-
         mono(lem(Word),
             ori(Dic),
             idnum(Idnum),
             pos(Pos),
             lab(Lablist),
             gw(Gwlist),
             deflex(Deflex),
             exlex(Exlex),
             def(Def)),
         allmembers(Defellist,Deflex),
         writeb(L,Word),tabb(L,2), writeb(L,Pos), tabb(L,2),
         writeb(L,Gwlist),tabb(L,2),writeb(L,Lablist),tabb(L,2),
         writeb(L,Idnum), tabb(L,2),writeb(L,Deflex),nlb(L),
         writeb(L,'Def: '), writeb(L,Def),
         nlb(L),nlb(L).

show(Word,Pos,df:or(Defellist),L) :-
         mono(lem(Word),
             ori(Dic),
             idnum(Idnum),
             pos(Pos),
             lab(Lablist),
             gw(Gwlist),
             deflex(Deflex),
             exlex(Exlex),
             def(Def)),
         anymember(Defellist,Deflex),
         writeb(L,Word),tabb(L,2), writeb(L,Pos), tabb(L,2),
         writeb(L,Gwlist),tabb(L,2),writeb(L,Lablist),tabb(L,2),
         writeb(L,Idnum), tabb(L,2),writeb(L,Deflex),nlb(L),
         writeb(L,'Def: '), writeb(L,Def),
         nlb(L),nlb(L).


show(Word,Pos,ex:Exel,L) :-
         Exel \= and(Candi),
         Exel \= or(Candi),
         mono(lem(Word),
             ori(Dic),
             idnum(Idnum),
             pos(Pos),
             lab(Lablist),
             gw(Gwlist),
             deflex(Deflex),
             exlex(Exlex),
             def(Def)),
             member(Exel,Exlex),
             writeb(L,Word),tabb(L,2), writeb(L,Pos), tabb(L,2),
             writeb(L,Gwlist),tabb(L,2),writeb(L,Lablist),tabb(L,2),
             writeb(L,Idnum), tabb(L,2),writeb(L,Exlex),nlb(L),
             writeb(L,'Def: '), writeb(L,Def),
             nlb(L),nlb(L).

show(Word,Pos,ex:and(Exellist),L) :-
         mono(lem(Word),
             ori(Dic),
             idnum(Idnum),
             pos(Pos),
             lab(Lablist),
             gw(Gwlist),
             deflex(Deflex),
             exlex(Exlex),
             def(Def)),
             allmembers(Exellist,Exlex),
             writeb(L,Word),tabb(L,2), writeb(L,Pos), tabb(L,2),
             writeb(L,Gwlist),tabb(L,2),writeb(L,Lablist),tabb(L,2),
             writeb(L,Idnum), tabb(L,2),writeb(L,Exlex),nlb(L),
             writeb(L,'Def: '), writeb(L,Def),
             nlb(L),nlb(L).

show(Word,Pos,ex:or(Exellist),L) :-
         mono(lem(Word),
             ori(Dic),
             idnum(Idnum),
             pos(Pos),
             lab(Lablist),
             gw(Gwlist),
             deflex(Deflex),
             exlex(Exlex),
             def(Def)),
             anymember(Exellist,Exlex),
             writeb(L,Word),tabb(L,2), writeb(L,Pos), tabb(L,2),
             writeb(L,Gwlist),tabb(L,2),writeb(L,Lablist),tabb(L,2),
             writeb(L,Idnum), tabb(L,2),writeb(L,Exlex),nlb(L),
             writeb(L,'Def: '), writeb(L,Def),
             nlb(L),nlb(L).

show(Idnum,L) :-
         mono(lem(Word),
             ori(Dic),
             idnum(Idnum),
             pos(Pos),
             lab(Lablist),
             gw(Gwlist),
             deflex(Deflex),
             exlex(Exlex),
             def(Def) ),
             writeb(L,Word),tabb(L,2), writeb(L,Pos), tabb(L,2),
             writeb(L,Gwlist),tabb(L,2),writeb(L,Lablist),tabb(L,2),
             writeb(L,Idnum), nlb(L),
             writeb(L,'Def: '), writeb(L,Def),
             nlb(L).


show(Word,Pos,L) :-
         mono(lem(Word),
             ori(Dic),
             idnum(Idnum),
             pos(Pos),
             lab(Lablist),
             gw(Gwlist),
             deflex(Deflex),
             exlex(Exlex),
             def(Def) ),
             writeb(L,Word),tabb(L,2), writeb(L,Pos), tabb(L,2),
             writeb(L,Gwlist),tabb(L,2),writeb(L,Lablist),tabb(L,2),
             writeb(L,Idnum), tabb(L,2),writeb(L,Exlex),nlb(L),
             writeb(L,'Def: '), writeb(L,Def),
             nlb(L),nlb(L).

       
% the relevant definition is displayed, as well as the identifier

%---------------------------------------------------------------------------

% connectedness through cooccurrence
% in Robert/Collins-Oxford/Hachette collocate lists
% hypothesis of connectedness through shared belonging
% to coll lists put forward by Montemagni et al.

% cf. Montemagni, S., Federici, S. and Pirrelli,V. 1996. 
% ‘Example-based Word Sense Disambiguation: a Paradigm-driven Approach’, 
% Euralex’96 Proceedings, Göteborg University, 151-160.  


% the cooccurrence lists are assigned as early as possible
% in the alphabetical ranking of the lexical items
% - it is therefore the 'smaller' word that should be explored
% an mt line looks like the following:

% mt(digestion,[[growth,1],[machine,1],[mind,1],[movement,1],
%               [reaction,1],[recovery,1],[stomach,4]]).

% this means that the word 'digestion' coocurs 1 time with 'growth'
% in a collocate list ... and 4 times with 'stomach'
% the sharing of 'digestion' with a word preceding 'digestion'
% should be looked for under that word

metameet(W1,W2,Weight) :-
        W1 @< W2,
        mt(W1,List),
        member([W2,Cooc],List),
        ( (W1=person;W2=person;W1=object;W2=object) ->
        % reduction due to high frequency
        % and poor semantic discriminatory power
               % of 'person' and 'object'
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

%-----------------------------------------------------------------------------

% connectedness through the sharing of Roget's categories
% three levels of delicacy in thesaurus organisation

% a r line looks like the following:

% r('antiquarian',[['n','122','4','4'],['n','492','4','2']]).
% which means that the word antiquarian is a noun
% that belongs to two category triples
% 122/4/4 and 492/4/2 where the broadest category is first (492),
% followed by sub-category(4) and sub-sub-category(2)


% we retrieve the list of categories associated with the two items
% and then we compute their intersection,
% accumulating the weights according to type of category matched

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
        sumlist(Commonlist,Weight),
        !.

rogetmeet(W1,W2,Weight) :-
        W2 @< W1,        
        r(W1,W1list),
        r(W2,W2list),
        i_roget(W2list,W1list,Commonlist),
        sumlist(Commonlist,Weight),
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

%---------------------------------------------------------------------------

% Connectedness through indic sharing in RC/OH indicator data base

% db struc (indic.pl) : e.g.
% ind(lemma('abacus'),pos(n),indic(['counting','frame'])).


indicmeet(Word1,Pos1,Word2,Pos2,IndicWeight) :-
     findall(Indic,
             ind(lemma(Word1),pos(Pos1),indic(Indic)),
             IndicList1),
     findall(Indic2,
             ind(lemma(Word2),pos(Pos2),indic(Indic2)),
             IndicList2),
     flatten(IndicList1, IL1),
     flatten(IndicList2, IL2),
     (is_set(IL1) -> Set1=IL1 ; list_to_set(IL1,Set1)),
     (is_set(IL2) -> Set2=IL2 ; list_to_set(IL2,Set2)),
     (member(Word1,Set2) -> Bonus1=5 ; Bonus1=0 ),
     (member(Word2,Set1) -> Bonus2=5 ; Bonus2=0 ),
     intersection(Set1,Set2,Res),
     length(Res,Len),
     (Len <3 -> IW is Len*2; IW is Len*4),
         % we prioritize 'non-incidental' sharing (threshold set to 3)
     sumlist([IW,Bonus1,Bonus2],IndicWeight),
     !.

indicmeet(_W,_P,_W2,_P2,0).

%------------------------------------------------------------------------------

% Connectedness through collocate sharing in RC/OH collocate data base

% db struc (coll.pl) : e.g.
% coll(lemma('abandonment'),pos(n),coll(['property','right'])).
% here, contrary to what we get through metameet,
% the two items are related if they POSSESS common elements
% in their collocate lists
% in metameet it is the copresence within a collocate list
% (associated with whatever item) that is significant

collmeet(Word1,Pos1,Word2,Pos2,CollWeight) :-
     findall(Coll,
             coll(lemma(Word1),pos(Pos1),coll(Coll)),
             CollList1),
     findall(Coll2,
             coll(lemma(Word2),pos(Pos2),coll(Coll2)),
             CollList2),
     flatten(CollList1, CL1),
     flatten(CollList2, CL2),
     (is_set(CL1) -> Set1=CL1 ; list_to_set(CL1,Set1)),
     (is_set(CL2) -> Set2=CL2 ; list_to_set(CL2,Set2)),
     (member(Word1,Set2) -> Bonus1=5 ; Bonus1=0 ),
     (member(Word2,Set1) -> Bonus2=5 ; Bonus2=0 ),
     intersection(Set1,Set2,Res),
     length(Res,Len),
     (Len <3 -> CW is Len*2 ; CW is Len*4),
       % cf indicator
     sumlist([CW,Bonus1,Bonus2],CollWeight),
     !.

collmeet(_W,_P,_W2,_P2,0).

%-------------------------------------------------------------------------

% Connectedness through envir sharing in RC/OH envir data base

% db struc (envir.pl) : e.g. e(hdwd('dative'),envir(['case','ending'])).

% the POS are not significant here

envirmeet(Word1,Word2,EnvirWeight) :-
     findall(Envir,
             e(hdwd(Word1),envir(Envir)),
             EnvirList1),
     findall(Envir2,
             e(hdwd(Word2),envir(Envir2)),
             EnvirList2),
     flatten(EnvirList1, EL1),
     flatten(EnvirList2, EL2),
     (is_set(EL1) -> Set1=EL1 ; list_to_set(EL1,Set1)),
     (is_set(EL2) -> Set2=EL2 ; list_to_set(EL2,Set2)),
     (member(Word1,Set2) -> Bonus1=2 ; Bonus1=0 ),
     (member(Word2,Set1) -> Bonus2=2 ; Bonus2=0 ),
     intersection(Set1,Set2,Res),
     length(Res,Len),
     (Len >1 -> EW is Len/2; EW is 0),
     % we neglect an envir sharing limited to a single element 
              % (probably non-significant most of the time)
     sumlist([EW,Bonus1,Bonus2],EnvirWeight),
     !.

envirmeet(_W,_W2,0).

%-------------------------------------------------------------------------

%------------
% Utilities : 
%------------

% allmembers(Subset,List)
allmembers([],_).
allmembers([H|Tail],List) :- member(H,List), allmembers(Tail,List).

% anymember(Candidates,List)
anymember(L,L1) :- intersection(L,L1,[_|_]).

% writing to both file and standard output
%------------------------------------------

writeqb(FO,X) :- writeq(FO,X), writeq(X).
writeb(FO,X) :- write(FO,X), write(X).
nlb(FO) :- nl(FO), nl.
tabb(FO,V) :- tab(V), tab(FO,V).

%---------------------------------------------------------------------------
% erasing recorded info
%----------------------

% erasing all items in a 'box'

eraseall(X) :-
     recorded(X,_,Ref),
     erase(Ref),
     fail.
     
eraseall(_).         

%------------------------------------------------------------------------
% erasing all items sharing a property
%--------------------------------------

% here : having the same weight (recorded as Max)

deleteref1(Max) :-
  recorded(res,
           str([_Output,local,Max,_Word1,_Idnum1,_Pos1,
                _Word2,_Idnum2,_Pos2,
                _ResLabs,_WeightLabs,
                _ResGws,_WeightGws,
                _ResDeflex,_WeightDeflex,
                _ResExlex,_WeightExlex]),
          Ref),
  erase(Ref),
  fail.

deleteref1(_Max).

deleteref2(Max) :-
  recorded(entries,
           mono(lem(_Word),
                ori(Max),
                idnum(_Idnum),
                pos(_Pos),
                lab(_Lablist),
                gw(_Gwlist),
                deflex(_Deflex),
                exlex(_Exlex),
                def(_DefW) ),
           Ref),
  erase(Ref),
  fail.

deleteref2(_Max).

pick(H,[H|T],T) :- !.
pick(H,[Other|T],[Other|T1]) :- pick(H,T,T1).

store(X) :- not(recorded(entries,X,_)),  recorda(entries,X,_).

% ordering of Idnums
myorder(A,B) :-  name(A,ListA), name(B,ListB), orderlist(ListA,ListB).
orderlist([Aprem|Areste],[Aprem|Breste]) :- orderlist(Areste,Breste).
orderlist([Aprem|Areste],[Bprem|Breste]) :- Aprem < Bprem.
orderlist([],_).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DIC MAINTENANCE AND STREAMLINING
% looking for candidates for a merge ('w:m' option)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% here we are dealing with a single lexeme-pos pair
% possibly giving rise to a whole bunch of entries in semdic
% as semdic takes over four monolinguals describing a similar lexical range
% (cide, cobuild, ldoce, wordnet)
% we investigate which pair of lex-pos/lex-pos
% yields the best match
% the successful pair is deemed to be a good candidate
% for an inter-dic and/or across-dic merge

% the process is meant to be executed recursively
% until the best l-p pair is not good enough to justify a merge
% this should be established through trial and error


dealwith([Word,pos:Pos,Word,pos:Pos,w:m,m:l,noadjust],Output) :-
     w(lem(Word),weight(LW)),
    %  LW = 1,
          % give merge proposals only for suitably heavy or light items
      eraseall(weights),
      eraseall(entries),
      recorda(weights,max(16),_),
          % we start with maximum weight (re)set
          % and make sure we have 'forgotten' data belonging to a previous pair
      dwmerge([Word,Pos,Word,Pos,w:m,m:l,noadjust],Output).


% dwmerge will come up with the best match
% for a given pair lex/pos across the dics making up semdic

% all info existentially quantified except:
% Word    Pos  Idnum
dwmerge([Word,Pos,Word,Pos,w:m,m:l,noadjust],Output) :-
    setof(data(Lablist1,Gwlist1,Deflex1,Exlex1,DefW1),
            DefW1^Dic1^Lablist1^Gwlist1^Deflex1^Exlex1^(mono(lem(Word),
             ori(Dic1),
             idnum(Idnum1),
             pos(Pos),
             lab(Lablist1),
             gw(Gwlist1),
             deflex(Deflex1),
             exlex(Exlex1),
             def(DefW1) )),
           DL1) ,

    setof(data(Lablist2,Gwlist2,Deflex2,Exlex2,DefW2),
         DefW2^Dic2^Lablist2^Gwlist2^Deflex2^Exlex2^(mono(lem(Word),
             ori(Dic2),
             idnum(Idnum2),
             pos(Pos),
             lab(Lablist2),
             gw(Gwlist2),
             deflex(Deflex2),
             exlex(Exlex2),
             def(DefW2) )),
         DL2) ,


   Idnum1 \= Idnum2,

   ( myorder(Idnum1,Idnum2) ->
                      concat_atom([Idnum1,Idnum2],'>',IdNum);
                      concat_atom([Idnum2,Idnum1],'>',IdNum)),
                      
                      % choose the appropriate separator for a given merge
                      % e.g. among "/ # & * @ > £" etc.

   collect(DL1,Lab1,Gw1,Def1,Ex1,DS1),
   collect(DL2,Lab2,Gw2,Def2,Ex2,DS2),

   flatten(Lab1,L1),
   flatten(Lab2,L2),
   flatten(Gw1,G1),
   flatten(Gw2,G2),
   flatten(Def1,D1),
   flatten(Def2,D2),
   flatten(Ex1,E1),
   flatten(Ex2,E2),
   flatten(DS1,DT1),
   flatten(DS2,DT2),
   (DT1 \= DT2 -> append(DT1,DT2,DefString); DefString=DT1),
   (L1 \= L2 -> (append(L1,L2,Et), remdup(Et,Labels)); Labels=L1),
   (G1 \= G2 -> (append(G1,G2,Guides), remdup(Guides, GuideWords));
                 GuideWords=G1),

   compute(lb,L1,L2,ResLabs,WeightLabs),
   compute(gw,G1,G2,ResGws,WeightGws),

   cw(def,Word,Word,D1,D2,ResDeflex,WeightDeflex),
   cw(ex,Word,Word,E1,E2,ResExlex,WeightExlex),

   sumlist([WeightLabs,WeightGws,WeightDeflex,WeightExlex],W),

   ( ( recorded(weights,max(Max),_),
     Max > W
     )
     ->
     true
     ;
     (recorded(weights,max(Max),Ref),
      (   W=Max -> store(mono(lem(Word),
                               ori(W),  % we use the ori field to store the
                                        % the weight of the merge
                               idnum(IdNum),
                               pos(Pos),
                               lab(Labels),
                               gw(GuideWords),
                               deflex(ResDeflex),
                               exlex(ResExlex),
                               def(DefString) ))
                   ;
                   (   erase(Ref),
                   recorda(weights,max(W),_),
                   deleteref2(Max),
                   store(mono(lem(Word),
                               ori(W),
                               idnum(IdNum),
                               pos(Pos),
                               lab(Labels),
                               gw(GuideWords),
                               deflex(ResDeflex),
                               exlex(ResExlex),
                               def(DefString) ))
                   )

     )
    )
   ),fail.


dwmerge([W,P,W,P,w:m,m:l,noadjust],Output) :-

      setof(Entry,
            recorded(entries,Entry,_),
            Entries),
      kill_them(Entries),
      output_them(Entries,Output),!.


output_them([],_).
output_them([Entry|MoreEntries],L) :- output_it(Entry,L),
                                    output_them(MoreEntries,L).

output_it(Entry,Stream) :- nlb(Stream),
                           writeqb(Stream,Entry),
                           writeb(Stream,'.').


kill_them([]).
kill_them([mono(lem(_),ori(_),idnum(IdNum),pos(_),lab(_),gw(_),deflex(_),
            exlex(_),def(_))|MoreEntries]) :-
            concat_atom([Idnum1,Idnum2],'>',IdNum),  % retrieve the 2 idnums
                                                     % of the merged entries
            assertz(kill(Idnum1)),
            assertz(kill(Idnum2)),
            kill_them(MoreEntries).





% we show only the top 3 pairings ('w:t3' option)

dealwith([Word1,pos:Pos1,Word2,pos:Pos2,w:t3,m:l,_noadjust],Output) :-
         % eraseall(top),
         dwt3([Word1,Pos1,Word2,Pos2,w:t3,m:l,noadjust],Output).

dwt3([Word1,Pos1,Word2,Pos2,w:t3,m:l,noadjust],Output) :-
    setof(data(Lablist1,Gwlist1,Deflex1,Exlex1),
          Def^Dic^Lablist1^Gwlist1^Deflex1^Exlex1^(mono(lem(Word1),
             ori(Dic),
             idnum(Idnum1),
             pos(Pos1),
             lab(Lablist1),
             gw(Gwlist1),
             deflex(Deflex1),
             exlex(Exlex1),
             def(Def) )),
           DL1) ,

    setof(data(Lablist2,Gwlist2,Deflex2,Exlex2),
         Def^Dic2^Lablist2^Gwlist2^Deflex2^Exlex2^(mono(lem(Word2),
             ori(Dic2),
             idnum(Idnum2),
             pos(Pos2),
             lab(Lablist2),
             gw(Gwlist2),
             deflex(Deflex2),
             exlex(Exlex2),
             def(Def) )),
         DL2) ,

   collect(DL1,Lab1,Gw1,Def1,Ex1),
   collect(DL2,Lab2,Gw2,Def2,Ex2),

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


   cw(def,Word1,Word2,D1,D2,ResDeflex,WeightDeflex),
   cw(ex,Word1,Word2,E1,E2,ResExlex,WeightExlex),

   sumlist([WeightLabs,WeightGws,WeightDeflex,WeightExlex],W),

   Wneg is -W,
   recorda(top, Wneg-p(Word1,Idnum1,Word2,Idnum2),_),
   fail.



% when all pairs have been recorded
% we can gather them all together with a findall clause on the recorded pairs
% and report on the results
% we end up by cleaning, i.e. erasing all recorded information

dwt3([W1,P1,W2,P2,w:t3,m:l,noadjust],Output) :-
                             findall(Pair,
                                    recorded(top,Pair,_),
                                   Pairs),
                              sort(Pairs,Sorted),
                              top(Sorted,Top),
                              reportraw(Top,Output),
                              reporttop(Top,Output),
                              eraseall(top),!.
                              
dwt3([W1,P1,W2,P2,w:t3,m:l,noadjust],Output) :-
      reportnil(Out), !.

top([Wa-p(W1a,I1a,W2a,I2a),Wb-p(W1b,I1b,W2b,I2b),Wc-p(W1c,I1c,W2c,I2c)|R],
     [Wa-p(W1a,I1a,W2a,I2a),Wb-p(W1b,I1b,W2b,I2b),Wc-p(W1c,I1c,W2c,I2c)]) :- !.
     
top([Wa-p(W1a,I1a,W2a,I2a),Wb-p(W1b,I1b,W2b,I2b)|R],
     [Wa-p(W1a,I1a,W2a,I2a),Wb-p(W1b,I1b,W2b,I2b)]) :- !.
     
top([Wa-p(W1a,I1a,W2a,I2a)|R],
     [Wa-p(W1a,I1a,W2a,I2a)]) :- !.

top([],[]).

reportnil(Out) :- nlb(Out),writeb(Out,'No match'), nlb(Out).

reportraw(T,Out) :- nlb(Out), writeqb(Out,T),  writeb(Out,'.'), nlb(Out).

reporttop([],Out).
reporttop([Top|MoreTops],Out) :- reportit(Top,Out),
                                 reporttop(MoreTops,Out).
                                 
                                 
reportit(Weight-p(W1,I1,W2,I2),Out) :-
         nlb(Out),
         Wneg is -Weight,
         writeb(Out,Wneg), writeb(Out,'  for the pair: '),
         nlb(Out),
         writeb(Out,W1),
         tabb(Out,1),
         writeb(Out,I1),
         tabb(Out,8),
         writeb(Out,W2),
         tabb(Out,1),
         writeb(Out,I2),
         nlb(Out),
         writeb(Out, 'i.e. the following entries : '),
         nlb(Out),
         show(I1,Out),
         show(I2,Out).
         
         
remdup([],[]).
remdup([X|Y],L) :-     member(X,Y),
                       remdup(Y,L),!.

remdup([X|Y],[X|Y1]) :- \+ member(X,Y) ,
                         remdup(Y,Y1),
                         !.
                         
                         
                         
% Dealing with Triplets
%%%%%%%%%%%%%%%%%%%%%%%%

% e.g. t(wear,v,tie,n,watch,n)

% the idea is to select the Word-Idnum pairs that appear
% in more than one relation
% for instance we choose the reading for wear that appears in both
% 'wear,v tie,n' and 'wear,v watch,n'
% similarly we choose the reading for watch that appears in both
% 'wear,v watch,n' and 'tie,n watch,n'

% we give the top 3 pairings for all relations
% and then show the Idnums that satisfy the requirement just discussed


dealwith(t(Target,PosT,Arrow1,PosArg,Arrow2,PosArg),L) :-
   best3(Target,PosT,Arrow1,PosArg, Top1),
   best3(Target,PosT,Arrow2,PosArg, Top2),
   best3(Arrow1,PosArg,Arrow2,PosArg, Top3),
   nlb(L), writeb(L,Top1), nlb(L), writeb(L,Top2), nlb(L),
   writeb(L,Top3), nlb(L), nlb(L),
   
   select_target(Target-IdTarg,Top1,Top2,FS1),
   select_arg1(Arrow1-IdA1,Top1,Top3,FS2),
   select_arg2(Arrow2-IdA2,Top2,Top3,FS3),
   
   (FS1=yes -> show(IdTarg,L); true),
   (FS2=yes -> show(IdA1,L); true),
   (FS3=yes -> show(IdA2,L); true),
    !.
   

select_target(Lemma-Idnum,L1,L2,yes) :-
      member(W1-p(Lemma,Idnum,_,_),L1),
      member(W2-p(Lemma,Idnum,_,_),L2),!.

select_target(Lemma-Idnum,L1,L2,no).

select_arg1(Lemma-Idnum,L1,L2,yes) :-
      member(W1-p(_,_,Lemma,Idnum),L1),
      member(W2-p(Lemma,Idnum,_,_),L2), !.
      
select_arg1(Lemma-Idnum,L1,L2,no).

select_arg2(Lemma-Idnum,L1,L2,yes) :-
      member(W1-p(_,_,Lemma,Idnum),L1),
      member(W2-p(_,_,Lemma,Idnum),L2),!.

select_arg2(Lemma-Idnum,L1,L2,no).


best3(Word1,Pos1,Word2,Pos2,_Top) :-
    setof(data(Lablist1,Gwlist1,Deflex1,Exlex1),
          Def^Dic^Lablist1^Gwlist1^Deflex1^Exlex1^(mono(lem(Word1),
             ori(Dic),
             idnum(Idnum1),
             pos(Pos1),
             lab(Lablist1),
             gw(Gwlist1),
             deflex(Deflex1),
             exlex(Exlex1),
             def(Def) )),
           DL1) ,

    setof(data(Lablist2,Gwlist2,Deflex2,Exlex2),
         Def^Dic2^Lablist2^Gwlist2^Deflex2^Exlex2^(mono(lem(Word2),
             ori(Dic2),
             idnum(Idnum2),
             pos(Pos2),
             lab(Lablist2),
             gw(Gwlist2),
             deflex(Deflex2),
             exlex(Exlex2),
             def(Def) )),
         DL2) ,

   collect(DL1,Lab1,Gw1,Def1,Ex1),
   collect(DL2,Lab2,Gw2,Def2,Ex2),

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


   cw(def,Word1,Word2,D1,D2,ResDeflex,WeightDeflex),
   cw(ex,Word1,Word2,E1,E2,ResExlex,WeightExlex),

   sumlist([WeightLabs,WeightGws,WeightDeflex,WeightExlex],W),

   Wneg is -W,
   recorda(top, Wneg-p(Word1,Idnum1,Word2,Idnum2),_),
   fail.



% when all pairs have been recorded
% we can gather them all together with a findall clause on the recorded pairs
% and report on the results
% we end up by cleaning, i.e. erasing all recorded information

best3(W1,P1,W2,P2,Top) :-
                             findall(Pair,
                                    recorded(top,Pair,_),
                                   Pairs),
                              sort(Pairs,Sorted),
                              top(Sorted,Top),
                              eraseall(top),!.



% COMPUTING LEXDIS WEIGHT
%%%%%%%%%%%%%%%%%%%%%%%%%%

% to compute the LEXDIS weight of all semdic entries
% execute the query : w(W,P).

dealwith(w(Word,Pos),Output) :-

    setof(data(Lablist1,Gwlist1,Deflex1,Exlex1),
          Def^Dic^Idnum^Lablist1^Gwlist1^Deflex1^Exlex1^(mono(lem(Word),
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
         Def^Dic2^Idnum2^Lablist2^Gwlist2^Deflex2^Exlex2^(mono(lem(Word),
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

% we distribute the data just collected : labels, guidewords,
%                                     definition core items, example core items

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


   cw(def,Word,Word,D1,D2,ResDeflex,WeightDeflex),
   cw(ex,Word,Word,E1,E2,ResExlex,WeightExlex),

   % idem for definition core items and example core items

   metameet(Word,Word,Metaweight),
   rogetmeet(Word,Word,Rogetweight),
   indicmeet(Word,Pos,Word,Pos,Indicweight),
   collmeet(Word,Pos,Word,Pos,Collweight),
   envirmeet(Word,Word,Envirweight),

   % weight assignation to collocate field sharing, Roget's category sharing,
   % indicator sharing, collocate sharing, sharing of environment

   sumlist([WeightLabs,WeightGws,WeightDeflex,WeightExlex,
         Metaweight,Rogetweight,Indicweight,Collweight,Envirweight],GW),

    % writing doc file
   % swritef(Towrite,'%60l;%15l;%15l\n',[Word,Pos,GW]),
   % writeb(Output,Towrite).

    % writing weights file
    Entry= w(Word,Pos,GW),
    output_it(Entry,Output).

%-----------------------------------------------------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%------------------------------------------END--------------------------------
