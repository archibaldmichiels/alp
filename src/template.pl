
% TEMPLATES
%%%%%%%%%%%

% template.pl


% Latest Update : December 2024
% © Archibald Michiels
% amichiels@uliege.be

% a SWI-Prolog program 
% https://www.swi-prolog.org/

% This file is read in by alpxxx.pl
% and provides templates and entries for verbs





%%%%%%%%
% VERBS
%%%%%%%%

/*
The lexarg structure associates the arg pattern of the verb with its lemma form.
A verb may include a number of different arglists, sometimes corresponding to different
readings of the verb - it all depends on the semantic granularity we want to achieve.
There is in the arglist a slot ('clause:List') reserved for storing constraints set on the clause
at the level of the whole clause, for instance polarity requirements set by multi-word units
(haud magni facere / non pili facere, etc).

Each arg is declared obligatory or optional ; only optional args can be skipped by the 'match' procedure
applied to each arg, whose task is to work out which bit of the input string can be parsed as a textual embodiment
of the argument.

The type of the arg indicates the nature of the string we are looking for to fill the arg position (np etc.).

The 'constraints' can store any constraint we wish to impose on the arg filler.

The 'mwuw' feature records a bonus value associated with the multi-word unit reading of a syntactic configuration.

Note that the value of the sem feature must be a list, even if the list is to contain one element only
(the most usual case).

In the morphological info slot, a row of three xs (xxx) means that the form is missing - no word forms will be
produced that need the missing root.

We make use of this facility, accompanied by the distinction between standard and deponent verbs, to account for the behaviour
of 'coepi' or 'audere':

verb([v(coepi,3,xxx,coept)],tr_cod,dep).    % treated as a deponent - semantically preserves 'active' voice
                                            % generates the 'coeptus' of 'coeptus sum' and friends, with 'active' meaning

verb([v(coepi,3,xxx,coep,xxx)],tr_cod,std). % treated as lacking everything except perfect tense and derived variants
                                            % enables the generation of 'coeperam', 'coepisse', etc.

To claim that 'coepi' is used in the passive voice with a passive infinitive does not seem to make much sense,
pace Jacques Michel, Grammaire de base du latin,
$ 257 : "Coepi prend la voix de l'infinitif avec lequel il se construit".

Marius Lavency (VSVS, $ 285)  is more cautious:
"Les verbes coepisse, 'commencer', desisse, 'cesser', prennent des formes passives
lorsque l'infinitif qui les complète est lui-même passif."

The treatment proposed here seems adequate for PARSING purposes (in a generator it would overgenerate, as a parsing component
it allows the 'active' reading of coepi to be maintained). I don't know that the following is interpretable if a passive VOICE is
insisted on: 'Cum procinctae igitur classes erant et instructa acies coeptumque in hostem progredi,
tibicines inter exercitum positi canere inceptabant.' Aulus Gellius, Noctes Atticae 1.11.pr.1.

The point is that inchoatives such as 'coepi' simply do not take part in the assignment of voice.
Their aspectual value is transparent with respect to diathesis.

*/

verb([v(abhorrere,2,abhorre,abhorru,xxx)],intr,std).
verb([v(abire,4,xxx,abi,abit)],intr,std).
verb([v(accipere,5,accip,accep,accept)],tr_cod,std).
verb([v(adipisci,3,adipisc,adept)],tr_cod,dep).
verb([v(adducere,3,adduc,addux,adduct)],tr_cod,std).
verb([v(adesse,3,xxx,adfu,xxx)],intr,std). 
% verb([v(adfligere,3,adflig,adflix,adflict)],tr_cod,std).
verb([v(affligere,3,afflig,afflix,afflict)],tr_cod,std).
verb([v(adimere,3,adim,adem,adempt)],tr_cod,std).
verb([v(adhibere,2,adhibe,adhibu,adhibit)],tr_cod,std).          
verb([v(agere,3,ag,eg,act)],tr_cod,std).
verb([v(agitare,1,agit,agitau,agitat)],intr,std).
verb([v(alere,3,al,alu,alt)],tr_cod,std).
verb([v(amare,1,am,amau,amat)],tr_cod,std).
verb([v(ambulare,1,ambul,ambulau,ambulat)],intr,std).
verb([v(amittere,3,amitt,amis,amiss)],tr_cod,std).
verb([v(angere,3,ang,xxx,xxx)],tr_cod,std).
verb([v(anteponere,3,antepon,anteposu,anteposit)],tr_cod,std).
verb([v(appellare,1,appell,appellau,appellat)],tr_cod,std).
verb([v(audere,2,aude,xxx,xxx)],tr_cod,std).
verb([v(audere,2,xxx,aus)],tr_cod,dep).
verb([v(augere,2,auge,aux,auct)],tr_cod,std).
verb([v(audire,4,audi,audiu,audit)],tr_cod,std).
verb([v(cadere,3,cad,cecid,cas)],intr,std).
verb([v(canere,3,can,cecin,cant)],tr_cod,std).
verb([v(capere,5,cap,cep,capt)],tr_cod,std).
verb([v(cedere,3,ced,cess,cess)],intr,std).
verb([v(celebrare,1,celebr,celebrau,celebrat)],tr_cod,std).
verb([v(cenare,1,cen,cenau,cenat)],intr,std).
verb([v(censere,2,cense,censu,cens)],tr_cod,std).
verb([v(clamare,1,clam,clamau,clamat)],tr_cod,std).
verb([v(claudere,3,claud,claus,claus)],tr_cod,std).
verb([v(coepi,3,xxx,coep,xxx)],tr_cod,std). 			% treated as lacking everything except perfect tense and derived variants
verb([v(coepi,3,xxx,coept)],tr_cod,dep).    			% treated as a deponent - semantically preserves 'active' voice
verb([v(cogere,3,cog,coeg,coact)],tr_cod,std).
verb([v(cogitare,1,cogit,cogitau,cogitat)],tr_inf,std).
verb([v(cognoscere,3,cognosc,cognou,cognit)],tr_cod,std).
verb([v(colere,3,col,colu,cult)],tr_cod,std).
verb([v(conciliare,1,concili,conciliau,conciliat)],tr_cod_coi,std).
verb([v(concupiscere,3,concupisc,concupiu,concupit)],tr_cod,std).
verb([v(conscire,4,consci,consciu,conscit)],tr_cod,std).
verb([v(consultare,1,consult,consultau,consultat)],intr,std).
verb([v(contemnere,3,contemn,contemps,contempt)],tr_cod,std).
verb([v(credere,3,cred,credid,credit)],tr_inf,std).
verb([v(cupere,5,cup,cupiu,cupit)],tr_cod,std).
verb([v(curare,1,cur,curau,curat)],intr,std).
verb([v(dare,1,d,ded,dat)],tr_cod_coi,std).
verb([v(debere,2,debe,debu,debut)],tr_cod,std).
verb([v(deesse,3,xxx,defu,xxx)],intr,std). 
verb([v(deligere,3,delig,deleg,delect)],tr_cod,std).
verb([v(delere,2,dele,deleu,delet)],tr_cod,std).
verb([v(dementare,1,dement,dementau,dementat)],tr_cod,std).
verb([v(desiderare,1,desider,desiderau,desiderat)],tr_cod,std).
verb([v(desinere,3,desin,desi,desit)],tr_cod,std).
verb([v(dicere,3,dic,dix,dict)],tr_inf,std).
verb([v(differre,3,xxx,distul,dislat)],tr_cod,std).
verb([v(dimicare,1,dimic,dimicau,dimicat)],intr,std).
verb([v(praedicere,3,praedic,praedix,praedict)],tr_inf,std).
verb([v(diligere,3,dilig,dilex,dilect)],tr_cod,std).
verb([v(diripere,5,dirip,diripu,dirept)],tr_cod,std).
verb([v(disperire,4,xxx,disperi,disperit)],intr,std).
verb([v(docere,2,doce,docu, doct)],tr_cod_coi,std).
verb([v(donare,1,don,donau,donat)],tr_cod_coi,std).
verb([v(dubitare,1,dubit,dubitau,dubitat)],intr,std).
verb([v(ducere,3,duc,dux,duct)],tr_cod,std).
verb([v(educere,3,educ,edux,educt)],tr_cod,std).
verb([v(eiicere,5,eiic,eiec,eiect)],tr_cod,std).  
verb([v(eligere,3,elig,eleg,elect)],tr_cod,std).
verb([v(eripere,5,erip,eripu,erept)],tr_cod,std).  
verb([v(errare,1,err,errau,errat)],intr,std).
verb([v(excipere,5,excip,excep,except)],tr_cod,std).
verb([v(excludere,3,exclud,exclus,exclus)],tr_cod,std).
verb([v(exhaurire,4,exhauri,exhaus,exhaust)],tr_cod,std).
verb([v(existimare,1,existim,existimau,existimat)],tr_inf,std).
verb([v(facere,5,fac,fec,fact)],tr_cod,std).
verb([v(satisfacere,5,satisfac,satisfec,satisfact)],tr_cod,std).
verb([v(fallere,3,fall,fefell,fals)],tr_cod,std).
verb([v(ferire,4,feri,xxx,xxx)],tr_cod,std).
verb([v(ferre,3,xxx,tul,lat)],tr_cod,std).
verb([v(frangere,3,frang,freg,fract)],tr_cod,std).
verb([v(fugere,5,fug,fug,fugit)],intr,std).
verb([v(gaudere,2,gaude,xxx,xxx)],intr,std).
verb([v(gaudere,2,xxx,gauis)],tr_cod,dep).
verb([v(gerere,3,ger,gess,gest)],tr_cod,std).
verb([v(habere,2,habe,habu,habit)],tr_cod,std).
verb([v(hortari,1,hort,hortat)],tr_cod,dep).
verb([v(iacere,2,iace,iacui,xxx)],intr,std).
verb([v(ignoscere,3,ignosc,ignou,ignot)],tr_cod,std).
verb([v(imponere,3,impon,imposu,imposit)],tr_cod,std).
verb([v(incipere,5,incip,incep,incept)],tr_cod,std).
verb([v(indicare,1,indic,indicau,indicat)],tr_cod,std).
verb([v(ineptire,4,inepti,xxx,xxx)],intr,std).
verb([v(insanire,4,insani,insaniu,insanit)],intr,std).
verb([v(intellegere,3,intelleg,intellex,intellect)],tr_cod,std).
verb([v(intendere,3,intend,intend,intent)],tr_cod,std).
verb([v(interdicere,3,interdic,interdix,interdict)],tr_cod_cplt,std).
verb([v(interficere,5,interfic,interfec,interfect)],tr_cod,std).
verb([v(inuenire,4,inueni,inuen,inuent)],tr_cod,std).
verb([v(ire,4,xxx,i,it)],intr,std).
verb([v(transire,4,xxx,i,it)],tr_cod,std).
verb([v(iubere,2,iube,iuss,iuss)],tr_inf,std).
verb([v(iudicare,1,iudic,iudicau,iudicat)],tr_inf,std).
verb([v(iungere,3,iung,iunx,iunct)],tr_cod,std).
verb([v(adiungere,3,adiung,adiunx,adiunct)],tr_cod,std).
verb([v(iuuare,1,iuu,iuu,iut)],tr_cod,std).
verb([v(laudare,1,laud,laudau,laudat)],tr_cod,std).
verb([v(legere,3,leg,leg,lect)],tr_cod,std).
verb([v(linquere,3,linqu,liqu,lict)],tr_cod,std).
verb([v(loqui,3,loqu,locut)],intr,dep).
verb([v(ludere,3,lud,lus,lus)],intr,std).
verb([v(lugere,2,luge,lux,luct)],tr_cod,std).
verb([v(maerere,2,maere,xxx,xxx)],intr,std).
verb([v(manere,2,mane,mans,mans)],intr,std).
verb([v(maturare,1,matur,maturau,maturat)],tr,std).
verb([v(memorare,1,memor,memorau,memorat)],tr_cod,std).
verb([v(metuere,3,metu,metu,metut)],tr_cod,std).
verb([v(minuere,3,minu,minu,minut)],tr_cod,std).
verb([v(miscere,2,misce,miscu,mixt)],tr_cod,std).
verb([v(mittere,3,mitt,mis,miss)],tr_cod_coi,std).
verb([v(mollire,4,molli,molliui,mollit)],tr_cod,std).
verb([v(monere,2,mone,monu,monit)],tr_cod,std).
verb([v(morari,1,mor,morat)],tr_cod,dep).
verb([v(mori,5,mor,mortu)],intr,dep).
verb([v(mouere,2,moue,mou,mot)],tr_cod,std).
verb([v(permouere,2,permoue,permou,permot)],tr_cod,std).
verb([v(mutare,1,mut,mutau,mutat)],tr_cod,std).
verb([v(commutare,1,commut,commutau,commutat)],tr_cod,std).
verb([v(narrare,1,narr,narrau,narrat)],tr_cod,std).
verb([v(nasci,3,nasc,nat)],intr,dep).
verb([v(negare,1,neg,negau,negat)],tr_inf,std).
verb([v(nescire,4,nesci,nesciu,nescit)],tr_inf,std).
verb([v(niti,3,nit,nis)],intr,dep).
verb([v(noscere,3,nosc,nou,xxx)],tr_cod,std).
verb([v(notare,1,not,notau,notat)],tr_cod,std).
verb([v(numerare,1,numer,numerau,numerat)],tr_cod,std).
verb([v(nuntiare,1,nunti,nuntiau,nuntiat)],tr_inf,std).
verb([v(obiectare,1,obiect,obiectau,obiectat)],tr_cod,std).
verb([v(obliuisci,3,obliuisc,oblit)],tr_cod,dep).
verb([v(obniti,3,obnit,obnix)],intr,dep).
verb([v(obtemperare,1,obtemper,obtemperau,obtemperat)],intr,std).
verb([v(occidere,3,occid,occid,occis)],tr_cod,std).
verb([v(occurrere,3,occurr,occurr,occurs)],intr,std).
verb([v(olfacere,5,olfac,olfec,olfact)],tr_cod,std).
verb([v(oppugnare,1,oppugn,oppugnau,oppugnat)],tr_cod,std).
verb([v(optare,1,opt,optau,optat)],tr_cod,std).
verb([v(orare,1,or,orau,orat)],tr_cod,std).
verb([v(ostendere,3,ostend,ostend,ostent)],tr_cod,std).
verb([v(parare,1,par,parau,parat)],tr_cod,std).
verb([v(parcere,3,parc,peperc,pars)],tr_cod,std).
verb([v(pati,5,pat,pass)],tr_cod,dep).
verb([v(pellere,3,pell,pepul,puls)],tr_cod,std).
verb([v(perdere,3,perd,perdid,perdit)],tr_cod,std).
verb([v(perficere,5,perfic,perfec,perfect)],tr_cod,std).
verb([v(perire,4,xxx,peri,perit)],intr,std).
verb([v(perspicere,5,perspic,perspex,perspect)],tr_cod,std).
verb([v(peruenire,4,peruen,peruen,peruent)],intr,std).
verb([v(petere,3,pet,petiu,petit)],tr_cod,std).
verb([v(placare,1,plac,placau,placat)],tr_cod,std).
verb([v(placere,2,place,placu,placit)],tr_coi,std).
verb([v(ponere,3,pon,posu,posit)],tr_cod,std).
verb([v(possidere,2,posside,possed,possess)],tr_cod,std).
verb([v(postulare,1,postul,postulau,postulat)],tr_cod,std).
verb([v(praebere,2,praebe,praebu,praebit)],tr_cod,std).
verb([v(praestare,1,praest,praestit,praestat)],intr,std).
verb([v(premere,3,prem,press,press)],tr_cod,std).
verb([v(procurare,1,procur,procurau,procurat)],tr_cod,std).
verb([v(proficisci,3,proficisc,profect)],intr,dep).
verb([v(pugnare,1,pugn,pugnau,pugnat)],intr,std).
verb([v(putare,1,put,putau,putat)],tr_inf,std).
verb([v(quaerere,3,quaer,quaesiu,quaesit)],tr_cod,std).
verb([v(quatere,5,quat,xxx,quass)],tr_cod,std).
verb([v(queri,3,quer,quest)],tr_cod,dep).
verb([v(rapere,5,rap,rapu,rapt)],tr_cod,std).
verb([v(recipere,5,recip,recep,recept)],tr_cod,std).
verb([v(recubare,1,recub,recubau,recubat)],intr,std).
verb([v(reddere,3,redd,redid,redit)],tr_cod,std).
verb([v(redire,4,xxx,redi,redit)],intr,std).
verb([v(referre,3,xxx,rettul,relat)],tr_cod,std).
verb([v(regnare,1,regn,regnau,regnat)],intr,std).
verb([v(relinquere,3,relinqu,reliqu,relict)],tr_cod,std).
verb([v(renouare,1,renou,renouau,renouat)],tr_cod,std).
verb([v(repudiare,1,repudi,repudiau,repudiat)],tr_cod,std).
verb([v(reuocare,1,reuoc,reuocau,reuocat)],tr_cod,std).
verb([v(reri,2,re,rat)],tr_inf,dep).
verb([v(respondere,2,responde,respond,respons)],tr_cod,std).
verb([v(restituere,3,restitu,restitu,restitut)],tr_cod,std).
verb([v(retinere,2,retine,retinu,retent)],tr_cod,std).
verb([v(rogare,1,rog,rogau,rogat)],tr_cod,std).
verb([v(salutare,1,salut,salutau,salutat)],tr_cod,std).
verb([v(scire,4,sci,sciu,scit)],tr_inf,std).
verb([v(scribere,3,scrib,scrips,script)],tr_cod,std).
verb([v(sedare,1,sed,sedau,sedat)],tr_cod,std).
verb([v(sentire,4,senti,sens,sens)],tr_cod,std).
verb([v(sequi_1,3,sequ,secut)],intr,dep).
verb([v(sequi_2,3,sequ,secut)],tr_cod,dep).
verb([v(serpere,3,serp,serps,xxx)],intr,std).
verb([v(seruare,1,seru,seruau,seruat)],tr_cod,std).
verb([v(sinere,3,sin,siu,sit)],tr_inf,std).
verb([v(solere,2,sole,xxx,xxx)],tr_cod,std).
verb([v(solere,2,xxx,solit)],tr_cod,dep).
verb([v(soluere,3,solu,solu,solut)],tr_cod,std).
verb([v(stare,1,st,stet,stat)],intr,std).
verb([v(studere,2,stude,studu,xxx)],tr_cod_dat,std).
verb([v(sudare,1,sud,sudau,sudat)],intr,std).
verb([v(suspendere,3,suspend,suspend,suspens)],tr_cod_coi,std).
verb([v(tacere,2,tace,tacu,tacit)],intr,std).
verb([v(tegere,3,teg,tex,tect)],tr_cod,std).
verb([v(tenere,2,tene,tenu,tent)],tr_cod,std).
verb([v(temptare,1,tempt,temptau,temptat)],tr_cod,std).
verb([v(tentare,1,tent,tentau,tentat)],tr_cod,std).
verb([v(timere,2,time,timu,xxx)],tr_cod,std).
verb([v(tollere,3,toll,sustul,sublat)],tr_cod,std).
verb([v(tradere,3,trad,tradid,tradit)],tr_inf,std).
verb([v(trahere,3,trah,trax,tract)],tr_cod,std).
verb([v(uenire,4,ueni,uen,uent)],intr,std).
verb([v(conuenire,4,conueni,conuen,conuent)],intr,std).
verb([v(uereri,2,uere,uerit)],tr_cod,dep).
verb([v(uersari,1,uers,uersat)],intr,dep).
verb([v(uertere,3,uert,uert,uers)],tr_cod,std).
verb([v(conuertere,3,conuert,conuert,conuers)],tr_cod,std).
verb([v(uidere,2,uide,uid,uis)],tr_cod,std).
verb([v(prouidere,2,prouide,prouid,prouis)],tr_cod,std).
verb([v(uideri,2,uide,uis)],intr,dep).
verb([v(uincere,3,uinc,uic,uict)],tr_cod,std).
verb([v(uitare,1,uit,uitau,uitat)],tr_cod,std).
verb([v(uiuere,3,uiu,uix,uict)],intr,std).
verb([v(uocare,1,uoc,uocau,uocat)],tr_cod,std).
verb([v(uolare,1,uol,uolau,uolat)],intr,std).
verb([v(usurpare,1,usurp,usurpau,usurpat)],tr_cod,std).
verb([v(uti,3,ut,us)],tr_cod_abl,dep).




/*

%%%%%%%%%%%%%%%%%%%
% VERB TEMPLATES
%%%%%%%%%%%%%%%%%%%

only some of the most frequent arg patterns are catered for in the following list

Intransitives with human subject
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lexarg(Xre,
       arglist:[ws(v_v,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]]])]).

Intransitives no constraint
%%%%%%%%%%%%%%%%%%%%%%%%%%%

lexarg(Xre,
       arglist:[ws(v_v,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]]])]).

Intransitive with infinitive cplt
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lexarg(Xre,
       arglist:[ws(v_v,intr,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     object:[type:pred,oblig:yes,constraints:[type:nonfinite_i]]])]).


Intransitives with prep cplt (prep specified) and human subject
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lexarg(Xre,
       arglist:[ws(v_v,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      prep_cplt:[type:pp,oblig:yes,constraints:[prep:PREP]]])]).

prep unspecified
%%%%%%%%%%%%%%%%

lexarg(Xre,
       arglist:[ws(v_v,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      prep_cplt:[type:pp,oblig:yes,constraints:[]]])]).



Simple transitive verbs with human subject
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lexarg(Xre,
       arglist:[
             ws(v_v,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:no,constraints:[case:acc]]])]).

human OBJECT
%%%%%%%%%%%%

lexarg(Xre,
       arglist:[ws(v_v,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:np,oblig:yes,constraints:[sem:[hum],case:acc]]])]).


with human dative object and human subject
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lexarg(Xre,
       arglist:[ws(v_v,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[sem:[hum],case:dat]]])]).


Transitive with prep cplt and human subject
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lexarg(Xre,
       arglist:[ws(v_v,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:no,constraints:[case:acc]],
                      prep_cplt:[type:pp,oblig:no,constraints:[prep:PREP]] ])]).


ab/ex alternative
%%%%%%%%%%%%%%%%%%

lexarg(Xre,
       arglist:[ws(v_v,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]],
                      prep_cplt:[type:pp,oblig:no,constraints:[prep:or([ab,ex])]] ])]).



tr with adjunct with specified CASE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


lexarg(Xre,
       arglist:[ws(v_v,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:no,constraints:[case:acc]],
                      adjunct:[type:np,oblig:no,constraints:[case:CASE]]])]).




verbs with nonfinite_i object (and human subject)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lexarg(Xre,
       arglist:[ws(v_v,tr_cod,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     object:[type:pred,oblig:yes,constraints:[type:nonfinite_i]]])]).


transitive with NP object or indirect question
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lexarg(Xre,
       arglist:[ws(v_v,tr_cod,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     object:[type:np,oblig:yes,constraints:[case:acc]]]),

                ws(v_v,tr_cod,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     object:[type:pred,oblig:yes,
                       constraints:[type:finite,gap:[],mood:subjunctive, checkint:yes]]])]).


with indirect object
%%%%%%%%%%%%%%%%%%%%

lexarg(Xre,
       arglist:[ws(v_v,tr_cod_coi,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      i_object:[type:np,oblig:no,case:dat,constraints:[case:dat, sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc, sem:[thing]]]])]).


with indirect object alone (human)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lexarg(Xre,
       arglist:[ws(v_v,tr_coi,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[]],
                     i_object:[type:np,oblig:yes,constraints:[case:dat, sem:[hum]]]])]).

with double acc
%%%%%%%%%%%%%%%%

lexarg(Xre,
       arglist:[ws(v_v,tr_cod_coi,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object_i:[type:np,oblig:yes,constraints:[case:acc, sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc, sem:[abstract]]]])]).


object and object cplt
%%%%%%%%%%%%%%%%%%%%%%%

lexarg(Xre,
       arglist:[
         % adj as object_cplt
                ws(v_v,tr_cod_cplt,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]],
                      object_cplt:[type:adjp,oblig:yes,constraints:[case:acc, type:std]]]),

         % np as object_cplt
                 ws(v_v,tr_cod_cplt,clause:[],mwuw:0,
                 args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                       object:[type:np,oblig:yes,constraints:[case:acc]],
                       object_cplt:[type:np,oblig:yes,constraints:[class:common,case:acc]]])]).


alternance
%%%%%%%%%%

lexarg(Xre,
       arglist:[ws(v_v,tr_cod_coi,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]],
                      i_object:[type:np,oblig:no,case:dat,constraints:[case:dat, sem:[hum]]]]),

                 ws(v_v,tr_cod_prep_cplt,clause:[],mwuw:0,
                 args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                       object:[type:np,oblig:yes,constraints:[case:acc]],
                       prep_cplt:[type:pp,oblig:no,constraints:[case:acc,sem:[hum], prep:ad]]])]).



acc cum infinitive
%%%%%%%%%%%%%%%%%%

lexarg(Xre,
       arglist:[ws(v_v,tr_inf,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:nonfinite]]])]).


with indirect object and finite clause object
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lexarg(Xre,
       arglist:[ws(v_v,tr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      i_object:[type:np,oblig:no,constraints:[case:acc]],
                      object:[type:pred,oblig:no,constraints:[type:finite,gap:[],mood:subjunctive,
                                                              argbound:yes, subordinator:or([ne,ut])]]])]).

with finite clause arg
%%%%%%%%%%%%%%%%%%%%%%%%

lexarg(Xre,
       arglist:[ws(v_v,tr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:no,constraints:[type:finite,gap:[],mood:subjunctive,
                                                              argbound:yes, subordinator:or([ne,ut])]]])]).

*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%
% VERB ENTRIES
%%%%%%%%%%%%%%



% ABHORRERE
% Imperatores timeo qui a pace abhorrent.
lexarg(abhorrere,
       arglist:[ws(abhorreo_detest,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      prep_cplt:[type:pp,oblig:yes,constraints:[prep:ab]]])]).
% ACCENDERE
lexarg(accendere,
       arglist:[ws(accendo_inflame,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).

% ACCIPERE
% Rex accepit dona ab hostibus.
lexarg(accipere,
       arglist:[ws(accipio_receive,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]],
                      prep_cplt:[type:pp,oblig:no,constraints:[prep:or([ab,ex])]] ])]).
% ADESSE
 lexarg(adesse,
     arglist:[ ws(adsum_assist,intr,clause:[],mwuw:0,
                 args:[subject:[type:np,oblig:yes,constraints:[]],
                       predicative:[type:np,oblig:no,constraints:[case:nom,class:common]],
                       cplt:[type:np,oblig:no,constraints:[case:dat]]]),

              ws(adsum_be_present,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      prep_cplt:[type:pp,oblig:no,constraints:[prep:in]]])]).

% AFFLIGERE
lexarg(affligere,
       arglist:[
             ws(adfligo_strike,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:np,oblig:yes,constraints:[case:acc,sem:[hum]]]])]).

% ADHIBERE
lexarg(adhibere,
       arglist:[
                 ws(adhibeo_use_as,tr_cod_cplt,clause:[],mwuw:0,
                 args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                       object:[type:np,oblig:yes,constraints:[sem:[hum],case:acc]],
                       object_cplt:[type:np,oblig:yes,constraints:[lex:or([patronus,dux,arbiter]),case:acc]]])]).

% ADICERE
lexarg(adicere,
       arglist:[ws(adicio_add,tr_cod_coi,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      i_object:[type:np,oblig:yes,constraints:[case:dat]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).

% ADIMERE
lexarg(adimere,
       arglist:[ws(adimo_deprive,tr_cod_coi,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      i_object:[type:np,oblig:no,case:dat,constraints:[case:dat, sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).
% ADIPISCI
lexarg(adipisci,
       arglist:[
             ws(adipiscor_obtain,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:no,constraints:[case:acc]]])]).


% AGERE
% Rem age.
% Ausus est in ecclesia tua agere negotium procurandi fructus mortis.
lexarg(agere,
       arglist:[ ws(negotium_ago_take_care_of_IDIOM,tr_cod,clause:[],mwuw:2,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc, lex:negotium]],
                      object_cplt:[type:pred, oblig:yes,constraints:[local_case:gen,type:gerund]] ]),

                ws(ago_be,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      prep_cplt:[type:pp,oblig:yes,constraints:[prep:in]] ]),


                ws(ago_do,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]] ])]).
% AGITARE
lexarg(agitare,
       arglist:[ws(agito_deliberate,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      prep_cplt:[type:pp,oblig:yes,constraints:[prep:de]]])]).
% ALERE
 lexarg(alere,
       arglist:[
             ws(alo_nourish,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:no,constraints:[case:acc]],
                      arg:[type:np,oblig:no,constraints:[case:abl]]])]). 
% AMARE
% Regina deam amat.
lexarg(amare,
       arglist:[
             ws(amo_love,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:no,constraints:[case:acc]]])]).
% AMBULARE
% Ambulo in horto.
lexarg(ambulare,
       arglist:[ws(ambulo_walk,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]]])]).

% AMITTERE
% Angebat Hamilcarem amissa Sicilia.
lexarg(amittere,
       arglist:[ws(amitto_let_go,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).
% ANGERE
% Angebat Hamilcarem amissa Sicilia.
lexarg(angere,
       arglist:[ws(ango_torment,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:np,oblig:yes,constraints:[sem:[hum],case:acc]]])]).
% APERIRE
lexarg(aperire,
       arglist:[ws(aperio_explain,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).
% APPELLARE
lexarg(appellare,
       arglist:[ws(appello_call,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]],
                      object_cplt:[type:np,oblig:no,constraints:[case:acc]]])]).
% ASSEQUI
lexarg(assequi,
       arglist:[
             ws(assequor_understand,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:no,constraints:[case:acc]]])]).

% AUDERE / SOLERE
% verb([v(solere,2,sole,xxx,xxx)],tr_cod,std).
% verb([v(solere,2,xxx,solit)],tr_cod,dep).
 lexarg(solere,
       arglist:[ws(soleo_be_used_to,tr_cod,clause:[],mwuw:0,
              args:[subject:[type:np,oblig:yes,constraints:[case:or([nom,acc])]],
                   object:[type:pred,oblig:yes,constraints:[type:nonfinite_i]]])]).

% verb([v(audere,2,aude,xxx,xxx)],tr_cod,std).
% verb([v(audere,2,xxx,aus)],tr_cod,dep).
 lexarg(audere,
       arglist:[ws(audeo_dare,tr_cod,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[case:or([nom,acc]),sem:[hum]]],
                     object:[type:pred,oblig:yes,constraints:[type:nonfinite_i]]])]).
% AUGERE
lexarg(augere,
       arglist:[ws(augeo_increase,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]]),

                ws(augeo_increase,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]]])]).
% AUDIRE
lexarg(audire,
       arglist:[
             ws(audio_hear,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:no,constraints:[case:acc]]]),

             ws(audio_hear_that,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]],
                      object_cplt:[type:adjp,oblig:yes,constraints:[case:acc, type:std]]]),

             ws(audio_hear_that,tr_cod,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     object:[type:pred,oblig:yes,
                       constraints:[type:finite,gap:[],argbound:yes,subordinator:or([quod, quia, quoniam])]]])]).







% CADERE
lexarg(cadere,
       arglist:[ws(cado_fall,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]]])]).
% CANERE
lexarg(canere,
       arglist:[
             ws(cano_sing,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:np,oblig:no,constraints:[case:acc]]])]).
% CAPERE
% Caesar mittit legiones legato urbis capiendae causa.
lexarg(capere,
       arglist:[ws(capio_take,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).
% CEDERE
lexarg(cedere,
       arglist:[ws(cedo_in_medium_be_shared,intr,clause:[],mwuw:2,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                       prep_cplt:[type:pp,oblig:yes,constraints:[prep:in,lex:medium]]]),

                ws(cedo_go,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                       prep_cplt:[type:pp,oblig:no,constraints:[]]]),

                ws(cedo_leave,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                       cplt:[type:np,oblig:yes,constraints:[case:abl]]]),

                 ws(cedo_yield,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      i_object:[type:np,oblig:yes,constraints:[case:dat]]]) ]).

        % DISCEDERE
        lexarg(discedere,
               arglist:[ws(discedo_part_from,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                       cplt:[type:pp,oblig:yes,constraints:[prep:ab]]])   ]).

% CELEBRARE
lexarg(celebrare,
       arglist:[
             ws(celebro_celebrate,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:no,constraints:[case:acc]]])]).
% CENARE
% Cenabis bene.
lexarg(cenare,
       arglist:[ws(ceno_dine,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]]])]).
% CENSERE
lexarg(censere,
       arglist:[ws(censeo_decide_that,tr_cod,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     object:[type:pred,oblig:yes,constraints:[type:gerund, local_case:acc]]]),

               ws(censeo_decide,tr_cod,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     object:[type:np,oblig:yes,constraints:[case:acc]]]),


		ws(censeo_decide_to,tr_cod,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     object:[type:pred,oblig:yes,constraints:[type:nonfinite_i]]]),

		ws(censeo_decide_that,tr_cod,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     object:[type:pred,oblig:yes,constraints:[type:nonfinite]]]),

                ws(censeo_decide_that,tr_cod,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     object:[type:pred,oblig:yes,constraints:[type:finite,gap:[],mood:subjunctive]]]),

                ws(censeo_decide_that,tr_cod,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     object:[type:pred,oblig:yes,constraints:[type:finite,gap:[],mood:subjunctive,
                              argbound:yes,subordinator:or([ne,ut])]]])]).

% CLAMARE
% Magna voce clamat.
lexarg(clamare,
       arglist:[ws(clamo_call,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:no,constraints:[case:acc]],
                      adjunct:[type:np,oblig:no,constraints:[case:abl]]])]).
% CLAUDERE
lexarg(claudere,
       arglist:[
             ws(claudo_close,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:no,constraints:[case:acc]]])]).

% COEPI
% Patrem amare coepi.
% Coeptus sum a patre amari.

% verb([v(coepi,3,xxx,coept)],tr_cod,dep).    % treated as a deponent - semantically preserves 'active' voice
% verb([v(coepi,3,xxx,coep,xxx)],tr_cod,std). % treated as lacking everything except perfect tense and derived variants

lexarg(coepi,
       arglist:[ ws(coepi_begin,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]]]),

               ws(coepi_begin_to,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[case:or([nom,acc])]],
                     object:[type:pred,oblig:yes,constraints:[type:nonfinite_i]]]),

               ws(coepi_begin,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[case:or([nom,acc]),sem:[hum]]],
                    object:[type:np,oblig:yes,constraints:[case:acc]]])]).

% COERCERE
lexarg(coercere,
       arglist:[ws(coerceo_constrain,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:np,oblig:yes,constraints:[case:acc]],
                      adjunct:[type:np,oblig:no,constraints:[case:abl]]])]).   % such an addition is advisable if text-based

% COGERE
lexarg(cogere,
       arglist:[ws(cogo_compel,tr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:np,oblig:yes,constraints:[case:acc,sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:finite,gap:[],mood:subjunctive,
                                                              argbound:yes, subordinator:ut]]]),


                ws(cogo_compel,tr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:np,oblig:yes,constraints:[case:acc,sem:[hum]]],
                      prep_cplt:[type:pp,oblig:yes,constraints:[prep:ad]]]),


                ws(cogo_compel,tr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:np,oblig:yes,constraints:[case:acc,sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:nonfinite_i]]])]).
% COGITARE
lexarg(cogitare,
       arglist:[ws(cogito_think_about,tr_cod,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     object:[type:np,oblig:yes,constraints:[case:acc]]]),

                ws(cogito_think_about,tr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc,sem:[hum]]],
                      prep_cplt:[type:pp,oblig:yes,constraints:[prep:de,case:abl]]]),

               ws(cogito_think_that,tr_inf,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:nonfinite]]]),


                ws(cogito_think_that_sby_should,tr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:no,constraints:[type:finite,gap:[],mood:subjunctive,
                                                              argbound:yes, subordinator:or([ne,ut])]]]),

               ws(cogito_think_whether,tr_cod,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,
                       constraints:[type:finite,gap:[],mood:subjunctive, checkint:yes]]])]).



% COGNOSCERE
% Cognouerat virtutem tuam.
lexarg(cognoscere,
       arglist:[ws(cognosco_learn,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).

	% NOSCERE
	% Nouerat virtutem tuam.
	lexarg(noscere,
       arglist:[ws(novi_know,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).
% COLERE
% Mihi colenda est uirtus.
lexarg(colere,
       arglist:[ws(colo_inhabit,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc, sem:[loc]]]]),

                ws(colo_practise,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc, sem:[abstract]]]])]).
% COMPLERE
lexarg(complere,
       arglist:[ws(compleo_complete,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).
% CONARI
lexarg(conari,
       arglist:[ ws(conor_endeavour,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     object:[type:pred,oblig:yes,constraints:[type:nonfinite_i]]]),

                ws(conor_undertake,tr_cod,clause:[],mwuw:0,
              args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                    object:[type:np,oblig:yes,constraints:[case:acc]]])]).

% CONCILIARE
lexarg(conciliare,
       arglist:[ws(concilio_gain,tr_cod_coi,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      i_object:[type:np,oblig:yes,constraints:[case:dat, sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).
% CONCUPISCERE
lexarg(concupiscere,
       arglist:[ws(concupisco_desire,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).
% CONSCIRE
lexarg(conscire,
       arglist:[ws(conscio_commit_suicide,tr_cod_coi,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      i_object:[type:np,oblig:yes,case:dat,constraints:[case:dat, lex:pp3refl]],
                      object:[type:np,oblig:yes,constraints:[case:acc, lex:mors]]])]).

% CONSULTARE
lexarg(consultare,
       arglist:[ws(consulto_deliberate,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      prep_cplt:[type:pp,oblig:yes,constraints:[prep:de]]])]).
% CONTEMNERE
lexarg(contemnere,
       arglist:[ws(contemno_despise,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).
% CREDERE
% Crede hoc mihi.
% Crediderunt Ciceronem oratorem optimum.
% Credunt Ciceronem oratorem optimum esse.
lexarg(credere,
       arglist:[ws(credo_believe,tr_inf,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      i_object:[type:np,oblig:no,case:dat,constraints:[sem:[hum],case:dat]],
                      object:[type:np,oblig:no,constraints:[case:acc]]]),

                ws(credo_believe_that,tr_inf,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:nonfinite]]]),

                 % adj as object_cplt
                ws(credo_believe_to_be,tr_cod_cplt,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]],
                      object_cplt:[type:adjp,oblig:yes,constraints:[case:acc]]]),

                % np as object_cplt
                 ws(credo_believe_to_be,tr_cod_cplt,clause:[],mwuw:0,
                 args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                       object:[type:np,oblig:yes,constraints:[case:acc]],
                       object_cplt:[type:np,oblig:yes,case:acc,constraints:[case:acc]]])]).

% CUPERE
lexarg(cupere,
       arglist:[ ws(cupio_want,tr_inf,clause:[],mwuw:0,
                 args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:nonfinite_i]]]),

                ws(cupio_want,tr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:finite,gap:[],mood:subjunctive,
                              argbound:yes, subordinator:or([ne,ut])]]]),


                ws(cupio_want,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]]),

                 ws(cupio_want_sbd_to,tr_inf,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:nonfinite]]]),

                 ws(cupio_want_sbd_to,tr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:finite,gap:[],mood:subjunctive]]])]).

% CURARE
lexarg(curare,
       arglist:[ws(curo_cure,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:np,oblig:no,constraints:[case:acc]]])]).

% CUSTODIRE
lexarg(custodire,
       arglist:[ws(custodio_protect,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).







% DARE
% Dedimus profecto grande documentum patientiae.
lexarg(dare,
       arglist:[ws(do_give,tr_cod_coi,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      i_object:[type:np,oblig:no,constraints:[case:dat, sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc, sem:[thing]]]]),

              ws(uela_do_set_sail,tr_cod,clause:[],mwuw:2,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:np,oblig:yes,constraints:[case:acc, lex:uelum, number:pl]],
                      prep_cplt:[type:pp,oblig:yes,constraints:[prep:or([in,ex,ab,ad])]]])]).
% DEBERE
lexarg(debere,
       arglist:[ws(debeo_owe,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:dat]]]),

                ws(debeo_must,tr_inf_no_subj,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:Sem]], % attempt to catch subject semantics and pass it on
									 % does NOT work
                      arg:[type:pred,oblig:yes,constraints:[sem:Sem,type:nonfinite_i]]])]).

% DEESSE
lexarg(deesse,
       arglist:[ws(desum_be_absent,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      cplt:[type:np,oblig:no,constraints:[case:dat]]])]).
% DEGENERARE
lexarg(degenerare,
       arglist:[ws(degenero_degenerate,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      prep_cplt:[type:pp,oblig:yes,constraints:[prep:ab]]])]).
% DELERE
% Karthago delenda est.
lexarg(delere,
       arglist:[ws(deleo_destroy,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).
% DELIGERE
lexarg(deligere,
       arglist:[ws(deligo_choose,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]]),

             ws(deligo_choose,tr_cod,clause:[],mwuw:0,
              args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     object:[type:pred,oblig:yes,
                       constraints:[type:finite,gap:[],mood:subjunctive, checkint:yes]]])]).


% DEMENTARE
% Quos vult perdere dementat.
lexarg(dementare,
       arglist:[ws(demento_madden,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc,sem:[hum]]]])]).
% DESERERE
lexarg(deserere,
       arglist:[ws(desero_abandon,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).
% DESIDERARE
lexarg(desiderare,
       arglist:[ws(desidero_miss,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).
% DESINERE
lexarg(desinere,
       arglist:[ ws(desino_stop,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                     object:[type:pred,oblig:yes,constraints:[type:nonfinite_i]]]),

              ws(desino_stop,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]]]),
                     

              ws(desino_stop,tr_cod,clause:[],mwuw:0,
              args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                    object:[type:np,oblig:yes,constraints:[case:acc]]])]).

% DICERE
% Elige cui dicas : tu mihi sola places.
% Dicunt militibus malis aqua et igni praetorem interdixisse.
% Marcus dixit regi magno salutem longam.
lexarg(dicere,
       arglist:[ws(dico_say_that,tr_inf,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      i_object:[type:np,oblig:no,case:dat,constraints:[case:dat, sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:nonfinite]]]),
               
         % + object and np as object cplt 
               ws(dico_call_sby_sth,tr_cod_cplt,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]],
                      object_cplt:[type:np,oblig:no,constraints:[case:acc]]]),
       
         % + object and adjp as object cplt 
             ws(dico_call_sby_sth,tr_cod_cplt,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]],
                      object_cplt:[type:adjp,oblig:yes,constraints:[case:acc, type:std]]]),


         % + finite clause as object
              ws(dico_say_that,tr_f,clause:[],mwuw:0,
              args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                    i_object:[type:np,oblig:no,case:dat,constraints:[case:dat, sem:[hum]]],
                    object:[type:pred,oblig:yes,constraints:[type:finite,gap:[], add:colon]]]),        % colon in front of direct speech

         % + unspecified np object
                ws(dico_say,tr_cod,clause:[],mwuw:0,
              args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                    i_object:[type:np,oblig:no,case:dat,constraints:[case:dat, sem:[hum]]],
                    object:[type:np,oblig:yes,constraints:[case:acc]]]),

         % mwu ALICUI SALUTEM DICERE
              ws(mwu_salutem_dico_greet_IDIOM,tr_cod_coi,clause:[],mwuw:2,
              args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                    i_object:[type:np,oblig:yes,case:dat,constraints:[case:dat, sem:[hum]]],
                    object:[type:np,oblig:yes,constraints:[case:acc, lex:salus]]])]).

% PRAEDICERE
lexarg(praedicere,
       arglist:[ws(praedico_foretell,tr_inf,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      i_object:[type:np,oblig:no,case:dat,constraints:[case:dat, sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:nonfinite]]])]).

% DIFFERRE
lexarg(differre,
       arglist:[ws(differre_postpone,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:no,constraints:[case:acc]],
                      prep_cplt:[type:pp,oblig:no,constraints:[prep:in, case:acc, sem:[time]]] ])]).


% DILIGERE
lexarg(diligere,
       arglist:[ws(diligo_love,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).
% DIMICARE
lexarg(dimicare,
       arglist:[ws(dimico_fight,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      prep_cplt:[type:pp,oblig:no,constraints:[prep:de]]])]).
% DIRIPERE
lexarg(diripere,
       arglist:[ws(diripio_sack_destroy,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).

% DISCERE
lexarg(discere,
       arglist:[ws(disco_learn,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:no,constraints:[case:acc]]]),

                ws(disco_learn,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:nonfinite_i]]]),  

                ws(disco_learn_that,tr_inf,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:nonfinite]]])]).


% DISPERIRE
lexarg(disperire,
       arglist:[ws(dispereo_perish,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]]])]).

                     
% DOCERE
% Credo ancillam grammaticam pueros docere.
lexarg(docere,
       arglist:[ws(doceo_teach,tr_cod_coi,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],            % person teaching
                      object_i:[type:np,oblig:yes,constraints:[case:acc, sem:[hum]]],   % person being taught
                      object:[type:np,oblig:yes,constraints:[case:acc, sem:[abstract]]]])]). % topic being taught
% object_i is like a direct object in that it requires the accusative case,
% but like an indirect object (i.e. an i_object, standard dative case indirect object) in the semantic role it fulfils

% DONARE
% Marco donat ciuitas immortalitatem.
% Marcum donat ciuitas immortalitate.
lexarg(donare,
       arglist:[ws(dono_donate,tr_cod_coi,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc, sem:[thing]]],
                      i_object:[type:np,oblig:yes,case:dat,constraints:[case:dat, sem:[hum]]]]),

                 ws(dono_donate,tr_cod_coi,clause:[],mwuw:0,
                 args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                       object:[type:np,oblig:yes,constraints:[case:abl, sem:[thing]]],
                       i_object:[type:np,oblig:yes,case:acc,constraints:[case:acc, sem:[hum]]]])]).
% DUBITARE
lexarg(dubitare,
       arglist:[ws(dubito_doubt,tr,clause:[[polarity:neg]],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:finite,gap:[],mood:subjunctive,
                                                              argbound:yes, subordinator:quin]]]),
                ws(dubito_hesitate,tr,clause:[[polarity:neg]],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:nonfinite_i]]]),

                ws(dubito_doubt,tr,clause:[[illocutionary_force:question]],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:finite,gap:[],mood:subjunctive,
                                                              argbound:yes, subordinator:quin]]]),
                ws(dubito_hesitate,tr,clause:[[illocutionary_force:question]],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:nonfinite_i]]])]).
% DUCERE
% Imperator legiones ad proelium duxit.
lexarg(ducere,
       arglist:[ws(duco_lead,tr_cod,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     object:[type:np,oblig:yes,constraints:[case:acc]],
                     prep_cplt:[type:pp,oblig:no,constraints:[prep:or([ad,in,ex,ab])]]])]).


	% ADDUCERE
	% Cupiditate regni adductus nouis rebus studebat. (Caesar, De Bello Gallico)
	lexarg(adducere,
        arglist:[ws(adduco_drive,tr_cod,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[]],
                     object:[type:np,oblig:yes,constraints:[case:acc,sem:[hum]]],
                     prep_cplt:[type:pp,oblig:no,constraints:[]]])]).

        % EDUCERE
        lexarg(educere,
        arglist:[ws(educo_take_out,tr_cod,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[]],
                     object:[type:np,oblig:yes,constraints:[case:acc,sem:[hum]]],
                     prep_cplt:[type:pp,oblig:no,constraints:[prep:or([ex,ab])]]])]).




% verb([v(edere_1,3,ed,ed,es)],tr_cod,std).
% verb([v(edere_2,3,ed,edid,edit)],tr_cod,std).

% EDERE_1
lexarg(edere_1,
       arglist:[
             ws(edere_1_eat_consume,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).

% EDERE_2
lexarg(edere_2,
       arglist:[
             ws(edere_2_bring_forth,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).



/*
lex(est, v, [pos:v, class:tr_cod, type:finite, lex:edere_1a, voice:act, txt:est, tense:present, kind:std, mood:indicative, number:sing, person:3]).
lex(es, v, [pos:v, class:tr_cod, type:finite, lex:edere_1a, voice:act, txt:es, tense:present, kind:std, mood:indicative, number:sing, person:2]).
lex(estis, v, [pos:v, class:tr_cod, type:finite, lex:edere_1a, voice:act, txt:estis, tense:present, kind:std, mood:indicative, number:pl, person:2]).
lex(esse, v, [pos:v, class:tr_cod, type:nonfinite, lex:edere_1a, voice:act, txt:esse, tense:present, kind:std, mood:infinitive, number:_, person:_]).
lex(esset, v, [txt:esset, lex:edere_1a, pos:v, class:tr_cod, type:finite, voice:act, tense:imperfect, kind:std, mood:subjunctive, number:sing, person:3]).
lex(esses, v, [txt:esses, lex:edere_1a, pos:v, class:tr_cod, type:finite, voice:act, tense:imperfect, kind:std, mood:subjunctive, number:sing, person:2]).
lex(essem, v, [txt:essem, lex:edere_1a, pos:v, class:tr_cod, type:finite, voice:act, tense:imperfect, kind:std, mood:subjunctive, number:sing, person:1]).
lex(essemus, v, [txt:essemus, lex:edere_1a, pos:v, class:tr_cod, type:finite, voice:act, tense:imperfect, kind:std, mood:subjunctive, number:pl, person:1]).
lex(essetis, v, [txt:essetis, lex:edere_1a, pos:v, class:tr_cod, type:finite, voice:act, tense:imperfect, kind:std, mood:subjunctive, number:pl, person:2]).
lex(essent, v, [txt:essent, lex:edere_1a, pos:v, class:tr_cod, type:finite, voice:act, tense:imperfect, kind:std, mood:subjunctive, number:pl, person:3]).
lex(es,v,[pos:v,class:tr_cod,type:finite,lex:edere_1a,voice:act,txt:es ,tense:present,kind:std,mood:imperative,number:sing,person:2]).
lex(este,v,[pos:v,class:tr_cod,type:finite,lex:edere_1a,voice:act,txt:este,tense:present,kind:std,mood:imperative,number:pl,person:2]).
lex(esto,v,[pos:v,class:tr_cod,type:finite,lex:edere_1a,voice:act,txt:esto ,tense:future,kind:std,mood:imperative,number:sing,person:2]).
lex(estote,v,[pos:v,class:tr_cod,type:finite,lex:edere_1a,voice:act,txt:estote,tense:future,kind:std,mood:imperative,number:pl,person:2]).
lex(esto,v,[pos:v,class:tr_cod,type:finite,lex:edere_1a,voice:act,txt:esto,tense:future,kind:std,mood:imperative,number:sing,person:3]).

*/

% EDERE_1a
lexarg(edere_1a,
       arglist:[
             ws(edere_1a_devour,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:np,oblig:yes,constraints:[lex:or([medulla,animus,ador,lolium]),case:acc]]])]).  
	
        % lexical restrictions to be assigned on a textual basis (Verg., Hor., etc)



% EIICERE
lexarg(eiicere,
       arglist:[
             ws(eiicio_expulse,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]],
                      prep_cplt:[type:pp,oblig:no,constraints:[prep:or([ab,ex])]] ]),

             ws(eiicio_expulse,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]],
                      cplt:[type:np,oblig:yes,constraints:[case:abl]] ])]).



% ELIGERE
% Elige cui dicas : tu mihi sola places.
lexarg(eligere,
       arglist:[ws(eligo_choose,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).
% ERIPERE
lexarg(eripere,
       arglist:[
             ws(eripio_snatch,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:no,constraints:[case:acc]],
                      prep_cplt:[type:pp,oblig:no,constraints:[prep:or([ab,ex])]] ])]).
% ERRARE
% Errare humanum est.
lexarg(errare,
       arglist:[ws(erro_be_mistaken,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]]])]).
% ERUDIRE
lexarg(erudire,
       arglist:[ws(erudio_instruct,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[sem:[hum],case:acc]],
                      prep_cplt:[type:pp,oblig:no,constraints:[prep:de]]])]).
% EXCIPERE
lexarg(excipere,
       arglist:[
             ws(excipio_take_out,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:no,constraints:[case:acc]]])]).

% EXCLUDERE
lexarg(excludere,
       arglist:[ws(excludo_throw_out,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:no,constraints:[case:acc]],
                      prep_cplt:[type:pp,oblig:no,constraints:[prep:ab]] ])]).
% EXHAURIRE
lexarg(exhaurire,
       arglist:[ws(exhaurio_endure,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).

% EXISTIMARE (+acc_cum_inf)
% Cicero epistulas optimas scripsisse existimabatur.
lexarg(existimare,
       arglist:[ws(existimo_deem_that,tr_inf,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:nonfinite]]])]).







% FACERE
	% ALIQUEM/QUOD (NON) PILI FACERE
	% Praetor non amabat milites nec faciebat pili cohortem.
	lexarg(facere,
        arglist:[ws(mwu_non_pili_facio_not_give_a_damn_IDIOM,tr_cod_cplt,clause:[[polarity:neg]],mwuw:2,
                 args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                       object:[type:np,oblig:yes,constraints:[case:acc]],
                       object_cplt:[type:phrase,frozen:yes,oblig:yes,constraints:[lex:pili]]]),
                 ws(mwu_tanti_facio_appreciate_IDIOM,tr_cod_cplt,clause:[],mwuw:2,
                 args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                       object:[type:np,oblig:yes,constraints:[case:acc]],
                       object_cplt:[type:phrase,frozen:yes,oblig:yes,constraints:[lex:tanti]]]),

	% Verba facere
	% Partibus factis, fecit verba leo.
                 ws(uerba_facio_speak_IDIOM,tr_cod,clause:[],mwuw:2,
              args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                    object:[type:np,oblig:yes,constraints:[case:acc, lex:uerbum, number:pl]]]),
	% Iter facere
	% Iter feci per Galliam
                ws(iter_facio_travel_IDIOM,tr_cod,clause:[],mwuw:2,
              args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                    object:[type:np,oblig:yes,constraints:[case:acc, lex:iter, number:sing]],
                    prep_cplt:[type:pp,oblig:no,constraints:[]]]),
        
         % Standard facio
      		  ws(facio_make,tr_cod,clause:[],mwuw:0,
              	args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                    object:[type:np,oblig:yes,constraints:[case:acc]],
                     object_cplt:[type:np,oblig:yes,case:acc,constraints:[case:acc]]]),
 
               ws(facio_make,tr_cod,clause:[],mwuw:0,
              args:[subject:[type:np,oblig:yes,constraints:[]],
                    object:[type:np,oblig:yes,constraints:[case:acc]]])]).

	% OLFACERE
          lexarg(olfacere,
         arglist:[ ws(olfacio_smell,tr_cod,clause:[],mwuw:0,
              args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                    object:[type:np,oblig:yes,constraints:[case:acc]]])]).

        % SATISFACERE
            lexarg(satisfacere,
         arglist:[ ws(satisfacio_satisfy,tr_cod,clause:[],mwuw:0,
              args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                    object:[type:np,oblig:yes,constraints:[sem:[hum],case:dat]]])]).


% FALLERE
lexarg(fallere,
       arglist:[ws(fallo_deceive,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:np,oblig:no,constraints:[sem:[hum],case:acc]]])]).
% FERIRE
lexarg(ferire,
       arglist:[ws(ferio_strike,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).
% FERRE
% Timeo Danaos et dona ferentes.
lexarg(ferre,
       arglist:[ws(fero_bear,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:np,oblig:yes,constraints:[case:acc]],
                      prep_cplt:[type:pp,oblig:no,constraints:[prep:ad]]]),

                ws(fero_bear,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      i_object:[type:np,oblig:no,case:dat,constraints:[case:dat, sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).
                      


	% REFERRE
	% De re refertur.
	lexarg(referre,
       arglist:[ws(refero_relate,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:no,constraints:[case:acc]],
                      prep_cplt:[type:pp,oblig:no,constraints:[]]])]).
% FESTINARE
lexarg(festinare,
       arglist:[ws(festino_hasten,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:no,constraints:[case:acc]]])]).
% FRANGERE
lexarg(frangere,
       arglist:[
             ws(frango_break,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:np,oblig:no,constraints:[case:acc]]])]).
% FUGERE
lexarg(fugere,
       arglist:[ws(fugio_flee,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]]]),

                ws(fugio_flee,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      prep_cplt:[type:pp,oblig:yes]]) ]).







% GAUDERE
lexarg(gaudere,
       arglist:[ws(gaudeo_rejoice,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]]])]).
% GERERE
lexarg(gerere,
       arglist:[
             ws(gero_manage,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:no,constraints:[case:acc]]]),

              ws(bellum_gero_make_war_IDIOM,tr_cod,clause:[],mwuw:2,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc, lex:bellum]],
                      prep_cplt:[type:pp,oblig:no,constraints:[prep:cum, case:abl]]])]).


% GRASSARI
lexarg(grassari,
       arglist:[ws(grassor_proceed,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]]]),

                ws(grassor_attack,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:no,constraints:[case:acc]]])]).




% HABERE
% Libellos Marci habet rex impudicos.
% Libellos impudicos habet regina documenta ingenii humani.
% Catilina nihil pensi neque sancti habere dicitur.
lexarg(habere,
       arglist:[ ws(habeo_have,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]]),

         % adj as object_cplt
                ws(habeo_consider_as,tr_cod_cplt,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc,gender:G,number:N]],
                      object_cplt:[type:adjp,oblig:yes,constraints:[case:acc, type:std, gender:G,number:N]]]),

         % np as object_cplt
                 ws(habeo_consider_as,tr_cod_cplt,clause:[],mwuw:0,
                 args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                       object:[type:np,oblig:yes,constraints:[case:acc,number:N1]],
                       object_cplt:[type:np,oblig:yes,constraints:[class:common,case:acc,number:N1]]]),

         % mwu  NIHIL PENSI NEQUE SANCTI HABERE
                 ws(mwu_nihil_pensi_neque_sancti_habeo_fear_neither_god_nor_man_IDIOM,tr_cod_cplt,clause:[],mwuw:2,
                 args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                       object_cplt:[type:phrase,frozen:yes,oblig:yes,constraints:[lex:nihil_pensi_neque_sancti]]])]).

% HABITARE
lexarg(habitare,
       arglist:[ws(habito_live_in,tr_cod,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[]],
                     object:[type:np,oblig:yes,constraints:[case:acc]]])]).
 
% HORTARI
% Caesari erat eundum Romam ad senatores hortandos.
lexarg(hortari,
       arglist:[ws(hortor_exhort,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc, sem:[hum]]]])]).







% IACERE
lexarg(iacere,
       arglist:[ws(iaceo_lie_down,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]]])]).
% IGNOSCERE
% Orat te mater ut filio ignoscas suo.
lexarg(ignoscere,
       arglist:[ws(ignosco_pardon,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:no,constraints:[case:acc]],
                      i_object:[type:np,oblig:yes,constraints:[sem:[hum],case:dat]]])]).
% IMPONERE
lexarg(imponere,
       arglist:[ws(impono_impose,tr_cod_coi,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      i_object:[type:np,oblig:no,constraints:[case:dat, sem:[hum]]],
                      object:[type:np,oblig:no,constraints:[case:acc]]])]).
% INCHOARE
lexarg(inchoare,
       arglist:[ws(inchoo_begin,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).
% INCIPERE
lexarg(incipere,
       arglist:[ ws(incipio_begin,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                     object:[type:pred,oblig:yes,constraints:[type:nonfinite_i]]]),

               ws(incipio_begin,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:np,oblig:no,constraints:[case:acc]]]) ]).
% INCUSARE
lexarg(incusare,
       arglist:[ws(incuso_accuse,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]],
                      arg:[type:pred,oblig:no,constraints:[type:finite, subordinator:quod, gap:[], argbound:yes,mood:subjunctive]]])]).

% INDICARE
% Me tabula sacer votiva paries indicat uvida suspendisse potenti vestimenta maris deo.
lexarg(indicare,
       arglist:[ws(indico_show_that,tr_inf,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:pred,oblig:yes,constraints:[type:nonfinite]],
                      adjunct:[type:np,oblig:no,constraints:[case:abl]]])]).
% INEPTIRE
lexarg(ineptire,
       arglist:[ws(ineptio_be_a_fool,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]]])]).

% INSANIRE
% Insanit rex.
lexarg(insanire,
       arglist:[ws(insanio_be_mad,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]]])]).
% INTELLEGERE
lexarg(intellegere,
       arglist:[ws(intellego_understand,tr_cod,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     object:[type:np,oblig:yes,constraints:[case:acc]]]),

              
               ws(intellego_understand_that,tr_inf,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:nonfinite]]])]).
% INTENDERE
lexarg(intendere,
       arglist:[ws(intendo_intensify,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).


% INTERDICERE (ALICUI AQUA ET IGNI INTERDICERE)
% Dicunt militibus malis aqua et igni praetorem interdixisse.
lexarg(interdicere,
       arglist:[ws(mwu_aqua_et_igni_interdico_exile_IDIOM,tr_cod_cplt,clause:[],mwuw:2,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[sem:[hum],case:dat]],
                      object_cplt:[type:phrase,frozen:yes,oblig:yes,constraints:[lex:aqua_et_igni]]])]).

% INTERFICERE
% Rex, cum interfecisset Marcum, magnitudinem sceleris sui perspexit.
lexarg(interficere,
       arglist:[ws(interficio_kill,tr_inf,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc,sem:[hum]]]])]).
% INVENIRE
lexarg(inuenire,
       arglist:[ws(inuenio_find_that_that,tr_inf,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:nonfinite]]]),


                ws(inuenio_find,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:no,constraints:[case:acc]]])]).
% IRE
% Eo Romam.
% Eo auxilium rogatum.
% Itum est in templum.
lexarg(ire,
       arglist:[ws(eo_go,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      prep_cplt:[type:pp,oblig:no,constraints:[prep:or([ad,in]), case:acc]],
                      cplt:[type:np,oblig:no,constraints:[case:abl,sem:[loc]]],
                      cplt:[type:np,oblig:no,constraints:[index:i(_),case:acc,sem:[city]]],                   % index:i(_) avoids dummy_nps
                      cplt:[type:pred,oblig:no,constraints:[type:nonfinite_i, mood:supine]] ]),

        % MWU : obuiam ire + datif
                 ws(obuiam_eo_oppose_IDIOM,intr,clause:[],mwuw:2,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      prep_cplt:[type:pp,oblig:yes,constraints:[prep:obuiam]] ])]).
	% ABIRE
	lexarg(abire,
        arglist:[ws(abeo_leave,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      prep_cplt:[type:pp,oblig:no,constraints:[prep:or([ab,ex]), case:abl]],
                      cplt:[type:pred,oblig:no,constraints:[type:nonfinite_i, mood:supine]] ])]).
	% REDIRE
	lexarg(redire,
        arglist:[ws(redeo_return,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      prep_cplt:[type:pp,oblig:no,constraints:[prep:or([ad,in]), case:acc]]  ])]).

% IUBERE (+acc_cum_inf)
% Iussit omnes tacere.
lexarg(iubere,
       arglist:[ws(iubeo_order_that,tr_inf,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:nonfinite]]]),

                ws(iubeo_bid_goodday_IDIOM,tr_inf,clause:[],mwuw:2,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:nonfinite, constituent_structure:[vg:[selected_reading:salveo_be_well]]
                     ]]])]).

% IUDICARE (+acc_cum_inf)
% Iudico te optimum praetorem esse.
lexarg(iudicare,
       arglist:[ws(iudico_judge_that,tr_inf,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:nonfinite]]])]).

% IUNGERE
lexarg(iungere,
       arglist:[
             ws(iungo_yoke,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:no,constraints:[case:acc]]])]).
	% ADIUNGERE
	lexarg(adiungere,
      		arglist:[
             		ws(adiungo_join,tr_cod,clause:[],mwuw:0,
                	args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                         	object:[type:np,oblig:yes,constraints:[case:acc]],
                                prep_cplt:[type:pp,oblig:no,constraints:[prep:ad]]  ]),

                        ws(adiungo_join,tr_cod,clause:[],mwuw:0,
                	args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                         	object:[type:np,oblig:yes,constraints:[case:acc]],
                                i_object:[type:np,oblig:yes,constraints:[case:dat]]  ])]).

% IUUARE
lexarg(iuuare,
       arglist:[
             ws(iuuo_help,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:no,constraints:[case:acc]]])]).







% LAUDARE
% Laudamus te.
lexarg(laudare,
       arglist:[ws(laudo_praise,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).
% LEGERE
% Rex vult legere libellos impudicos quos serva Marci scripsit.
lexarg(legere,
       arglist:[ws(lego_read,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:no,constraints:[case:acc]]])]).
lexarg(legere,
       arglist:[ws(lego_choose,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:no,constraints:[case:acc]],
                      i_object:[type:np,oblig:yes,constraints:[case:dat]]  ])]).

% LOQUI
lexarg(loqui,
       arglist:[ws(loquor_speak,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      prep_cplt:[type:pp,oblig:no,constraints:[prep:de, case:abl]],
                      prep_cplt:[type:pp,oblig:no,constraints:[prep:cum, case:abl, sem:[hum]]] ])]).
% LUDERE
% Eant lusum.
lexarg(ludere,
       arglist:[ws(ludo_play,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]]])]).

% LUGERE
lexarg(lugere,
       arglist:[ws(lugeo_be_afflicted,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]]]),

                ws(lugeo_mourn,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[sem:[hum],case:acc]]]),

                 ws(lugeo_lament,tr_inf,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:nonfinite]]])]).








% MAERERE
lexarg(maerere,
       arglist:[ws(maereo_be_afflicted,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      cplt:[type:np,oblig:no,constraints:[case:abl]]])]).
% MANERE
lexarg(manere,
       arglist:[ws(maneo_stay,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]]])]).

% REMANERE
lexarg(remanere,
       arglist:[ws(remaneo_stay,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]]])]).

% MATURARE
lexarg(maturare,
       arglist:[ws(maturo_bring_to_maturity,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).
% MEMINI
% conjugated forms too few to be linked to a pattern
% entered separately as lex clauses
% Memento documenti patientiae nostrae.
lexarg(memini,
       arglist:[ws(memini_remember,tr_cod,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                    object:[type:np,oblig:no,constraints:[case:or([acc,gen])]]]),

                ws(memini_remember,tr_inf,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:nonfinite_i]]])]).

% MEMORARE
lexarg(memorare,
       arglist:[
             ws(memoro_recall,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).


% METUERE
lexarg(metuere,
       arglist:[
             ws(metuo_fear,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).
% MINUERE
lexarg(minuere,
       arglist:[ws(minuo_diminish,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).
% MISCERE
lexarg(miscere,
       arglist:[
             ws(v_v,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).
% MITTERE
% Regina putat regem epistulas longas ad ancillam misisse.
% Dixit rex reginam librum pulchrum misisse Marco.
lexarg(mittere,
       arglist:[ws(mitto_send,tr_cod_coi,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]],
                      cplt:[type:pred,oblig:no,constraints:[type:nonfinite_i, mood:supine]], 
                      i_object:[type:np,oblig:no,case:dat,constraints:[case:dat, sem:[hum]]]]),

                 ws(mitto_send,tr_cod_prep_cplt,clause:[],mwuw:0,
                 args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                       object:[type:np,oblig:yes,constraints:[case:acc]],
                       cplt:[type:pred,oblig:no,constraints:[type:nonfinite_i, mood:supine]],
                       prep_cplt:[type:pp,oblig:no,constraints:[case:acc,sem:[hum], prep:ad]]]),

                  ws(mitto_send,tr_cod_prep_cplt,clause:[],mwuw:0,
                 args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                       object:[type:np,oblig:yes,constraints:[case:acc]],
                       prep_cplt:[type:pp,oblig:yes,constraints:[case:acc, prep:or([ad,in])]]])]).

% MOLLIRE
lexarg(mollire,
       arglist:[
             ws(mollio_appease,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:np,oblig:no,constraints:[case:acc]]])]).

% MONERE
lexarg(monere,
       arglist:[
             ws(moneo_advise,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[sem:[hum],case:acc]],
                      cplt:[type:pred,oblig:yes,constraints:[type:nonfinite_i]]])]).
% MORARI
lexarg(morari,
       arglist:[ws(moror_tarry,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]]]),

                ws(moror_detain,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).


% MORI
lexarg(mori,
       arglist:[ws(mori_die,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]]])]).
% MOVERE
lexarg(mouere,
       arglist:[ws(moueo_move,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]]]),

                ws(moueo_move,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).

% PERMOVERE
lexarg(permouere,
       arglist:[ ws(permoueo_move,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).

% MUTARE
lexarg(mutare,
       arglist:[ws(muto_change,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]]]),

                ws(muto_change,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).

% COMMUTARE
lexarg(commutare,
       arglist:[ws(commuto_change_into,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:np,oblig:yes,constraints:[case:acc]],
                      prep_cplt:[type:pp,oblig:no,constraints:[case:acc, prep:in]]])]).







% NARRARE
lexarg(narrare,
       arglist:[ws(narro_tell,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[sem:[abstract],case:acc]]]),

                ws(narro_tell_that,tr_inf,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:nonfinite]]])]).
% NASCI
lexarg(nasci,
       arglist:[ws(nasci_be_born,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]]])]).

% NEGARE (+acc_cum_inf)
% Negat se libellos impudicos scribere.

% Cicero,  Epistulae ad Familiares 9.14.1.1:
% ***negant*** enim [se dubitare ***quin*** tu meis praeceptis et consiliis obtemperans
% praestantissimum te civem et singularem consulem praebeas].

% if the object arg is of type:pred, we add a feature inherited_pol:neg to the object pred
% when a constraint on neg polarity is checked, both the pol feature and the inherited_pol feature are queried
% if one of them matches, the constraint is deemed to be satisfied
% not clear yet how to make it work... !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

%  object:[type:pred,oblig:yes,inherited_pol:neg,constraints:[type:nonfinite]]])]). NEGARE Object Specs
% arglist:[ws(dubito_doubt,tr,clause:[[polarity:neg]],mwuw:0,                       DUBITARE Clause Constraints

lexarg(negare,
       arglist:[ws(nego_deny_that,tr_inf,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:nonfinite]]])]).

% NITI
lexarg(niti,
       arglist:[ ws(nitor_endeavour,tr_inf,clause:[],mwuw:0,
                 args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:nonfinite_i]]]),

                ws(nitor_endeavour,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:finite,gap:[],mood:subjunctive,
                              argbound:yes, subordinator:or([ne,ut])]]])]).

% NOTARE
lexarg(notare,
       arglist:[ws(noto_blame,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]],
                      adjunct:[type:np,oblig:yes,constraints:[case:abl]]])]).

% NUMERARE
lexarg(numerare,
       arglist:[ws(numero_number,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).
               

% NUNTIARE (+acc_cum_inf)
% Nuntiatum est hostes vinum amare.
lexarg(nuntiare,
       arglist:[ws(nuntio_announce_that,tr_inf,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:nonfinite]]])]).





% OBIECTARE
lexarg(obiectare,
       arglist:[ws(obiecto_reproach,tr_cod_coi,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      i_object:[type:np,oblig:yes,constraints:[case:dat, sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).

% OBIIECERE
lexarg(obiicere,
       arglist:[ws(obiicio_throw,tr_cod_coi,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      i_object:[type:np,oblig:yes,constraints:[case:dat]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).

% OBLIUISCI
% Non obliviscar sermones tuos - Pascal, Mémorial
% Oblita est periculi ancilla fortior dominis multis
% Obliuiscitur rex reginam epistulas longas scripsisse ancillae Marci.
lexarg(obliuisci,
       arglist:[ws(obliuiscor_forget,tr_cod,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     object:[type:np,oblig:no,constraints:[case:or([acc,gen])]]]),
              
                ws(obliuiscor_forget_to,tr_inf,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:nonfinite_i]]]),

               ws(obliuiscor_forget,tr_cod,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     object:[type:pred,oblig:yes,
                       constraints:[type:finite,gap:[],mood:subjunctive, flagint:wh_question]]]),

                ws(obliuiscor_forget_that,tr_inf,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:nonfinite]]])]).
% OBNITI
lexarg(obniti,
       arglist:[ws(obnitor_oppose,intr,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     i_object:[type:np,oblig:yes,constraints:[case:dat]]])]).

% OBTEMPERARE
lexarg(obtemperare,
       arglist:[ws(obtempero_obey,tr_coi,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     i_object:[type:np,oblig:yes,constraints:[case:dat]]])]).
% OCCIDERE
lexarg(occidere,
       arglist:[ws(occido_kill,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:no,constraints:[sem:[hum],case:acc]]])]).
% OCCULERE
lexarg(occulere,
       arglist:[ws(occulo_conceal,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).
% OCCURRERE
lexarg(occurrere,
       arglist:[ws(occurro_occur,intr,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[]],
                     i_object:[type:np,oblig:no,constraints:[sem:[hum],case:dat]]])]).

% OPPUGNARE
% Decem annos urbem oppugnaverunt.
lexarg(oppugnare,
       arglist:[ws(oppugno_attack,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).
% OPTARE
lexarg(optare,
       arglist:[ws(opto_choose,tr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:no,constraints:[type:finite,gap:[],mood:subjunctive,
                                                              argbound:yes, subordinator:or([ne,ut])]]]),

                ws(opto_choose,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).
% ORARE
% Orat te mater ut filio ignoscas suo.
lexarg(orare,
       arglist:[ws(oro_ask,tr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      i_object:[type:np,oblig:no,constraints:[case:acc]],
                      object:[type:pred,oblig:no,constraints:[type:finite,gap:[],mood:subjunctive,
                                                              argbound:yes, subordinator:or([ne,ut])]]])]).
% ORDINARE
lexarg(ordinare,
       arglist:[ws(ordino_arrange,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).

% OSTENDERE
lexarg(ostendere,
       arglist:[ws(ostendo_show_that,tr_inf,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:nonfinite]]]),

                 ws(ostendo_show,tr_cod,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     object:[type:pred,oblig:yes,
                       constraints:[type:finite,gap:[],mood:subjunctive, flagint:wh_question]]]),

                ws(ostendo_show,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:no,constraints:[case:acc]]]) ]).







% PARARE
lexarg(parare,
       arglist:[
             ws(paro_prepare,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:no,constraints:[case:acc]],
                      prep_cplt:[type:pp,oblig:no,constraints:[prep:ad]] ])]).
% PARCERE
lexarg(parcere,
       arglist:[ws(parco_spare,tr_coi,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      i_object:[type:np,oblig:yes,constraints:[case:dat]]])]).
% PATI
lexarg(pati,
       arglist:[ws(patior_bear,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).

% PELLERE
lexarg(pellere,
       arglist:[
             ws(pello_expulse,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:np,oblig:yes,constraints:[sem:[hum],case:acc]],
                      prep_cplt:[type:pp,oblig:no,constraints:[prep:or([ab,ex])]] ]),

             ws(pello_expulse,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:np,oblig:yes,constraints:[sem:[hum],case:acc]],
                      cplt:[type:np,oblig:yes,constraints:[case:abl]] ])]).

% EXPELLERE
lexarg(expellere,
       arglist:[
             ws(expello_expulse,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:np,oblig:yes,constraints:[sem:[hum],case:acc]],
                      prep_cplt:[type:pp,oblig:no,constraints:[prep:ex]] ])]).

            

% PERDERE
% Quos uult perdere.
lexarg(perdere,
       arglist:[ws(perdo_ruin_or_lose,tr_cod,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[]],
                     object:[type:np,oblig:yes,constraints:[case:acc]]])]).
% PERFICERE
lexarg(perficere,
       arglist:[ws(perficio_carry_out,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).
% PERIRE
lexarg(perire,
       arglist:[ws(perire_perish,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]]])]).

% PERSPICERE
% Rex, cum interfecisset Marcum, magnitudinem sceleris sui perspexit.
lexarg(perspicere,
       arglist:[ws(perspicio_see,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).
% PETERE
% Petiuit consulatum magna cum cura.
% Thessaliam ex negotio petebam.
lexarg(petere,
       arglist:[ws(peto_try_to_reach_or_get,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]]),

                ws(peto_consulatum_run_for_consul_IDIOM,tr_cod,clause:[],mwuw:2,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc,lex:consulatus]]])]).

	% APPETERE

      lexarg(appetere,
       arglist:[ws(appeto_try_to_reach_or_get,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).

% PLACARE
lexarg(placare,
       arglist:[ws(placo_appease,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).

% PLACERE
% Elige cui dicas : tu mihi sola places.
lexarg(placere,
       arglist:[ws(placeo_please,tr_coi,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[]],
                     i_object:[type:np,oblig:yes,constraints:[case:dat, sem:[hum]]]])]).

% PONERE
% Italiam peto causa videndae Romae in montibus positae.
lexarg(ponere,
       arglist:[ws(pono_put,tr_cod,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[]],
                     object:[type:np,oblig:yes,constraints:[case:acc]],
                      prep_cplt:[type:pp,oblig:yes,constraints:[]]])]).

	% ANTEPONERE

	lexarg(anteponere,
      		 arglist:[ws(antepono_prefer,tr_cod,clause:[],mwuw:0,
           		    args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
          		          object:[type:np,oblig:yes,constraints:[case:acc]],
                     		  object_i:[type:np,oblig:yes,constraints:[case:dat, sem:[hum]]]])]).


% POSSIDERE
% Nostrae artes admirabilem utilitatem possident.
lexarg(possidere,
       arglist:[ws(possideo_possess,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:np,oblig:no,constraints:[case:acc]]])]).
% POSTULARE
lexarg(postulare,
       arglist:[ws(postulo_request,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      prep_cplt:[type:pp,oblig:no,constraints:[prep:ab]],
                      object:[type:pred,oblig:yes,constraints:[type:finite,gap:[],mood:subjunctive]]]),


                ws(postulo_request,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).

% POTIRI
lexarg(potiri,
       arglist:[
             ws(potior_take_possession_of,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object_i:[type:np,oblig:yes,constraints:[case:or([gen,abl])]]])]).
% PRODESSE
lexarg(prodesse,
       arglist:[ws(prosum_help,intr,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     i_object:[type:np,oblig:yes,constraints:[case:dat, sem:[hum]]]])]).
% PROFICISCI
lexarg(proficisci,
       arglist:[ws(proficiscor_leave,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      arg:[type:np,oblig:no,constraints:[case:acc,sem:[city]]],
                      prep_cplt:[type:pp,oblig:no,constraints:[prep:or([ex,ab])]],
                      prep_cplt:[type:pp,oblig:no,constraints:[prep:ad]]])]).

% PUTARE (+acc_cum_inf)
% Rex quem putas reginam amare amat ancillam reginae.
% Humani nihil a me alienum puto
lexarg(putare,
       arglist:[ws(puto_think_that,tr_inf,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:nonfinite]]]),

               ws(puto_think_that,tr_cod,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     object:[type:pred,oblig:yes,constraints:[type:gerund, local_case:acc]]]), 

               ws(puto_deem,tr_cod_cplt,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                       object:[type:np,oblig:yes,constraints:[case:acc,number:N,gender:G]],
                       object_cplt:[type:adjp,oblig:yes,constraints:[case:acc, number:N,gender:G]]]),


                ws(puto_deem,tr_cod_cplt,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                       object:[type:np,oblig:yes,constraints:[case:acc]],
                       object_cplt:[type:np,oblig:yes,constraints:[case:acc]]])]).
% (SE) PRAEBERE
lexarg(praebere,
       arglist:[
         % adj as object_cplt
                ws(se_praebere_prove_oneself_to_be,tr_cod_cplt,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc,lex:or([pp3refl,pp1sg,pp2sg,pp1pl,pp2pl])]],
                      object_cplt:[type:adjp,oblig:yes,constraints:[case:acc, type:std]]]),

         % np as object_cplt
                 ws(v_v,tr_cod_cplt,clause:[],mwuw:0,
                 args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                       object:[type:np,oblig:yes,constraints:[case:acc,lex:or([pp3refl,pp1sg,pp2sg,pp1pl,pp2pl])]],
                       object_cplt:[type:np,oblig:yes,constraints:[class:common,case:acc]]])]).
% PRAESTARE
lexarg(praestare,
       arglist:[ws(praesto_excel,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]]])]).
% PREMERE
lexarg(premere,
       arglist:[
             ws(v_v,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:np,oblig:no,constraints:[case:acc]]])]).
% PROCURARE
lexarg(procurare,
       arglist:[ws(procuro_procure,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).
% PUGNARE
% Pugnavere Romani cum Germanis.
lexarg(pugnare,
       arglist:[ws(pugno_fight,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      prep_cplt:[type:pp,oblig:no,constraints:[prep:cum]]])]).





% QUAERERE
% Rex quaerebat uitam beatam in natura.
% Quaero num pater tuus uenerit.
% Quaero ueneritne pater tuus.
lexarg(quaerere,
       arglist:[ws(quaero_seek,tr_cod,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     object:[type:np,oblig:no,constraints:[case:acc]]]),

                ws(quaero_ask,tr_cod,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     object:[type:pred,oblig:yes,
                     constraints:[type:finite,gap:[],mood:subjunctive,checkint:yes]]])]).
                       

% QUATERE
lexarg(quatere,
       arglist:[
             ws(quatio_shake,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).


% QUERI
lexarg(queri,
       arglist:[
             ws(queror_complain,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:no,constraints:[case:acc]]]),

             ws(queror_complain,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      prep_cplt:[type:pp,oblig:yes,constraints:[prep:or([cum,apud])]],
                      object:[type:pred,oblig:no,constraints:[type:finite,gap:[],
                                                              argbound:yes, subordinator:quod]]]),
              ws(queror_complain,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      prep_cplt:[type:pp,oblig:yes,constraints:[prep:de]]]),  

              ws(queror_complain,tr_inf,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:nonfinite]]]),

             ws(queror_complain,tr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:finite,gap:[],
                                                              argbound:yes, subordinator:quod]]])]).




% RAPERE
lexarg(rapere,
       arglist:[ws(rapio_seize,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]],
                      prep_cplt:[type:pp,oblig:no,constraints:[prep:ad]] ])]).
% RECIPERE
lexarg(recipere,
       arglist:[
             ws(recipio_receive,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:no,constraints:[case:acc]]])]).
% RECUBARE
% Rege sub tegmine fagi recubante scribit regina epistulam ad servum Marci.
lexarg(recubare,
       arglist:[ws(recubo_lie_down,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      prep_cplt:[type:pp,oblig:yes,constraints:[]]])]).
% REDDERE
lexarg(reddere,
       arglist:[ws(reddo_give_back,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]]),

                ws(reddo_make,tr_cod_cplt,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                       object:[type:np,oblig:yes,constraints:[case:acc,number:N,gender:G]],
                       object_cplt:[type:adjp,oblig:yes,constraints:[case:acc, number:N,gender:G]]])]).
% REGNARE
% Tres annos regnavit.
lexarg(regnare,
       arglist:[ws(regno_reign,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]]])]).

% RELINQUERE
lexarg(relinquere,
       arglist:[ws(relinquo_leave,tr_cod,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     object:[type:np,oblig:yes,constraints:[case:acc]]])]).
	% LINQUERE
	lexarg(linquere,
       arglist:[ws(linquo_leave,tr_cod,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     object:[type:np,oblig:yes,constraints:[case:acc]]])]).
% RENOVARE
% quies renovavit corpora animosque
lexarg(renouare,
       arglist:[ws(renouo_renew,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).

% REOR (+acc_cum_inf)
% Omnes hostes rogaturos esse auxilium ratus est.
lexarg(reri,
       arglist:[ws(reor_think_that,tr_inf,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:nonfinite]]]),
               
                ws(reor_think_it_appropriate_to,tr_inf,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:gerund,local_case:acc]]])]).

% REPUDIARE
lexarg(repudiare,
       arglist:[ws(repudio1_reject,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc,sem:[abstract] ]]])]).

lexarg(repudiare,
       arglist:[ws(repudio2_repudiate,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc,sem:[hum]]]])]).

% RESTITUERE
lexarg(restituere,
       arglist:[ws(restituo_restore,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).
% ROGARE
% Eo auxilium rogatum.
% Rogebant quae fortuna exercitus esset.
lexarg(rogare,
       arglist:[ws(rogo_ask_for,tr_cod,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     object:[type:np,oblig:yes,constraints:[case:acc]]]),

                ws(rogo_ask,tr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      i_object:[type:np,oblig:no,constraints:[case:acc]],
                      object:[type:pred,oblig:yes,constraints:[type:finite,gap:[],mood:subjunctive,
                                                              argbound:yes, subordinator:or([ne,ut])]]]),

               ws(rogo_ask,tr_cod,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,
                       constraints:[type:finite,gap:[],mood:subjunctive, checkint:yes]]])]).







% SALVERE
lexarg(saluere,
       arglist:[ws(salveo_be_well,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]]])]).

% SALUTARE
lexarg(salutare,
       arglist:[
             ws(saluto_greet,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:no,constraints:[case:acc,sem:[hum]]]])]).

% SCIRE (+acc_cum_inf)
% Vincere scis.
% Caesar se Germanos vicisse sciebat.
lexarg(scire,
       arglist:[ws(scio_know_that,tr_inf,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     object:[type:pred,oblig:yes,constraints:[type:nonfinite]]]),

         % + subjectless nonfinite clause - vincere scis
              ws(scio_know_how_to,tr_inf_no_subj,clause:[],mwuw:0,
              args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                    object:[type:pred,oblig:yes,constraints:[type:nonfinite_i]]])]).

	% NESCIRE (+acc_cum_inf)
	% Victoria uti nescis.
	lexarg(nescire,
       arglist:[ws(nescio_not_know_that,tr_inf,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:nonfinite]]]),

         % + subjectless nonfinite clause - petere consulatum nescis
                ws(nescio_not_know_how_to,tr_inf_no_subj,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:nonfinite_i]]])]).

% SCRIBERE
% Si rex amasset servas, scripsisset libellos impudicos.
lexarg(scribere,
       arglist:[ws(scribo_write,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:no,constraints:[case:acc]],
                      i_object:[type:np,oblig:no,case:dat,constraints:[case:dat, sem:[hum]]]]),


                 ws(scribo_write,tr_cod_prep_cplt,clause:[],mwuw:0,
                 args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                       object:[type:np,oblig:yes,constraints:[case:acc]],
                       prep_cplt:[type:pp,oblig:no,constraints:[case:acc,sem:[hum], prep:ad]]])]).
% SEDARE
lexarg(sedare,
       arglist:[
             ws(sedo_appease,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:np,oblig:no,constraints:[case:acc]]])]).
% SEGREGARE
lexarg(segregare,
       arglist:[ws(segrego_separate,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).
% SENTIRE
lexarg(sentire,
arglist:[ws(sentio_perceive,tr_inf,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:nonfinite]]]),

        ws(sentio_perceive,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:no,constraints:[case:acc]]]),

        ws(sentio_perceive,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,
                              constraints:[type:finite,gap:[],mood:subjunctive, checkint:yes]]])]).
% SEQUI I
% Sequuntur caedes.
lexarg(sequi_1,
       arglist:[ws(sequor_follow,intr,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[]]])]).

% SEQUI II
% Crediderunt me te secutum fuisse.
lexarg(sequi_2,
       arglist:[ws(sequor_follow,tr_cod,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[]],
                     object:[type:np,oblig:yes,constraints:[case:acc]]])]).

% SERPERE
% specifically handcrafted for Virgil's "Hanc sine tempora circum inter victrices hederam tibi serpere lauros"
lexarg(serpere,
       arglist:[ws(serpo_crawl,intr,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[]],
                     prep_cplt:[type:pp,oblig:yes,constraints:[prep:inter]],
                     prep_cplt:[type:pp,oblig:yes,constraints:[prep:circum]]])]).
% SERUARE
% Aestiuo serues ubi piscem tempore quaeris? (Martial, 2.78)
lexarg(seruare,
       arglist:[ws(servo_keep_safe,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]]),

                ws(servo_keep,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]],
                      object_cplt:[type:adjp,oblig:no,constraints:[case:acc]]])]).
% SINERE
lexarg(sinere,
arglist:[ws(sino_allow,tr_inf,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:nonfinite]]]),

           ws(sino_allow,tr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:finite,gap:[],mood:subjunctive]]])]).

% SOLUERE
lexarg(soluere,
       arglist:[ws(soluo_release,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).

% SPERARE
lexarg(sperare,
       arglist:[ws(spero_hope_for,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:no,constraints:[case:acc]]]),

                
                ws(spero_hope_that,tr_inf,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:nonfinite]]])]).

% STARE
lexarg(stare,
       arglist:[ws(sto_stand,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]]])]).

% STUDERE
% nouis rebus studebat.
lexarg(studere,
       arglist:[ws(studeo_desire,tr_cod,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     object:[type:np,oblig:yes,constraints:[case:dat]]]),

               ws(studeo_want_to,tr_inf_no_subj,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     object:[type:pred,oblig:yes,constraints:[type:nonfinite_i]]])]).

% SUDARE
lexarg(sudare,
       arglist:[ws(sudo_sweat,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]]])]).

% SUSPENDERE
% Me tabula sacer votiva paries indicat uvida suspendisse potenti vestimenta maris deo.
lexarg(suspendere,
       arglist:[ws(suspendo_dedicate,tr_cod_coi,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      i_object:[type:np,oblig:no,constraints:[case:dat, sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc, sem:[thing]]]])]).






% TACERE
% Tacebant omnes senatores.
lexarg(tacere,
       arglist:[ws(taceo_keep_silent,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]]])]).
% TEGERE
lexarg(tegere,
       arglist:[
             ws(tego_cover,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:np,oblig:no,constraints:[case:acc]]])]).
% TEMPTARE
lexarg(temptare,
       arglist:[ws(tempto_try,intr,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     object:[type:pred,oblig:yes,constraints:[type:nonfinite_i]]]),
                ws(tempto_tempt,tr_cod,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     object:[type:np,oblig:yes,constraints:[case:acc]]])]).

lexarg(tentare,
       arglist:[ws(tento_try,intr,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     object:[type:pred,oblig:yes,constraints:[type:nonfinite_i]]]),

                 ws(tento_tempt,tr_cod,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     object:[type:np,oblig:yes,constraints:[case:acc]]])]).



  
% TENERE
lexarg(tenere,
       arglist:[ws(teneo_hold,tr_cod,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     object:[type:np,oblig:yes,constraints:[case:acc]]]),
               
                 ws(teneo_keep,tr_cod_cplt,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                       object:[type:np,oblig:yes,constraints:[case:acc,number:N,gender:G]],
                       object_cplt:[type:np,oblig:yes,constraints:[case:acc, number:N,gender:G]]])]).
	% RETINERE
	lexarg(retinere,
             arglist:[ws(retineo_keep,tr_cod,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     object:[type:np,oblig:yes,constraints:[case:acc]]])]).
       	% OBTINERE
	lexarg(obtinere,
             arglist:[ws(obtineo_maintain,tr_cod,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     object:[type:np,oblig:yes,constraints:[case:acc]]])]).

% TIMERE
% Timeo Danaos et dona ferentes.
% Timeo amicis meis.
% Timeo ne veniant ad urbem capiendam.
lexarg(timere,
       arglist:[ws(timeo_fear,tr_cod,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     object:[type:np,oblig:yes,constraints:[case:acc]]]),

               ws(timeo_fear_for,tr_cod,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     object:[type:np,oblig:yes,constraints:[case:dat]]]),

               ws(timeo_fear_that,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,
                              constraints:[type:finite,gap:[],mood:subjunctive, argbound:yes,subordinator:or([ne,ut])]]])]).
% TOLLERE
lexarg(tollere,
       arglist:[
             ws(tollo_raise_up,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:np,oblig:yes,constraints:[case:acc]],
                      prep_cplt:[type:pp,oblig:no,constraints:[]] ] )] ).

% TRADERE (+acc_cum_inf)
% Cicero litteras longas scribere traditur.
lexarg(tradere,
       arglist:[ws(trado_report_that,tr_inf,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:nonfinite]]]),
             
                ws(trado_report,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:no,constraints:[case:acc]],
                      i_object:[type:np,oblig:no,constraints:[case:dat,sem:[hum]]]]),

                ws(trado_yield,tr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]],
                      i_object:[type:np,oblig:yes,constraints:[case:dat,sem:[hum]]]])]).
% TRAHERE
lexarg(trahere,
       arglist:[
             ws(traho_drag,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:np,oblig:no,constraints:[case:acc]]])]).
% DETRAHERE
lexarg(detrahere,
       arglist:[
             ws(detraho_pull_down,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:np,oblig:no,constraints:[case:acc]]])]).
% TRANSIRE
lexarg(transire,
       arglist:[ws(transeo_spend,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc, sem:[time]]],
                       adjunct:[type:adjunct,oblig:yes,constraints:[value:manner_means]]])]).
% TRUCIDARE
lexarg(trucidare,
       arglist:[
             ws(trucido_kill,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:no,constraints:[sem:[hum],case:acc]]])]).
                      


             




% USURPARE
lexarg(usurpare,
       arglist:[
             ws(usurpo_use,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:no,constraints:[case:acc]]])]).
% UTI
% Cicerone magistro usi sunt multi magistri.
lexarg(uti,
       arglist:[ws(utor_use,tr_cod,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[]],
                     object_i:[type:np,oblig:yes,constraints:[case:abl]],
                     object_cplt:[type:np,oblig:no,constraints:[case:abl,lextype:full]]])]).
                    % prep_cplt:[type:pp,oblig:no,constraints:[prep:in, case:abl]]])]).



% VACARE
lexarg(uacare,
       arglist:[
             ws(uaco_be_available,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      arg:[type:np,oblig:no,constraints:[case:dat]]])]).
% VENIRE
% Timeo ne veniant.
lexarg(uenire,
       arglist:[ws(uenio_come,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      prep_cplt:[type:pp,oblig:no,constraints:[prep:or([ad,in]), case:acc]],
                      cplt:[type:np,oblig:no,constraints:[case:acc,sem:[city]]],
                      cplt:[type:pred,oblig:no,constraints:[type:nonfinite_i, mood:supine]] ])]).

        % CONVENIRE
        lexarg(conuenire,
          arglist:[ws(conuenio_gather,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      prep_cplt:[type:pp,oblig:no,constraints:[prep:or([ad,in]), case:acc]],
                      cplt:[type:np,oblig:no,constraints:[case:acc,sem:[city]]],
                      cplt:[type:pred,oblig:no,constraints:[type:nonfinite_i, mood:supine]] ])]).

	% PERUENIRE
	lexarg(peruenire,
        arglist:[ws(peruenio_arrive,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      prep_cplt:[type:pp,oblig:yes,constraints:[prep:or([ad,in]), case:acc]]] )  ]).
% VERERI
% Ne hostis vincat vereor.
% Vereor Italiam petere.
lexarg(uereri,
       arglist:[ws(uereor_fear,tr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:nonfinite_i]]]),

                ws(uereor_fear,tr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:finite,gap:[],mood:subjunctive,
                              argbound:yes, subordinator:or([ne,ut])]]])]).

% UERSARI
lexarg(uersari,
       arglist:[ws(uersor_dwell,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      prep_cplt:[type:pp,oblig:yes,constraints:[prep:or([cum,in]), case:abl]]])]).

% UERTERE
lexarg(uertere,
       arglist:[
             ws(uerto_turn,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:no,constraints:[case:acc]],
                      prep_cplt:[type:pp,oblig:no,constraints:[prep:or([ad,in])]] ])]).
% CONVERTERE
lexarg(conuertere,
       arglist:[
             ws(conuerto_turn,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:no,constraints:[case:acc]],
                      prep_cplt:[type:pp,oblig:no,constraints:[prep:or([ad,in])]] ])]).

% UESPERASCERE
lexarg(uesperascere,
       arglist:[ws(dies_uesperascit_it_is_getting_dark,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[lex:dies]]])]).


% VIDERE
% Italiam peto causa videndae Romae in montibus positae.
lexarg(uidere,
       arglist:[ws(uideo_see,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:np,oblig:no,constraints:[case:acc]]]),

                ws(uideo_see,tr_inf,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:nonfinite]]]),  

                ws(uideo_see,tr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:finite,gap:[],mood:indicative,
                              argbound:yes, subordinator:ut]]])]).


	% PROVIDERE
        lexarg(prouidere,
     		arglist:[ws(prouideo_see_to_it_that_not,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:pred,oblig:yes,constraints:[type:finite,gap:[],mood:subjunctive,
                              argbound:yes, subordinator:ne]]])]).
% VIDERI
lexarg(uideri,
       arglist:[ws(uideor_seem,intr,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,                constraints:[case:C]],
                     i_object:[type:np,oblig:no,                constraints:[case:dat,sem:[hum]]],
                     predicative:[type:np,oblig:yes, 	
                                                                constraints:[case:C,class:common]]]),
             ws(uideor_seem,intr,clause:[],mwuw:0, 
              args:[subject:[type:np,oblig:yes,constraints:[]],
                     i_object:[type:np,oblig:no,                constraints:[case:dat,sem:[hum]]],
                     object:[type:pred,oblig:yes,               constraints:[type:nonfinite_i]]]),

             ws(uideor_seem_appropriate,intr,clause:[],mwuw:2,                                      % weightier 
              args:[ arg:[type:np,oblig:yes,                constraints:[case:dat,sem:[hum]]],
                     subject:[type:pred,oblig:yes,               constraints:[type:nonfinite_i]]]),

             ws(uideor_seem,intr,clause:[],mwuw:0,  
               args:[subject:[type:np,oblig:yes,                 constraints:[number:Nb2,gender:Gender2,case:Case2]],
                     i_object:[type:np,oblig:no,                 constraints:[case:dat,sem:[hum]]],    
                     predicative:[type:adjp,oblig:yes, 
                   					         constraints:[number:Nb2,gender:Gender2,case:Case2]]]),

              ws(uideor_seem,intr,clause:[],mwuw:0,  
              args:[subject:[type:pred,oblig:yes,                 constraints:[type:nonfinite_i, mood:except([supine])]],
                              predicative:[type:adjp,oblig:yes,  constraints:[number:sing,gender:neuter,case:nom]]])]).

% VINCERE
% Vincere scis.
lexarg(uincere,
       arglist:[ws(uinco_win,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:np,oblig:no,constraints:[sem:[hum],case:acc]]])]).
% VITARE
% Parsimonia est ars uitandi sumptus superuacuos.
lexarg(uitare,
       arglist:[ws(uito_avoid,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]]])]).
% UIUERE
% Vixerunt.
% Vixit vitam longam beatamque.
lexarg(uiuere,
       arglist:[ws(uiuo_live,intr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:no,constraints:[lex:uita,case:acc]]])]).

% UOCARE
% Nauta rationes puellae in dubium uocat.
% Vocavit matrem eius et non uenit.
lexarg(uocare,
       arglist:[ws(uoco_call,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]],
                      object_cplt:[type:np,oblig:no,constraints:[case:acc]]]),

                ws(uoco_call,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[case:acc]],
                      prep_cplt:[type:pp,oblig:yes,constraints:[prep:ad, case:acc]]]),
                     

                ws(uoco_call,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[sem:[hum],case:acc]]]),
                

         % mwu IN IUS VOCARE
             ws(mwu_in_ius_uoco_bring_to_court_IDIOM,tr_cod_cplt,clause:[],mwuw:2,
             args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                   object:[type:np,oblig:yes,constraints:[sem:[hum],case:acc]],
                   object_cplt:[type:phrase,frozen:yes,oblig:yes,constraints:[lex:in_ius]]]),

         % mwu IN DUBIUM VOCARE
              ws(mwu_in_dubium_uoco_call_into_doubt_IDIOM,tr_cod_cplt,clause:[],mwuw:2,
              args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                    object:[type:np,oblig:yes,constraints:[sem:[abstract],case:acc]],
                    object_cplt:[type:phrase,frozen:yes,oblig:yes,constraints:[lex:in_dubium]]])]).


	% REUOCARE
	lexarg(reuocare,
       arglist:[ws(reuoco_call_back,tr_cod,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:np,oblig:yes,constraints:[sem:[hum],case:acc]]])]).

% UOLARE
       lexarg(uolare,
       arglist:[ws(uolo_fly,intr,clause:[],mwuw:0,
                  args:[subject:[type:np,oblig:yes,constraints:[]],
                      prep_cplt:[type:pp,oblig:no,constraints:[prep:or([ad,in]), case:acc]]])]).



% POSSE VELLE NOLLE MALLE and FIERI
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




aux(posse, value:possibility_capacity).
aux(uelle, value:will).
% aux(cupere, value:will).
aux(studere, value:will).
aux(nolle, value:negative_will).
aux(malle, value:preference).
aux(solere, value:habit).
aux(debere, value:deontic).
aux(coepi, value:inchoative).
aux(incipere, value:inchoative).
aux(desinere, value:cessative).
aux(uideri, value:plausibility).
aux(audere, value:dare).


% We regard the above verbs as aux when governing an infinitive, not an acc-cum-inf clause or a finite one


% POSSE (+ subjectless nonfinite clause)
% Legere possunt.
verb([v(aux,posse,possum,potes,potest,possumus,potestis,possunt,
            possim,possis,possit,possimus,possitis,possint,
            poteram,poteras,poterat,poteramus,poteratis,poterant,
            potero,poteris,poterit,poterimus,poteritis,poterunt,
            possem,posses,posset,possemus,possetis,possent,
            posse,potu)],tr_inf_no_subj,std).

lexarg(posse,
       arglist:[ws(possum_be_able_to,tr_inf_no_subj,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[]],
                      object:[type:pred,oblig:yes,constraints:[type:nonfinite_i]]])]).


% VELLE (+ subjectless nonfinite clause)
%  Epistulas tuas legere uolo.
verb([v(aux,uelle,uolo,uis,uult,uolumus,uultis,uolunt,
        uelim,uelis,uelit,uelimus,uelitis,uelint,
        uolebam,uolebas,uolebat,uolebamus,uolebatis,uolebant,
        uolam,uoles,uolet,uolemus,uoletis,uolent,
        uellem,uelles,uellet,uollemus,uolletis,uollent,
        uelle,uolu)],tr_inf_no_subj,std).
lexarg(uelle,
       arglist:[% ws(uolo_want_to,tr_inf_no_subj,clause:[],mwuw:0,
               % args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                 %    object:[type:pred,oblig:yes,constraints:[type:nonfinite_i]]]),

                ws(uolo_want_sbd_to,tr,clause:[],mwuw:0,
                args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                      object:[type:pred,oblig:yes,constraints:[type:finite,gap:[],mood:subjunctive]]]),

                ws(uolo_want_sbd_to,tr_inf,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     object:[type:pred,oblig:yes,constraints:[type:nonfinite]]])]).

% NOLLE (+ subjectless nonfinite clause)
% Nolite Lugdunum ire.
verb([v(aux,nolle,nolo,xxx,xxx,nolumus,xxx,nolunt,
        nolim,nolis,nolit,nolimus,nolitis,nolint,
        nolebam,nolebas,nolebat,nolebamus,nolebatis,nolebant,
        xxx,xxx,xxx,xxx,xxx,xxx,
        nollem,nolles,nollet,nollemus,nolletis,nollent,
        nolle,nolu)],tr_inf_no_subj,std).
lexarg(nolle,
       arglist:[% ws(nolo_refuse_to,tr_inf_no_subj,clause:[],mwuw:0,
               % args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                 %    object:[type:pred,oblig:yes,constraints:[type:nonfinite_i]]]),

                ws(nolo_refuse_to,tr_inf,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     object:[type:pred,oblig:yes,constraints:[type:nonfinite]]])]).

% MALLE (+ subjectless nonfinite clause)
%  Rex Italiam petere mavult.
verb([v(aux,malle,malo,mauis,mauult,malumus,mauultis,malunt,
        malim,malis,malit,malimus,malitis,malint,
        malebam,malebas,malebat,malebamus,malebatis,malebant,
        xxx,xxx,xxx,xxx,xxx,xxx,
        mallem,malles,mallet,mallemus,malletis,mallent,
        malle,malu)],tr_inf_no_subj,std).
lexarg(malle,
       arglist:[% ws(malo_prefer_to,tr_inf_no_subj,clause:[],mwuw:0,
               % args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                 %    object:[type:pred,oblig:yes,constraints:[type:nonfinite_i]]]),

                ws(malo_prefer_to,tr_inf,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[sem:[hum]]],
                     object:[type:pred,oblig:yes,constraints:[type:nonfinite]]])]).

% FIERI
% Fit ut omnes me  libellos impudicos legere sciant.
% Fiat lux et facta est lux.
verb([v(aux,fieri,fio, fis,fit,fimus, fitis, fiunt,
        fiam, fias, fiat, fiamus, fiatis, fiant,
        fiebam,fiebas,fiebat,fiebamus,fiebatis,fiebant,
        fiam,fies,fiet,fiemus,fietis,fient,
        fierem,fieres,fieret,fieremus,fieretis,fierent,
        fieri,xxx)],vpred,std).

lexarg(fieri,
       arglist:[  ws(fio_become,vpred,clause:[],mwuw:0,
                 args:[subject:[type:np,oblig:yes,constraints:[case:or([nom,acc])]],
                 predicative:[type:np,oblig:yes, constraints:[case:or([nom,acc]), class:common]]]),

               ws(fio_become,vpred,clause:[],mwuw:0,
               args:[subject:[type:np,oblig:yes,constraints:[]]]),

               ws(fio_become,vpred,clause:[],mwuw:0,
               args:[subject:[type:pred,oblig:yes,constraints:[type:finite,gap:[],mood:subjunctive,
                                                              argbound:yes,subordinator:ut]]])]).


% IMPERSONAL VERBS
%%%%%%%%%%%%%%%%%%

% CONSTAT
verb([v(imp,constat,constat,constet,constabat,constabit,constaret,constare,constit)],vimp,std).
lexarg(constat,
       arglist:[ws(constat_it_is_established_that,intr,clause:[],mwuw:0,
               args:[arg:[type:pred,oblig:yes,constraints:[type:nonfinite]]])]).
% DECET
verb([v(imp,decet,decet,deceat,decebat,decebit,deceret,decere,decu)],vimp,std).
lexarg(decet,
       arglist:[ws(decet_it_behoves,tr_coi,clause:[],mwuw:0,
               args:[subject:[type:pred,oblig:yes,constraints:[type:nonfinite_i]],
                     arg:[type:np,oblig:no,constraints:[case:acc, sem:[hum]]]])]).

              
% FALLIT
verb([v(imp,fallit,fallit,fallat,fallebat,fallet,falleret,fallere,fefell)],vimp,std).
lexarg(fugit,
       arglist:[ws(fallit_it_escapes,tr_coi,clause:[],mwuw:0,
               args:[subject:[type:pred,oblig:yes,constraints:[type:nonfinite_i]],
                     arg:[type:np,oblig:yes,constraints:[case:acc, sem:[hum]]]])]).
% FUGIT
verb([v(imp,fugit,fugit,fugiat,fugiebat,fugiet,fugeret,fugere,fug)],vimp,std).
lexarg(fugit,
       arglist:[ws(fugit_it_escapes,tr_coi,clause:[],mwuw:0,
               args:[subject:[type:pred,oblig:yes,constraints:[type:nonfinite_i]],
                    arg:[type:np,oblig:yes,constraints:[case:acc, sem:[hum]]]])]).
% INTEREST
verb([v(imp,interest,interest,intersit,intererat,intererit,interesset,interesse,interfu)],vimp,std).
lexarg(interest,
       arglist:[ws(interest_it_matters_that,intr,clause:[],mwuw:0,
               args:[subject:[type:pred,oblig:yes,constraints:[type:nonfinite]],
                     arg:[type:np,oblig:no,constraints:[case:gen]],
                     arg:[type:adjp,oblig:no,constraints:[case:abl, type:poss]],
                      prep_cplt:[type:pp,oblig:no,constraints:[case:acc, prep:ad]]]),

                ws(interest_it_matters_that,intr,clause:[],mwuw:0,
               args:[subject:[type:pred,oblig:yes,constraints:[type:finite,gap:[],mood:subjunctive, checkint:yes]],
                      arg:[type:np,oblig:no,constraints:[case:gen]],
                     arg:[type:adjp,oblig:no,constraints:[case:abl, type:poss]],
                      prep_cplt:[type:pp,oblig:no,constraints:[case:acc, prep:ad]]] ),

                 ws(interest_it_matters_that,intr,clause:[],mwuw:0,
               args:[subject:[type:pred,oblig:yes,constraints:[type:finite,gap:[],mood:subjunctive,argbound:yes, subordinator:or([ut,ne])]],
                     arg:[type:np,oblig:no,constraints:[case:gen]],
                     arg:[type:adjp,oblig:no,constraints:[case:abl, type:poss]],
                      prep_cplt:[type:pp,oblig:no,constraints:[case:acc, prep:ad]]] )]).
% IUUAT
verb([v(imp,iuuat,iuuat,iuuet,iuuabat,iuuabit,iuuaret,iuuare,iuu)],vimp,std).
lexarg(iuuat,
       arglist:[ws(iuuat_it_pleases,tr_coi,clause:[],mwuw:0,
               args:[subject:[type:pred,oblig:yes,constraints:[type:nonfinite_i]],
                     arg:[type:np,oblig:yes,constraints:[case:acc, sem:[hum]]]])]).
% LICET
verb([v(imp,licet,licet,liceat,licebat,licebit,liceret,licere,licu)],vimp,std).
lexarg(licet,
       arglist:[ws(licet_permission_is_given_to,tr_coi,clause:[],mwuw:0,
               args:[subject:[type:pred,oblig:yes,constraints:[type:nonfinite_i]]]),
                
                 ws(licet_permission_is_given_to,intr,clause:[],mwuw:0,
               args:[arg:[type:pred,oblig:yes,constraints:[gap:[],type:finite,gap:[],mood:subjunctive]]])]).


% OPORTET
verb([v(imp,oportet,oportet,oporteat,oportebat,oportebit,oporteret,oportere,oportu)],vimp,std).
lexarg(oportet,
       arglist:[ws(oportet_it_is_necessary_for_X_to,intr,clause:[],mwuw:0,
               args:[subject:[type:pred,oblig:yes,constraints:[type:nonfinite]]]),

                ws(oportet_it_is_necessary_to,intr,clause:[],mwuw:0,
               args:[subject:[type:pred,oblig:yes,constraints:[type:nonfinite_i]]]),

                ws(oportet_it_is_necessary_that,intr,clause:[],mwuw:0,
               args:[subject:[type:pred,oblig:yes,constraints:[type:finite,gap:[],mood:subjunctive]]])]).

% PAENITET
verb([v(imp,paenitet,paenitet,paeniteat,paenitebat,paenitebit,paeniteret,paenitere,paenitu)],vimp,std).
lexarg(paenitet,
       arglist:[ws(paenitet_regret_doing,tr_coi,clause:[],mwuw:0,
               args:[subject:[type:pred,oblig:yes,constraints:[type:nonfinite_i]],
                     arg:[type:np,oblig:yes,constraints:[case:acc, sem:[hum]]]]),

                  % eos peccatorum paenitet
                ws(paenitet_regret,tr_coi,clause:[],mwuw:0,
               args:[subject:[type:dummy],                  % the arglist needs a subject, a dummy one is provided
                     arg:[type:np,oblig:yes,constraints:[case:gen]],
                     cplt:[type:np,oblig:yes,constraints:[case:acc, sem:[hum]]]])]).
% PERTINET
verb([v(imp,pertinet,pertinet,pertineat,pertinebat,pertinebit,pertineret,pertinere,pertinu)],vimp,std).
lexarg(pertinet,
       arglist:[ws(pertinet_it_is_important_that,intr,clause:[],mwuw:0,
               args:[subject:[type:pred,oblig:yes,constraints:[type:nonfinite]],
                     prep_cplt:[type:pp,oblig:no,constraints:[case:acc, prep:ad]]]),

                ws(pertinet_it_is_important_that,intr,clause:[],mwuw:0,
               args:[subject:[type:pred,oblig:yes,constraints:[type:finite,gap:[],mood:subjunctive, checkint:yes]],
                     prep_cplt:[type:pp,oblig:no,constraints:[case:acc, prep:ad]]])]).
% PLACET
verb([v(imp,placet,placet,placeat,placebat,placebit,placeret,placere,placu)],vimp,std).
lexarg(placet,
       arglist:[ws(placet_it_be_decided_to,tr_coi,clause:[],mwuw:0,
               args:[subject:[type:pred,oblig:yes,constraints:[type:nonfinite_i, tense:present]],
		arg:[type:np,oblig:no,constraints:[case:dat, sem:[hum]]]]),

               ws(placet_it_be_decided_to,tr_coi,clause:[],mwuw:0,
               args:[subject:[type:pred,oblig:yes,constraints:[type:finite,gap:[],mood:subjunctive,argbound:yes,subordinator:ut]],
		arg:[type:np,oblig:no,constraints:[case:dat, sem:[hum]]]])]).

% Coniuratis tamen metu proditionis permotis placitum maturare caedem 15.52.1 Tac. Ann

/*

% Catered for under uideri
% UIDETUR
verb([v(imp,uidetur,uidetur,uideatur,uidebatur,uidebitur,uideretur,uideri,????)],vimp,std).
lexarg(uidetur,
       arglist:[ws(uidetur_it_seems_appropriate_to,tr_coi,clause:[],mwuw:0,
               args:[subject:[type:pred,oblig:yes,constraints:[type:nonfinite_i]],
		arg:[type:np,oblig:no,constraints:[case:dat, sem:[hum]]]])]).

*/             

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SUM
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ESSE is MUCH TOO expensive in the present framework
% sth will have to be done to improve efficiency

% !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
% ESSE, coordination and clause-level adjuncts are responsible for a large part of the inefficiency plaguing ALP
% !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


verb([v(esse,3,xxx,fu,xxx)],v_esse,std).
lexarg(esse,
       arglist:[     % Marcus legatus est.
		     % Pecunia magna documentum est avaritiae.

                     ws(sum_be,v_esse,clause:[],mwuw:0,
                        args:[subject:[type:np,oblig:yes,		
                                                                constraints:[case:C]],
                              predicative:[type:np,oblig:yes, 	
                                                                constraints:[case:C,class:common]]]),

                      % number and gender constraints on the predicative np would be too hard -
                      % the case constraint is the only one to be really binding
                 

                     % Marcus bonus est.

                     ws(sum_be,v_esse,clause:[],mwuw:0,
                        args:[subject:[type:np,oblig:yes,
                   					         constraints:[number:Nb2,gender:Gender2,case:Case2]],
                              predicative:[type:adjp,oblig:yes, 
                   					         constraints:[number:Nb2,gender:Gender2,case:Case2]]]),
                     % here the triple constraint is necessary



                     % Karthago delenda est.

                     ws(sum_be,v_esse,clause:[],mwuw:0,
                         args:[subject:[type:np,oblig:yes,
                                                                  constraints:[number:Nb2,gender:Gender2,case:Case2]],
                               predicative:[type:gerundive,oblig:yes, 
                                                                  constraints:[number:Nb2,gender:Gender2,case:Case2]]]),

                                 % if an arg is opened (dative np) for the subject of the gerund,
                                 % we have a case of 'est_alicui_have_to', for which see below



                    % Malum est insanire / quod boni maerebant incitamentum erat / Malum erat quod boni maerebant    


                          ws(sum_be,v_esse,clause:[],mwuw:0,
                       args:[subject:[type:pred,oblig:yes,
                                                                 constraints:[type:nonfinite_i, mood:except([supine])]],
                              predicative:[type:adjp,oblig:yes,  constraints:[number:sing,gender:neuter,case:nom]]]),

                      ws(sum_be,v_esse,clause:[],mwuw:0,
                          args:[subject:[type:free_sub,oblig:yes,
                                                                   constraints:[]],
                                predicative:[type:adjp,oblig:yes,  constraints:[number:sing,gender:neuter,case:nom]]]),

                       ws(sum_be,v_esse,clause:[],mwuw:0,
                          args:[subject:[type:free_sub,oblig:yes,
                                                                   constraints:[]],
                                predicative:[type:np,oblig:yes,    constraints:[number:sing,gender:neuter,case:nom]]]),



                  % Obliuisci est in nostra potestate.

                     ws(sum_be,v_esse,clause:[],mwuw:0,
                          args:[subject:[type:pred,oblig:yes,       constraints:[mood:except([supine])]],
                                predicative:[type:pp,oblig:yes,     constraints:[sem:[abstract]]]]),

             
                  % Marcus erat in templo.

                     ws(sum_be,v_esse,clause:[],mwuw:0,
                         args:[subject:[type:np,oblig:yes,         constraints:[]],
                               predicative:[type:pp,oblig:yes,     constraints:[]]]),


                   % Marcus erat Lugduni, domi

                     ws(sum_be_in,v_esse,clause:[],mwuw:0,
                        args:[subject:[type:np,oblig:yes,      constraints:[sem:[hum]]],
                              adjunct:[type:adjunct,oblig:yes, constraints:[value:place]]]),


                   % ubi, quando, cur, quare, etc. with esse

                     ws(sum_be_in,v_esse,clause:[],mwuw:0,
                        args:[subject:[type:np,oblig:yes,       constraints:[case:or([nom,acc])]],
                              predicative:[type:advp,oblig:yes, constraints:[type:int]]]),


                   % note below the semantic values assigned, perceptible in the translations:
                   % note also the bonuses assigned in the mwuw feature value
                   % for multi-word unit readings of configurations involving sum and extra
                   % syntactic and lexical material

                   % Opus est mihi Marci patientia.

                     ws(opus_est_need_IDIOM,v_esse,clause:[],mwuw:2,
                         args:[subject:[type:np,oblig:yes,      constraints:[lex:opus,txt:opus,pathlist:[_]]],
                               i_object:[type:np,oblig:yes,      constraints:[case:dat,sem:[hum]]],		% not really obligatory
                               object:[type:np,oblig:yes,       constraints:[case:abl]]]),

	        	% note the constraints on the subject:
		        % it must be a wordform whose lex is opus (used to avoid a context-retrievable subject being posited)
		        % it must be the wordform opus itself (opera, etc are not ok here)
		        % it must be on its own (pathlist value is made up of a single p(x,y) structure
                        % (the word opus cannot be qualified: * opus populorum)


		% Est imperatoris boni capere urbes.

                     ws(est_alicuius_be_the_mark_of_IDIOM,v_esse,clause:[],mwuw:2,
                         args:[subject:[type:pred,oblig:yes,   constraints:[type:nonfinite_i,mood:except([supine])]],
                               arg:[type:np,oblig:yes,    constraints:[case:gen,sem:[hum]]]]),

	
        	% Caesari urbs erat capienda. (2)
                % Caesari eundum Romam erat.  (1)
                % the i_object slot houses the subject of the verb derived from the gerund

                % with true gerund (1):

                    ws(est_alicui_have_to_IDIOM,v_esse,clause:[],mwuw:2,
                        args:[subject:[type:pred,oblig:yes,     constraints:[type:gerund]],
                              i_object:[type:np,oblig:yes,      constraints:[case:dat,sem:[hum]]]]),

                % with gerundive  (2):

                    ws(est_alicui_have_to_IDIOM,v_esse,clause:[],mwuw:2,
                       args:[subject:[type:np,oblig:yes,         constraints:[number:Nb2,gender:Gender2,case:Case2]],
                             predicative:[type:adjp,oblig:yes,   constraints:[type:gdiv,number:Nb2,gender:Gender2,case:Case2]],
                             i_object:[type:np,oblig:yes,        constraints:[case:dat,sem:[hum]]]]),



                % necesse est

                    ws(necesse_est_be_necessary_IDIOM,v_esse,clause:[],mwuw:2,
                      args:[subject:[type:pred,oblig:yes,        constraints:[type:nonfinite_i, mood:except([supine])]],
                            predicative:[type:adjp,oblig:yes,    constraints:[lex:necesse]],
                            arg:[type:np,oblig:no,               constraints:[case:dat,sem:[hum]]]]),

                    ws(necesse_est_be_necessary_IDIOM,v_esse,clause:[],mwuw:2,
                       args:[subject:[type:pred,oblig:yes,        constraints:[type:nonfinite]],
                             predicative:[type:adjp,oblig:yes,    constraints:[lex:necesse]]]),

                    ws(necesse_est_be_necessary_IDIOM,v_esse,clause:[],mwuw:2,
                        args:[subject:[type:pred,oblig:yes,
                                                                   constraints:[type:finite,gap:[],mood:subjunctive,
                                                                                         argbound:yes, subordinator:ut]],
                              predicative:[type:adjp,oblig:yes,    constraints:[lex:necesse]]]),

                    ws(necesse_est_be_necessary_IDIOM,v_esse,clause:[],mwuw:2,
                         args:[arg:[type:pred,oblig:yes,
                                                                   constraints:[type:finite,gap:[],mood:subjunctive, 
                                                                                          argbound:no]],  % note arg, not subject
                               predicative:[type:adjp,oblig:yes,   constraints:[lex:necesse]]]),


                 % Caesari est pecunia magna.

                    ws(est_alicui_possess_IDIOM,v_esse,clause:[],mwuw:2,
                        args:[subject:[type:np,oblig:yes,         constraints:[case:or([nom,acc])]],
                              i_object:[type:np,oblig:yes,        constraints:[case:dat,sem:[hum]]]])]).






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ADJECTIVE AND NOUN ARG BEARERS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% arg-bearing adjectives
%%%%%%%%%%%%%%%%%%%%%%%%

% having two lexarg structures rather than a single one with optional arg seems to prove more efficient


lexarg(alienus,
      arg:[type:pp,oblig:no,constraints:[case:abl,sem:[hum], prep:ab]]).

lexarg(cupidus,
      arg:[type:np,oblig:no,constraints:[case:gen]]).
lexarg(cupidus,
      arg:[type:pred, oblig:no,constraints:[local_case:gen,type:gerund]]).

lexarg(peritus,
      arg:[type:np,oblig:no,constraints:[case:gen]]).
lexarg(peritus,
      arg:[type:pred, oblig:no,constraints:[local_case:gen,type:gerund]]).

lexarg(plenus,
      arg:[type:np,oblig:no,constraints:[case:or([abl,gen])]]).

lexarg(ratus,
      arg:[type:pp,oblig:yes,constraints:[prep:in,case:acc,lex:aeternum]]).

lexarg(ultimus,
      arg:[type:np,oblig:no,constraints:[case:dat]]).

lexarg(supremus,
      arg:[type:np,oblig:no,constraints:[case:dat]]).


lexarg(uacuus,
      arg:[type:pp,oblig:no,constraints:[case:abl,prep:ab]]).
lexarg(uacuus,
      arg:[type:np,oblig:no,constraints:[case:abl]]).




% arg-bearing nouns
%%%%%%%%%%%%%%%%%%%


lexarg(ars,
        arg:[type:pred, constraints:[local_case:gen,type:gerund]]).

% defensor eius : his defensor
lexarg(defensor,
       arg:[type:np,constraints:[case:gen]]).

lexarg(diuersitas,
       arg:[type:pp,constraints:[prep:inter, case:acc]]).

% euentus eius : his death
lexarg(euentus,
       arg:[type:np,constraints:[case:gen]]).


% quies inter labores
lexarg(quies,
       arg:[type:pp,constraints:[prep:inter, case:acc]]).

lexarg(scientia,
        arg:[type:pred, constraints:[local_case:gen,type:gerund]]).

% senatusconsultum - senatusconsultum de mathematicis Italia pellendis Tacitus Ann XII 52 3
% lexarg(senatusconsultum,
 %     arg:[type:pp,constraints:[prep:de]]).
% correct but time-consuming...

% studium - studia in eos a quibus prouincias contemni intellegunt Cic Pro Murena XX 42
 lexarg(studium,
       arg:[type:pp,constraints:[prep:in, case:acc]]).

% studium - studio uidendae nouae urbis TL Ab Urb 1982
 lexarg(studium,
       arg:[type:pred, constraints:[local_case:gen,type:gerund]]).


% initium - initium ferendi ad uespasianum imperii
 lexarg(initium,
       arg:[type:pred, constraints:[local_case:gen,type:gerund]]).


% tempus - differs curandi tempus in annum?
 lexarg(tempus,
       arg:[type:pred, constraints:[local_case:gen,type:gerund]]).




/*
% included in the phrase negotium agere + gerund

lexarg(negotium,
        arg:[type:pred, constraints:[local_case:gen,type:gerund]]).
*/


% either we extend the args of scribere with ad+HUM or we associate the AD+NP as prep_cplt of
% an arg-bearing noun - not BOTH


/*
lexarg(epistula,
      arg:[type:pp,constraints:[case:acc,sem:[hum], prep:ad]]).

lexarg(litterae,
      arg:[type:pp,constraints:[case:acc,sem:[hum], prep:ad]]).
*/



%%%%%
% ADJ
%%%%%

% The n:Value feature is meant to capture the availability of the adj as a noun
% n:no - cannot be read as a noun  e.g. aureus
% n:yes - can always be read as noun e.g. clarus 
% n:n  - only neuter gender readable as noun: uerum / uera
% n:hpl - readable as a noun if both plural and with human reference : boni


% 1


/* % unus, unius, uni
declension_endings(adj,a1,
                 [_NIL,um,ius,i,o,
                  a,am,ius,i,a,
                  um,um,ius,i,o,
                  i,os,orum,is,is,
                 ae,as,arum,is,is,
                 a,a,orum,is,is]).
*/

adj(alius,ali,a1,tool,n:no,[xxx,xxx]).
adj(alter,alter,a1,tool,n:yes,[xxx,xxx]).
adj(neuter,neutr,a1,tool,n:yes,[xxx,xxx]).
adj(nullus,null,a1,tool,n:no,[xxx,xxx]).
adj(nonnullus,nonnull,a1,tool,n:no,[xxx,xxx]).
adj(solus,sol,a1,std,n:no,[xxx,xxx]).   
adj(totus,tot,a1,std,n:no,[xxx,xxx]).
adj(ullus,ull,a1,tool,n:no,[xxx,xxx]).
adj(unus,un,a1,tool,n:yes,[xxx,xxx]).
adj(uter,utr,a1,tool,n:yes,[xxx,xxx]).





adj(quantus,quant,1,int,[xxx,xxx]).

adj(acer,acer,1,std,n:no,[acri,acerrim]).
adj(aequus,aequ,1,std,n:no,[aequ,aequissim]).
adj(aestiuus,aestiu,1,std,n:no,[xxx,xxx]).
adj(aeternus,aetern,1,std,n:no,[xxx,xxx]).
adj(alienus,alien,1,std,n:no,[alien,alienissim]).
adj(altus,alt,1,std,n:no,[alt,altissim]).
adj(amplus,ampl,1,std,n:no,[ampl,amplissim]).
adj(antiquus,antiqu,1,std,n:no,[antiqu,antiquissim]).
adj(arduus,ardu,1,std,n:no,[ardu,arduissim]).
adj(asper,asper,1,std,n:no,[asper,asperim]).
adj(aureus,aure,1,std,n:no,[xxx,xxx]).
adj(beatus,beat,1,std,n:hpl,[beat,beatissim]).
adj(bellus,bell,1,std,n:no,[bell,bellissim]).
adj(bonus,bon,1,std,n:hpl,[mel,optim]).
adj(britannicus,britannic,1,std,n:no,[xxx,xxx]).
adj(caecus,caec,1,std,n:hpl,[caec,caecissim]).
adj(certus,cert,1,std,n:no,[cert,certissim]).
adj(incertus,incert,1,std,n:no,[incert,incertissim]).
adj(ceterus,ceter,1,std,n:hpl,[xxx,xxx]).
adj(citus,cit,1,std,n:no,[cit,xxx]).
adj(clarus,clar,1,std,n:yes,[clar,clarissim]).
adj(commodus,commod,1,std,n:n,[commod,commodissim]).
adj(cunctus,cunct,1,std,n:yes,[xxx,xxx]).
adj(cupidus,cupid,1,std,n:hpl,[cupid,cupidissim]).
adj(designatus,designat,1,std,n:hpl,[xxx,xxx]).
adj(dexter,dextr,1,std,n:no,[xxx,xxx]).
adj(dignus,dign,1,std,n:hpl,[dign,dignissim]).
adj(diuersus,diuers,1,std,n:yes,[xxx,xxx]).
adj(diuinus,diuin,1,std,n:no,[xxx,xxx]).
adj(doctus,doct,1,std,n:yes,[doct,doctissim]).
adj(dubius,dubi,1,std,n:no,[xxx,xxx]).
adj(durus,dur,1,std,n:no,[dur,durissim]).
adj(externus,extern,1,std,n:yes,[xxx,xxx]).
adj(extremus,extrem,1,std,n:yes,[xxx,xxx]).
adj(falsus,fals,1,std,n:no,[xxx,xxx]).
adj(ferus,fer,1,std,n:no,[feroc,ferocissim]).
adj(fessus,fess,1,std,n:hpl,[xxx,xxx]).
adj(formosus,formos,1,std,n:no,[formos,formosissim]).
adj(fortunatus,fortunat,1,std,n:yes,[fortunat,fortunatissim]).
adj(germanicus,germanic,1,std,n:no,[xxx,xxx]).
adj(graecus,graec,1,std,n:no,[xxx,xxx]).
adj(gratus,grat,1,std,n:hpl,[grat,gratissim]).
adj(ingratus,ingrat,1,std,n:hpl,[ingrat,ingratissim]).
adj(pergratus,pergrat,1,std,n:no,[pergrat,pergratissim]).
adj(humanus,human,1,std,n:no,[human,humanissim]).
adj(ignarus,ignar,1,std,n:hpl,[xxx,xxx]).
adj(immotus,immot,1,std,n:no,[xxx,xxx]).
adj(impudicus,impudic,1,std,n:no,[impudic,impudicissim]).
adj(incommodus,incommod,1,std,n:no,[incommod,incommodissim]).
adj(insanus,insan,1,std,n:no,[insan,insanissim]).             % normally n:hpl here set to no : insani feriant sine litora fluctus Verg.
adj(inuidus,inuid,1,std,n:hpl,[inuid,inuidissim]).
adj(iucundus,iucund,1,std,n:hpl,[iucund,iucundissim]).
adj(laetus,laet,1,std,n:hpl,[laet,laetissim]).
adj(liber,liber,1,std,n:no,[liber,liberrim]).
adj(longus,long,1,std,n:no,[long,longissim]).
adj(maestus,maest,1,std,n:hpl,[maest,maestissim]).
adj(magnus,magn,1,std,n:n,[ma,maxim]).
adj(malus,mal,1,std,n:n,[pe,pessim]).
adj(medius,medi,1,std,n:yes,[xxx,xxx]).
adj(mirus,mir,1,std,n:no,[mir,mirissim]).
adj(miser,miser,1,std,n:hpl,[miser,miserrim]).
adj(mortuus,mortu,1,std,n:hpl,[xxx,xxx]).
adj(multus,mult,1,std,n:yes,[xxx,xxx]).
adj(niger,nigr,1,std,n:hpl,[nigr,nigrissim]).
adj(nouus,nou,1,std,n:hpl,[recent,recentissim]).
adj(nudus,nud,1,std,n:hpl,[xxx,nudissim]).
adj(obscurus,obscur,1,std,n:no,[obscur,obscurissim]).
adj(paruus,paru,1,std,n:hpl,[min,minim]).
adj(patulus,patul,1,std,n:no,[patul,patulissim]).
adj(peritus,perit,1,std,n:hpl,[perit,peritissim]).
adj(plenus,plen,1,std,n:no,[plen,plenissim]).
adj(praecipuus,praecipu,1,std,n:no,[xxx,xxx]).
adj(primus,prim,1,std,n:yes,[xxx,xxx]).
adj(propinquus,propinqu,1,std,n:no,[prop,proxim]).
adj(prosper,prosper,1,std,n:hpl,[prosper,prosperrim]).
adj(publicus,public,1,std,n:no,[public,publicissim]).
adj(pulcher,pulchr,1,std,n:hpl,[pulchr,pulcherrim]).
adj(romanus,roman,1,std,n:no,[xxx,xxx]).
adj(rusticus,rustic,1,std,n:no,[rustic,rusticissim]).
adj(sacer,sacr,1,std,n:n,[sanct,sanctissim]).
adj(saeuus,saeu,1,std,n:hpl,[saeu,saeuissim]).
adj(saluus,salu,1,std,n:hpl,[xxx,xxx]).
adj(summus,summ,1,std,n:n,[xxx,xxx]).
adj(superuacuus,superuacu,1,std,n:no,[xxx,xxx]).
adj(superus,super,1,std,n:yes,[xxx,xxx]).
adj(supremus,suprem,1,std,n:hpl,[xxx,xxx]).
adj(tener,tener,1,std,n:no,[tener,tenerrim]).
adj(tutus,tut,1,std,n:no,[tut,tutissim]).
adj(uacuus,uacu,1,std,n:no,[xxx,uacuissim]).
adj(uerus,uer,1,std,n:n,[uer,uerissim]).
adj(uitellianus,uitellian,1,std,n:hpl,[xxx,xxx]).
adj(uiuus,uiu,1,std,n:hpl,[xxx,xxx]).
adj(ultimus,ultim,1,std,n:yes,[xxx,xxx]).
adj(unicus,unic,1,std,n:no,[xxx,xxx]).
adj(uotiuus,uotiu,1,std,n:no,[xxx,xxx]).
adj(urbanus,urban,1,std,n:no,[urban,urbanissim]).
adj(uuidus,uuid,1,std,n:no,[uuid,uuidissim]).


adj(meus,me,1,poss,n:no,[xxx,xxx]).
adj(tuus,tu,1,poss,n:no,[xxx,xxx]).
adj(suus,su,1,poss,n:no,[xxx,xxx]).
adj(noster,nostr,1,poss,n:no,[xxx,xxx]).
adj(uester,uestr,1,poss,n:no,[xxx,xxx]).




% 2

/* % fortis, fortis, forte
declension_endings(adj,2,
                  [_NIL,em,is,i,i,
                   e,e,is,i,i,
                  es,es,is,ium,ibus,ibus,
                  ia,ia,ium,ibus,ibus]).

*/

adj(qualis,qual,2,int,n:no,[xxx,xxx]).

adj(admirabilis,admirabil,2,std,n:no,[admirabil,xxx]).
adj(amabilis,amabil,2,std,n:no,[amabil,amabilissim]).
adj(breuis,breu,2,std,n:no,[breu,breuissim]).
adj(ciuilis,ciuil,2,std,n:no,[xxx,xxx]).
adj(communis,commun,2,std,n:no,[xxx,xxx]).
adj(difficilis,difficil,2,std,n:no,[difficil,difficilim]).
adj(diues,diuit,b2,std,n:hpl,[diuit,diuitissim]).
adj(dulcis,dulc,2,std,n:no,[dulc,dulcissim]).
adj(facilis,facil,2,std,n:no,[facil,facillim]).
adj(fortis,fort,2,std,n:no,[fort,fortissim]).
adj(grandis,grand,2,std,n:no,[grand,grandissim]).
adj(grauis,grau,2,std,n:no,[grau,grauissim]).
adj(immortalis,immortal,2,std,n:hpl,[xxx,xxx]).
adj(irritabilis,irritabil,2,std,n:no,[irritabil,irritabilissim]).
adj(leuis,leu,2,std,n:no,[leu,leuissim]).
adj(mortalis,mortal,2,std,n:hpl,[xxx,xxx]).
adj(mollis,moll,2,std,n:no,[moll,mollissim]).
adj(pauper,pauper,b2,std,n:hpl,[pauper,pauperrim]).
adj(singularis,singular,2,std,n:no,[singular,singularissim]).
adj(tristis,trist,2,std,n:no,[trist,tristissim]).
adj(turpis,turp,2,std,n:hpl,[turp,turpissim]).
adj(uenalis,uenal,2,std,n:no,[uenal,uenalissim]).
adj(uetus,ueter,b2,std,n:no,[uetust,ueterrim]).



% ingens, ingens, ingens
% declension_endings(adj,a2,
  %                [_NIL,em,is,i,i,
  %                 _NIL,_NIL,is,i,i,
  %                es,es,ium,ibus,ibus,
  %                ia,ia,ium,ibus,ibus]).



adj(felix,felic,a2,std,n:yes,[felic,felicissim]).
adj(ingens,ingent,a2,std,n:no,[ingent,ingentissim]).
adj(par,par,a2,std,n:no,[xxx,xxx]).
adj(potens,potent,a2,std,n:yes,[potent,potentissim]).
adj(praestans,praestant,a2,std,n:no,[praestant,praestantissim]).
adj(sapiens,sapient,a2,std,n:yes,[sapient,sapientissim]).
adj(sospes,sospit,a2,std,n:no,[xxx,xxx]).
adj(uictrix,uictric,a2,std,n:yes,[xxx,xxx]).






%%%%%%%
% NOUNS
%%%%%%%

% the ab:Value feature registers whether the noun in the ablative can function as head of an adjunct
% ab:no - cannot have such function
% ab:mm - potential head of a manner-means adjunct
% ab:loc and ab:time - idem with appropriate semantic values


noun(1,fem,amica,amic,class:common, ab:no, sem:[hum],[]).
noun(1,fem,amicitia,amiciti,class:common, ab:mm, sem:[abstract],[]).
noun(1,fem,ancilla,ancill,class:common, ab:no, sem:[hum],[]).
noun(1,fem,anima,anim,class:common, ab:no, sem:[hum,thing,abstract],[]).
noun(1,fem,aqua,aqu,class:common, ab:no, sem:[thing],[]).
noun(1,fem,asia,asi,class:common, ab:no, sem:[loc,thing],[]).
noun(1,fem,auaritia,auariti,class:common, ab:mm, sem:[abstract],[]).
noun(1,fem,aula,aul,class:common, ab:no, sem:[thing,loc],[]).
noun(1,fem,aura,aur,class:common, ab:no, sem:[thing,loc],[]).
noun(1,fem,cannae,cann,class:common, ab:no, sem:[city, thing],[nb:pl]). % pluralia tantum
noun(1,fem,causa,caus,class:common, ab:no, sem:[thing, abstract],[]).
noun(1,fem,coma,com,class:common, ab:no, sem:[thing],[]).
noun(1,fem,copiae,copi,class:common, ab:no, sem:[hum],[nb:pl]). % pluralia tantum
noun(1,fem,corona,coron,class:common, ab:no, sem:[thing],[]).
noun(1,fem,cura,cur,class:common, ab:mm, sem:[abstract],[]).
noun(1,fem,curia,curi,class:common, ab:no, sem:[thing,loc],[]).
noun(1,fem,dea,de,class:common, ab:no, sem:[hum],[]).
noun(1,fem,discordia,discordi,class:common, ab:mm, sem:[abstract],[]).
noun(1,fem,diuitiae,diuiti,class:common, ab:no, sem:[thing],[nb:pl]).  % pluralia tantum
noun(1,fem,domina,domin,class:common, ab:no, sem:[hum],[]).
noun(1,fem,ecclesia,ecclesi,class:common, ab:no, sem:[hum, thing,loc],[]).
noun(1,fem,epistula,epistul,class:common, ab:mm, sem:[thing, abstract],[]).
noun(1,fem,fama,fam,class:common, ab:mm, sem:[abstract,hum],[]).
noun(1,fem,femina,femin,class:common, ab:no, sem:[hum],[]).
noun(1,fem,filia,fili,class:common, ab:no, sem:[hum],[]).
noun(1,fem,flamma,flamm,class:common, ab:no, sem:[thing],[]).
noun(1,fem,forma,form,class:common, ab:mm, sem:[thing, abstract],[]).
noun(1,fem,fortuna,fortun,class:common, ab:mm, sem:[abstract,hum],[]).
noun(1,fem,fuga,fug,class:common, ab:mm, sem:[abstract],[]).
noun(1,fem,gallia,galli,class:common, ab:no, sem:[loc,thing],[]).
noun(1,fem,gloria,glori,class:common, ab:mm, sem:[abstract],[]).
noun(1,fem,grammatica,grammatic,class:common, ab:no, sem:[abstract],[]).
noun(1,fem,gratia,grati,class:common, ab:mm, sem:[abstract],[]).
noun(1,fem,hedera,heder,class:common, ab:no, sem:[thing],[]).
noun(1,fem,hispania,hispani,class:common, ab:no, sem:[loc,thing],[]).
noun(1,fem,historia,histori,class:common, ab:no, sem:[abstract],[]).
noun(1,fem,hora,hor,class:common, ab:no, sem:[time],[]).
noun(1,fem,ignauia,ignaui,class:common, ab:mm, sem:[abstract],[]).
noun(1,fem,iniuria,iniuri,class:common, ab:mm, sem:[abstract],[]).
noun(1,fem,inuidia,inuidi,class:common, ab:mm, sem:[abstract],[]).
noun(1,fem,ira,ir,class:common, ab:mm, sem:[abstract],[]).
noun(1,fem,italia,itali,class:common, ab:no, sem:[loc,thing],[nb:sg]).
noun(1,fem,ithaca,ithac,class:common, ab:no, sem:[loc,thing],[nb:sg]).
noun(1,fem,iustitia,iustiti,class:common, ab:mm, sem:[abstract],[]).
noun(1,fem,iuuenca,iuuenc,class:common, ab:no, sem:[thing,hum],[]).
noun(1,fem,lacrima,lacrim,class:common, ab:mm, sem:[thing],[]).
noun(1,fem,laetitia,laetiti,class:common, ab:mm, sem:[abstract],[]).
noun(1,fem,litterae,litter,class:common, ab:no, sem:[thing, abstract],[nb:pl]). % pluralia tantum
noun(1,fem,lutetia,luteti,class:common, ab:no, sem:[city,thing],[nb:sg]).
noun(1,fem,luxuria,luxuri,class:common, ab:mm, sem:[abstract],[]).
noun(1,fem,memoria,memori,class:common, ab:mm, sem:[abstract],[]).
noun(1,fem,misericordia,misericordi,class:common, ab:mm, sem:[abstract],[]).
noun(1,fem,mora,mor,class:common, ab:mm, sem:[abstract],[]).
noun(1,fem,natura,natur,class:common, ab:no, sem:[abstract, thing],[]).
noun(1,fem,nuptiae,nupti,class:common, ab:no, sem:[thing, abstract],[nb:pl]). % pluralia tantum
noun(1,fem,parsimonia,parsimoni,class:common, ab:mm, sem:[abstract],[]).
noun(1,fem,patientia,patienti,class:common, ab:mm, sem:[abstract],[]).
noun(1,fem,patria,patri,class:common, ab:no, sem:[thing, abstract],[]).
noun(1,fem,pecunia,pecuni,class:common, ab:mm, sem:[thing],[]).
noun(1,fem,poena,poen,class:common, ab:mm, sem:[abstract],[]).
noun(1,fem,potentia,potenti,class:common, ab:mm, sem:[abstract],[]).
noun(1,fem,praeda,praed,class:common, ab:no, sem:[thing],[]).
noun(1,fem,praetura,praetur,class:common, ab:no, sem:[abstract],[]).
noun(1,fem,prouincia,prouinci,class:common, ab:no, sem:[loc, abstract],[]).
noun(1,fem,puella,puell,class:common, ab:no, sem:[hum],[]).
noun(1,fem,pugna,pugn,class:common, ab:no, sem:[abstract],[]).
noun(1,fem,quaestura,quaestur,class:common, ab:no, sem:[thing, abstract],[]).
noun(1,fem,regina,regin,class:common, ab:no, sem:[hum],[]).
noun(1,fem,ripa,rip,class:common, ab:no, sem:[thing,loc],[]).
noun(1,fem,roma,rom,class:common, ab:no, sem:[city,thing,hum],[nb:sg]).
noun(1,fem,sapientia,sapienti,class:common, ab:mm, sem:[abstract],[]).
noun(1,fem,sardinia,sardini,class:common, ab:no, sem:[loc,thing],[nb:sg]).
noun(1,fem,scientia,scienti,class:common, ab:mm, sem:[abstract],[]).
noun(1,fem,sententia,sententi,class:common, ab:no, sem:[abstract],[]).
noun(1,fem,serua,seru,class:common, ab:no, sem:[hum, thing],[]).
noun(1,fem,sicilia,sicili,class:common, ab:no, sem:[loc,thing],[nb:sg]).
noun(1,fem,silua,silu,class:common, ab:no, sem:[thing,loc],[]).
noun(1,fem,superbia,superbi,class:common, ab:mm, sem:[abstract],[]).
noun(1,fem,tabula,tabul,class:common, ab:no, sem:[thing],[]).
noun(1,fem,terra,terr,class:common, ab:no, sem:[thing,loc],[]).
noun(1,fem,thessalia,thessali,class:common, ab:no, sem:[loc,thing],[nb:sg]).
noun(1,fem,troia,troi,class:common, ab:no, sem:[city,thing],[nb:sg]).
noun(1,fem,turba,turb,class:common, ab:no, sem:[hum],[]).
noun(1,fem,uia,ui,class:common, ab:mm, sem:[abstract,thing,loc],[]).
noun(1,fem,uictoria,uictori,class:common, ab:mm, sem:[abstract],[]).
noun(1,fem,uita,uit,class:common, ab:no, sem:[thing, abstract,time],[]).
noun(1,fem,umbra,umbr,class:common, ab:no, sem:[thing,loc],[]).
noun(1,fem,umbria,umbri,class:common, ab:no, sem:[thing,loc],[nb:sg]).
noun(1,fem,unda,und,class:common, ab:no, sem:[thing],[]).



noun(1,masc,agricola,agricol,class:common, ab:no, sem:[hum],[]).
noun(1,masc,belga,belg,class:common, ab:no, sem:[hum],[]).
noun(1,masc,nauta,naut,class:common, ab:no, sem:[hum],[]).






noun(2, masc, alumnus, alumn,class:common, ab:no,  sem:[hum],[]).
noun(2, masc, amicus, amic,class:common, ab:no,  sem:[hum],[]).
noun(2, masc, animus, anim, class:common, ab:mm, sem:[abstract, thing],[]).
noun(2, masc, annus, ann,class:common, ab:time,  sem:[time, abstract],[]).
% noun(2, masc, bonus, bon,class:common, ab:no,  sem:[hum],[nb:pl]).
noun(2, masc, campus, camp,class:common, ab:no,  sem:[thing,loc],[]).
noun(2, masc, cauus, cau,class:common, ab:loc,  sem:[thing,loc],[]).
noun(2, masc, coniuratus, coniurat,class:common, ab:no,  sem:[hum],[nb:pl]).
noun(2, masc, danaus, dana,class:common, ab:no,  sem:[hum],[nb:pl]).
noun(2, masc, deus, de,class:common, ab:no,  sem:[hum],[]).
noun(2, masc, discipulus, discipul,class:common, ab:no,  sem:[hum],[]).
noun(2, masc, dominus, domin,class:common, ab:no,  sem:[hum],[]).
noun(2, masc, equus, equ,class:common, ab:mm,  sem:[thing, hum],[]).
noun(2, masc, filius, fili,class:common, ab:no,  sem:[hum],[]).
noun(2, masc, gallus, gall,class:common, ab:no,  sem:[hum],[nb:pl]).
noun(2, masc, germanus, german,class:common, ab:no,  sem:[hum],[nb:pl]).
noun(2, masc, graecus, graec,class:common, ab:no,  sem:[hum],[nb:pl]).
noun(2, masc, hortus, hort, class:common, ab:no, sem:[loc,thing],[]).
noun(2, masc, inimicus, inimic,class:common, ab:no,  sem:[hum],[]).
% noun(2, masc, inuidus, inuid,class:common, ab:no,  sem:[hum],[]).
noun(2, masc, legatus, legat,class:common, ab:no,  sem:[hum],[]).
noun(2, masc, libellus, libell, class:common, ab:no, sem:[abstract, thing],[]).
noun(2, masc, locus, loc, class:common, ab:loc, sem:[loc,abstract, thing],[]).
noun(2, masc, maritus, marit,class:common, ab:no,  sem:[hum],[]).
noun(2, masc, modus, mod, class:common, ab:mm, sem:[abstract, thing],[]).
noun(2, masc, morbus, morb, class:common, ab:mm, sem:[abstract, thing],[]).
noun(2, masc, mundus, mund, class:common, ab:no, sem:[thing,loc],[]).
noun(2, masc, murus, mur, class:common, ab:no, sem:[thing,loc],[]).
noun(2, masc, nasus, nas,class:common, ab:no,  sem:[thing],[]).
noun(2, masc, natus, nat,class:common, ab:no,  sem:[hum],[]).
noun(2, masc, numerus, numer,class:common, ab:no,  sem:[abstract],[]).
noun(2, masc, oculus, ocul, class:common, ab:no, sem:[abstract, thing],[]).
noun(2, masc, patronus, patron,class:common, ab:no,  sem:[hum],[]).
noun(2, masc, pessimus, pessim,class:common, ab:no,  sem:[hum],[nb:pl]).
noun(2, masc, populus, popul,class:common, ab:no,  sem:[hum],[]).
noun(2, masc, praefectus, praefect,class:common, ab:no,  sem:[hum],[]).
noun(2, masc, proximus, proxim,class:common, ab:no,  sem:[hum],[]).
noun(2, masc, romanus, roman,class:common, ab:no,  sem:[hum],[nb:pl]).
noun(2, masc, seruus, seru,class:common, ab:no,  sem:[hum, thing],[]).
noun(2, masc, socius, soci,class:common, ab:no,  sem:[hum],[]).
noun(2, masc, somnus, somn,class:common, ab:no,  sem:[thing, abstract],[]).
noun(2, masc, troianus, troian,class:common, ab:no,  sem:[hum],[nb:pl]).
noun(2, masc, uentus, uent,class:common, ab:mm,  sem:[thing],[]).





noun(2, masc, ager, agr,class:common, ab:no,  sem:[thing,loc],[]).
noun(2, masc, arbiter, arbitr,class:common, ab:no,  sem:[hum],[]).
noun(2, masc, faber, fabr,class:common, ab:no,  sem:[hum],[]).
noun(2, masc, gener, gener,class:common, ab:no,  sem:[hum],[]).
noun(2, masc, liber, libr,class:common, ab:no,  sem:[thing,abstract],[]).
noun(2, masc, liberi,liber,class:common, ab:no, sem:[hum],[nb:pl]). % pluralia tantum
noun(2, masc, magister, magistr,class:common, ab:no,  sem:[hum],[]).
noun(2, masc, puer, puer,class:common, ab:no,  sem:[hum],[]).
noun(2, masc, socer, socer,class:common, ab:no,  sem:[hum],[]).
noun(2, masc, uesper, uesper,class:common, ab:no,  sem:[time],[]).
noun(2, masc, uir, uir,class:common, ab:no,  sem:[hum],[]).




noun(2, fem, fagus, fag,class:common, ab:no,  sem:[thing],[]).
noun(2, fem, laurus, laur, class:common, ab:no, sem:[thing],[]).
noun(2, fem, populus, popul, class:common, ab:no, sem:[thing],[]).



noun(2, neuter, arbitrium, arbitri,class:common, ab:mm,  sem:[abstract, thing],[]).
noun(2, neuter, artificium, artifici,class:common, ab:mm,  sem:[abstract, thing],[]).
noun(2, neuter, aruum, aru,class:common, ab:no,  sem:[thing,loc],[]).
noun(2, neuter, astrum, astr,class:common, ab:no,  sem:[thing,loc,abstract],[]).
noun(2, neuter, aurum, aur,class:common, ab:mm,  sem:[thing],[]).
noun(2, neuter, auxilium, auxili,class:common, ab:mm,  sem:[thing, abstract],[]).
noun(2, neuter, bellum, bell,class:common, ab:time,  sem:[thing],[]).
noun(2, neuter, beneficium, benefici,class:common, ab:mm,  sem:[abstract, thing],[]).
noun(2, neuter, bonum, bon,class:common, ab:no,  sem:[thing, abstract],[]).
noun(2, neuter, caelum, cael,class:common, ab:no,  sem:[thing],[]).
noun(2, neuter, consilium, consili,class:common, ab:mm,  sem:[abstract,hum],[]).
noun(2, neuter, documentum, document,class:common, ab:no,  sem:[thing,abstract],[]).
noun(2, neuter, donum, don,class:common, ab:mm,  sem:[thing],[]).
noun(2, neuter, edictum, edict,class:common, ab:mm,  sem:[thing,abstract],[]).
noun(2, neuter, exemplum, exempl,class:common, ab:mm,  sem:[thing,abstract],[]).
noun(2, neuter, exilium, exili,class:common, ab:no,  sem:[hum],[nb:pl]).
noun(2, neuter, factum, fact,class:common, ab:no,  sem:[abstract],[]).
noun(2, neuter, fatum, fat,class:common, ab:no,  sem:[abstract],[]).
noun(2, neuter, ferrum, ferr,class:common, ab:mm,  sem:[thing,abstract],[]).
noun(2, neuter, forum, for,class:common, ab:loc,  sem:[thing,loc],[]).
noun(2, neuter, imperium, imperi,class:common, ab:no,  sem:[thing, abstract],[]).
noun(2, neuter, incitamentum, incitament,class:common, ab:mm,  sem:[abstract],[]).
noun(2, neuter, incommodum, incommod,class:common, ab:mm,  sem:[abstract],[]).
noun(2, neuter, ingenium, ingeni,class:common, ab:mm,  sem:[abstract],[]).
noun(2, neuter, lugdunum, lugdun,class:common, ab:no,  sem:[city,thing, abstract],[nb:sg]). % only sing
noun(2, neuter, membrum, membr,class:common, ab:no,  sem:[thing],[]).
noun(2, neuter, negotium, negoti,class:common, ab:mm,  sem:[abstract],[]).
noun(2, neuter, nubilum, nubil,class:common, ab:no,  sem:[thing,abstract],[]).
noun(2, neuter, odium, odi,class:common, ab:mm,  sem:[abstract],[]).
noun(2, neuter, officium, offici,class:common, ab:mm,  sem:[abstract],[]).
noun(2, neuter, oppidum, oppid,class:common, ab:no,  sem:[thing,loc],[]).
noun(2, neuter, otium, oti,class:common, ab:mm,  sem:[abstract],[]).
noun(2, neuter, peccatum, peccat,class:common, ab:no,  sem:[abstract],[]).
noun(2, neuter, periculum, pericul,class:common, ab:mm,  sem:[thing,abstract],[]).
noun(2, neuter, praeceptum, praecept,class:common, ab:mm,  sem:[abstract],[]).
noun(2, neuter, praemium, praemi,class:common, ab:mm,  sem:[thing, abstract],[]).
noun(2, neuter, praetorium, praetori,class:common, ab:no,  sem:[hum,thing, abstract,loc],[]).
noun(2, neuter, speculum, specul,class:common, ab:mm,  sem:[hum, thing, loc],[]).
noun(2, neuter, pretium, preti,class:common, ab:mm,  sem:[thing, abstract],[]).
noun(2, neuter, probrum, probr,class:common, ab:mm,  sem:[abstract],[]).
noun(2, neuter, proelium, proeli,class:common, ab:mm,  sem:[thing, abstract],[]).
noun(2, neuter, regnum, regn,class:common, ab:no,  sem:[thing, abstract],[]).
noun(2, neuter, saxum, sax,class:common, ab:no,  sem:[thing,loc],[]).
noun(2, neuter, scutum, scut,class:common, ab:mm,  sem:[thing],[]).
noun(2, neuter, signum, sign,class:common, ab:mm,  sem:[thing, abstract],[]).
noun(2, neuter, silentium, silenti,class:common, ab:mm,  sem:[abstract],[]).
noun(2, neuter, solacium, solaci,class:common, ab:mm,  sem:[abstract],[]).
noun(2, neuter, spatium, spati,class:common, ab:no,  sem:[thing, loc],[]).
noun(2, neuter, spectaculum, spectacul,class:common, ab:mm,  sem:[thing],[]).
noun(2, neuter, speculum, specul,class:common, ab:mm,  sem:[hum, thing, loc],[]). % hum metaphorically cf. Ovid
noun(2, neuter, studium, studi,class:common, ab:mm,  sem:[abstract],[]).
noun(2, neuter, subsidium, subsidi,class:common, ab:no,  sem:[abstract],[]).
noun(2, neuter, tectum, tect,class:common, ab:no,  sem:[thing, loc],[]).
noun(2, neuter, telum,tel,class:common, ab:mm,  sem:[thing],[]).
noun(2, neuter, templum, templ,class:common, ab:no,  sem:[thing, loc],[]).
noun(2, neuter, tergum, terg,class:common, ab:no,  sem:[thing, loc],[]).
noun(2, neuter, telum,tel,class:common, ab:mm,  sem:[thing],[]).
noun(2, neuter, uenenum,uenen,class:common, ab:mm,  sem:[thing],[]).
noun(2, neuter, uerbum, uerb,class:common, ab:mm,  sem:[abstract],[]).
noun(2, neuter, uestigium,uestigi,class:common, ab:no,  sem:[thing],[]).
noun(2, neuter, uestimentum,uestiment,class:common, ab:no,  sem:[thing],[]).
noun(2, neuter, uinculum, uincul,class:common, ab:mm,  sem:[thing,abstract],[]).
noun(2, neuter, uinum, uin,class:common, ab:no,  sem:[thing],[]).
noun(2, neuter, uitium, uiti,class:common, ab:mm,  sem:[abstract],[]).

noun(2, neuter, arma, arm,class:common, ab:mm,  sem:[thing],[nb:pl]).  % pluralia tantum
noun(2, neuter, castra, castr,class:common, ab:loc,  sem:[thing, loc],[nb:pl]).  % pluralia tantum
noun(2, neuter, milia, mili,class:common, ab:no,  sem:_,[nb:pl]).  % pluralia tantum
noun(2, neuter, rostra, rostr,class:common, ab:no,  sem:[thing, loc],[nb:pl]).  % pluralia tantum






noun(3,masc,amnis,amn,ium,class:common, ab:no, sem:[thing,loc],[]).
noun(3,masc,amor,amor,um,class:common, ab:mm, sem:[abstract,hum],[]).
noun(3,masc,ciuis,ciu,ium,class:common, ab:no, sem:[hum],[]).
noun(3,masc,clamor,clamor,um,class:common, ab:mm, sem:[abstract],[]).
noun(3,masc,consul,consul,um,class:common, ab:no, sem:[hum],[]).
noun(3,masc,custos,custod,um,class:common, ab:no, sem:[hum],[]).
noun(3,masc,defensor,defensor,um,class:common, ab:no, sem:[hum],[]).
noun(3,masc,dolor,dolor,um,class:common, ab:mm, sem:[abstract],[]).
noun(3,masc,dux,duc,um,class:common, ab:no, sem:[hum],[]).
noun(3,masc,eques,equit,um,class:common, ab:mm, sem:[hum],[]).
noun(3,masc,error,error,um,class:common, ab:no, sem:[abstract],[]).
noun(3,masc,finis,fin,ium,class:common, ab:no, sem:[thing,loc],[]).
noun(3,masc,frater,fratr,um,class:common, ab:no, sem:[hum],[]).
noun(3,masc,furor,furor,um,class:common, ab:mm, sem:[abstract],[]).
noun(3,masc,gladiator,gladiator,um,class:common, ab:no, sem:[hum],[]).
noun(3,masc,homo,homin,um,class:common, ab:no, sem:[hum],[]).
noun(3,masc,honor,honor,um,class:common, ab:no, sem:[abstract],[]).
noun(3,masc,hostis,host,ium,class:common, ab:no, sem:[hum],[]).
noun(3,masc,ignis,ign,ium,class:common, ab:mm, sem:[thing],[]).
noun(3,masc,imperator,imperator,um,class:common, ab:no, sem:[hum],[]).
noun(3,masc,iudex,iudic,um,class:common, ab:no, sem:[hum],[]).
noun(3,masc,iuuenis,iuuen,um,class:common, ab:no, sem:[hum],[]).
noun(3,masc,labor,labor,um,class:common, ab:mm, sem:[abstract],[]).
noun(3,masc,leo,leon,um,class:common, ab:no, sem:[thing, hum],[]).
noun(3,masc,maiores,maior,um,class:common, ab:no, sem:[hum],[nb:pl]).    % pluralia tantum
noun(3,masc,miles,milit,um,class:common, ab:no, sem:[hum],[]).
noun(3,masc,mons,mont,ium,class:common, ab:no, sem:[loc,thing],[]).
noun(3,masc,mos,mor,um,class:common, ab:mm, sem:[abstract],[]).
noun(3,masc,mus,mur,ium,class:common, ab:no, sem:[thing,hum],[]).
noun(3,masc,orator,orator,um,class:common, ab:no, sem:[hum],[]).
noun(3,masc,orbis,orb,ium,class:common, ab:no, sem:[loc,thing],[]).
noun(3,masc,oriens,orient,um,class:common, ab:no, sem:[thing, loc],[nb:sg]).
noun(3,masc,paries,pariet,um,class:common, ab:no, sem:[thing],[]).
noun(3,masc,pater,patr,um,class:common, ab:no, sem:[hum],[]).
noun(3,masc,pes,ped,um,class:common, ab:no, sem:[thing, abstract],[]).
noun(3,masc,piscis,pisc,ium,class:common, ab:no, sem:[thing],[]).     
noun(3,masc,praetor,praetor,um,class:common, ab:no, sem:[hum],[]).
noun(3,masc,pudor,pudor,um,class:common, ab:mm, sem:[abstract],[]).
noun(3,masc,rex,reg,um,class:common, ab:no, sem:[hum],[]).
noun(3,masc,rumor,rumor,um,class:common, ab:mm, sem:[abstract,thing],[]).
noun(3,masc,saltator,saltator,um,class:common, ab:no, sem:[hum],[]).
noun(3,masc,sanguis,sangu,ium,class:common, ab:no, sem:[thing],[]).                                 
noun(3,masc,senator,senator,um,class:common, ab:no, sem:[hum],[]).
noun(3,masc,senex,sen,um,class:common, ab:no, sem:[hum],[]).
noun(3,masc,senior,senior,um,class:common, ab:no, sem:[hum],[]).
noun(3,masc,sermo,sermon,um,class:common, ab:mm, sem:[thing, abstract],[]).
noun(3,masc,sol,sol,um,class:common, ab:no, sem:[thing, abstract],[nb:sg]).
noun(3,masc,timor,timor,um,class:common, ab:mm, sem:[abstract],[]).
noun(3,masc,uates,uat,um,class:common, ab:no, sem:[hum],[]).
noun(3,masc,uictor,uictor,um,class:common, ab:no, sem:[hum],[]).


noun(3,fem,actio,action,um,class:common, ab:mm, sem:[abstract],[]).
noun(3,fem,adoptio,adoption,um,class:common, ab:mm, sem:[abstract],[]).
noun(3,fem,aetas,aetat,um,class:common, ab:no, sem:[abstract],[]).
noun(3,fem,arbor,arbor,um,class:common, ab:no, sem:[thing,loc],[]).
noun(3,fem,ars,art,ium,class:common, ab:mm, sem:[abstract],[]).
noun(3,fem,auris,aur,ium,class:common, ab:no, sem:[thing],[]).
noun(3,fem,caedes,caed,ium,class:common, ab:mm, sem:[abstract],[]).
noun(3,fem,carthago,carthagin,um,class:common, ab:loc,  sem:[city,thing, abstract],[nb:sg]).
noun(3,fem,celebritas,celebritat,um,class:common, ab:no, sem:[abstract],[]).
noun(3,fem,ciuitas,ciuitat,um,class:common, ab:no, sem:[hum,loc],[]).
noun(3,fem,cohors,cohort,ium,class:common, ab:no, sem:[hum],[]).
noun(3,fem,clades,clad,um,class:common, ab:mm, sem:[abstract],[]).
noun(3,fem,classis,class,ium,class:common, ab:no, sem:[thing,abstract],[]).
noun(3,fem,coniuratio,coniuration,um,class:common, ab:mm, sem:[abstract],[]).
noun(3,fem,consuetudo,consuetudin,um,class:common, ab:mm, sem:[abstract],[]).
noun(3,fem,cupiditas,cupiditat,um,class:common, ab:mm, sem:[abstract],[]).
noun(3,fem,defensio,defension,um,class:common, ab:mm, sem:[abstract],[]).
noun(3,fem,dignitas,dignitat,um,class:common, ab:mm, sem:[abstract],[]).
noun(3,fem,diuersitas,diuersitat,um,class:common, ab:mm, sem:[abstract],[]).
noun(3,fem,hereditas,heredit,um,class:common, ab:mm, sem:[abstract,thing],[]).
noun(3,fem,facultas,facultat,um,class:common, ab:mm, sem:[abstract],[]).
noun(3,fem,fax,fac,um,class:common, ab:mm, sem:[thing],[]).
noun(3,fem,fines,fin,ium,class:common, ab:no, sem:[thing, loc],[nb:pl]). % pluralia tantum
noun(3,fem,gens,gent,ium,class:common, ab:no, sem:[hum],[]).
noun(3,fem,hiems,hiem,um,class:common, ab:time, sem:[abstract,time],[]).
noun(3,fem,illuminatio,illumination,um,class:common, ab:mm, sem:[abstract],[]).
noun(3,fem,immortalitas,immortalitat,um,class:common, ab:no, sem:[abstract, thing],[]).
noun(3,fem,karthago,karthagin,um,class:common, ab:no,  sem:[city,thing, abstract],[nb:sg]).
noun(3,fem,laus,laud,um,class:common, ab:mm, sem:[abstract],[]).
noun(3,fem,legio,legion,um,class:common, ab:no, sem:[hum, thing],[]).
noun(3,fem,lex,leg,um,class:common, ab:mm, sem:[abstract],[]).
noun(3,fem,liberalitas,liberalitat,um,class:common, ab:no, sem:[abstract],[]).
noun(3,fem,libertas,libertat,um,class:common, ab:mm, sem:[abstract],[]).
noun(3,fem,lux,luc,um,class:common, ab:mm, sem:[thing],[]).
noun(3,fem,magnitudo,magnitudin,um,class:common, ab:no, sem:[abstract],[]).
noun(3,fem,mater,matr,um,class:common, ab:no, sem:[hum],[]).
noun(3,fem,mens,ment,ium,class:common, ab:mm, sem:[abstract],[]).
noun(3,fem,mors,mort,ium,class:common, ab:mm, sem:[abstract, thing, hum],[]). 
noun(3,fem,nauis,nau,ium,class:common, ab:no, sem:[thing,loc],[]).
noun(3,fem,nobilitas,nobilitat,um,class:common, ab:no, sem:[abstract],[]).
noun(3,fem,nouitas,nouitat,um,class:common, ab:no, sem:[abstract],[]).
noun(3,fem,nox,noct,um,class:common, ab:time, sem:[abstract, time],[]).
noun(3,fem,nux,nuc,um,class:common, ab:no, sem:[thing],[]).
noun(3,fem,opinio,opinion,um,class:common, ab:mm, sem:[abstract],[]).
noun(3,fem,ops,op,um,class:common, ab:mm, sem:[thing],[]).
noun(3,fem,oratio,oration,um,class:common, ab:mm, sem:[abstract],[]).
noun(3,fem,pars,part,ium,class:common, ab:no, sem:[abstract, thing, hum, loc],[]).
noun(3,fem,pax,pac,um,class:common, ab:mm, sem:[abstract],[nb:sg]).
noun(3,fem,petitio,petition,um,class:common, ab:mm, sem:[abstract],[]).
noun(3,fem,pietas,pietat,um,class:common, ab:mm, sem:[abstract],[]).
noun(3,fem,plebs,pleb,um,class:common, ab:no, sem:[human,abstract],[nb:sg]).
noun(3,fem,potestas,potestat,um,class:common, ab:mm, sem:[abstract],[]).
noun(3,fem,proditio,prodition,um,class:common, ab:mm, sem:[abstract],[]).
noun(3,fem,quies,quiet,um,class:common, ab:mm, sem:[abstract],[nb:sg]).
noun(3,fem,ratio,ration,um,class:common, ab:mm, sem:[abstract],[]).
noun(3,fem,regio,region,um,class:common, ab:no, sem:[thing,loc],[]).
noun(3,fem,salus,salut,um,class:common, ab:no, sem:[abstract],[]).
noun(3,fem,sedes,sed,um,class:common, ab:no, sem:[thing,loc,abstract],[]).
noun(3,fem,sollemnitas,sollemnitat,um,class:common, ab:no, sem:[abstract],[]).
noun(3,fem,soror,soror,um,class:common, ab:no, sem:[hum],[]).
noun(3,fem,sors,sort,ium,class:common, ab:no, sem:[abstract],[]).
noun(3,fem,tellus,tellur,um,class:common, ab:no, sem:[thing,loc],[]).
noun(3,fem,uastitas,uastitat,um,class:common, ab:no, sem:[abstract],[]).
noun(3,fem,uestis,uest,ium,class:common, ab:no, sem:[thing],[]).
noun(3,fem,uires,uir,ium,class:common, ab:no, sem:[thing,abstract],[nb:pl]). % pluralia tantum
noun(3,fem,uirgo,uirgin,um,class:common, ab:no, sem:[hum],[]).
noun(3,fem,uirtus,uirtut,um,class:common, ab:mm, sem:[abstract, hum],[]). % hum : metaphorical assignment
noun(3,fem,uoluntas,uoluntat,um,class:common, ab:mm, sem:[abstract],[]).
noun(3,fem,uoluptas,uoluptat,um,class:common, ab:mm, sem:[abstract],[]).
noun(3,fem,uox,uoc,um,class:common, ab:mm, sem:[abstract],[]).
noun(3,fem,urbs,urb,ium,class:common, ab:loc, sem:[hum,thing,loc],[]).
noun(3,fem,utilitas,utilitat,um,class:common, ab:no, sem:[abstract],[]).
noun(3,fem,uxor,uxor,um,class:common, ab:no, sem:[hum],[]).


noun(3,or([masc,fem]),comes,comit,um,class:common, ab:no, sem:[hum],[]).
noun(3,or([masc,fem]),coniux,coniug,um,class:common, ab:no, sem:[hum],[]).
noun(3,or([masc,fem]),heres,hered,um,class:common, ab:no, sem:[hum],[]).
noun(3,or([masc,fem]),parens,parent,um,class:common, ab:no, sem:[hum],[]).
% noun(3,or([masc,fem]),sapiens,sapient,um,class:common, ab:no, sem:[hum],[]).




noun(3,neuter,caput,capit,um,class:common, ab:no, sem:[thing,hum,abstract],[]).
noun(3,neuter,carmen,carmin,um,class:common, ab:mm, sem:[thing,abstract],[]).
noun(3,neuter,cor,cord,um,class:common, ab:no, sem:[thing,abstract],[]).
noun(3,neuter,corpus,corpor,um,class:common, ab:no, sem:[thing],[]).
noun(3,neuter,dedecus,dedecor,um,class:common, ab:mm, sem:[abstract,thing],[]).
noun(3,neuter,facinus,facinor,um,class:common, ab:no, sem:[abstract],[]).
noun(3,neuter,flumen,flumin,um,class:common, ab:loc, sem:[thing, loc],[]).
noun(3,neuter,funus,funer,um,class:common, ab:no, sem:[thing,abstract],[]).
noun(3,neuter,genus,gener,um,class:common, ab:no, sem:[abstract],[]).
noun(3,neuter,iter,itiner,um,class:common, ab:no, sem:[abstract, loc],[]).
noun(3,neuter,ius,iur,um,class:common, ab:mm, sem:[abstract],[]).
noun(3,neuter,lac,lact,um,class:common, ab:no, sem:[thing],[]).
noun(3,neuter,latus,later,um,class:common, ab:no, sem:[thing, loc],[]).
noun(3,neuter,limen,limin,um,class:common, ab:no, sem:[thing, loc],[]).
noun(3,neuter,litus,litor,um,class:common, ab:no, sem:[thing, loc],[]).
noun(3,neuter,mare,mar,um,class:common, ab:no, sem:[thing, loc],[nb:sg]).
noun(3,neuter,munus,muner,um,class:common, ab:no, sem:[abstract,thing],[]).
noun(3,neuter,nemus,nemor,um,class:common, ab:no, sem:[thing, loc],[]).
noun(3,neuter,nomen,nomin,um,class:common, ab:mm, sem:[thing,abstract],[]).
noun(3,neuter,numen,numin,um,class:common, ab:mm, sem:[thing,abstract],[]).
noun(3,neuter,opus,oper,um,class:common, ab:mm, sem:[abstract,thing],[]).
noun(3,neuter,os,or,um,class:common, ab:no, sem:[thing],[]).
noun(3,neuter,pectus,pector,um,class:common, ab:no, sem:[thing],[]).
noun(3,neuter,pecus,pecor,um,class:common, ab:no, sem:[thing],[]).
noun(3,neuter,poema,poemat,um,class:common, ab:mm, sem:[thing],[]).
noun(3,neuter,pondus,ponder,um,class:common, ab:mm, sem:[thing,abstract],[]).
noun(3,neuter,rus,rur,um,class:common, ab:no, sem:[thing, loc],[]).
noun(3,neuter,scelus,sceler,um,class:common, ab:no, sem:[abstract],[]).
noun(3,neuter,sidus,sider,um,class:common, ab:no, sem:[thing],[]).
noun(3,neuter,tegmen,tegmin,um,class:common, ab:no, sem:[thing, loc],[]).
noun(3,neuter,tempora,tempor,um,class:common, ab:no, sem:[thing],[nb:pl]).
noun(3,neuter,tempus,tempor,um,class:common, ab:no, sem:[abstract,time],[]).
noun(3,neuter,ulnus,ulner,um,class:common, ab:no, sem:[thing,abstract],[]).





noun(4,fem,iuuentus,iuuent,class:common, ab:no, sem:[abstract,time],[]).
noun(4,fem,manus,man,class:common, ab:mm, sem:[thing],[]).
noun(4,fem,tribus,trib,class:common, ab:no, sem:[hum],[]).



noun(4,masc,casus,cas,class:common, ab:mm, sem:[thing,abstract],[]).
noun(4,masc,cursus,curs,class:common, ab:no, sem:[thing,abstract],[]).
noun(4,masc,consulatus,consulat,class:common, ab:no, sem:[abstract,time],[]).
noun(4,masc,dilectus,dilect,class:common, ab:no, sem:[abstract,time],[]).
noun(4,masc,delectus,delect,class:common, ab:no, sem:[abstract,time],[]).
noun(4,masc,euentus,euent,class:common, ab:no, sem:[thing,abstract],[]).
noun(4,masc,exercitus,exercit,class:common, ab:no, sem:[thing,hum,loc],[]).
noun(4,masc,fluctus,fluct,class:common, ab:no, sem:[thing,loc],[]).
noun(4,masc,fructus,fruct,class:common, ab:no, sem:[thing,abstract],[]).
noun(4,masc,gradus,grad,class:common, ab:no, sem:[thing,abstract],[]).
noun(4,masc,impetus,impet,class:common, ab:mm, sem:[abstract],[]).
noun(4,masc,metus,met,class:common, ab:mm, sem:[abstract],[]).
noun(4,masc,risus,ris,class:common, ab:mm, sem:[abstract],[]).
noun(4,masc,senatus,senat,class:common, ab:no, sem:[hum,abstract],[]).
noun(4,masc,spiritus,spirit,class:common, ab:mm, sem:[abstract],[]).
noun(4,masc,sumptus,sumpt,class:common, ab:no, sem:[thing,abstract],[]).
noun(4,masc,uultus,uult,class:common, ab:no, sem:[thing,abstract],[]).
noun(4,masc,usus,us,class:common, ab:mm, sem:[thing,abstract],[]).






noun(5,or([masc,fem]),dies,di,class:common, ab:time, sem:[time, thing, abstract],[]).
noun(5,fem,effigies,effigi,class:common, ab:no, sem:[thing],[]).
noun(5,fem,fides,fid,class:common, ab:mm, sem:[abstract],[nb:sg]).
noun(5,fem,res,r,class:common, ab:no, sem:[thing, abstract],[]).
noun(5,fem,species,speci,class:common, ab:mm, sem:[abstract],[]).
noun(5,fem,spes,sp,class:common, ab:no, sem:[abstract],[]).







%%%%%%%
% NAMES
%%%%%%%

% Gaia, Marcus, etc.  used as proper names for individuals
% therefore no plural generated

noun(1,fem,arria,arri,class:proper,ab:no, sem:[hum],[nb:sg]).
noun(1,fem,gaia,gai,class:proper,ab:no, sem:[hum],[nb:sg]).
noun(1,fem,lesbia,lesbi,class:proper,ab:no, sem:[hum],[nb:sg]).
noun(1,fem,minerua,mineru,class:proper,ab:no, sem:[hum],[nb:sg]).

noun(1,masc,catilina,catilin,class:proper,ab:no, sem:[hum],[nb:sg]).
noun(1,masc,dolabella,dolabell,class:proper,ab:no, sem:[hum],[nb:sg]).
noun(1,masc,galba,galb,class:proper,ab:no, sem:[hum],[nb:sg]).
noun(1,masc,iugurtha,iugurth,class:proper,ab:no, sem:[hum],[nb:sg]).
noun(1,masc,murena,muren,class:proper,ab:no, sem:[hum],[nb:sg]).
noun(1,masc,numida,numid,class:proper,ab:no, sem:[hum],[nb:pl]).

noun(2,masc,marcus,marc,class:proper,ab:no, sem:[hum],[nb:sg]).
noun(2,masc,meliboeus,meliboe,class:proper,ab:no, sem:[hum],[nb:sg]).
noun(2,masc,augustinus,augustin,class:proper,ab:no, sem:[hum],[nb:sg]).
noun(2,masc,petrus,petr,class:proper,ab:no, sem:[hum],[nb:sg]).
noun(2,masc,silanus,silan,class:proper,ab:no, sem:[hum],[nb:sg]).
noun(2,masc,homerus,homer,class:proper,ab:no, sem:[hum],[nb:sg]).
noun(2,masc,catullus,catull,class:proper,ab:no, sem:[hum],[nb:sg]).
noun(2,masc,tityrus,tityr,class:proper,ab:no, sem:[hum],[nb:sg]).
noun(2,masc,uinius,uini,class:proper,ab:no, sem:[hum],[nb:sg]).


noun(3,masc,caesar,caesar,um,class:proper,ab:no, sem:[hum],[nb:sg]).
noun(3,masc,cato,caton,um,class:proper,ab:no, sem:[hum],[nb:sg]).
noun(3,masc,cicero,ciceron,um,class:proper,ab:no, sem:[hum],[nb:sg]).
noun(3,masc,hamilcar,hamilcar,um,class:proper,ab:no, sem:[hum],[nb:sg]).
noun(3,masc,hannibal,hannibal,um,class:proper,ab:no, sem:[hum],[nb:sg]).
noun(3,masc,laco,lacon,um,class:proper,ab:no, sem:[hum],[nb:sg]).
noun(3,masc,nero,neron,um,class:proper,ab:no, sem:[hum],[nb:sg]).
noun(3,masc,macro,macron,um,class:proper,ab:no, sem:[hum],[nb:sg]).
noun(3,masc,otho,othon,um,class:proper,ab:no, sem:[hum],[nb:sg]).

noun(3,fem,ceres,cerer,um,class:proper,ab:no, sem:[hum],[nb:sg]).
noun(3,fem,uenus,uener,um,class:proper,ab:no, sem:[hum],[nb:sg]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
