s(s(P,VP,QM)) --> pronoun(P) ,verb_phrase(VP),question_mark(QM).

s(s(PRO,II,QM)) --> pronoun(PRO),infinitive_inflection(II),question_mark(QM).
s(s(PRO,II,QM)) --> pronoun(PRO),infinitive_inflection(II),question_mark(QM).

s(s(NP,VP,FS)) --> noun_phrase(NP),verb_phrase(VP),full_stop(FS).
s(s(NP,VP,PN,FS)) --> noun_phrase(NP),verb_phrase(VP),proper_noun(PN),full_stop(FS).
s(s(NP,VP,PP,FS)) --> noun_phrase(NP),verb_phrase(VP),proper_prep(PP),full_stop(FS).
s(s(NP,VP,PP,PN,FS)) --> noun_phrase(NP),verb_phrase(VP),proper_prep(PP),proper_noun(PN),full_stop(FS).

s(s(NP,VP,C,S2,FS)) --> noun_phrase(NP),verb_phrase(VP),conj(C),s2(S2),full_stop(FS).
s(s(NP,VP,PN,C,S2,FS)) --> noun_phrase(NP),verb_phrase(VP),proper_noun(PN),conj(C),s2(S2),full_stop(FS).
s(s(NP,VP,PP,C,S2,FS)) --> noun_phrase(NP),verb_phrase(VP),proper_prep(PP),conj(C),s2(S2),full_stop(FS).
s(s(NP,VP,PP,PN,C,S2,FS)) --> noun_phrase(NP),verb_phrase(VP),proper_prep(PP),proper_noun(PN),conj(C),s2(S2),full_stop(FS).

s(s(P,VP)) --> pronoun(P) ,verb_phrase(VP).

s(s(PRO,II)) --> pronoun(PRO),infinitive_inflection(II).
s(s(PRO,II)) --> pronoun(PRO),infinitive_inflection(II).

s(s(NP,VP)) --> noun_phrase(NP),verb_phrase(VP).
s(s(NP,VP,PN)) --> noun_phrase(NP),verb_phrase(VP),proper_noun(PN).
s(s(NP,VP,PP)) --> noun_phrase(NP),verb_phrase(VP),proper_prep(PP).
s(s(NP,VP,PP,PN)) --> noun_phrase(NP),verb_phrase(VP),proper_prep(PP),proper_noun(PN).

s(s(NP,VP,C,S2)) --> noun_phrase(NP),verb_phrase(VP),conj(C),s2(S2).
s(s(NP,VP,PN,C,S2)) --> noun_phrase(NP),verb_phrase(VP),proper_noun(PN),conj(C),s2(S2).
s(s(NP,VP,PP,C,S2)) --> noun_phrase(NP),verb_phrase(VP),proper_prep(PP),conj(C),s2(S2).
s(s(NP,VP,PP,PN,C,S2)) --> noun_phrase(NP),verb_phrase(VP),proper_prep(PP),proper_noun(PN),conj(C),s2(S2).



s2(s(NP,VP)) --> noun_phrase(NP),verb_phrase(VP).
s2(s(NP,VP,PN)) --> noun_phrase(NP),verb_phrase(VP),proper_noun(PN).
s2(s(NP,VP,PP)) --> noun_phrase(NP),verb_phrase(VP),proper_prep(PP).
s2(s(NP,VP,PP,PN)) --> noun_phrase(NP),verb_phrase(VP),proper_prep(PP),proper_noun(PN).

%s2(s2(NP,VP)) --> noun_phrase(NP),verb_phrase(VP).
%s2(s2(NP,VP,PN)) --> noun_phrase(NP),verb_phrase(VP),proper_noun(PN).
%s2(s2(NP,VP,PP)) --> noun_phrase(NP),verb_phrase(VP),proper_prep(PP).
%s2(s2(NP,VP,PP,PN)) --> noun_phrase(NP),verb_phrase(VP),proper_prep(PP),proper_noun(PN).


infinitive_inflection(infinitive_inflection(PAST,NP,PRESENT)) --> past(PAST),noun_phrase(NP),present(PRESENT).
infinitive_inflection(infinitive_inflection(PRESENT,NP,PRESENT)) --> present(PRESENT),noun_phrase(NP),present(PRESENT).

noun_phrase(noun_phrase(DET,N,C,NP2)) --> det(DET),noun(N),conj(C),noun_phrase2(NP2).
noun_phrase(noun_phrase(ADJ,N,C,NP2)) --> adj(ADJ),noun(N),conj(C),noun_phrase2(NP2).
noun_phrase(noun_phrase(DET,ADJ,N,C,NP2)) --> det(DET),adj(ADJ),noun(N),conj(C),noun_phrase2(NP2).
%                               plural
noun_phrase(noun_phrase(PLU,C,NP2)) --> plural(PLU),conj(C),noun_phrase2(NP2).
noun_phrase(noun_phrase(ADJ,PLU,C,NP2)) --> adj(ADJ),plural(PLU),conj(C),noun_phrase2(NP2).

noun_phrase(noun_phrase(DET,N)) --> det(DET),noun(N).
noun_phrase(noun_phrase(ADJ,N)) --> adj(ADJ),noun(N).
noun_phrase(noun_phrase(DET,ADJ,N)) --> det(DET),adj(ADJ),noun(N).
noun_phrase(noun_phrase(ADJ1,ADJ2,N)) --> adj(ADJ1),adj(ADJ2),noun(N).
noun_phrase(noun_phrase(DET,ADJ1,ADJ2,N)) --> det(DET), adj(ADJ1),adj(ADJ2),noun(N).

%                               plural
noun_phrase(noun_phrase(ADJ1,ADJ2,PLU)) --> adj(ADJ1),adj(ADJ2),plural(PLU).
noun_phrase(noun_phrase(ADJ,PLU)) --> adj(ADJ),plural(PLU).
noun_phrase(noun_phrase(PLU)) --> plural(PLU).


noun_phrase2(noun_phrase(DET,N)) --> det(DET),noun(N).
noun_phrase2(noun_phrase(DET,ADJ,N)) --> det(DET),adj(ADJ),noun(N).
noun_phrase2(noun_phrase(DET,ADJ1,ADJ2,N)) --> det(DET),adj(ADJ1),adj(ADJ2),noun(N).
noun_phrase2(noun_phrase(ADJ,N)) --> adj(ADJ),noun(N).
noun_phrase2(noun_phrase(ADJ1,ADJ2,N)) --> adj(ADJ1),adj(ADJ2),noun(N).

%                               plural
noun_phrase2(noun_phrase(PLU)) --> plural(PLU).
noun_phrase2(noun_phrase(ADJ1,ADJ2,PLU)) --> adj(ADJ1),adj(ADJ2),plural(PLU).
noun_phrase2(noun_phrase(ADJ,PLU)) --> adj(ADJ),plural(PLU).



verb_phrase(verb_phrase(ADV,V,NP)) --> adverb(ADV),verb(V),noun_phrase(NP).
verb_phrase(verb_phrase(V,NP)) --> verb(V),noun_phrase(NP).
verb_phrase(verb_phrase(PAST)) --> past(PAST).
verb_phrase(verb_phrase(ADV,PAST)) --> adverb(ADV),past(PAST).
verb_phrase(verb_phrase(ADV,V,NP1,NP2)) --> adverb(ADV),verb(V),noun_phrase(NP1),noun_phrase(NP2).
verb_phrase(verb_phrase(V,NP1,NP2)) --> verb(V),noun_phrase(NP1),noun_phrase(NP2).

verb_phrase(verb_phrase(V1,C,V2))  --> verb(V1),conj(C),verb_phrase(V2).

verb_phrase(verb_phrase(ADV,V,NP,C,V2)) --> adverb(ADV),verb(V),noun_phrase(NP),conj(C),verb_phrase(V2).
verb_phrase(verb_phrase(V,NP,C,V2)) --> verb(V),noun_phrase(NP),conj(C),verb_phrase(V2).
verb_phrase(verb_phrase(PAST,C,V2)) --> past(PAST),conj(C),verb_phrase(V2).
verb_phrase(verb_phrase(ADV,PAST,C,V2)) --> adverb(ADV),past(PAST),conj(C),verb_phrase(V2).
verb_phrase(verb_phrase(ADV,V,NP1,NP2,C,V2)) --> adverb(ADV),verb(V),noun_phrase(NP1),noun_phrase(NP2),conj(C),verb_phrase(V2).
verb_phrase(verb_phrase(V,NP1,NP2,C,V2)) --> verb(V),noun_phrase(NP1),noun_phrase(NP2),conj(C),verb_phrase(V2).


proper_noun(proper_noun(P,NP)) --> prep(P),noun_phrase(NP).
proper_noun(proper_noun(P,N)) --> prep(P),noun(N).


proper_prep(proper_noun(P,N)) --> prep(P),noun_phrase(N).


det(det(the)) --> [the].
det(det(a)) --> [a].
det(det(every)) --> [every].
det(det(some)) --> [some].
det(det(many)) --> [many].

adj(adj(poor)) --> [poor].
adj(adj(young)) --> [young].
adj(adj(large)) --> [large].
adj(adj(big)) --> [big].
adj(adj(old)) --> [old].
adj(adj(white)) --> [white].
adj(adj(brilliant)) --> [brilliant].
adj(adj(talented)) --> [talented].
adj(adj(bright)) --> [bright].
adj(adj(empty)) --> [empty].
adj(adj(hollow)) --> [hollow].
adj(adj(depressed)) --> [depressed].
adj(adj(broken)) --> [broken].
adj(adj(happy)) --> [happy].
adj(adj(handsome)) --> [handsome].
adj(adj(pretty)) --> [pretty].
adj(adj(black)) --> [black].
adj(adj(small)) --> [small].
adj(adj(new)) --> [new].
adj(adj(shallow)) --> [shallow].
adj(adj(bale)) --> [bale].
adj(adj(skilled)) --> [skilled].
adj(adj(stupid)) --> [stupid].
adj(adj(flamboyant)) --> [flamboyant].


noun(noun(student)) --> [student].
noun(noun(students)) --> [students].
noun(noun(professor)) --> [professor].
noun(noun(professors)) --> [professors].
noun(noun(researcher)) --> [researcher].
noun(noun(researchers)) --> [researchers].
noun(noun(scientist)) --> [scientist].
noun(noun(scientists)) --> [scientists].
noun(noun(lecturers)) --> [lecturers].
noun(noun(lecturer)) --> [lecturer].
noun(noun(shed)) --> [shed].
noun(noun(envelope)) --> [envelope].
noun(noun(boy)) --> [boy].
noun(noun(box)) --> [box].
noun(noun(room)) --> [room].
noun(noun(school)) --> [school].
noun(noun(woman)) --> [woman].
noun(noun(man)) --> [man].
noun(noun(building)) --> [building].
noun(noun(girl)) --> [girl].
noun(noun(tree)) --> [tree].
noun(noun(ball)) --> [ball].
noun(noun(mat)) --> [mat].
noun(noun(bat)) --> [bat].


plural(plural(lecturers)) --> [lecturers].%
plural(plural(scientists)) --> [scientists].
plural(plural(students)) --> [students].%
plural(plural(professors)) --> [professors].%
plural(plural(researchers)) --> [researchers].%



pronoun(pronoun(who)) --> [who].
pronoun(pronoun(what)) --> [what].


past(past(appreciated)) --> [appreciated].
past(past(watched)) --> [watched].
past(past(climbed)) --> [climbed].
past(past(admired)) --> [admired].
past(past(pushed)) --> [pushed].
past(past(stored)) --> [stored].
past(past(gave)) --> [gave].
past(past(helped)) --> [helped].
past(past(hit)) --> [hit].
past(past(stole)) --> [stole].
past(past(stalked)) --> [stalked].
past(past(stabbed)) --> [stabbed].
past(past(stunned)) --> [stunned].
past(past(broke)) --> [broke].
past(past(fed)) --> [fed].
past(past(felt)) --> [felt].
past(past(attacked)) --> [attacked].
past(past(defended)) --> [defended].
past(past(won)) --> [won].
past(past(defeated)) --> [defeated].
past(past(deleted)) --> [deletde].
past(past(diagnosed)) --> [diagnosed].
past(past(dedicated)) --> [dedicated].
past(past(did)) --> [did].




verb(verb(appreciated)) --> [appreciated].
verb(verb(watched)) --> [watched].
verb(verb(climbed)) --> [climbed].
verb(verb(admired)) --> [admired].
verb(verb(pushed)) --> [pushed].
verb(verb(stored)) --> [stored].
verb(verb(gave)) --> [gave].
verb(verb(helped)) --> [helped].
verb(verb(stole)) --> [stole].
verb(verb(stalked)) --> [stalked].
verb(verb(stabbed)) --> [stabbed].
verb(verb(stunned)) --> [stunned].
verb(verb(broke)) --> [broke].
verb(verb(fed)) --> [fed].
verb(verb(felt)) --> [felt].
verb(verb(attacked)) --> [attacked].
verb(verb(defended)) --> [defended].
verb(verb(won)) --> [won].
verb(verb(defeated)) --> [defeated].
verb(verb(deleted)) --> [deletde].
verb(verb(diagnosed)) --> [diagnosed].
verb(verb(dedicated)) --> [dedicated].

verb(verb(appreciate)) --> [appreciate].
verb(verb(watch)) --> [watch].
verb(verb(climb)) --> [climb].
verb(verb(admire)) --> [admire].
verb(verb(push)) --> [push].
verb(verb(pushes)) --> [pushes].
verb(verb(store)) --> [store].
verb(verb(stores)) --> [stores].
verb(verb(give)) --> [give].
verb(verb(gives)) --> [gives].
verb(verb(did)) --> [did].
verb(verb(do)) --> [do].
verb(verb(take)) --> [take].
verb(verb(help)) --> [help].
verb(verb(helps)) --> [helps].
verb(verb(hit)) --> [hit].
verb(verb(steal)) --> [steal].
verb(verb(stalk)) --> [stalk].
verb(verb(stab)) --> [stab].
verb(verb(stun)) --> [stun].
verb(verb(break)) --> [break].
verb(verb(feed)) --> [feed].
verb(verb(feel)) --> [feel].
verb(verb(attack)) --> [attack].
verb(verb(defend)) --> [defend].
verb(verb(win)) --> [win].
verb(verb(defeat)) --> [defeat].
verb(verb(delete)) --> [delete].
verb(verb(diagnose)) --> [diagnose].
verb(verb(dedicate)) --> [dedicate].




present(present(hit)) --> [hit].
present(present(appreciate)) --> [appreciate].
present(present(watch)) --> [watch].
present(present(climb)) --> [climb].
present(present(push)) --> [push].
present(present(store)) --> [store].
present(present(do)) --> [do].
present(present(give)) --> [give].
present(present(take)) --> [take].
present(present(help)) --> [help].
present(present(steal)) --> [steal].
present(present(stalk)) --> [stalk].
present(present(stab)) --> [stab].
present(present(stun)) --> [stun].
present(present(break)) --> [break].
present(present(feed)) --> [feed].
present(present(feel)) --> [feel].
present(present(attack)) --> [attack].
present(present(defend)) --> [defend].
present(present(win)) --> [win].
present(present(defeat)) --> [defeat].
present(present(delete)) --> [delete].
present(present(diagnose)) --> [diagnose].
present(present(dedicate)) --> [dedicate].


adverb(adverb(quickly)) --> [quickly].
adverb(adverb(easily)) --> [easily].
adverb(adverb(peacefully)) --> [peacefully].
adverb(adverb(carefully)) --> [carefully].
adverb(adverb(slowly)) --> [slowly].
adverb(adverb(badly)) --> [badly].
adverb(adverb(closely)) --> [closely].
adverb(adverb(well)) --> [well].
adverb(adverb(fast)) --> [fast].
adverb(adverb(cheerfully)) --> [cheerfully].
adverb(adverb(skilfully )) --> [skillfully].


prep(prep(on)) --> [on].
prep(prep(in)) --> [in].
prep(prep(after)) --> [after].
prep(prep(behind)) --> [behind].
prep(prep(at)) --> [at].
prep(prep(since)) --> [since].
prep(prep(for)) --> [for].
prep(prep(ago)) --> [ago].
prep(prep(before)) --> [before].
prep(prep(by)) --> [by].
prep(prep(till)) --> [till].


full_stop(punc('.')) --> ['.'].
question_mark(punc('?')) --> ['?'].

conj(conj(and)) --> [and].