(* H:\ML Yoann *)

type taxonomie_t = Bacterie|Eucaryote|Archee;;


type ecotype_t = Mesophile|Thermophile|Psychrophile;;


type organisme_t = {id_O:int; nom:string; taxonomie:taxonomie_t; ecotype:ecotype_t};;


type genome_t = {id_G:int; taille:float; genes:int; date:int};;


let org_1 = {id_O = 1; nom = "Escherichia_coli"; taxonomie = Bacterie; ecotype = Mesophile};;
let org_2 = {id_O = 2; nom = "Saccharomyces_cerevisiae"; taxonomie = Eucaryote; ecotype = Mesophile};;
let org_3 = {id_O = 3; nom = "Arabidopsis_thaliana"; taxonomie = Eucaryote; ecotype = Mesophile};;
let org_4 = {id_O = 15; nom = "Sulfolobus_solfataricus"; taxonomie = Archee; ecotype = Thermophile};;
let org_5 = {id_O = 16; nom = "Thermotoga_maritima"; taxonomie = Bacterie; ecotype = Thermophile};; 
let org_6 = {id_O = 17; nom = "Pyrococcus_furiosus"; taxonomie = Archee; ecotype = Thermophile};;
let org_7 = {id_O = 20; nom = "Caenorhabditis_elegans"; taxonomie = Eucaryote; ecotype = Mesophile};;


let organisme = [org_1;org_2;org_3;org_4;org_5;org_6;org_7];;


let gen_1 = {id_G = 1; taille = 4.6; genes = 4600; date = 2001};;
let gen_2 = {id_G = 15; taille = 2.9; genes = 3034; date = 2001};;
let gen_3 = {id_G = 16; taille = 2.0; genes = 1763; date = 2006};;
let gen_4 = {id_G = 17; taille = 1.9; genes = 1700; date = 2002};;


let genome = [gen_1;gen_2;gen_3;gen_4];;


let rec search_thermophile liste_Org =
match liste_Org with
	|[]->[]
	|organisme_t::r when organisme_t.ecotype=Thermophile->organisme_t.nom::(search_thermophile r)
	|organisme_t::r-> search_thermophile r;;


let rec search_B4_2001 genome = 
match genome with
	|[]-> 0
	|e::r when e.date <2001-> 1+(search_B4_2001 r)
	|e::r->(search_B4_2001 r);;

let gen_5 = {id_G = 18; taille = 5.6; genes = 5600; date = 1998};;

let genome = [gen_1;gen_2;gen_3;gen_4;gen_5];;


let rec fullGenom i g =
match g with
	|[]->false
	|e::r when e.id_G = i ->true
	|e::r ->fullGenom i r;;

let rec listgeno organisme genome =
match organisme with
	|[]->[]
	|e::r when (fullGenom e.id_O genome)=true->e.nom::(listgeno r genome)
	|e::r->listgeno r genome;;

let calculPercentCoding genome = (float_of_int(genome.genes)/.(genome.taille*.1000.))*.100.;;

let moreThanNinety gene = calculPercentCoding gene >= 90.0;;

let rec whichGenom i gene=
	match gene with
	|[]->failwith"Genome not found"
	|e::r when e.id_G=i ->e
	|e::r->whichGenom i r;;
val whichGenom : int -> genome_t

let org_8 = {id_O = 18; nom = "Organisme_questiontrois"; taxonomie = Eucaryote; ecotype = Mesophile};;
let organisme = [org_1;org_2;org_3;org_4;org_5;org_6;org_7;org_8];;

let rec listOrgCodingMoreThanNinety genom org =
	match org with
	|[]->[]
	|e::r when (fullGenom e.id_O genom)&& moreThanNinety (whichGenom e.id_O genome)->(e.nom, calculPercentCoding (whichGenom e.id_O genome))::listOrgCodingMoreThanNinety genom r
	|e::r->listOrgCodingMoreThanNinety genom r;;

let rec fusionBases b1 b2=
match (b1,b2) with
	|([],b2) -> b2
	|(b1,[]) -> b1
	|(e1::n1,e2::n2) when e1.id_G<e2.id_G -> e1::(fusionBases n1 b2)
	|(e1::n1,e2::n2) when e1.id_G>e2.id_G -> e2::(fusionBases b1 n2)
	|(e::n1,_::n2) -> e::(fusionBases n1 n2);;

let gen_30={id_G=30; taille=6.0; genes= 3624; date=2014};;
let gen_31={id_G=31; taille=1.2; genes= 766; date=2014};;
let gen_32={id_G=32; taille=9.0; genes= 8374; date=2014};;
let gen_33={id_G=33; taille=2.3; genes= 128; date=2014};;

let genomeB1 = [gen_30; gen_31; gen_32; gen_33];;

let gen_40={id_G=40; taille=6.0; genes= 4524; date=2014};;
let gen_41={id_G=41; taille=1.2; genes= 525; date=2014};;
let gen_42={id_G=42; taille=9.0; genes= 2345; date=2014};;
let gen_43={id_G=43; taille=2.3; genes= 1234; date=2014};;

let genomeB2 = [gen_40; gen_41; gen_42; gen_43];;

let listeAllBases =[genome; genomeB1; genomeB2];;

let rec fusion_liste_base l =
match l with
|[] -> []
|[e] -> [e]
|e::b::n -> fusion_liste_base((fusionBases e b)::(fusion_liste_base n));;
