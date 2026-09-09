import json,pathlib
ROOT=pathlib.Path(__file__).resolve().parents[1]
questions=[]
def q(level,domain,title,statement,answer,variants,family=None):
 i=f'Q{len(questions)+1:02}'
 rows=[]
 for j,(body,tags,note) in enumerate(variants):
  rows.append(dict(id=f'{i}-{j+1}',content=body,research=dict(targets=tags.split(),analysis=note)))
 questions.append(dict(id=i,level=level,domain=domain,title=title,statement=statement,referenceAnswer=answer,family=family or i,productions=rows))
# Codes are hypotheses about expectations, not scores for respondents.
q('3e','Géométrie','Longueur et justification',r'Le triangle $ABC$ est rectangle en $B$, avec $AC=5$ cm et $BC=3$ cm. Calculer $AB$ en justifiant.', '4 cm',[
(r'$AC^2=AB^2+BC^2$ par le théorème de Pythagore. Donc $AB^2=25-9=16$. Comme $AB$ est une longueur, $AB=4$ cm.','REF','Déduction, signe et unité explicités.'),
(r'$AB=\sqrt{5^2-3^2}=4$ cm.','ECO','Théorème et positivité implicites ; calcul valide, pas de lacune logique nécessaire.'),
(r'$AB^2=25-9=16$, donc $AB=4$.','UNIT EXPL','Résultat numérique juste ; unité absente, théorème non nommé.'),
(r'$AB=4$ cm car $4^2+3^2=5^2$.','REC EXPL','Vérifie une valeur candidate sans expliciter son unicité ; celle-ci est récupérable par positivité, à distinguer d’une erreur de réciproque.')], 'pythagore')
q('4e','Géométrie','Réciproque ou théorème',r'$AB=3$, $BC=4$, $AC=5$ (en cm). Démontrer que $ABC$ est rectangle.', 'Rectangle en B',[
(r'$AC^2=25=9+16=AB^2+BC^2$. Par la réciproque du théorème de Pythagore, le triangle est rectangle en $B$.','REF','Référence attendue.'),
(r'$3^2+4^2=5^2$. Donc le triangle est rectangle en $B$, par le théorème de Pythagore.','NAME','Nom imprécis ; implication utilisée correcte si le théorème désigne la caractérisation.'),
(r'Le triangle est rectangle en $B$, donc $AB^2+BC^2=AC^2$. Les valeurs donnent bien $9+16=25$. Il est donc rectangle.','CIRC','Conclusion posée comme prémisse ; vérification numérique présente mais son rôle justificatif est à réorganiser.'),
(r'$9+16=25$. Rectangle.','ECO EXPL','Argument récupérable mais sommet et correspondance des longueurs non explicités.')])
q('5e','Géométrie','Angles d’un triangle',r'Dans un triangle $ABC$, $\widehat A=50^\circ$ et $\widehat B=60^\circ$. Calculer $\widehat C$ en justifiant.', '70°',[
(r'La somme des angles d’un triangle vaut $180^\circ$. Ainsi $\widehat C=180^\circ-50^\circ-60^\circ=70^\circ$.','REF','Règle et calcul corrects.'),
(r'$180-50-60=70$. Donc $\widehat C=70^\circ$.','EXPL','Somme des angles implicite ; unités intermédiaires omises mais conclusion précise.'),
(r'$\widehat C=180-50=130-60=70^\circ$.','EQ','Chaîne d’égalités fausse ; calcul séquentiel compréhensible.'),
(r'La somme des angles vaut $180^\circ$. Donc $\widehat C=180^\circ-50^\circ-60^\circ=80^\circ$.','CALC','Erreur arithmétique isolée, raisonnement correct.')])
q('3e','Algèbre','Équation linéaire',r'Résoudre $3x+2=14$ dans $\mathbb R$ et justifier.', 'S={4}',[
(r'$3x+2=14\iff3x=12\iff x=4$. L’ensemble des solutions est $\{4\}$.','REF','Équivalences valides.'),
(r'Pour $x=4$, $3\times4+2=14$. Donc la solution est $4$.','EXH','Existence vérifiée ; absence d’autre solution non justifiée dans le texte.'),
(r'$3x+2=14$, donc $3x=12$, donc $x=4$.','REC','Opérations réversibles ; ne pas coder les « donc » comme erreur logique automatique.'),
(r'$3x=14-2=12/3=4$. Donc $x=4$.','EQ','Résultat correct, égalités intermédiaires fausses.')], 'lineaire')
q('3e','Algèbre','Produit nul',r'Résoudre $(x-2)(x+3)=0$ dans $\mathbb R$. Justifier.', 'S={-3,2}',[
(r'Un produit est nul si et seulement si l’un des facteurs est nul. Ainsi $x=2$ ou $x=-3$ ; $S=\{-3;2\}$.','REF','Exhaustif et suffisant.'),
(r'Il faut $x-2=0$ et $x+3=0$. Donc $S=\{-3;2\}$.','CONNECT','« et » incompatible avec l’ensemble final ; intention récupérable.'),
(r'Pour $x=2$ ou $x=-3$, le produit vaut zéro. Ce sont les solutions.','EXH','Seulement vérification des deux valeurs.'),
(r'$(x-2)(x+3)=0\iff x=2$.','EXH LOG','Perd la racine -3, résultat incomplet.')], 'produit')
q('4e','Arithmétique','Somme de deux impairs',r'Démontrer que la somme de deux entiers impairs est paire.', 'Somme paire',[
(r'Écrivons les deux entiers $2p+1$ et $2q+1$, avec $p,q\in\mathbb Z$. Leur somme vaut $2(p+q+1)$ et $p+q+1$ est entier : elle est paire.','REF','Témoins indépendants.'),
(r'Les deux nombres sont $2n+1$ et $2n+1$. Leur somme vaut $4n+2=2(2n+1)$, donc elle est paire.','NAME GEN','Même lettre impose deux nombres égaux ; perte de généralité effective.'),
(r'$3+5=8$, $7+9=16$ et $11+13=24$. La somme de deux impairs est donc toujours paire.','EMP GEN','Exemples seulement.'),
(r'Chaque nombre impair laisse une unité après regroupement par paires. Les deux unités restantes forment une paire supplémentaire ; la somme se regroupe donc entièrement par paires.','REGISTER','Argument générique verbal valide ; pas de symbolisme nécessaire.')])
q('3e','Arithmétique','Divisibilité',r'Démontrer que le produit de deux entiers pairs est divisible par $4$.', 'Divisible par 4',[
(r'Soient $a=2p$ et $b=2q$ avec $p,q\in\mathbb Z$. Alors $ab=4pq$ ; comme $pq$ est entier, $4$ divise $ab$.','REF','Définitions explicitées.'),
(r'$a=2p$, $b=2q$, donc $ab=4pq$, d’où le résultat.','DOMAIN EXPL','Domaines des paramètres implicites dans le contexte.'),
(r'Les deux nombres sont pairs, donc leur produit est pair, donc divisible par $4$.','LOG','Dernière implication fausse, conclusion vraie.'),
(r'$2\times4=8$ et $6\times8=48$. C’est donc vrai.','EMP GEN','Exemples insuffisants.')])
q('5e','Calcul','Fractions',r'Calculer exactement $\frac12+\frac13$, en montrant le calcul.', '5/6',[
(r'$\frac12+\frac13=\frac36+\frac26=\frac56$.','REF','Calcul exact sans phrases superflues.'),
(r'$\frac12+\frac13=0{,}833$.','APPROX','Confond approximation et égalité ; ne répond pas exactement.'),
(r'$\frac12+\frac13=\frac{1+1}{2+3}=\frac25$.','CALC','Règle de calcul erronée.'),
(r'$\frac56$.','TASK EXPL','Résultat exact sans le calcul demandé.')])
q('4e','Géométrie','Parallélisme',r'Les droites $d$ et $e$ sont perpendiculaires à une même droite $f$ dans le plan. Démontrer que $d$ et $e$ sont parallèles.', 'd parallèle e',[
(r'Deux droites d’un plan perpendiculaires à une même droite sont parallèles. Comme $d\perp f$ et $e\perp f$, on a $d\parallel e$.','REF','Théorème utilisé avec ses hypothèses.'),
(r'$d\perp f$ et $e\perp f$, donc $d\parallel e$.','ECO','Théorème implicite, déduction valide.'),
(r'Sur mon dessin, $d$ et $e$ ne se coupent pas. Elles sont donc parallèles.','FIG','Absence d’intersection visible non probante.'),
(r'$d\parallel e$, donc elles font le même angle avec $f$. Elles sont donc parallèles.','CIRC','Conclusion réutilisée comme donnée.')])
q('3e','Statistiques','Moyenne pondérée',r'Une série comprend deux valeurs égales à $10$ et trois valeurs égales à $20$. Calculer sa moyenne et justifier.', '16',[
(r'La somme est $2\times10+3\times20=80$ pour $5$ valeurs. La moyenne est $80/5=16$.','REF','Pondération et effectif corrects.'),
(r'$\frac{2\times10+3\times20}{5}=16$.','ECO','Calcul autoportant.'),
(r'La moyenne est $(10+20)/2=15$.','MODEL','Ignore les effectifs.'),
(r'$2\times10=20+3\times20=80/5=16$.','EQ','Enchaînement de calculs écrit comme égalités fausses.')])
q('3e','Probabilités','Dé équilibré',r'On lance un dé équilibré à six faces numérotées de $1$ à $6$. Calculer la probabilité d’obtenir un nombre pair et justifier.', '1/2',[
(r'Les six issues sont équiprobables et trois sont paires : $2,4,6$. La probabilité est $3/6=1/2$.','REF','Modèle et dénombrement explicites.'),
(r'$P(\text{pair})=3/6=1/2$.','EXPL','Équiprobabilité donnée mais favorable non détaillé.'),
(r'Il y a deux possibilités : pair ou impair. Donc la probabilité est $1/2$.','MODEL','Deux catégories ne suffisent pas à établir équiprobabilité ; conclusion juste ici.'),
(r'J’ai obtenu $50$ nombres pairs en $100$ lancers, donc la probabilité exacte est $1/2$.','EMP APPROX','Fréquence ne prouve pas probabilité exacte.')])
q('3e','Algèbre','Carré et signe',r'Résoudre $x^2=9$ dans $\mathbb R$ en justifiant.', 'S={-3,3}',[
(r'$x^2=9\iff(x-3)(x+3)=0\iff x=3\text{ ou }x=-3$.','REF','Deux racines.'),
(r'$x^2=9\iff x=\sqrt9=3$.','EXH LOG','Oublie la racine négative.'),
(r'$3^2=9$ et $(-3)^2=9$, donc $S=\{-3;3\}$.','EXH','Vérifie sans argument explicite d’exhaustivité.'),
(r'$x=\pm3$.','ECO TASK','Résultat juste ; justification demandée absente.')], 'carre')
q('2de','Algèbre','Équation linéaire',r'Résoudre $3x+2=14$ dans $\mathbb R$ et justifier.', 'S={4}',[
(r'$3x+2=14\iff3x=12\iff x=4$. L’ensemble des solutions est $\{4\}$.','REF','Même stimulus que Q04, niveau seul modifié.'),
(r'Pour $x=4$, $3\times4+2=14$. Donc la solution est $4$.','EXH','Existence sans unicité explicite.'),
(r'$3x+2=14$, donc $3x=12$, donc $x=4$.','REC','Calculs réversibles malgré l’absence de symbole équivalence.'),
(r'$3x=14-2=12/3=4$. Donc $x=4$.','EQ','Résultat correct avec égalités fausses.')], 'lineaire')
q('2de','Algèbre','Division par l’inconnue',r'Résoudre $x^2=2x$ dans $\mathbb R$.', 'S={0,2}',[
(r'$x^2=2x\iff x(x-2)=0\iff x=0\text{ ou }x=2$.','REF','Factorisation exhaustive.'),
(r'On divise par $x$ : $x=2$. Donc $S=\{2\}$.','DOMAIN EXH','Division exclut zéro sans traitement.'),
(r'Si $x=0$, l’égalité est vraie. Sinon on divise par $x$ et on obtient $x=2$, qui convient aussi. Donc $S=\{0;2\}$.','REGISTER','Disjonction de cas valide.'),
(r'En divisant par $x$, on obtient $x=2$. On ajoute aussi $0$, qui convient. Donc $S=\{0;2\}$.','DOMAIN EXPL','Répare le cas perdu ; domaine de la division reste implicite, ensemble final exact.')])
q('2de','Algèbre','Racine carrée',r'Résoudre $\sqrt{x+2}=x$ dans $\mathbb R$.', 'S={2}',[
(r'Il faut $x\ge0$. Sur ce domaine, $\sqrt{x+2}=x\iff x+2=x^2\iff(x-2)(x+1)=0$. Seul $2$ est positif ou nul, donc $S=\{2\}$.','REF','Signe et équivalence contrôlés.'),
(r'$\sqrt{x+2}=x\iff x+2=x^2\iff x=2\text{ ou }x=-1$. Donc $S=\{-1;2\}$.','REC DOMAIN','Élévation au carré non réversible sans signe.'),
(r'Une solution vérifie $x^2-x-2=0$, donc vaut $2$ ou $-1$. Vérification : $2$ convient ; $-1$ ne convient pas. Donc $S=\{2\}$.','REGISTER','Analyse puis synthèse complète sans domaine initial explicite.'),
(r'$x+2=x^2$, donc $x=2$. Vérification : $\sqrt4=2$.','EXH EXPL','Racine -1 omise dans analyse ; résultat exact, justification d’exclusion absente.')])
q('2de','Analyse','Quotient simplifié',r'Simplifier $f(x)=\frac{x^2-1}{x-1}$ et préciser son domaine de définition.', 'D=R\\{1}; f(x)=x+1 sur D',[
(r'$D_f=\mathbb R\setminus\{1\}$. Pour $x\ne1$, $f(x)=\frac{(x-1)(x+1)}{x-1}=x+1$.','REF','Restriction conservée.'),
(r'$f(x)=x+1$, donc $D_f=\mathbb R$.','DOMAIN','Simplification étend à tort le domaine.'),
(r'$f(x)=x+1$ pour $x\ne1$.','ECO','Domaine exprimé sans notation ensembliste ; réponse suffisante.'),
(r'$f(x)=x+1$.','DOMAIN TASK','Domaine demandé non indiqué.')])
q('2de','Algèbre','Inéquation',r'Résoudre $-2x<6$ dans $\mathbb R$. Justifier.', ']-3,+inf[',[
(r'En divisant par $-2$, on inverse le sens de l’inégalité : $x>-3$. Donc $S=]-3;+\infty[$.','REF','Règle explicitée.'),
(r'$-2x<6\iff x>-3$.','ECO','Transformation correcte, règle implicite.'),
(r'$-2x<6\iff x<-3$.','LOG','Sens non inversé.'),
(r'$x>-3$, donc $S=[-3;+\infty[$.','BOUND','Conclusion inclut indûment la borne.')])
q('1re spé','Algèbre','Second degré',r'Résoudre $3x^2-5x+2=0$ dans $\mathbb R$ en justifiant.', 'S={2/3,1}',[
(r'$3x^2-5x+2=(3x-2)(x-1)$. Le produit est nul exactement pour $x=2/3$ ou $x=1$. Donc $S=\{2/3;1\}$.','REF','Factorisation complète.'),
(r'$\Delta=25-24=1$. Donc $x=\frac{5\pm1}{6}$, soit $S=\{2/3;1\}$.','ECO','Formule de résolution implicite, valide.'),
(r'$\Delta=1$. Puis $\Delta=\frac{5-1}{6}=2/3$ et l’autre solution est $1$. Donc $S=\{2/3;1\}$.','NAME','Réaffecte delta au résultat ; récupération séquentielle possible, pas simultanée.'),
(r'$\Delta=25-24=1$. Donc $x=\frac{5\pm1}{3}$, soit $S=\{4/3;2\}$.','CALC','Dénominateur erroné.')])
q('1re spé','Analyse','Dérivée et variation',r'Pour $f(x)=x^2-4x+1$ sur $\mathbb R$, déterminer les intervalles de monotonie en justifiant.', 'Décroît jusqu’à 2, croît après',[
(r'$f’(x)=2x-4$, négatif pour $x<2$ et positif pour $x>2$. Donc $f$ décroît sur $]-\infty;2]$ et croît sur $[2;+\infty[$.','REF','Lien signe-variation correct.'),
(r'$2x-4<0\iff x<2$ ; $2x-4>0\iff x>2$. Décroissante puis croissante, changement en $2$.','NAME EXPL','Dérivée non identifiée, intervalles récupérables.'),
(r'$f’(x)=2x-4$ est croissante ; donc $f$ est croissante sur $\mathbb R$.','LOG','Confond variation de dérivée et signe.'),
(r'$f(0)=1$, $f(2)=-3$, $f(4)=1$. Donc $f$ décroît jusqu’à $2$, puis croît.','EMP','Trois valeurs ne prouvent pas les variations.')])
q('1re spé','Probabilités','Probabilité conditionnelle',r'$P(A)=0{,}4$, $P(B\mid A)=0{,}5$ et $P(B\mid\overline A)=0{,}2$. Calculer $P(B)$ et justifier.', '0.32',[
(r'$A$ et $\overline A$ forment une partition. Donc $P(B)=0{,}4\times0{,}5+0{,}6\times0{,}2=0{,}32$.','REF','Probabilités totales.'),
(r'$P(B)=0{,}4\times0{,}5+0{,}6\times0{,}2=0{,}32$.','ECO','Partition implicite.'),
(r'$P(B)=(0{,}5+0{,}2)/2=0{,}35$.','MODEL','Moyenne non pondérée.'),
(r'$P(A\mid B)=0{,}4\times0{,}5+0{,}6\times0{,}2=0{,}32$. Donc $P(B)=0{,}32$.','NAME','Calcul correct attaché à un objet différent ; assertion conditionnelle fausse.')])
q('Tle spé','Analyse','Limite d’un quotient',r'Calculer $\lim_{x\to+\infty}\frac{2x+1}{x+3}$ en justifiant.', '2',[
(r'Pour $x>0$, $\frac{2x+1}{x+3}=\frac{2+1/x}{1+3/x}$. Le numérateur tend vers $2$ et le dénominateur vers $1\ne0$. La limite vaut $2$.','REF','Règles de limite justifiées.'),
(r'$\frac{2x+1}{x+3}\sim\frac{2x}{x}=2$, donc la limite vaut $2$.','REGISTER','Équivalent valide ; disponibilité de ce registre à préciser au prétest.'),
(r'$\frac{2x+1}{x+3}=\frac{2x}{x}=2$ à l’infini.','EQ LIMIT','Égalités fausses pour x fini ; langage de passage à limite non formalisé.'),
(r'Le quotient est de la forme $\infty/\infty$, donc sa limite est $1$.','LOG','Forme indéterminée traitée comme opération.')])
q('Tle spé','Analyse','Récurrence',r'$u_0=1$ et $u_{n+1}=2u_n$ pour $n\in\mathbb N$. Démontrer que $u_n=2^n$ pour tout $n\in\mathbb N$.', 'u_n=2^n',[
(r'$u_0=1=2^0$. Si $u_n=2^n$ pour un entier $n\ge0$, alors $u_{n+1}=2\times2^n=2^{n+1}$. La propriété est donc vraie pour tout $n\ge0$ par récurrence.','REF','Initialisation et hérédité.'),
(r'Si $u_n=2^n$, alors $u_{n+1}=2u_n=2^{n+1}$. Donc la propriété est vraie pour tout $n$.','INIT','Initialisation absente, non fausse.'),
(r'On suppose que pour tout $n$, $u_n=2^n$. Alors $u_{n+1}=2^{n+1}$. La propriété est démontrée.','QUANT CIRC INIT','Hypothèse globale contient conclusion ; base omise.'),
(r'$u_0=1$, $u_1=2$, $u_2=4$, $u_3=8$. On voit que $u_n=2^n$.','EMP GEN','Échantillon seulement.')])
q('Tle spé','Analyse','Intégrale',r'Calculer $\int_0^1 2x\,dx$ en justifiant.', '1',[
(r'Une primitive de $x\mapsto2x$ est $F(x)=x^2$. Ainsi $\int_0^1 2x\,dx=F(1)-F(0)=1$.','REF','Primitive et bornes.'),
(r'$\int_0^1 2x\,dx=[x^2]_0^1=1$.','ECO','Notation standard suffisante.'),
(r'$\int_0^1 2x\,dx=x^2=1$.','EQ NAME','Variable libre assimilée à un nombre ; bornes implicites.'),
(r'$\int_0^1 2x\,dx=2$.','CALC EXPL','Valeur fausse sans justification ; combinaison volontaire.')])
q('Tle spé','Analyse','Existence et unicité',r'Démontrer que $x^3+x-1=0$ possède une unique solution réelle.', 'Une unique racine dans ]0,1[',[
(r'$f(x)=x^3+x-1$ est continue sur $\mathbb R$, avec $f(0)=-1$ et $f(1)=1$. Le TVI donne une racine dans $]0;1[$. Comme $f’(x)=3x^2+1>0$, $f$ est strictement croissante sur $\mathbb R$, donc cette racine est unique.','REF','Existence et unicité distinctes.'),
(r'$f(x)=x^3+x-1$ est continue, $f(0)<0<f(1)$. Par le TVI il y a une unique racine réelle.','EXH LOG','TVI seul ne donne pas unicité.'),
(r'$f’(x)=3x^2+1>0$, donc il existe une unique racine.','EXIST NAME','Croissance stricte donne au plus une racine ; existence et définition f absentes.'),
(r'Le graphe coupe l’axe une seule fois, donc il y a une unique solution.','FIG','Lecture graphique sans contrôle global.')])
q('2de','Géométrie','Milieu et coordonnées',r'$A(1;2)$ et $B(5;6)$. Déterminer les coordonnées du milieu $M$ de $[AB]$ en montrant le calcul.', 'M(3,4)',[
(r'$M\left(\frac{1+5}{2};\frac{2+6}{2}\right)$, donc $M(3;4)$.','REF','Formule lisible.'),
(r'$M=(3;4)$ car $(1+5)/2=3$ et $(2+6)/2=4$.','REGISTER','Identification point-couple conventionnelle, pas erreur intrinsèque.'),
(r'$x=(1+5)/2=3$, puis $x=(2+6)/2=4$. Donc $M(3;4)$.','NAME','Même nom pour abscisse puis ordonnée ; calculs justes.'),
(r'$M(3;4)$.','TASK EXPL','Résultat seul malgré calcul demandé.')])
q('2de','Algèbre','Identité et exemples',r'L’égalité $(x+1)^2=x^2+1$ est-elle vraie pour tout réel $x$ ? Justifier.', 'Non; x=1 contre-exemple',[
(r'Non : pour $x=1$, $(x+1)^2=4$ alors que $x^2+1=2$.','REF','Un contre-exemple suffit à réfuter universelle.'),
(r'Non : $(x+1)^2-(x^2+1)=2x$, non nul lorsque $x\ne0$.','REGISTER','Réfutation algébrique générale.'),
(r'Pour $x=0$, les deux membres valent $1$, donc oui.','EMP GEN','Exemple utilisé pour universalité.'),
(r'Non, car elle n’est vraie pour aucun réel.','QUANT LOG','Réponse non juste mais justification fausse : égalité vraie en zéro.')])
q('1re spé','Analyse','Tangente',r'Pour $f(x)=x^2$, déterminer une équation de la tangente au point d’abscisse $1$. Justifier.', 'y=2x-1',[
(r'$f(1)=1$ et $f’(1)=2$. La tangente a pour équation $y=f’(1)(x-1)+f(1)=2x-1$.','REF','Formule complète.'),
(r'$y=2(x-1)+1=2x-1$.','ECO EXPL','Pente et point implicites.'),
(r'$f’(x)=2x$, donc la tangente est $y=2x$.','LOG','Oublie point d’attache.'),
(r'$f(x)=2x-1$ est l’équation de la tangente.','NAME','Confond fonction originale et fonction affine, intention claire.')])
q('Tle spé','Probabilités','Indépendance',r'$P(A)=0{,}5$, $P(B)=0{,}4$, $P(A\cap B)=0{,}2$. Les événements sont-ils indépendants ? Justifier.', 'Oui',[
(r'$P(A)P(B)=0{,}5\times0{,}4=0{,}2=P(A\cap B)$. Par définition, $A$ et $B$ sont indépendants.','REF','Critère correct.'),
(r'$0{,}5\times0{,}4=0{,}2$, donc oui.','ECO','Correspondance probabilités implicite.'),
(r'Ils sont indépendants, donc $P(A\cap B)=P(A)P(B)=0{,}2$. Donc oui.','CIRC','Organisation circulaire mais égalité vérifiable dans texte ; tester réparation du lecteur.'),
(r'Oui, car $A\cap B=0{,}2$.','TYPE EXPL','Événement identifié à probabilité ; critère absent.')])
q('Sup 1','Analyse','Somme de limites',r'$f(x)\to a$ et $g(x)\to b$ lorsque $x\to0$. Démontrer avec $\varepsilon,\delta$ que $f(x)+g(x)\to a+b$.', 'Limite a+b',[
(r'Soit $\varepsilon>0$. Il existe $\delta_1,\delta_2>0$ tels que $0<|x|<\delta_1$ entraîne $|f(x)-a|<\varepsilon/2$ et $0<|x|<\delta_2$ entraîne $|g(x)-b|<\varepsilon/2$. Posons $\delta=\min(\delta_1,\delta_2)$. Alors $0<|x|<\delta$ entraîne $|f(x)+g(x)-a-b|<\varepsilon$ par inégalité triangulaire.','REF','Dépendances et intersection maîtrisées.'),
(r'Soit $\varepsilon>0$. Pour $f$, choisissons $\delta>0$ donnant $|f(x)-a|<\varepsilon/2$ si $0<|x|<\delta$. Pour $g$, choisissons $\delta>0$ donnant $|g(x)-b|<\varepsilon/2$ si $0<|x|<\delta$. On prend le plus petit des deux $\delta$. La somme des écarts est alors inférieure à $\varepsilon$.','NAME','Collision de nom réparée verbalement par « deux » ; mathématique récupérable.'),
(r'Soit $\varepsilon>0$. Choisissons $\delta>0$ pour que $|f(x)-a|<\varepsilon/2$ dès que $0<|x|<\delta$. Alors aussi $|g(x)-b|<\varepsilon/2$, donc la somme tend vers $a+b$.','QUANT LOG','Même seuil imposé à g sans justification, contrairement à la version avec minimum.'),
(r'Par le théorème de la limite d’une somme, la limite vaut $a+b$.','TASK','Théorème vrai mais précisément à démontrer ici ; ne satisfait pas méthode demandée.')])
q('Sup 1','Analyse','Dépendance du seuil',r'Démontrer par définition que $1/n\to0$ quand $n\to+\infty$.', 'Limite 0',[
(r'Soit $\varepsilon>0$. Choisissons un entier $N>1/\varepsilon$. Pour tout $n\ge N$, $|1/n|\le1/N<\varepsilon$.','REF','N dépend de epsilon, non de n.'),
(r'Choisissons $N=1000$. Pour tout $\varepsilon>0$ et tout $n\ge N$, $1/n<\varepsilon$. Donc la limite est $0$.','QUANT LOG','Seuil uniforme impossible.'),
(r'Pour tout $n\ge1$, choisissons $\varepsilon=2/n$. Alors $1/n<\varepsilon$, donc la limite vaut $0$.','QUANT','Ordre des quantificateurs inversé.'),
(r'$1/n$ devient aussi petit que l’on veut quand $n$ devient assez grand. Donc la limite est $0$.','EXPL TASK','Paraphrase intuitive sans construction du seuil demandé.')])
q('Sup 1','Logique','Témoins existentiels',r'Pour $a,b\in\mathbb Z$, on suppose $3\mid a$ et $3\mid b$. Démontrer $3\mid(a+b)$.', '3 divise a+b',[
(r'Il existe $p,q\in\mathbb Z$ tels que $a=3p$ et $b=3q$. Alors $a+b=3(p+q)$, avec $p+q\in\mathbb Z$.','REF','Témoins indépendants.'),
(r'Il existe $k\in\mathbb Z$ tel que $a=3k$ et $b=3k$. Alors $a+b=6k$ est divisible par $3$.','NAME QUANT','Témoin commun non justifié, exclut a différent b.'),
(r'$a=3k$ pour un entier $k$. Puis $b=3k$ pour un autre entier $k$. La somme vaut trois fois la somme des deux entiers, donc est divisible par $3$.','NAME','« autre » maintient indépendance sémantique malgré collision notationnelle.'),
(r'$a+b=3(a/3+b/3)$, donc $3\mid(a+b)$.','DOMAIN EXPL','Intégralité du quotient récupérable dans les hypothèses.')])
q('Sup 1','Analyse','Variables muettes',r'Calculer $\int_0^1 t\,dt+\int_0^1 x\,dx$ en justifiant.', '1',[
(r'Chaque intégrale vaut $1/2$, donc la somme vaut $1$.','REF','Variables liées distinctes sans effet.'),
(r'$\int_0^1 t\,dt+\int_0^1 x\,dx=\int_0^1 x\,dx+\int_0^1 x\,dx=2[x^2/2]_0^1=1$.','SCOPE','Renommage légal ; témoin négatif pour une interdiction absolue de réemploi.'),
(r'Les lettres sont différentes, on ne peut pas additionner ces intégrales.','SCOPE LOG','Méconnaît liaison et nature scalaire.'),
(r'$\int_0^1 t\,dt+\int_0^1 x\,dx=t^2/2+x^2/2=1$.','EQ SCOPE','Variables libres apparaissent après intégration définie.')])
q('Sup 1','Algèbre','Inclusion d’ensembles',r'Démontrer $A\cap(B\cup C)=(A\cap B)\cup(A\cap C)$.', 'Égalité',[
(r'Pour tout $x$, $x\in A\cap(B\cup C)\iff(x\in A)\land(x\in B\lor x\in C)\iff(x\in A\cap B)\lor(x\in A\cap C)$. Les deux ensembles ont donc les mêmes éléments.','REF','Équivalences complètes.'),
(r'Si $x\in A\cap(B\cup C)$, alors $x\in A$ et $x\in B$ ou $x\in C$. Ainsi $x\in(A\cap B)\cup(A\cap C)$. Donc les ensembles sont égaux.','EXH','Une seule inclusion démontrée.'),
(r'Par distributivité de l’intersection sur l’union, les deux ensembles sont égaux.','CIRC TASK','Propriété à démontrer invoquée sous son nom, sauf acquis préalable différent.'),
(r'Un élément du premier est dans $A$ et dans au moins l’un de $B,C$ ; c’est exactement être dans au moins l’un de $A\cap B,A\cap C$.','REGISTER','Preuve verbale bidirectionnelle valable.')])
q('Sup 1','Algèbre','Injectivité',r'Démontrer que $f:\mathbb R\to\mathbb R$, $f(x)=3x+1$, est injective.', 'Injective',[
(r'Soient $a,b\in\mathbb R$ tels que $f(a)=f(b)$. Alors $3a+1=3b+1$, donc $a=b$. Ainsi $f$ est injective.','REF','Sens correct.'),
(r'Si $a=b$, alors $3a+1=3b+1$, donc $f(a)=f(b)$. Donc $f$ est injective.','REC','Prouve seulement préservation égalité.'),
(r'$f(a)=f(b)\iff3a+1=3b+1\iff a=b$. Donc $f$ est injective.','DOMAIN ECO','Variables implicitement universelles dans R ; valide.'),
(r'$f$ est strictement croissante sur $\mathbb R$, car son coefficient directeur est positif ; elle est donc injective.','REGISTER','Alternative valide sans reprendre définition.')])
q('Sup 1','Analyse','Supremum',r'Déterminer et justifier $\sup\{1-1/n:n\ge1\}$.', '1',[
(r'Tous les termes sont inférieurs à $1$. Si $b<1$, choisir $n>1/(1-b)$ donne $1-1/n>b$ ; $b$ n’est pas majorant. Donc le supremum est $1$.','REF','Majorant et minimalité.'),
(r'Pour tout $n\ge1$, $1-1/n<1$. Donc le supremum est $1$.','EXH','Majorant seul ne suffit pas.'),
(r'La suite tend vers $1$, donc le supremum est $1$.','LOG EXPL','Convergence seule insuffisante ; contrôle de tous les termes absent.'),
(r'Le maximum est $1$, puisque les termes s’en approchent sans l’atteindre.','TYPE LOG','Confond supremum et maximum ; contradiction interne.')])
q('Sup 1','Analyse','Continuité et dérivabilité',r'L’affirmation « toute fonction continue sur $\mathbb R$ est dérivable sur $\mathbb R$ » est-elle vraie ? Justifier.', 'Non: valeur absolue',[
(r'Non. $f(x)=|x|$ est continue sur $\mathbb R$. En $0$, son taux d’accroissement vaut $1$ à droite et $-1$ à gauche ; elle n’y est pas dérivable.','REF','Contre-exemple contrôlé.'),
(r'Non : $f(x)=|x|$.','EXPL','Exemple canonique sans vérification.'),
(r'Oui, car une fonction dérivable est continue.','REC','Réciproque abusive.'),
(r'Non : $f(x)=1/x$ n’est pas dérivable en $0$.','DOMAIN','Contre-exemple hors hypothèses : non définie continue sur R.')])
q('Sup 1','Algèbre','Produit de matrices',r'Pour des matrices carrées réelles $A,B$ de même taille, développer $(A+B)^2$ sans supposer $AB=BA$.', 'A²+AB+BA+B²',[
(r'$(A+B)^2=(A+B)(A+B)=A^2+AB+BA+B^2$.','REF','Ordre conservé.'),
(r'$(A+B)^2=A^2+2AB+B^2$.','MODEL','Transfère identité commutative indûment.'),
(r'$(A+B)^2=A^2+AB+BA+B^2=A^2+2AB+B^2$.','LOG','Début valide puis simplification fausse.'),
(r'On pose $C=AB$ et $C=BA$. Alors $(A+B)^2=A^2+C+C+B^2$.','NAME LOG','Collision impose égalité injustifiée ; résultat non valide généralement.')])
q('Sup 1','Arithmétique','Parité par contraposition',r'Pour $n\in\mathbb Z$, démontrer que si $n^2$ est pair, alors $n$ est pair.', 'Implication vraie',[
(r'Par contraposition, si $n$ est impair, $n=2k+1$ avec $k\in\mathbb Z$, et $n^2=2(2k^2+2k)+1$ est impair. Donc si $n^2$ est pair, $n$ est pair.','REF','Contraposée complète.'),
(r'Si $n$ est pair, $n=2k$, donc $n^2=4k^2$ est pair. Cela prouve le résultat.','REC','Réciproque à la place de proposition cible.'),
(r'$n^2$ pair signifie $n^2=2k$. Donc $n=2\sqrt{k/2}$ et $n$ est pair.','DOMAIN','Coefficient pas nécessairement entier.'),
(r'Un impair au carré est impair, donc un entier dont le carré est pair est pair.','ECO EXPL','Lemme implicite standard ; raisonnement correct.')])
q('Sup 1','Analyse','Suite bornée',r'La suite $u_n=(-1)^n$ converge-t-elle ? Justifier.', 'Non',[
(r'$u_{2n}=1$ et $u_{2n+1}=-1$. Ces deux sous-suites ont des limites différentes ; la suite ne converge pas.','REF','Critère correct.'),
(r'Elle est bornée entre $-1$ et $1$, donc elle converge.','LOG','Bornée ne suffit pas.'),
(r'Elle alterne indéfiniment entre deux valeurs distinctes, $1$ et $-1$, donc elle ne peut s’approcher d’une seule limite.','REGISTER','Explication intuitive générale valide, formalisation moindre.'),
(r'$u_n=1$ puis $u_n=-1$, donc $1=-1$, impossible : la suite diverge.','NAME EQ','Variation de n tue par égalité de valeurs ; argument incorrect, conclusion vraie.')])
q('Sup 1','Analyse','Unicité de limite',r'Démontrer qu’une suite réelle convergente possède une seule limite.', 'Unicité',[
(r'Supposons $u_n\to a$ et $u_n\to b$ avec $a\ne b$. Posons $\varepsilon=|a-b|/3$. Pour $n$ assez grand, $|u_n-a|<\varepsilon$ et $|u_n-b|<\varepsilon$. Alors $|a-b|\le|a-u_n|+|u_n-b|<2|a-b|/3$, contradiction.','REF','Absurdum et seuil commun implicite standard.'),
(r'Si $u_n\to a$ et $u_n\to b$, alors $a=\lim u_n=b$.','CIRC','Notation de limite unique présuppose ce qui est à établir.'),
(r'À partir d’un certain rang, $u_n=a$ et $u_n=b$. Donc $a=b$.','LIMIT LOG','Convergence confondue avec stationnarité.'),
(r'Si deux limites étaient distinctes, des voisinages disjoints autour d’elles devraient tous deux contenir tous les termes à partir d’un certain rang. C’est impossible.','REGISTER','Argument topologique verbal complet, au niveau indiqué.')])

# Three deliberately non-reference sets: different flaws, no prescribed best copy.
questions[7]['productions'][0].update(content=r'$\frac12+\frac13=\frac36+\frac26=\frac55=1$.')
questions[7]['productions'][0]['research']=dict(targets=['CALC'],analysis='Mise au même dénominateur correcte puis erreur finale. Les quatre réponses à Q08 manquent une exigence distincte : calcul exact, exactitude, règle opératoire ou calcul demandé.')
questions[22]['productions'][0].update(content=r'$f(x)=x^3+x-1$. La calculatrice donne $f(0{,}682)\approx-0{,}001$ et $f(0{,}683)\approx0{,}002$. Il y a donc une unique solution réelle.')
questions[22]['productions'][0]['research']=dict(targets=['EMP','EXIST','EXH'],analysis='Valeurs approchées sans contrôle d’erreur ni continuité explicitée ; unicité non établie. Dans Q23 aucune rédaction ne justifie complètement existence et unicité : cette insuffisance est volontairement commune, ses raisons diffèrent.')
questions[36]['productions'][0].update(content=r'$(A+B)^2=(A+B)(A+B)=A^2+AB+B^2$.')
questions[36]['productions'][0]['research']=dict(targets=['CALC'],analysis='Omission du terme BA lors de la distribution. Aucune rédaction de Q37 ne fournit une identité généralement valide ; les autres variantes commutent abusivement ou identifient AB et BA.')

assert len(questions)==40,len(questions)
(ROOT/'research/bank.json').write_text(json.dumps({'version':'0.1.0','status':'Banque exploratoire à prétester','questions':questions},ensure_ascii=False,indent=2)+'\n')
public={'version':'0.1.0','questions':[{k:v for k,v in q.items() if k not in ('referenceAnswer','family','productions')}|{'family':q['family'],'productions':[{'id':p['id'],'content':p['content']} for p in q['productions']]} for q in questions]}
for folder in ['docs/data','www/data']:
 (ROOT/folder).mkdir(exist_ok=True)
 (ROOT/folder/'bank.json').write_text(json.dumps(public,ensure_ascii=False,indent=2)+'\n')
print(len(questions),'questions;',sum(len(q['productions']) for q in questions),'rédactions')
lines=['# Notices de conception — 40 questions, 160 rédactions','','Les codes sont définis dans `codebook.json`. Une cible ne signifie pas nécessairement une violation : lire l’analyse. Les codes non cités ne sont pas réputés automatiquement respectés.','']
for item in questions:
 lines += [f"## {item['id']} — {item['level']} — {item['title']}",'',item['statement'],'',f"Réponse de référence : {item['referenceAnswer']}",'',f"Famille de comparaison : {item['family']}",'']
 for prod in item['productions']:
  lines += [f"### {prod['id']}",'',prod['content'],'','Cibles / contrôles : '+', '.join(prod['research']['targets'])+'.','',prod['research']['analysis'],'']
(ROOT/'research/notices.md').write_text('\n'.join(lines))
