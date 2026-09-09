# Notices de conception — 40 questions, 160 rédactions

Les codes sont définis dans `codebook.json`. Une cible ne signifie pas nécessairement une violation : lire l’analyse. Les codes non cités ne sont pas réputés automatiquement respectés.

## Q01 — 3e — Longueur et justification

Le triangle $ABC$ est rectangle en $B$, avec $AC=5$ cm et $BC=3$ cm. Calculer $AB$ en justifiant.

Réponse de référence : 4 cm

Famille de comparaison : pythagore

### Q01-1

$AC^2=AB^2+BC^2$ par le théorème de Pythagore. Donc $AB^2=25-9=16$. Comme $AB$ est une longueur, $AB=4$ cm.

Cibles / contrôles : REF.

Déduction, signe et unité explicités.

### Q01-2

$AB=\sqrt{5^2-3^2}=4$ cm.

Cibles / contrôles : ECO.

Théorème et positivité implicites ; calcul valide, pas de lacune logique nécessaire.

### Q01-3

$AB^2=25-9=16$, donc $AB=4$.

Cibles / contrôles : UNIT, EXPL.

Résultat numérique juste ; unité absente, théorème non nommé.

### Q01-4

$AB=4$ cm car $4^2+3^2=5^2$.

Cibles / contrôles : REC, EXPL.

Vérifie une valeur candidate sans expliciter son unicité ; celle-ci est récupérable par positivité, à distinguer d’une erreur de réciproque.

## Q02 — 4e — Réciproque ou théorème

$AB=3$, $BC=4$, $AC=5$ (en cm). Démontrer que $ABC$ est rectangle.

Réponse de référence : Rectangle en B

Famille de comparaison : Q02

### Q02-1

$AC^2=25=9+16=AB^2+BC^2$. Par la réciproque du théorème de Pythagore, le triangle est rectangle en $B$.

Cibles / contrôles : REF.

Référence attendue.

### Q02-2

$3^2+4^2=5^2$. Donc le triangle est rectangle en $B$, par le théorème de Pythagore.

Cibles / contrôles : NAME.

Nom imprécis ; implication utilisée correcte si le théorème désigne la caractérisation.

### Q02-3

Le triangle est rectangle en $B$, donc $AB^2+BC^2=AC^2$. Les valeurs donnent bien $9+16=25$. Il est donc rectangle.

Cibles / contrôles : CIRC.

Conclusion posée comme prémisse ; vérification numérique présente mais son rôle justificatif est à réorganiser.

### Q02-4

$9+16=25$. Rectangle.

Cibles / contrôles : ECO, EXPL.

Argument récupérable mais sommet et correspondance des longueurs non explicités.

## Q03 — 5e — Angles d’un triangle

Dans un triangle $ABC$, $\widehat A=50^\circ$ et $\widehat B=60^\circ$. Calculer $\widehat C$ en justifiant.

Réponse de référence : 70°

Famille de comparaison : Q03

### Q03-1

La somme des angles d’un triangle vaut $180^\circ$. Ainsi $\widehat C=180^\circ-50^\circ-60^\circ=70^\circ$.

Cibles / contrôles : REF.

Règle et calcul corrects.

### Q03-2

$180-50-60=70$. Donc $\widehat C=70^\circ$.

Cibles / contrôles : EXPL.

Somme des angles implicite ; unités intermédiaires omises mais conclusion précise.

### Q03-3

$\widehat C=180-50=130-60=70^\circ$.

Cibles / contrôles : EQ.

Chaîne d’égalités fausse ; calcul séquentiel compréhensible.

### Q03-4

La somme des angles vaut $180^\circ$. Donc $\widehat C=180^\circ-50^\circ-60^\circ=80^\circ$.

Cibles / contrôles : CALC.

Erreur arithmétique isolée, raisonnement correct.

## Q04 — 3e — Équation linéaire

Résoudre $3x+2=14$ dans $\mathbb R$ et justifier.

Réponse de référence : S={4}

Famille de comparaison : lineaire

### Q04-1

$3x+2=14\iff3x=12\iff x=4$. L’ensemble des solutions est $\{4\}$.

Cibles / contrôles : REF.

Équivalences valides.

### Q04-2

Pour $x=4$, $3\times4+2=14$. Donc la solution est $4$.

Cibles / contrôles : EXH.

Existence vérifiée ; absence d’autre solution non justifiée dans le texte.

### Q04-3

$3x+2=14$, donc $3x=12$, donc $x=4$.

Cibles / contrôles : REC.

Opérations réversibles ; ne pas coder les « donc » comme erreur logique automatique.

### Q04-4

$3x=14-2=12/3=4$. Donc $x=4$.

Cibles / contrôles : EQ.

Résultat correct, égalités intermédiaires fausses.

## Q05 — 3e — Produit nul

Résoudre $(x-2)(x+3)=0$ dans $\mathbb R$. Justifier.

Réponse de référence : S={-3,2}

Famille de comparaison : produit

### Q05-1

Un produit est nul si et seulement si l’un des facteurs est nul. Ainsi $x=2$ ou $x=-3$ ; $S=\{-3;2\}$.

Cibles / contrôles : REF.

Exhaustif et suffisant.

### Q05-2

Il faut $x-2=0$ et $x+3=0$. Donc $S=\{-3;2\}$.

Cibles / contrôles : CONNECT.

« et » incompatible avec l’ensemble final ; intention récupérable.

### Q05-3

Pour $x=2$ ou $x=-3$, le produit vaut zéro. Ce sont les solutions.

Cibles / contrôles : EXH.

Seulement vérification des deux valeurs.

### Q05-4

$(x-2)(x+3)=0\iff x=2$.

Cibles / contrôles : EXH, LOG.

Perd la racine -3, résultat incomplet.

## Q06 — 4e — Somme de deux impairs

Démontrer que la somme de deux entiers impairs est paire.

Réponse de référence : Somme paire

Famille de comparaison : Q06

### Q06-1

Écrivons les deux entiers $2p+1$ et $2q+1$, avec $p,q\in\mathbb Z$. Leur somme vaut $2(p+q+1)$ et $p+q+1$ est entier : elle est paire.

Cibles / contrôles : REF.

Témoins indépendants.

### Q06-2

Les deux nombres sont $2n+1$ et $2n+1$. Leur somme vaut $4n+2=2(2n+1)$, donc elle est paire.

Cibles / contrôles : NAME, GEN.

Même lettre impose deux nombres égaux ; perte de généralité effective.

### Q06-3

$3+5=8$, $7+9=16$ et $11+13=24$. La somme de deux impairs est donc toujours paire.

Cibles / contrôles : EMP, GEN.

Exemples seulement.

### Q06-4

Chaque nombre impair laisse une unité après regroupement par paires. Les deux unités restantes forment une paire supplémentaire ; la somme se regroupe donc entièrement par paires.

Cibles / contrôles : REGISTER.

Argument générique verbal valide ; pas de symbolisme nécessaire.

## Q07 — 3e — Divisibilité

Démontrer que le produit de deux entiers pairs est divisible par $4$.

Réponse de référence : Divisible par 4

Famille de comparaison : Q07

### Q07-1

Soient $a=2p$ et $b=2q$ avec $p,q\in\mathbb Z$. Alors $ab=4pq$ ; comme $pq$ est entier, $4$ divise $ab$.

Cibles / contrôles : REF.

Définitions explicitées.

### Q07-2

$a=2p$, $b=2q$, donc $ab=4pq$, d’où le résultat.

Cibles / contrôles : DOMAIN, EXPL.

Domaines des paramètres implicites dans le contexte.

### Q07-3

Les deux nombres sont pairs, donc leur produit est pair, donc divisible par $4$.

Cibles / contrôles : LOG.

Dernière implication fausse, conclusion vraie.

### Q07-4

$2\times4=8$ et $6\times8=48$. C’est donc vrai.

Cibles / contrôles : EMP, GEN.

Exemples insuffisants.

## Q08 — 5e — Fractions

Calculer exactement $\frac12+\frac13$, en montrant le calcul.

Réponse de référence : 5/6

Famille de comparaison : Q08

### Q08-1

$\frac12+\frac13=\frac36+\frac26=\frac55=1$.

Cibles / contrôles : CALC.

Mise au même dénominateur correcte puis erreur finale. Les quatre réponses à Q08 manquent une exigence distincte : calcul exact, exactitude, règle opératoire ou calcul demandé.

### Q08-2

$\frac12+\frac13=0{,}833$.

Cibles / contrôles : APPROX.

Confond approximation et égalité ; ne répond pas exactement.

### Q08-3

$\frac12+\frac13=\frac{1+1}{2+3}=\frac25$.

Cibles / contrôles : CALC.

Règle de calcul erronée.

### Q08-4

$\frac56$.

Cibles / contrôles : TASK, EXPL.

Résultat exact sans le calcul demandé.

## Q09 — 4e — Parallélisme

Les droites $d$ et $e$ sont perpendiculaires à une même droite $f$ dans le plan. Démontrer que $d$ et $e$ sont parallèles.

Réponse de référence : d parallèle e

Famille de comparaison : Q09

### Q09-1

Deux droites d’un plan perpendiculaires à une même droite sont parallèles. Comme $d\perp f$ et $e\perp f$, on a $d\parallel e$.

Cibles / contrôles : REF.

Théorème utilisé avec ses hypothèses.

### Q09-2

$d\perp f$ et $e\perp f$, donc $d\parallel e$.

Cibles / contrôles : ECO.

Théorème implicite, déduction valide.

### Q09-3

Sur mon dessin, $d$ et $e$ ne se coupent pas. Elles sont donc parallèles.

Cibles / contrôles : FIG.

Absence d’intersection visible non probante.

### Q09-4

$d\parallel e$, donc elles font le même angle avec $f$. Elles sont donc parallèles.

Cibles / contrôles : CIRC.

Conclusion réutilisée comme donnée.

## Q10 — 3e — Moyenne pondérée

Une série comprend deux valeurs égales à $10$ et trois valeurs égales à $20$. Calculer sa moyenne et justifier.

Réponse de référence : 16

Famille de comparaison : Q10

### Q10-1

La somme est $2\times10+3\times20=80$ pour $5$ valeurs. La moyenne est $80/5=16$.

Cibles / contrôles : REF.

Pondération et effectif corrects.

### Q10-2

$\frac{2\times10+3\times20}{5}=16$.

Cibles / contrôles : ECO.

Calcul autoportant.

### Q10-3

La moyenne est $(10+20)/2=15$.

Cibles / contrôles : MODEL.

Ignore les effectifs.

### Q10-4

$2\times10=20+3\times20=80/5=16$.

Cibles / contrôles : EQ.

Enchaînement de calculs écrit comme égalités fausses.

## Q11 — 3e — Dé équilibré

On lance un dé équilibré à six faces numérotées de $1$ à $6$. Calculer la probabilité d’obtenir un nombre pair et justifier.

Réponse de référence : 1/2

Famille de comparaison : Q11

### Q11-1

Les six issues sont équiprobables et trois sont paires : $2,4,6$. La probabilité est $3/6=1/2$.

Cibles / contrôles : REF.

Modèle et dénombrement explicites.

### Q11-2

$P(\text{pair})=3/6=1/2$.

Cibles / contrôles : EXPL.

Équiprobabilité donnée mais favorable non détaillé.

### Q11-3

Il y a deux possibilités : pair ou impair. Donc la probabilité est $1/2$.

Cibles / contrôles : MODEL.

Deux catégories ne suffisent pas à établir équiprobabilité ; conclusion juste ici.

### Q11-4

J’ai obtenu $50$ nombres pairs en $100$ lancers, donc la probabilité exacte est $1/2$.

Cibles / contrôles : EMP, APPROX.

Fréquence ne prouve pas probabilité exacte.

## Q12 — 3e — Carré et signe

Résoudre $x^2=9$ dans $\mathbb R$ en justifiant.

Réponse de référence : S={-3,3}

Famille de comparaison : carre

### Q12-1

$x^2=9\iff(x-3)(x+3)=0\iff x=3\text{ ou }x=-3$.

Cibles / contrôles : REF.

Deux racines.

### Q12-2

$x^2=9\iff x=\sqrt9=3$.

Cibles / contrôles : EXH, LOG.

Oublie la racine négative.

### Q12-3

$3^2=9$ et $(-3)^2=9$, donc $S=\{-3;3\}$.

Cibles / contrôles : EXH.

Vérifie sans argument explicite d’exhaustivité.

### Q12-4

$x=\pm3$.

Cibles / contrôles : ECO, TASK.

Résultat juste ; justification demandée absente.

## Q13 — 2de — Équation linéaire

Résoudre $3x+2=14$ dans $\mathbb R$ et justifier.

Réponse de référence : S={4}

Famille de comparaison : lineaire

### Q13-1

$3x+2=14\iff3x=12\iff x=4$. L’ensemble des solutions est $\{4\}$.

Cibles / contrôles : REF.

Même stimulus que Q04, niveau seul modifié.

### Q13-2

Pour $x=4$, $3\times4+2=14$. Donc la solution est $4$.

Cibles / contrôles : EXH.

Existence sans unicité explicite.

### Q13-3

$3x+2=14$, donc $3x=12$, donc $x=4$.

Cibles / contrôles : REC.

Calculs réversibles malgré l’absence de symbole équivalence.

### Q13-4

$3x=14-2=12/3=4$. Donc $x=4$.

Cibles / contrôles : EQ.

Résultat correct avec égalités fausses.

## Q14 — 2de — Division par l’inconnue

Résoudre $x^2=2x$ dans $\mathbb R$.

Réponse de référence : S={0,2}

Famille de comparaison : Q14

### Q14-1

$x^2=2x\iff x(x-2)=0\iff x=0\text{ ou }x=2$.

Cibles / contrôles : REF.

Factorisation exhaustive.

### Q14-2

On divise par $x$ : $x=2$. Donc $S=\{2\}$.

Cibles / contrôles : DOMAIN, EXH.

Division exclut zéro sans traitement.

### Q14-3

Si $x=0$, l’égalité est vraie. Sinon on divise par $x$ et on obtient $x=2$, qui convient aussi. Donc $S=\{0;2\}$.

Cibles / contrôles : REGISTER.

Disjonction de cas valide.

### Q14-4

En divisant par $x$, on obtient $x=2$. On ajoute aussi $0$, qui convient. Donc $S=\{0;2\}$.

Cibles / contrôles : DOMAIN, EXPL.

Répare le cas perdu ; domaine de la division reste implicite, ensemble final exact.

## Q15 — 2de — Racine carrée

Résoudre $\sqrt{x+2}=x$ dans $\mathbb R$.

Réponse de référence : S={2}

Famille de comparaison : Q15

### Q15-1

Il faut $x\ge0$. Sur ce domaine, $\sqrt{x+2}=x\iff x+2=x^2\iff(x-2)(x+1)=0$. Seul $2$ est positif ou nul, donc $S=\{2\}$.

Cibles / contrôles : REF.

Signe et équivalence contrôlés.

### Q15-2

$\sqrt{x+2}=x\iff x+2=x^2\iff x=2\text{ ou }x=-1$. Donc $S=\{-1;2\}$.

Cibles / contrôles : REC, DOMAIN.

Élévation au carré non réversible sans signe.

### Q15-3

Une solution vérifie $x^2-x-2=0$, donc vaut $2$ ou $-1$. Vérification : $2$ convient ; $-1$ ne convient pas. Donc $S=\{2\}$.

Cibles / contrôles : REGISTER.

Analyse puis synthèse complète sans domaine initial explicite.

### Q15-4

$x+2=x^2$, donc $x=2$. Vérification : $\sqrt4=2$.

Cibles / contrôles : EXH, EXPL.

Racine -1 omise dans analyse ; résultat exact, justification d’exclusion absente.

## Q16 — 2de — Quotient simplifié

Simplifier $f(x)=\frac{x^2-1}{x-1}$ et préciser son domaine de définition.

Réponse de référence : D=R\{1}; f(x)=x+1 sur D

Famille de comparaison : Q16

### Q16-1

$D_f=\mathbb R\setminus\{1\}$. Pour $x\ne1$, $f(x)=\frac{(x-1)(x+1)}{x-1}=x+1$.

Cibles / contrôles : REF.

Restriction conservée.

### Q16-2

$f(x)=x+1$, donc $D_f=\mathbb R$.

Cibles / contrôles : DOMAIN.

Simplification étend à tort le domaine.

### Q16-3

$f(x)=x+1$ pour $x\ne1$.

Cibles / contrôles : ECO.

Domaine exprimé sans notation ensembliste ; réponse suffisante.

### Q16-4

$f(x)=x+1$.

Cibles / contrôles : DOMAIN, TASK.

Domaine demandé non indiqué.

## Q17 — 2de — Inéquation

Résoudre $-2x<6$ dans $\mathbb R$. Justifier.

Réponse de référence : ]-3,+inf[

Famille de comparaison : Q17

### Q17-1

En divisant par $-2$, on inverse le sens de l’inégalité : $x>-3$. Donc $S=]-3;+\infty[$.

Cibles / contrôles : REF.

Règle explicitée.

### Q17-2

$-2x<6\iff x>-3$.

Cibles / contrôles : ECO.

Transformation correcte, règle implicite.

### Q17-3

$-2x<6\iff x<-3$.

Cibles / contrôles : LOG.

Sens non inversé.

### Q17-4

$x>-3$, donc $S=[-3;+\infty[$.

Cibles / contrôles : BOUND.

Conclusion inclut indûment la borne.

## Q18 — 1re spé — Second degré

Résoudre $3x^2-5x+2=0$ dans $\mathbb R$ en justifiant.

Réponse de référence : S={2/3,1}

Famille de comparaison : Q18

### Q18-1

$3x^2-5x+2=(3x-2)(x-1)$. Le produit est nul exactement pour $x=2/3$ ou $x=1$. Donc $S=\{2/3;1\}$.

Cibles / contrôles : REF.

Factorisation complète.

### Q18-2

$\Delta=25-24=1$. Donc $x=\frac{5\pm1}{6}$, soit $S=\{2/3;1\}$.

Cibles / contrôles : ECO.

Formule de résolution implicite, valide.

### Q18-3

$\Delta=1$. Puis $\Delta=\frac{5-1}{6}=2/3$ et l’autre solution est $1$. Donc $S=\{2/3;1\}$.

Cibles / contrôles : NAME.

Réaffecte delta au résultat ; récupération séquentielle possible, pas simultanée.

### Q18-4

$\Delta=25-24=1$. Donc $x=\frac{5\pm1}{3}$, soit $S=\{4/3;2\}$.

Cibles / contrôles : CALC.

Dénominateur erroné.

## Q19 — 1re spé — Dérivée et variation

Pour $f(x)=x^2-4x+1$ sur $\mathbb R$, déterminer les intervalles de monotonie en justifiant.

Réponse de référence : Décroît jusqu’à 2, croît après

Famille de comparaison : Q19

### Q19-1

$f’(x)=2x-4$, négatif pour $x<2$ et positif pour $x>2$. Donc $f$ décroît sur $]-\infty;2]$ et croît sur $[2;+\infty[$.

Cibles / contrôles : REF.

Lien signe-variation correct.

### Q19-2

$2x-4<0\iff x<2$ ; $2x-4>0\iff x>2$. Décroissante puis croissante, changement en $2$.

Cibles / contrôles : NAME, EXPL.

Dérivée non identifiée, intervalles récupérables.

### Q19-3

$f’(x)=2x-4$ est croissante ; donc $f$ est croissante sur $\mathbb R$.

Cibles / contrôles : LOG.

Confond variation de dérivée et signe.

### Q19-4

$f(0)=1$, $f(2)=-3$, $f(4)=1$. Donc $f$ décroît jusqu’à $2$, puis croît.

Cibles / contrôles : EMP.

Trois valeurs ne prouvent pas les variations.

## Q20 — 1re spé — Probabilité conditionnelle

$P(A)=0{,}4$, $P(B\mid A)=0{,}5$ et $P(B\mid\overline A)=0{,}2$. Calculer $P(B)$ et justifier.

Réponse de référence : 0.32

Famille de comparaison : Q20

### Q20-1

$A$ et $\overline A$ forment une partition. Donc $P(B)=0{,}4\times0{,}5+0{,}6\times0{,}2=0{,}32$.

Cibles / contrôles : REF.

Probabilités totales.

### Q20-2

$P(B)=0{,}4\times0{,}5+0{,}6\times0{,}2=0{,}32$.

Cibles / contrôles : ECO.

Partition implicite.

### Q20-3

$P(B)=(0{,}5+0{,}2)/2=0{,}35$.

Cibles / contrôles : MODEL.

Moyenne non pondérée.

### Q20-4

$P(A\mid B)=0{,}4\times0{,}5+0{,}6\times0{,}2=0{,}32$. Donc $P(B)=0{,}32$.

Cibles / contrôles : NAME.

Calcul correct attaché à un objet différent ; assertion conditionnelle fausse.

## Q21 — Tle spé — Limite d’un quotient

Calculer $\lim_{x\to+\infty}\frac{2x+1}{x+3}$ en justifiant.

Réponse de référence : 2

Famille de comparaison : Q21

### Q21-1

Pour $x>0$, $\frac{2x+1}{x+3}=\frac{2+1/x}{1+3/x}$. Le numérateur tend vers $2$ et le dénominateur vers $1\ne0$. La limite vaut $2$.

Cibles / contrôles : REF.

Règles de limite justifiées.

### Q21-2

$\frac{2x+1}{x+3}\sim\frac{2x}{x}=2$, donc la limite vaut $2$.

Cibles / contrôles : REGISTER.

Équivalent valide ; disponibilité de ce registre à préciser au prétest.

### Q21-3

$\frac{2x+1}{x+3}=\frac{2x}{x}=2$ à l’infini.

Cibles / contrôles : EQ, LIMIT.

Égalités fausses pour x fini ; langage de passage à limite non formalisé.

### Q21-4

Le quotient est de la forme $\infty/\infty$, donc sa limite est $1$.

Cibles / contrôles : LOG.

Forme indéterminée traitée comme opération.

## Q22 — Tle spé — Récurrence

$u_0=1$ et $u_{n+1}=2u_n$ pour $n\in\mathbb N$. Démontrer que $u_n=2^n$ pour tout $n\in\mathbb N$.

Réponse de référence : u_n=2^n

Famille de comparaison : Q22

### Q22-1

$u_0=1=2^0$. Si $u_n=2^n$ pour un entier $n\ge0$, alors $u_{n+1}=2\times2^n=2^{n+1}$. La propriété est donc vraie pour tout $n\ge0$ par récurrence.

Cibles / contrôles : REF.

Initialisation et hérédité.

### Q22-2

Si $u_n=2^n$, alors $u_{n+1}=2u_n=2^{n+1}$. Donc la propriété est vraie pour tout $n$.

Cibles / contrôles : INIT.

Initialisation absente, non fausse.

### Q22-3

On suppose que pour tout $n$, $u_n=2^n$. Alors $u_{n+1}=2^{n+1}$. La propriété est démontrée.

Cibles / contrôles : QUANT, CIRC, INIT.

Hypothèse globale contient conclusion ; base omise.

### Q22-4

$u_0=1$, $u_1=2$, $u_2=4$, $u_3=8$. On voit que $u_n=2^n$.

Cibles / contrôles : EMP, GEN.

Échantillon seulement.

## Q23 — Tle spé — Intégrale

Calculer $\int_0^1 2x\,dx$ en justifiant.

Réponse de référence : 1

Famille de comparaison : Q23

### Q23-1

$f(x)=x^3+x-1$. La calculatrice donne $f(0{,}682)\approx-0{,}001$ et $f(0{,}683)\approx0{,}002$. Il y a donc une unique solution réelle.

Cibles / contrôles : EMP, EXIST, EXH.

Valeurs approchées sans contrôle d’erreur ni continuité explicitée ; unicité non établie. Dans Q23 aucune rédaction ne justifie complètement existence et unicité : cette insuffisance est volontairement commune, ses raisons diffèrent.

### Q23-2

$\int_0^1 2x\,dx=[x^2]_0^1=1$.

Cibles / contrôles : ECO.

Notation standard suffisante.

### Q23-3

$\int_0^1 2x\,dx=x^2=1$.

Cibles / contrôles : EQ, NAME.

Variable libre assimilée à un nombre ; bornes implicites.

### Q23-4

$\int_0^1 2x\,dx=2$.

Cibles / contrôles : CALC, EXPL.

Valeur fausse sans justification ; combinaison volontaire.

## Q24 — Tle spé — Existence et unicité

Démontrer que $x^3+x-1=0$ possède une unique solution réelle.

Réponse de référence : Une unique racine dans ]0,1[

Famille de comparaison : Q24

### Q24-1

$f(x)=x^3+x-1$ est continue sur $\mathbb R$, avec $f(0)=-1$ et $f(1)=1$. Le TVI donne une racine dans $]0;1[$. Comme $f’(x)=3x^2+1>0$, $f$ est strictement croissante sur $\mathbb R$, donc cette racine est unique.

Cibles / contrôles : REF.

Existence et unicité distinctes.

### Q24-2

$f(x)=x^3+x-1$ est continue, $f(0)<0<f(1)$. Par le TVI il y a une unique racine réelle.

Cibles / contrôles : EXH, LOG.

TVI seul ne donne pas unicité.

### Q24-3

$f’(x)=3x^2+1>0$, donc il existe une unique racine.

Cibles / contrôles : EXIST, NAME.

Croissance stricte donne au plus une racine ; existence et définition f absentes.

### Q24-4

Le graphe coupe l’axe une seule fois, donc il y a une unique solution.

Cibles / contrôles : FIG.

Lecture graphique sans contrôle global.

## Q25 — 2de — Milieu et coordonnées

$A(1;2)$ et $B(5;6)$. Déterminer les coordonnées du milieu $M$ de $[AB]$ en montrant le calcul.

Réponse de référence : M(3,4)

Famille de comparaison : Q25

### Q25-1

$M\left(\frac{1+5}{2};\frac{2+6}{2}\right)$, donc $M(3;4)$.

Cibles / contrôles : REF.

Formule lisible.

### Q25-2

$M=(3;4)$ car $(1+5)/2=3$ et $(2+6)/2=4$.

Cibles / contrôles : REGISTER.

Identification point-couple conventionnelle, pas erreur intrinsèque.

### Q25-3

$x=(1+5)/2=3$, puis $x=(2+6)/2=4$. Donc $M(3;4)$.

Cibles / contrôles : NAME.

Même nom pour abscisse puis ordonnée ; calculs justes.

### Q25-4

$M(3;4)$.

Cibles / contrôles : TASK, EXPL.

Résultat seul malgré calcul demandé.

## Q26 — 2de — Identité et exemples

L’égalité $(x+1)^2=x^2+1$ est-elle vraie pour tout réel $x$ ? Justifier.

Réponse de référence : Non; x=1 contre-exemple

Famille de comparaison : Q26

### Q26-1

Non : pour $x=1$, $(x+1)^2=4$ alors que $x^2+1=2$.

Cibles / contrôles : REF.

Un contre-exemple suffit à réfuter universelle.

### Q26-2

Non : $(x+1)^2-(x^2+1)=2x$, non nul lorsque $x\ne0$.

Cibles / contrôles : REGISTER.

Réfutation algébrique générale.

### Q26-3

Pour $x=0$, les deux membres valent $1$, donc oui.

Cibles / contrôles : EMP, GEN.

Exemple utilisé pour universalité.

### Q26-4

Non, car elle n’est vraie pour aucun réel.

Cibles / contrôles : QUANT, LOG.

Réponse non juste mais justification fausse : égalité vraie en zéro.

## Q27 — 1re spé — Tangente

Pour $f(x)=x^2$, déterminer une équation de la tangente au point d’abscisse $1$. Justifier.

Réponse de référence : y=2x-1

Famille de comparaison : Q27

### Q27-1

$f(1)=1$ et $f’(1)=2$. La tangente a pour équation $y=f’(1)(x-1)+f(1)=2x-1$.

Cibles / contrôles : REF.

Formule complète.

### Q27-2

$y=2(x-1)+1=2x-1$.

Cibles / contrôles : ECO, EXPL.

Pente et point implicites.

### Q27-3

$f’(x)=2x$, donc la tangente est $y=2x$.

Cibles / contrôles : LOG.

Oublie point d’attache.

### Q27-4

$f(x)=2x-1$ est l’équation de la tangente.

Cibles / contrôles : NAME.

Confond fonction originale et fonction affine, intention claire.

## Q28 — Tle spé — Indépendance

$P(A)=0{,}5$, $P(B)=0{,}4$, $P(A\cap B)=0{,}2$. Les événements sont-ils indépendants ? Justifier.

Réponse de référence : Oui

Famille de comparaison : Q28

### Q28-1

$P(A)P(B)=0{,}5\times0{,}4=0{,}2=P(A\cap B)$. Par définition, $A$ et $B$ sont indépendants.

Cibles / contrôles : REF.

Critère correct.

### Q28-2

$0{,}5\times0{,}4=0{,}2$, donc oui.

Cibles / contrôles : ECO.

Correspondance probabilités implicite.

### Q28-3

Ils sont indépendants, donc $P(A\cap B)=P(A)P(B)=0{,}2$. Donc oui.

Cibles / contrôles : CIRC.

Organisation circulaire mais égalité vérifiable dans texte ; tester réparation du lecteur.

### Q28-4

Oui, car $A\cap B=0{,}2$.

Cibles / contrôles : TYPE, EXPL.

Événement identifié à probabilité ; critère absent.

## Q29 — Sup 1 — Somme de limites

$f(x)\to a$ et $g(x)\to b$ lorsque $x\to0$. Démontrer avec $\varepsilon,\delta$ que $f(x)+g(x)\to a+b$.

Réponse de référence : Limite a+b

Famille de comparaison : Q29

### Q29-1

Soit $\varepsilon>0$. Il existe $\delta_1,\delta_2>0$ tels que $0<|x|<\delta_1$ entraîne $|f(x)-a|<\varepsilon/2$ et $0<|x|<\delta_2$ entraîne $|g(x)-b|<\varepsilon/2$. Posons $\delta=\min(\delta_1,\delta_2)$. Alors $0<|x|<\delta$ entraîne $|f(x)+g(x)-a-b|<\varepsilon$ par inégalité triangulaire.

Cibles / contrôles : REF.

Dépendances et intersection maîtrisées.

### Q29-2

Soit $\varepsilon>0$. Pour $f$, choisissons $\delta>0$ donnant $|f(x)-a|<\varepsilon/2$ si $0<|x|<\delta$. Pour $g$, choisissons $\delta>0$ donnant $|g(x)-b|<\varepsilon/2$ si $0<|x|<\delta$. On prend le plus petit des deux $\delta$. La somme des écarts est alors inférieure à $\varepsilon$.

Cibles / contrôles : NAME.

Collision de nom réparée verbalement par « deux » ; mathématique récupérable.

### Q29-3

Soit $\varepsilon>0$. Choisissons $\delta>0$ pour que $|f(x)-a|<\varepsilon/2$ dès que $0<|x|<\delta$. Alors aussi $|g(x)-b|<\varepsilon/2$, donc la somme tend vers $a+b$.

Cibles / contrôles : QUANT, LOG.

Même seuil imposé à g sans justification, contrairement à la version avec minimum.

### Q29-4

Par le théorème de la limite d’une somme, la limite vaut $a+b$.

Cibles / contrôles : TASK.

Théorème vrai mais précisément à démontrer ici ; ne satisfait pas méthode demandée.

## Q30 — Sup 1 — Dépendance du seuil

Démontrer par définition que $1/n\to0$ quand $n\to+\infty$.

Réponse de référence : Limite 0

Famille de comparaison : Q30

### Q30-1

Soit $\varepsilon>0$. Choisissons un entier $N>1/\varepsilon$. Pour tout $n\ge N$, $|1/n|\le1/N<\varepsilon$.

Cibles / contrôles : REF.

N dépend de epsilon, non de n.

### Q30-2

Choisissons $N=1000$. Pour tout $\varepsilon>0$ et tout $n\ge N$, $1/n<\varepsilon$. Donc la limite est $0$.

Cibles / contrôles : QUANT, LOG.

Seuil uniforme impossible.

### Q30-3

Pour tout $n\ge1$, choisissons $\varepsilon=2/n$. Alors $1/n<\varepsilon$, donc la limite vaut $0$.

Cibles / contrôles : QUANT.

Ordre des quantificateurs inversé.

### Q30-4

$1/n$ devient aussi petit que l’on veut quand $n$ devient assez grand. Donc la limite est $0$.

Cibles / contrôles : EXPL, TASK.

Paraphrase intuitive sans construction du seuil demandé.

## Q31 — Sup 1 — Témoins existentiels

Pour $a,b\in\mathbb Z$, on suppose $3\mid a$ et $3\mid b$. Démontrer $3\mid(a+b)$.

Réponse de référence : 3 divise a+b

Famille de comparaison : Q31

### Q31-1

Il existe $p,q\in\mathbb Z$ tels que $a=3p$ et $b=3q$. Alors $a+b=3(p+q)$, avec $p+q\in\mathbb Z$.

Cibles / contrôles : REF.

Témoins indépendants.

### Q31-2

Il existe $k\in\mathbb Z$ tel que $a=3k$ et $b=3k$. Alors $a+b=6k$ est divisible par $3$.

Cibles / contrôles : NAME, QUANT.

Témoin commun non justifié, exclut a différent b.

### Q31-3

$a=3k$ pour un entier $k$. Puis $b=3k$ pour un autre entier $k$. La somme vaut trois fois la somme des deux entiers, donc est divisible par $3$.

Cibles / contrôles : NAME.

« autre » maintient indépendance sémantique malgré collision notationnelle.

### Q31-4

$a+b=3(a/3+b/3)$, donc $3\mid(a+b)$.

Cibles / contrôles : DOMAIN, EXPL.

Intégralité du quotient récupérable dans les hypothèses.

## Q32 — Sup 1 — Variables muettes

Calculer $\int_0^1 t\,dt+\int_0^1 x\,dx$ en justifiant.

Réponse de référence : 1

Famille de comparaison : Q32

### Q32-1

Chaque intégrale vaut $1/2$, donc la somme vaut $1$.

Cibles / contrôles : REF.

Variables liées distinctes sans effet.

### Q32-2

$\int_0^1 t\,dt+\int_0^1 x\,dx=\int_0^1 x\,dx+\int_0^1 x\,dx=2[x^2/2]_0^1=1$.

Cibles / contrôles : SCOPE.

Renommage légal ; témoin négatif pour une interdiction absolue de réemploi.

### Q32-3

Les lettres sont différentes, on ne peut pas additionner ces intégrales.

Cibles / contrôles : SCOPE, LOG.

Méconnaît liaison et nature scalaire.

### Q32-4

$\int_0^1 t\,dt+\int_0^1 x\,dx=t^2/2+x^2/2=1$.

Cibles / contrôles : EQ, SCOPE.

Variables libres apparaissent après intégration définie.

## Q33 — Sup 1 — Inclusion d’ensembles

Démontrer $A\cap(B\cup C)=(A\cap B)\cup(A\cap C)$.

Réponse de référence : Égalité

Famille de comparaison : Q33

### Q33-1

Pour tout $x$, $x\in A\cap(B\cup C)\iff(x\in A)\land(x\in B\lor x\in C)\iff(x\in A\cap B)\lor(x\in A\cap C)$. Les deux ensembles ont donc les mêmes éléments.

Cibles / contrôles : REF.

Équivalences complètes.

### Q33-2

Si $x\in A\cap(B\cup C)$, alors $x\in A$ et $x\in B$ ou $x\in C$. Ainsi $x\in(A\cap B)\cup(A\cap C)$. Donc les ensembles sont égaux.

Cibles / contrôles : EXH.

Une seule inclusion démontrée.

### Q33-3

Par distributivité de l’intersection sur l’union, les deux ensembles sont égaux.

Cibles / contrôles : CIRC, TASK.

Propriété à démontrer invoquée sous son nom, sauf acquis préalable différent.

### Q33-4

Un élément du premier est dans $A$ et dans au moins l’un de $B,C$ ; c’est exactement être dans au moins l’un de $A\cap B,A\cap C$.

Cibles / contrôles : REGISTER.

Preuve verbale bidirectionnelle valable.

## Q34 — Sup 1 — Injectivité

Démontrer que $f:\mathbb R\to\mathbb R$, $f(x)=3x+1$, est injective.

Réponse de référence : Injective

Famille de comparaison : Q34

### Q34-1

Soient $a,b\in\mathbb R$ tels que $f(a)=f(b)$. Alors $3a+1=3b+1$, donc $a=b$. Ainsi $f$ est injective.

Cibles / contrôles : REF.

Sens correct.

### Q34-2

Si $a=b$, alors $3a+1=3b+1$, donc $f(a)=f(b)$. Donc $f$ est injective.

Cibles / contrôles : REC.

Prouve seulement préservation égalité.

### Q34-3

$f(a)=f(b)\iff3a+1=3b+1\iff a=b$. Donc $f$ est injective.

Cibles / contrôles : DOMAIN, ECO.

Variables implicitement universelles dans R ; valide.

### Q34-4

$f$ est strictement croissante sur $\mathbb R$, car son coefficient directeur est positif ; elle est donc injective.

Cibles / contrôles : REGISTER.

Alternative valide sans reprendre définition.

## Q35 — Sup 1 — Supremum

Déterminer et justifier $\sup\{1-1/n:n\ge1\}$.

Réponse de référence : 1

Famille de comparaison : Q35

### Q35-1

Tous les termes sont inférieurs à $1$. Si $b<1$, choisir $n>1/(1-b)$ donne $1-1/n>b$ ; $b$ n’est pas majorant. Donc le supremum est $1$.

Cibles / contrôles : REF.

Majorant et minimalité.

### Q35-2

Pour tout $n\ge1$, $1-1/n<1$. Donc le supremum est $1$.

Cibles / contrôles : EXH.

Majorant seul ne suffit pas.

### Q35-3

La suite tend vers $1$, donc le supremum est $1$.

Cibles / contrôles : LOG, EXPL.

Convergence seule insuffisante ; contrôle de tous les termes absent.

### Q35-4

Le maximum est $1$, puisque les termes s’en approchent sans l’atteindre.

Cibles / contrôles : TYPE, LOG.

Confond supremum et maximum ; contradiction interne.

## Q36 — Sup 1 — Continuité et dérivabilité

L’affirmation « toute fonction continue sur $\mathbb R$ est dérivable sur $\mathbb R$ » est-elle vraie ? Justifier.

Réponse de référence : Non: valeur absolue

Famille de comparaison : Q36

### Q36-1

Non. $f(x)=|x|$ est continue sur $\mathbb R$. En $0$, son taux d’accroissement vaut $1$ à droite et $-1$ à gauche ; elle n’y est pas dérivable.

Cibles / contrôles : REF.

Contre-exemple contrôlé.

### Q36-2

Non : $f(x)=|x|$.

Cibles / contrôles : EXPL.

Exemple canonique sans vérification.

### Q36-3

Oui, car une fonction dérivable est continue.

Cibles / contrôles : REC.

Réciproque abusive.

### Q36-4

Non : $f(x)=1/x$ n’est pas dérivable en $0$.

Cibles / contrôles : DOMAIN.

Contre-exemple hors hypothèses : non définie continue sur R.

## Q37 — Sup 1 — Produit de matrices

Pour des matrices carrées réelles $A,B$ de même taille, développer $(A+B)^2$ sans supposer $AB=BA$.

Réponse de référence : A²+AB+BA+B²

Famille de comparaison : Q37

### Q37-1

$(A+B)^2=(A+B)(A+B)=A^2+AB+B^2$.

Cibles / contrôles : CALC.

Omission du terme BA lors de la distribution. Aucune rédaction de Q37 ne fournit une identité généralement valide ; les autres variantes commutent abusivement ou identifient AB et BA.

### Q37-2

$(A+B)^2=A^2+2AB+B^2$.

Cibles / contrôles : MODEL.

Transfère identité commutative indûment.

### Q37-3

$(A+B)^2=A^2+AB+BA+B^2=A^2+2AB+B^2$.

Cibles / contrôles : LOG.

Début valide puis simplification fausse.

### Q37-4

On pose $C=AB$ et $C=BA$. Alors $(A+B)^2=A^2+C+C+B^2$.

Cibles / contrôles : NAME, LOG.

Collision impose égalité injustifiée ; résultat non valide généralement.

## Q38 — Sup 1 — Parité par contraposition

Pour $n\in\mathbb Z$, démontrer que si $n^2$ est pair, alors $n$ est pair.

Réponse de référence : Implication vraie

Famille de comparaison : Q38

### Q38-1

Par contraposition, si $n$ est impair, $n=2k+1$ avec $k\in\mathbb Z$, et $n^2=2(2k^2+2k)+1$ est impair. Donc si $n^2$ est pair, $n$ est pair.

Cibles / contrôles : REF.

Contraposée complète.

### Q38-2

Si $n$ est pair, $n=2k$, donc $n^2=4k^2$ est pair. Cela prouve le résultat.

Cibles / contrôles : REC.

Réciproque à la place de proposition cible.

### Q38-3

$n^2$ pair signifie $n^2=2k$. Donc $n=2\sqrt{k/2}$ et $n$ est pair.

Cibles / contrôles : DOMAIN.

Coefficient pas nécessairement entier.

### Q38-4

Un impair au carré est impair, donc un entier dont le carré est pair est pair.

Cibles / contrôles : ECO, EXPL.

Lemme implicite standard ; raisonnement correct.

## Q39 — Sup 1 — Suite bornée

La suite $u_n=(-1)^n$ converge-t-elle ? Justifier.

Réponse de référence : Non

Famille de comparaison : Q39

### Q39-1

$u_{2n}=1$ et $u_{2n+1}=-1$. Ces deux sous-suites ont des limites différentes ; la suite ne converge pas.

Cibles / contrôles : REF.

Critère correct.

### Q39-2

Elle est bornée entre $-1$ et $1$, donc elle converge.

Cibles / contrôles : LOG.

Bornée ne suffit pas.

### Q39-3

Elle alterne indéfiniment entre deux valeurs distinctes, $1$ et $-1$, donc elle ne peut s’approcher d’une seule limite.

Cibles / contrôles : REGISTER.

Explication intuitive générale valide, formalisation moindre.

### Q39-4

$u_n=1$ puis $u_n=-1$, donc $1=-1$, impossible : la suite diverge.

Cibles / contrôles : NAME, EQ.

Variation de n tue par égalité de valeurs ; argument incorrect, conclusion vraie.

## Q40 — Sup 1 — Unicité de limite

Démontrer qu’une suite réelle convergente possède une seule limite.

Réponse de référence : Unicité

Famille de comparaison : Q40

### Q40-1

Supposons $u_n\to a$ et $u_n\to b$ avec $a\ne b$. Posons $\varepsilon=|a-b|/3$. Pour $n$ assez grand, $|u_n-a|<\varepsilon$ et $|u_n-b|<\varepsilon$. Alors $|a-b|\le|a-u_n|+|u_n-b|<2|a-b|/3$, contradiction.

Cibles / contrôles : REF.

Absurdum et seuil commun implicite standard.

### Q40-2

Si $u_n\to a$ et $u_n\to b$, alors $a=\lim u_n=b$.

Cibles / contrôles : CIRC.

Notation de limite unique présuppose ce qui est à établir.

### Q40-3

À partir d’un certain rang, $u_n=a$ et $u_n=b$. Donc $a=b$.

Cibles / contrôles : LIMIT, LOG.

Convergence confondue avec stationnarité.

### Q40-4

Si deux limites étaient distinctes, des voisinages disjoints autour d’elles devraient tous deux contenir tous les termes à partir d’un certain rang. C’est impossible.

Cibles / contrôles : REGISTER.

Argument topologique verbal complet, au niveau indiqué.
