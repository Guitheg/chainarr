# chainarr
 Moteur d'inférences d'ordre 0 en chaînage arrière

Vous devez implémenter en OCAML un algorithme de démonstration en chaînage arrière utilisant
une politique premier but, première règle. Il devra respecter le cycle de base d'un moteur
d'inférences : (1) détection des règles applicables, (2) choix de la règle à appliquer (résolution de
conflits), (3) application de cette règle, (4) marquage de la règle utilisée. Et ceci tant que la liste des
buts n’est pas vide et qu’il reste des règles à appliquer.
Une règle ne pourra être utilisée que pour démontrer un fait qui n'est pas déjà dans BF. Tout fait
démontré devra être rajouté à la base de faits BF (voir l'exemple de session de la page précédente).
Vous devrez compter le nombre d'utilisation de chaque règle grâce à un compteur pour éviter les
problèmes de bouclage. Le programme que vous avez à écrire devra offrir les fonctionnalités
suivantes :
    + Il doit travailler en chaînage arrière, selon une politique premier but, première règle (qui est la
plus simple à implémenter). Il doit aussi :
    + backtracker en cas d'échec de la preuve dans une branche,
    + traiter les problèmes de bouclage (par exemple en limitant la profondeur de recherche ou
en bornant le nombre d'utilisation de chaque règle grâce à un compteur associé). Tout fait
prouvé devra être rajouté à la base de faits.
    + donner la première preuve, et les preuves suivantes si l’utilisateur le demande.
    + Il doit permettre de tracer le raisonnement du moteur au fur et à mesure (inspirez-vous de
l'exemple de session de la page précédente). Il faudra donc :
    + numéroter les règles,
    + permettre de suivre le fonctionnement du moteur d'inférences en affichant les traces de la
résolution.
