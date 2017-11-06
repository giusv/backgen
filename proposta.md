Introduzione
========

In questo documento presenter� Extensys (Extensible Enterprise Systems), ovvero la mia proposta per accrescere la produttivit� e ridurre il time-to-market nell'ambito dei progetti della Divisione.
Extensys � un linguaggio estensibile per la definizione di applicazioni enterprise destinate alla manipolazione di dati strutturati. Pi� in dettaglio, esso � un linguaggio funzionale che consente all'analista di definire un modello strutturato di un'applicazione enterprise, dal quale � poi possibile generare un insieme diversificato di artefatti:
 - documentazione in linguaggio naturale destinata alla descrizione funzionale dell'applicazione (utilizzabile ad esempio nella stesura del Documento dei Requisiti Funzionali - DRF)
 - codice applicativo di back-end in linguaggio Java EE (REST, EJB, DAO, JPA e relative DDL)
 - codice applicativo di front-end in linguaggio Angular/Typescript
 - codice di test di unit�/integrazione/stress con generazione automatica di dataset di test

Un ulteriore artefatto generabile, per quella niche di applicazioni in cui � ricaduto il progetto AIA, � il seguente:
 - codice per il parsing di minilinguaggi utente di specifica (indicatori di business, business rules in genere) e la relativa generazione di codice (attualmente Javascript + SQL, ma in alcun modo limitata a questi) per la loro esecuzione a runtime.

Le potenzialit� del linguaggio non si arrestano per� ai citati artefatti. Sarebbe possibile anche generare:
 - report di conteggio dei function point relativi alle diverse funzionalit� 
 - slides per la presentazione dei requisiti funzionali all'utente/committente
 - codice di test dell'interfaccia U2A
 - videomanuali per l'illustrazione delle funzionalit� dell'interfaccia U2A, utili a supporto dell'operativit� degli utenti decentralizzati (filiali).

Tali potenzialit� derivano dalla citata estensbilit� del linguaggio, che, come descriver� nel seguito, � in realt� una torre di linguaggi, ciascuno costruito utilizzando i servizi messi a disposizione dal livello sottostante (alla stregua delle pile ISO/OSI dei protocolli di rete). In questo contesto, l'aggiunta di una nuova caratteristica ad Extensys si risolve nell'aggiunta di nuova sintassi per la sua specifica, insieme al corrispondente generatore che pu� far uso dell'insieme di servizi gi� esistenti. 

Il sistema � realizzato in Lisp, e attualmente implementa buona parte delle citate funzionalit� core in circa seimila righe di codice. E' mia intenzione proseguire nel suo sviluppo, magari nell'ambito delle opportunit� offerte dal sistema di feedback aziendale, al fine di renderlo pienamente utilizzabile nelle fasi di specifica/analisi di uno dei prossimi progetti a cui collaborer�. In maniera parziale e non integrata, esso � gi� stato utilizzato nel progetto AIA come supporto alla stesura dei requisiti funzionali, alla produzione di una importante parte del codice applicativo (parser del linguaggio degli indicatori antifrode), e alla generazione di codice e dati di test.

Dettagli
===============
In questa sezione fornir� alcuni dettagli implementativi di Extensys, e in particolare descriver� i vantaggi del paradigma "tower of languages".
Il linguaggio Lisp � conosciuto per le opportunit� che esso offre allo sviluppatore di creare, tramite il suo celebre macro system, veri e propri linguaggi di programmazione "intermedi", ovvero delle astrazioni sintattiche in cui lo sviluppo dell'applicazione risulta pi� agevole.
Nel design di Estensys, ho spinto questo paradigma ancora pi� oltre: ho creato diversi livelli di linguaggi intermedi, ciascuno fondato sulle astrazioni messe a disposizione dal livello inferiore, e in grado di fornire "servizi" al livello superiore.
Alla base della torre ho posto le primitive Lisp che consentono di definire un qualsiasi linguaggio intermedio di Extensys. Il costrutto fondamentale , ovvero simboli terminali e non terminali di una attributed grammar. Il concetto di attributed grammar � fondamentale nella teoria dei linguaggi e compilatori. Essa consente al contempo di definire il modo con cui i simboli del linguaggio possono essere combinati tra loro (le cosiddette produzioni), e di specificare le azioni semantiche associate ad ogni simbolo quando esso viene incontrato durante il parsing (generazione di codice). Nel caso specifico di Extensys, le azioni semantiche a un certo sono specificate facendo uso dei simboli messi a disposizione dal livello inferiore. 

Al primo livello della torre vi � il linguaggio doc: esso consente di specificare un qualsiasi documento testuale mediante primitive di scrittura interpolata, concatenazione orizzontale e verticale, e nesting. Come � intuitivo pensare, doc sar� poi usato per la generazione di codice indentato. 

Al secondo livello della torre, ho posto i linguaggi per la produzione di artefatti nei pi� comuni linguaggi di programmazione: Java, Typescript, SQL, HTML, XML, Json/JsonSchema, Latex. Per ciascuno di essi, ho definito primitive sintattiche per la specifica di costrutti del linguaggio stesso (denominate nel seguito "sintassi Extensys"), come ad esempio:
 - classi, metodi, annotazioni, oggetti e strutture di controllo per Java e Typescript
 - selezione di campi, clausole di filtraggio e costrutti di Data Definition Language per il linguaggio SQL
 - nodi e attributi per HTML e XML
 - tipi primitivi, oggetti e array per JSON
 - sezioni, paragrafi, stili e riferimenti per il linguaggio Latex

Fornisco un esempio di applicazione del paradigma tower of languages per il linguaggio Java. Supponiamo che un analista abbia prodotto un costrutto Java utilizzando la sintassi Extensys, ad esempio una classe con alcuni metodi. Egli non dovr� preoccuparsi allora di "impaginare" il codice, indentandolo ad esepio secondo il comune buon senso. I singoli costrutti Java che compongono la classe sapranno autonomamente trasformarsi in un costrutto del linguaggio doc del livello sottostante, e quindi produrre un file di testo automaticamente indentato e impaginato.

Ancor pi�, come sar� chiaro tra poco, difficilmente un analista umano scriver� codice Java nella sintassi Extensys: al contrario, tale sintassi � adatta ad essere utilizzata come output prodotto da un altro linguaggio di livello superiore, che fornir� primitive di astrazione sintattica su concetti pi� vicini all'analista umano, che verranno poi automaticamente trasformati nella classe Java di cui sopra, che ne rappresenta una possibile implementazione. 

Veniamo allora ai linguaggi costituenti il terzo livello della torre, sui quali mi soffermer� pi� in dettaglio. Questo � il livello che attualmente � utilizzato dall'analista umano per la specifica del modello dell'applicazione enterprise da realizzare. Per comprendere le scelte 



istruzioni di selezione, proiezione, prodotto per l'algebra relazionale da cui si genera SQL
