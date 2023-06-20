#lang racket

; Prüft, ob in einer nicht verschachtelten Liste die gesuchte Zeichenkette vorhanden ist
; der Default-Wert von f ist eine leere Liste
(define (containsNode haystack needle [f '()])
  (if
   (equal? (car haystack) needle)
   (append f haystack)
   (if
    (> (length haystack) 1)
    (containsNode (cdr haystack) needle (append f (list (car haystack))))
    #f
    ))
  )

  (define (roundDecimal number nachKommastellen)
  (string->number (real->decimal-string number 1))
  )

; Gibt alle Kanten zurück, die den übergebenen Knoten beinhalten.
(define (pathsWithNode liste node [endlist '()])
  (if
   (> (length liste) 0)
   (if
    (containsNode (car liste) node)
    (pathsWithNode (cdr liste) node (append (list (car liste)) endlist))
    (pathsWithNode (cdr liste) node endlist)
    )
   endlist
   )
  )

; Diese Funktion erhöht das Gewicht aller Kanten zu Nachbarknoten um das Gewicht des aktuellen Knoten.
(define (updateWeightNeighborNodes paths weightCurrentNode [result '()])
(cond
((equal? paths '()) result)
(else (updateWeightNeighborNodes (cdr paths) weightCurrentNode (append result (list (reverse (append (list (roundDecimal (+ (last (car paths)) weightCurrentNode) 1)) (cdr (reverse (car paths)))))))))
)
)

; Diese Funktion setzt zwingend voraus, dass nur ein Pfad existiert, der den
; Ansprüchen entspricht. Existieren mehrere wird nur der erste ausgegeben.
; Die Reihenfolge der Knoten ist nicht entscheidend.
(define (getPathWithTwoNodes liste a b)
(cond
; Wenn Pfade mehr vorhanden sind heisst das auch, dass bisher kein Pfad gefunden wurde,
; der den Parametern entspricht, dann wird false ausgegeben.
((equal? liste '()) #f)
; Wenn die beiden Knoten in dem ersten Pfad der Liste vorkommen wird dieser Pfad ausgegeben.
((and (or (equal? a (caar liste)) (equal? b (caar liste))) (or (equal? b (cadar liste)) (equal? a (cadar liste)))) (car liste))
; Wenn noch Pfade vorhanden sind, der aktuelle Pfad aber nicht den Parametern entspricht
; ruft sich die Funktion selbst ohne den aktuelle Pfad auf.
(else (getPathWithTwoNodes (cdr liste) a b))
)
)

(define (addBestPaths seenPaths newPaths nodes)
(cond
; wenn newPaths leer ist wird seenPaths zurueckgegeben
((equal? newPaths '()) seenPaths)
((or (member (caar newPaths) nodes) (member (cadar newPaths) nodes)) (addBestPaths seenPaths (cdr newPaths) nodes))
; wenn noch kein Pfad fuer diese beiden Knoten gespeichert ist wird der aktuelle Pfad hinzugefuegt
((equal? (getPathWithTwoNodes seenPaths (caar newPaths) (cadar newPaths)) #f) (addBestPaths (append seenPaths (list (car newPaths))) (cdr newPaths) nodes))
; wenn guenstigerer Pfad gefunden
((< (last (car newPaths)) (last (getPathWithTwoNodes seenPaths (caar newPaths) (cadar newPaths)))) (addBestPaths (append (remove (getPathWithTwoNodes seenPaths (caar newPaths) (cadar newPaths)) seenPaths) (list (car newPaths))) (cdr newPaths) nodes))
(else (addBestPaths seenPaths (cdr newPaths) nodes))
)
)

(define (getSmallestUnvisitedNode visitedNodes paths)
(cond
((equal? (member (caar (sort paths #:key last <)) visitedNodes) #f) (caar (sort paths #:key last <)))
((equal? (member (cadar (sort paths #:key last <)) visitedNodes) #f) (cadar (sort paths #:key last <)))
(else #f)
)
)

(define (getWeightOfSmallestUnvisitedNode visitedNodes paths)
(last (car (sort paths #:key last <)))
)

(define (printResult nodes weight [result "Der Pfad "])
(cond
((empty? nodes) (string-append result " ist mit einem Gewicht von " (number->string weight) " der leichteste Pfad."))
((empty? (cdr nodes)) (printResult (cdr nodes) weight (string-append result (car nodes))))
(else (printResult (cdr nodes) weight (string-append result (car nodes) "->")))
)
)

; Diese Funktion extrahiert aus der Ausgabe der dijkstra-Funktion den tatsächlichen Pfad von dem Start zum Ziel heraus. Dazu wird am Ende begonnen. Der Parameter weight entspricht 
; zu Beginn dem Gewicht des gesamten Pfades von Start bis Ziel. Es wird nach dem Pfad gesucht, der den Endknoten beinhaltet und außerdem das Gewicht im Funktionsparameter weight aufweist.
; In dem Ergebnis der dijkstra-Funktion entspricht das Gewicht einer Kante immer dem Gesamtgewicht des Pfades vom Start bis zu dem jeweiligen Knoten. Beispiel: Zwei Pfade verbinden den 
; Startpunkt s und den Endpunkt e, der Knoten dazwischen heißt a. Nun haben die Kanten (s,a) und (a,e) ein Gewicht von 2 und 3. In der Ausgabe der dijkstra-Funktion haben diese beiden Kanten
; allerdings ein "Gewicht" von 2 und 5. Dabei entspricht das "Gewicht" der Kante (a,e) dem Pfadgewicht von dem Punkt s bis e. Dieser Prozess wird in dieser Funktion genutzt und umgekehrt.
; Damit diese Funktion fehlerfrei funktioniert dürfen nicht mehrere Kanten die gleichen Knoten miteinander verbinden.
(define (getRoute graph start current startGraph [weight 0] [result '()])
(display weight)
(display " | ")
(display result)
(display " | ")
(display graph)
(display "\n")
  (cond
  ((empty? graph) result)
  ((and (containsNode (car graph) current) (equal? weight (last (car graph)))) (getRoute (cdr graph) start (getSmallestUnvisitedNode (list current) (list (car graph))) startGraph (roundDecimal (- weight (last (getPathWithTwoNodes startGraph (caar graph) (cadar graph)))) 1) (append result (list (getPathWithTwoNodes startGraph (caar graph) (cadar graph))))))
  ((number? (last graph)) (getRoute (cdr (reverse graph)) start current startGraph (last graph)))
  (else (getRoute (cdr graph) start current startGraph weight result))
))

(define (dijkstra2 currentNode ziel liste [currentWeight 0] [nodes '()] [markedPaths '()] [previous ""])
; (display "   ")
; (display currentWeight)
; (display "   |    ")
; (display currentNode)
; (display "    |   ")
; (display (getSmallestUnvisitedNode (append (list currentNode) nodes) (addBestPaths markedPaths (updateWeightNeighborNodes (pathsWithNode liste currentNode) currentWeight) nodes)))
; (display "   | ")
; ; (display (or (equal? previous "")))
; (display (not (containsNode (car (sort (addBestPaths markedPaths (updateWeightNeighborNodes (pathsWithNode liste currentNode) currentWeight) nodes) #:key last <)) previous)))
; ; (display previous)
; (display " | ")
; (display nodes)
; (display " | ")
; (display (sort (addBestPaths markedPaths (updateWeightNeighborNodes (pathsWithNode liste currentNode) currentWeight) nodes) #:key last <))
; (display "\n")
(cond
  ((equal? ziel currentNode) (list (car (sort (addBestPaths markedPaths (updateWeightNeighborNodes (pathsWithNode liste currentNode) currentWeight) nodes) #:key last <)) currentWeight))
  (else (append 
    (list (car (sort (addBestPaths markedPaths (updateWeightNeighborNodes (pathsWithNode liste currentNode) currentWeight) nodes) #:key last <)))
    (dijkstra2
      ; currentNode
      (getSmallestUnvisitedNode (append (list currentNode) nodes) (addBestPaths markedPaths (updateWeightNeighborNodes (pathsWithNode liste currentNode) currentWeight) nodes))
      ; ziel
      ziel
      ; liste
      liste
      ; currentWeight
      (getWeightOfSmallestUnvisitedNode (append (list currentNode) nodes) (addBestPaths markedPaths (updateWeightNeighborNodes (pathsWithNode liste currentNode) currentWeight) nodes))
      ; nodes
      (append nodes (list currentNode))
      ; markedPaths
      (cdr (sort (addBestPaths markedPaths (updateWeightNeighborNodes (pathsWithNode liste currentNode) currentWeight) nodes) #:key last <))
      ; previous
      currentNode
    ))
    )
))
; (display "weight | current | next  | markedPaths\n")

; Dies ist die Aufruffunktion für die dijkstra-Funktion. Indem noch die Funktion getRoute aufgerufen wird kann der genaue Pfad vom Start bis zum Ziel aus der Ausgabe der dijkstra-
; Funktion extrahiert werden. Die Ausgabe der dijkstra-Funktion wird in dieser Funktion über umwege verwendet. Dies liegt daran, dass diese Ausgabe an verschiedenen Stellen in dieser
; Funktion genutzt wird. Es ist effizienter diese Ausgabe durch einen rekursiven Aufruf in den Parametern zu speichern als jedes mal die komplette dijkstra-Funktion auszuführen.
(define (executeDijkstra start ziel graph [editedGraph '()])
(if (not(empty? editedGraph))(display (string-append "Der untenstehende Pfad hat mit einem Gewicht von " (number->string (last editedGraph)) " das geringste Gewicht.\n")) (display ""))
(cond
((empty? editedGraph) (executeDijkstra start ziel graph (dijkstra2 start ziel graph)))
(else (reverse (getRoute editedGraph start ziel graph)))
))

(define graph1 '(("A" "B" 1)("A" "B" 3)("A" "C" 3)("B" "C" 2)("B" "D" 4)("C" "D" 1)))
(define graph2 '(("S" "A" 7)("S" "B" 2)("S" "C" 3)("A" "B" 3)("A" "D" 4)("D" "B" 4)("D" "F" 5)("F" "H" 3)("B" "H" 1)("H" "G" 2)("G" "E" 2)("K" "E" 5)("I" "K" 4)("J" "K" 4)("I" "J" 6)("L" "J" 4)("L" "I" 4)("C" "L" 2)))
; Der Graph3 entspricht dem Beispielgraphen aus "Abenteuer Geschichte" von J. Gallenbacher.
(define graph3 '(("M" "A" 6.7)("M" "X" 2.3)("M" "C" 5.6)("M" "I" 9.0)("C" "I" 8.2)("C" "X" 4.6)("A" "D" 6.6)("A" "N" 3.8)("A" "B" 14.3)("X" "N" 6.4)("N" "D" 4.1)("I" "B" 7)("B" "D" 13)("N" "Z" 5.8)("D" "L" 18.9)("B" "Z" 6.2)("Z" "L" 7.8)("I" "H" 13.4)("I" "P" 10.5)("B" "H" 5.6)("P" "H" 7.1)("H" "L" 21.1)("Z" "Y" 4.5)("H" "Y" 5.9)("Y" "G" 5.5)("L" "G" 11.8)("P" "K" 5.1)("H" "K" 6)("Y" "K" 3.6)("K" "G" 11.5)("P" "F" 11.6)("K" "F" 6.1)("K" "E" 6.2)("G" "E" 12.1)("O" "E" 19.5)("F" "E" 15.6)("P" "O" 18.2)("Q" "O" 5.3)("F" "O" 2.9)))
(executeDijkstra "Q" "E" graph3)