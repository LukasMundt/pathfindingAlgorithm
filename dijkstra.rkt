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

(define (updateWeightNeighborNodes paths weightCurrentNode [result '()])
(cond
((equal? paths '()) result)
(else (updateWeightNeighborNodes (cdr paths) weightCurrentNode (append result (list (reverse (append (list (+ (last (car paths)) weightCurrentNode)) (cdr (reverse (car paths)))))))))
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

(define (dijkstra currentNode ziel liste [currentWeight 0] [nodes '()] [markedPaths '()] [result '()])
(display result)
(display "\n")
(cond
  ((equal? ziel currentNode) (printResult (append nodes (list currentNode)) currentWeight))
  (else (dijkstra
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
    ; result
    (sort (append result (list (car (sort (addBestPaths markedPaths (updateWeightNeighborNodes (pathsWithNode liste currentNode) currentWeight) nodes) #:key last <)))) #:key last <)
  ))
))

(define (dijkstra2 currentNode ziel liste [currentWeight 0] [nodes '()] [markedPaths '()] [previous ""])
(display "   ")
(display currentWeight)
(display "   |    ")
(display currentNode)
(display "    |   ")
(display (getSmallestUnvisitedNode (append (list currentNode) nodes) (addBestPaths markedPaths (updateWeightNeighborNodes (pathsWithNode liste currentNode) currentWeight) nodes)))
(display "   | ")
; (display (or (equal? previous "")))
(display (not (containsNode (car (sort (addBestPaths markedPaths (updateWeightNeighborNodes (pathsWithNode liste currentNode) currentWeight) nodes) #:key last <)) previous)))
; (display previous)
(display " | ")
(display nodes)
(display " | ")
(display (sort (addBestPaths markedPaths (updateWeightNeighborNodes (pathsWithNode liste currentNode) currentWeight) nodes) #:key last <))
(display "\n")
(cond
  ((equal? ziel currentNode) (list currentNode currentWeight))
  (else (append 
    (list currentNode)
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
    )))
))

(define graph1 '(("A" "B" 1)("A" "B" 3)("A" "C" 3)("B" "C" 2)("B" "D" 4)("C" "D" 1)))
(define graph2 '(("S" "A" 7)("S" "B" 2)("S" "C" 3)("A" "B" 3)("A" "D" 4)("D" "B" 4)("D" "F" 5)("F" "H" 3)("B" "H" 1)("H" "G" 2)("G" "E" 2)("K" "E" 5)("I" "K" 4)("J" "K" 4)("I" "J" 6)("L" "J" 4)("L" "I" 4)("C" "L" 2)))
(display "weight | current | next  | markedPaths\n")
(dijkstra2 "S" "L" graph2)
; (define (executeDijkstra start ziel liste)
; (reverse (cdr (reverse (dijkstra2 start ziel liste))))
; (dijkstra2 start ziel liste)
; )
; (executeDijkstra "S" "H" graph2)