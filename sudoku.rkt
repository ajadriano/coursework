#lang racket

(provide flattensolution)
(provide transform)
(provide solve)
(provide isvalidsudoku)
(provide allcellssingleton)
(provide getcell)
(provide getblock)
(provide getothercandidatesfromrow)
(provide getothercandidatesfromcolumn)
(provide getothercandidatesfromblock)

(define (isvalidsudoku list)
	(cond
		((not (list? list)) (error "Invalid sudoku puzzle"))
		((not (andmap list? list)) (error "Invalid sudoku puzzle"))
		((not (= (length list) 9)) (error "Invalid sudoku puzzle"))
		((not (andmap (lambda (x) (= (length x) 9)) list)) (error "Invalid sudoku puzzle"))
		((not (andmap (lambda (x) (andmap (lambda (y) (and (<= y 9) (>= y 0))) x)) list)) (error "Invalid sudoku puzzle"))
		(else list)
	)
)

(define (getcell row column list) (list-ref (list-ref list row) column))

(define (getblockid row column)
	(+ (* (quotient row 3) 3) (quotient column 3))
)

(define (getblock index list)
	(let (
			(rowstart (* (quotient index 3) 3))
			(columnstart (* (remainder index 3) 3))
		 )
		 (for/list ([x (in-range 0 9)]) 
		 	(getcell (+ (quotient x 3) rowstart) (+ (remainder x 3) columnstart) list)
		 ) 
	)	
)

(define (getothercandidatesfromrow rowindex exemptedcolumnindex puzzle) 
	(remove-duplicates
		(flatten
			(let ((focusedrow (list-ref puzzle rowindex)))
				(for/list ([x (in-range 0 9)] #:when (not (= exemptedcolumnindex x))) 
					(list-ref focusedrow x)
				)
			)
		)
	)
)

(define (getothercandidatesfromcolumn exemptedrowindex columnindex puzzle) 
	(remove-duplicates
		(flatten
			(let ((focusedcolumn (map (lambda (row) (list-ref row columnindex)) puzzle)))
				(for/list ([x (in-range 0 9)] #:when (not (= exemptedrowindex x))) 
					(list-ref focusedcolumn x)
				)
			)
		)
	)
)

(define (getothercandidatesfromblock exemptedrowindex exemptedcolumnindex puzzle) 
	(remove-duplicates
		(flatten
			(let ((focusedblock (getblock (getblockid exemptedrowindex exemptedcolumnindex) puzzle)))			
				(for/list ([x (in-range 0 9)] #:when (not (= (+ (* (remainder exemptedrowindex 3) 3) (remainder exemptedcolumnindex 3)) x))) 
					(list-ref focusedblock x)
				)
			)
		)
	)
)

(define (dofunctionuntil func list checklist)
	(let ((newlist (func list))
		 )
		(cond
			((checklist list) list)
			((equal? newlist list) (error "no solution found"))
			(else (dofunctionuntil func newlist checklist))
		)
	)
)

(define (puzzlecellvisitor visitorfunc currentrow currentcolumn puzzle)
	(cond
		((and (= currentrow 8) (= currentcolumn 8)) (visitorfunc currentrow currentcolumn puzzle))
		((= currentcolumn 8) (visitorfunc currentrow currentcolumn (puzzlecellvisitor visitorfunc (+ currentrow 1) 0 puzzle)))
		(else (visitorfunc currentrow currentcolumn (puzzlecellvisitor visitorfunc currentrow (+ currentcolumn 1) puzzle)))
	)
)

(define (removefromlist itemstoremove list) 
	(if (empty? itemstoremove) 
		list 
		(removefromlist (cdr itemstoremove) (remove (car itemstoremove) list))
	)
)

(define (tryreducetosingleton cell row column puzzle)
	(let((reducedtosingletonusingrow (removefromlist (getothercandidatesfromrow row column puzzle) cell))
		 (reducedtosingletonusingcolumn (removefromlist (getothercandidatesfromcolumn row column puzzle) cell))
		 (reducedtosingletonusingblock (removefromlist (getothercandidatesfromblock row column puzzle) cell))
		)
		(cond
			((= (length reducedtosingletonusingrow) 1) reducedtosingletonusingrow)
			((= (length reducedtosingletonusingcolumn) 1) reducedtosingletonusingcolumn)
			((= (length reducedtosingletonusingblock) 1) reducedtosingletonusingblock)
			(else cell)
		) 
	)
)

(define (removecellvalueincandidatelist rowindex columnindex list)
	(for/list ([currentrow 9]) (for/list ([currentcolumn 9]) 
		 (let* (
		 	(cellvaluetoremove (getcell rowindex columnindex list))
		 	(currentcell (getcell currentrow currentcolumn list))
		 	(samecell (lambda () (and (= rowindex currentrow) (= columnindex currentcolumn))))
		 	(singleton (lambda () (= (length currentcell) 1)))
		 	(samerow (lambda () (= rowindex currentrow)))
		 	(samecolumn (lambda () (= columnindex currentcolumn)))
		 	(sameblock (lambda () (= (getblockid rowindex columnindex) (getblockid currentrow currentcolumn))))
		 	)
		 	(cond
		 		((samecell) currentcell)
		 		((or (samerow) (samecolumn) (sameblock))
		 			 (cond
		 			 	((equal? cellvaluetoremove currentcell) (error "duplicate values"))
		 			 	((singleton) currentcell)
		 			 	(else (removefromlist cellvaluetoremove currentcell)
		 			 	)
		 			 )		 
		 		)
		 		(else currentcell)	
		 	)
			)
	))
)

(define (reducetosingleton rowindex columnindex puzzle)
	(for/list ([currentrow 9]) (for/list ([currentcolumn 9]) 
		 (let ((currentcell (getcell currentrow currentcolumn puzzle)))
		 	(cond
		 		((and (= rowindex currentrow) (= columnindex currentcolumn)) 
		 			(tryreducetosingleton currentcell rowindex columnindex puzzle))
		 		(else currentcell)	
		 	)
			)
	))
)

(define (solvecell rowindex columnindex puzzle)
	(cond
		 ((= (length (getcell rowindex columnindex puzzle)) 1) (removecellvalueincandidatelist rowindex columnindex puzzle))
		 (else 
		 	(let ((updatedpuzzle (reducetosingleton rowindex columnindex puzzle)))
		 		(if (= (length (getcell rowindex columnindex updatedpuzzle)) 1)
					(removecellvalueincandidatelist rowindex columnindex updatedpuzzle)
					updatedpuzzle
		 		)
		 	)
		 )	
	)
) 

(define (allcellssingleton puzzle)
	(andmap (lambda (x) (andmap (lambda (y) (= (length y) 1)) x)) puzzle)
)

(define (flattensolution puzzle)
	(map (lambda (x) (map (lambda (y) (car y))x)) puzzle)
)

(define (transform puzzle) 
	(let* (
			(firsttransform (lambda (a) (if (= a 0) '(1 2 3 4 5 6 7 8 9) (list a))))
			(secondtransform (lambda (b) (map firsttransform b)))
		  )
		(map secondtransform (isvalidsudoku puzzle))
	)
)

(define (solve puzzle)
	(let ((puzzlewithcandidates (transform puzzle))
		  (solveallcells (lambda (x) (puzzlecellvisitor solvecell 0 0 x)))
		  )
		(flattensolution (dofunctionuntil solveallcells puzzlewithcandidates allcellssingleton))
	)
)

