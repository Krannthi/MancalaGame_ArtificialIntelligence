(defpackage :kranthi-gopu
(:use :common-lisp-user :common-lisp)
(:export computer-make-move))
(in-package :kranthi-gopu)

;;;; Once you've done this, you need to write your code.  Here
;;;; is a rough sketch of three functions you will find handy.
;;;; You don't need to implement them like this, except for the
;;;; COMPUTER-MAKE-MOVE function. You can write your code in
;;;; any fashion you like in this file, so long as you do a
;;;; proper alpha-beta heuristic search and your evaluation
;;;; function is stronger than just comparing the differences in
;;;; the mancalas.

(defun alpha-beta (state current-depth max-depth max-player expand terminal evaluate alpha beta)

;; "Does alpha-beta search.  Note that there is the addition of
;; a variable called MAX-PLAYER rather than a function which specifies
;; if it's max's turn.  It's just more convenient in this system.
;; The MAX-PLAYER variable is set to either *player-1*
;; or to *player-2* and indicates if *player-1* or *player-2* should
;; be considered 'max' (the other player is then considered to be
;; 'min')"

(if (or (funcall terminal state) (>= current-depth max-depth)) (return-from alpha-beta (funcall evaluate state max-player))
(let (score (search (funcall expand state)))
     (if (= max-player (state-turn state))
         (dolist (temp-state search alpha)
		    (setf score (alpha-beta temp-state (+ 1 current-depth) max-depth max-player expand terminal evaluate alpha beta))
         	(if (> score alpha) (setf alpha score))
            (if (>= alpha beta) (return-from alpha-beta beta))
         ) 
        (dolist (temp-state search beta)
		    (setf score (alpha-beta temp-state (+ 1 current-depth) max-depth max-player expand terminal evaluate alpha beta))
         	(if (< score beta) (setf beta score))
            (if (>= alpha beta) (return-from alpha-beta alpha))
        )
	   		 
      )
)
)
)	  


(defun distance-between-pits-of-a-player (x-pit empty-pit player-mancala-pit)

;;"Finds the distance between any two pits on the board moving in counter-clock wise direction from the 'x-pit' to 'empty-pit' " 

(cond ((< x-pit empty-pit) (return-from distance-between-pits-of-a-player (- empty-pit x-pit)))
      ((> x-pit empty-pit) (return-from distance-between-pits-of-a-player (+ (- player-mancala-pit x-pit) (+ 1 empty-pit) *num-pits*)))  
)
)


(defun possible-big-win-stones-for-a-player (player-left-pit player board)

;;"This function returns the sum of all possible points a player could win using big-win-rule "

(let ((count 0) empty-pit current-pit)
       (dotimes (x *num-pits* count)  
	   (setf empty-pit (+ player-left-pit x))
	   (when (and (= (svref board empty-pit) 0) (not (> (svref board (pit-opposite empty-pit)) 0)))
            (block inner (dotimes (y *num-pits*)
                          (setf current-pit (+ player-left-pit y))
	                      (when (not (= current-pit empty-pit))
	                           (if (= (svref board current-pit) 
	                                  (distance-between-pits-of-a-player current-pit empty-pit (mancala-pit player)))
	                               (progn (incf count (svref board (pit-opposite empty-pit))) (return-from inner))) 
       
	   ))))))
)



(defun possible-go-again-moves (player-left-pit player board)

;;"This function is used to find the maximum-possible go-again moves for a player" 

(let ((count 0) (player-mancala (mancala-pit player)))
        (dotimes (x *num-pits* count) 
		(if (= (svref board (+ player-left-pit x)) (- player-mancala x)) (incf count)))
)
)


(defun distance-from-win-for-a-player (player state)

;; Minimum coins here means the minimum majority required for win. that is if there are 6 pits and 4 coins each, then minimum score for win is 25.
;;"This function returns the difference between the minimum coins and mancala-pit coins if minimum coins is greater than mancala coins, else it returns
;; the inverse of difference between the mancala-pit coins and minimum coins, if the difference is zero it returns score. "

(let (
     (temp (- (svref (state-board state) (mancala-pit player)) (+ 1 (* *num-pits* *initial-stones-per-pit*))))
     )

  (cond ((= temp 0) (return-from distance-from-win-for-a-player (score state player)))
        ((> temp 0) (return-from distance-from-win-for-a-player (/ 1 temp)))
        (t (return-from distance-from-win-for-a-player (* -1 temp)))
  )
)  
)

(defun evaluate (state max-player)

;;   "Evaluates the game situation for MAX-PLAYER.
;; Returns the value of STATE for MAX-PLAYER (who
;; is either *player-1* or *player-2*).  This should
;; be a value ranging from *min-wins* to *max-wins*."
;; 
;;

;;(150 75 100 200)  (100 20 20 60) (100 20 20 60)
	 
(let* (
       (final-score (* 100 (score state max-player))) 
       
	   (min-player (other-player max-player))
       
	   (max-left-pit (left-pit max-player)) 
	   
	   (min-left-pit (left-pit min-player))
	   
	   (temp-board (state-board state))
	   
	   (final-score (+ final-score (if (eql *big-win-rule* t) 
	   (* 20 (- (possible-big-win-stones-for-a-player max-left-pit max-player temp-board) 
	   
	   (possible-big-win-stones-for-a-player min-left-pit min-player temp-board))) 0)))
	   
       (final-score (+ final-score (if (eql *go-again-rule* t) (* 20 (- (possible-go-again-moves max-left-pit max-player temp-board) 
	   (possible-go-again-moves min-left-pit min-player temp-board))) 0)))
	   
	   (final-score (+ final-score (* 60 (/ 1 (distance-from-win-for-a-player max-player state)))))  
	   
	  
	
	)
	 
	 

  (return-from evaluate final-score)



)
)


(defun shuffle (lis)
"Shuffles a list. This is used for randomizing the moves."
(let ((vec (apply #'vector lis)) bag (len (length lis)))
    (dotimes (x len)
      (let ((i (random (- len x))))
	(rotatef (svref vec i) (svref vec (- len x 1)))
	(push (svref vec (- len x 1)) bag)))
    bag)
	
)  


(defun computer-make-move (state max-depth)
 
;; "Given a state, makes a move and returns the new state.
;; If there is no move to make (end of game) returns nil.
;; Each time this function calls the top-level
;; alpha-beta function to search for the quality of a state,
;; computer-make-move should print out the state (using PRINT,
;; not PRINT-STATE) that is being searched.
;; Only search up to max-depth.  The computer should assume
;; that he is the player who's turn it is right now in STATE"
 

 (if (game-overp state) (return-from computer-make-move nil)
 (let
      (best score (best-score *min-wins*) (search (shuffle (moves state))) (player-max (state-turn state)))
	  (dolist (temp-state search best)
	   (print "searching:")
	   (print temp-state)
	   (setf score (alpha-beta temp-state 0 max-depth player-max #'moves #'game-overp #'evaluate *min-wins* *max-wins*))
	    (if (> score best-score)
            (progn (setf best temp-state) (setf best-score score))
        ) 			
	  
      )
          
 )
 )
 )
 


#|                                               MANCALA-GAME

This project mainly deals with developing a heuristic function for the Mancala-game that performs well compared to the normal score function 
given in the assignment. The game is played using the three functions 'computer-make-move', 'alpha-beta' and 'evaluate'. 'Computer-make-move'
function makes the move for the 'computer's turn'. It takes 'current state' and 'max-depth' as parameters and then collects all the child 
states for the current state from the 'moves' function. It then calls 'alpha-beta' function on each of these states and takes the move that
gives it the best-score. I also added a 'shuffle' function to shuffle the states recieved from 'moves' function as it would help us run the program
multiple times to know who is a better player. 

'alpha-beta' function recursively traverses until the 'max-depth' is reached or 'game has reached a final state' and then evaluates on each
of the states. In case there is a chance of pruning in the tree, this function takes care of this, so that we need not traverse through all the
states. 'alpha-beta' is basically 'Min-max' algorithm with a pruning technique added.   
									   
'Evaluate' is the key function for the program. It is a 'heuristic' function that evaluates the states at maximun-depth or a 'game-over' state.
This function evaluates a state and returns a value which indicates whether a state is advantageous to 'max-player' or 'min-player' when two state
evaluations are compared. In my 'evaluate' function, I am taking in to consideration various rules of 'Mancala'. The 'final-score' The function
returns is a weighted sum of all the factors taken into account. These factors are weighed depending on how they effect the 'ax-player's' game.
The factors I took into consideration are, 'big-win-rule', 'go-again-rule', score, 'distance-from-win-for-a-player'.

As 'big-win-rule' helps the player gain more points it is essential to take it into consideration. Here, I am calculating for a state, the total
possible-big-win-stones-for-a-player and I am finding the difference of them (between 'Max-player' and 'Min-player'). As this tells who has more
advantage in this state. 

'go-again-rule' also increases the probability of a player maximizing his score, so it is a necessarry factor for a player's win. Here I am finding
the maximum possible 'go-again' points for each player and finding their difference (between 'Max-player' and 'Min-player').

'Score' which is the difference of the Mancala-pits between 'Max-player' and 'Min-player' indicates who is currently in lead. 'Distance-from-win-for-a-player' 
lets us know what is the minimum distance for his win. The 'Score' value could be same for different states but 'Distance-from-win-for-a-player' changes, 
which helps us get better idea of who is closer to win (Here we call it with respect to max-player and see how far the max-player is from win. I considered 
various weights and observed the 'game'. These values would give better results according to my analysis.  

|#



(in-package :cl-user)