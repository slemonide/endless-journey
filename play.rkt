#lang racket
(require 2htdp/image)

;; =============================================================================
;; Data structures:

;; Node is one of:
;; - false
;; - (node Node Node Node Node)
(struct node (n s e w u d) #:mutable #:transparent)
;; interp. a node with its neighbours
;;  n is the node to the North
;;  s is the node to the South
;;  e is the node to the East
;;  u is the node above
;;  d is the node below
;;  false means no node
;; INVARIANT: if node1 refers to node2,
;;            then node2 refers to node1

(define NO-NODE #f)

(define LONELY-NODE (node #f #f #f #f #f #f))

;
; 1 - 2
;
(define NODE2
  (shared ([-1- (node #f #f -2- #f #f #f)]
           [-2- (node #f #f #f -2- #f #f)])
    -1-))

;
; 1 - 2
; |   |
; 3 - 4
;
(define NODE-SQUARE0
  (shared ([-1- (node #f -3- -2- #f #f #f)]
           [-2- (node #f -4- #f -1- #f #f)]
           [-3- (node -1- #f #f -2- #f #f)]
           [-4- (node -2- #f #f -3- #f #f)])
    -1-))
(define NODE-SQUARE
  (shared ([-1- (node #f -3- -2- #f #f #f)]
           [-2- (node #f -4- #f -1- #f #f)]
           [-3- (node -1- #f -4- #f #f #f)]
           [-4- (node -2- #f #f -3- #f #f)])
    -1-))

;     5
;     |
; 4 - 1 - 2
;     |
;     3
(define NODE-CROSS
  (shared ([-1- (node -5- -3- -2- -4- #f #f)]
           [-2- (node #f  #f  #f  -1- #f #f)]
           [-3- (node -1- #f  #f  #f  #f #f)]
           [-4- (node #f  #f  -1- #f  #f #f)]
           [-5- (node #f  -1- #f  #f  #f #f)]) -1-))

;
;         7 - 8 - 9
;         |       |
;     5 - 6       10
;     |   |
; 4 - 1 - 2
;     |
;     3
(define NODE-HARD
  ;;                    ^   v    >   <  u  d
  (shared ([-1-  (node -5- -3-  -2- -4- #f #f)]
           [-2-  (node -6-  #f  #f  -1- #f #f)]
           [-3-  (node -1- #f   #f  #f  #f #f)]
           [-4-  (node #f  #f   -1- #f  #f #f)]
           [-5-  (node #f  -1-  -6- #f  #f #f)]
           [-6-  (node -7- -2-  #f  -5- #f #f)]
           [-7-  (node #f  -6-  -8-  #f #f #f)]
           [-8-  (node #f  #f   -9- -7- #f #f)]
           [-9-  (node #f  -10- #f  -8- #f #f)]
           [-10- (node -9- #f   #f  #f  #f #f)]) -1-))

;
; - 1 - 2       5 - 1 -
;       |       |
;       3 - 6 - 4
;
(define NODE-INFINITE1
  ;;                  ^   v  >  <  u  d
  (shared ([-1- (node #f #f -2- -5- #f #f)]
           [-2- (node #f -3- #f -1- #f #f)]
           [-3- (node -2- #f -6- #f #f #f)]
           [-4- (node -5- #f #f -6- #f #f)]
           [-5- (node #f -4- -1- #f #f #f)]
           [-6- (node #f #f -4- -3- #f #f)])
    -1-))

;
;     1       3
;     |       |
; 5 - 7 - 6 - 5 - 7
;     |       |
;     8       4
;     |       |
; 3 - 1 - 2 - 3 - 1
;     |       |
;     7       5
;
(define NODE-INFINITE2
  ;;                   ^   v   >   <  u  d
  (shared ([-1- (node -8- -7- -2- -3- #f #f)]
           [-2- (node #f  #f  -3- -1- #f #f)]
           [-3- (node -4- -5- -1- -2- #f #f)]
           [-4- (node -5- -3- #f  #f  #f #f)]
           [-5- (node -3- -4- -7- -6- #f #f)]
           [-6- (node #f  #f  -5- -7- #f #f)]
           [-7- (node -1- -8- -6- -5- #f #f)]
           [-8- (node -7- -1- #f  #f  #f #f)])
    -1-))

;
;     1       3
;     |       |
; 5 - 7 - 6 - 5 - 7
;             |
;             4
;             |
; 3 - 1 - 2 - 3 - 1
;     |       |
;     7 - 6 - 5
;
(define NODE-INFINITE3
  ;;                   ^   v   >   <  u  d
  (shared ([-1- (node #f  -7- -2- -3- #f #f)]
           [-2- (node #f  #f  -3- -1- #f #f)]
           [-3- (node -4- -5- -1- -2- #f #f)]
           [-4- (node -5- -3- #f  #f  #f #f)]
           [-5- (node -3- -4- -7- -6- #f #f)]
           [-6- (node #f  #f  -5- -7- #f #f)]
           [-7- (node -1- #f  -6- -5- #f #f)])
    -1-))

;; Container is one of:
;;  - false
;;  - (container Container Container Container Container Thing)
(struct container node (contents) #:mutable #:transparent)
;; interp. a containter is a node with contents
;;   - false means no container
;; INVARIANT: all items in Container refer to the Container

;; Thing is (thing Image Container)
(struct thing (img container) #:mutable #:transparent)
;; interp. anything that can be rendered and placed in a node

;; Cube is (material Image Container)
(struct cube thing () #:transparent)
;; interp. a cube occupies a whole node, it is not transparent
;; if @ is a player and # is a node, then, when viewed from the top:
;;   - rows smaller than -2 are not rendered
;;   - rows greater than  1 are rendered as black squares
;;   - for row -2, no shaders are applied
;;   - for row -1, one shade is applied
;;   - for row  0, two shades are applied
;;   - for row  1, three shades are applied

;; -4 #######################################
;; -3 #                               #######
;; -2 ##                            ###     #<- |
;; -1 ###   @                       |       #   | view range
;;  0 ##########                 ############   |
;;  1 ###############        ################<- |
;;  2 #################    ##################
;;  3 #######################################

;; Entity is (entity Image Container)
(struct entity thing () #:transparent)
;; interp. an entity does not occupy a whole cube, can be transparent
;; on the following illustration, @, H, | and _ are entities
;; only entities from row -2 to row 2 are rendered, and they are not shaded
;; higher entities overlap lower entities

;; -3 #H#####################################
;; -2 #H                              #######
;; -1 ##                            ###     #
;;  0 ###   @                       |  _    #
;;  1 ##########                /######H#####
;;  2 ###############H      /##########H#####
;;  3 #################\  /##########     ###
;;  4 #######################################

;;

;; Player is (player Image Container)
(struct player entity ())
;; interp. a player

;; Utilities:

;; Pos is (pos Number Number)
(struct pos (x y) #:transparent)
;; interp. a point in 2d-space, with origin at the upper left corer of the
;; screen, x and y are measured in the chunks of SIZE pixels

(define P1 (pos 0 0)) ;upper left corner
(define P2 (pos pi -2.123))

;; =============================================================================
;; Constants:

;; Game consants:
(define SIZE 32)
(define SHADE (square SIZE 120 "black"))
(define NODE-IMG (square SIZE "solid" "blue"))

;; Window constants:
;; HEIGHT and WIDTH are measured in chunks of SIZE pixels
(define HEIGHT (quotient 600 SIZE))
(define WIDTH (quotient 800 SIZE))
;(define MTS (empty-scene (* WIDTH SIZE) (* HEIGHT SIZE)))
(define MTS (rectangle (* WIDTH SIZE) (* HEIGHT SIZE) 0 "white"))

(define CTR (pos (quotient WIDTH 2) (quotient HEIGHT 2)))

;; Images
(define PLAYER-IMG (bitmap "graphics/player.png"))
(define WALL-IMG (bitmap "graphics/wall.png"))
(define LADDER-IMG (bitmap "graphics/ladder.png"))
(define URAMP-LEFT (bitmap "graphics/ramp-left.png"))
(define URAMP-DOWN (rotate 90 URAMP-LEFT))
(define URAMP-RIGHT (rotate 90 URAMP-DOWN))
(define URAMP-UP (rotate 90 URAMP-RIGHT))
(define DRAMP-RIGHT (bitmap "graphics/ramp-down.png"))
(define DRAMP-UP (rotate 90 DRAMP-RIGHT))
(define DRAMP-LEFT (rotate 90 DRAMP-UP))
(define DRAMP-DOWN (rotate 90 DRAMP-LEFT))

#;
(let ([W WALL-IMG]
      [F FLOOR-MIDDLE-IMG]
      [P (overlay PLAYER-IMG DRAMP-RIGHT)]
      [K (overlay PLAYER-IMG FLOOR-MIDDLE-IMG)]
      [U URAMP-LEFT]
      [R DRAMP-RIGHT]
      [A DRAMP-UP]
      [L DRAMP-LEFT]
      [D DRAMP-DOWN]
      [B FLOOR-BOTTOM-IMG]
      [T FLOOR-TOP-IMG]
      [E (overlay LADDER-IMG FLOOR-MIDDLE-IMG)]
      [H (overlay PLAYER-IMG FLOOR-TOP-IMG)]
      [h (overlay PLAYER-IMG FLOOR-BOTTOM-IMG)])
  (above (beside W W W W W W W W W W W W W)
         (beside W E W F F F W K F F F F W)
         (beside W F W F K F W F F F F F W)
         (beside W F F F F F W F F P B h W)
         (beside T T T H F F W F F F F F W)
         (beside T U F F F F F F F F F F W)
         (beside W W W W F F W W W W W W W)
         (beside W F F F F F F F F F K F W)
         (beside W F F F F F F F D D F F W)
         (beside W F F F F F F R B B L F W)
         (beside W K F F F F F R B B L F W)
         (beside W F F F F F F F A A F F W)
         (beside W W W W W W W W W W W W W)))

#;
(let ([S (square 32 120 "black")]
        [L (square 32 120 "white")]) (beside (overlay S S S WALL-IMG)
                                             (overlay S S WALL-IMG)
                                             (overlay S WALL-IMG)
                                             WALL-IMG
                                             (overlay L WALL-IMG)
                                             (overlay L L WALL-IMG)
                                             (overlay L L L WALL-IMG)))
#;
(let ([P PLAYER-IMG]
        [I WALL-IMG]
        [S (square 32 120 "black")]
        [L (square 32 120 "white")])
    (above
     (beside I I I I I I I)
     (beside I
             (overlay S P S S I)
             (overlay S P S I)
             (overlay P S I)
             (overlay P L I)
             (overlay L P L I)
             I)
    (beside I I I I I I I)))
#;
(let ([S (square 32 120 "black")]
      [L (square 32 120 "white")]) (beside (overlay S S S WALL-IMG)
                                           (overlay S S WALL-IMG)
                                           (overlay S WALL-IMG)
                                           (overlay L WALL-IMG)
                                           (overlay L L WALL-IMG)
                                           (overlay L L L WALL-IMG)))
#;
(let ([P PLAYER-IMG]
        [I WALL-IMG]
        [S (square 32 120 "black")]
        [L (square 32 120 "white")])
    (above
     (beside I I I I I I I)
     (beside I
             (overlay P S S S I)
             (overlay P S S I)
             (overlay P S I)
             (overlay P L I)
             (overlay P L L I)
             I)
    (beside I I I I I I I)))
#;
(let ([P PLAYER-IMG]
        [I WALL-IMG]
        [S (square 32 120 "black")]
        [L (square 32 120 "white")])
    (let ([A (overlay S I)]          ; -1
          [B (overlay S S I)]        ;  0
          [C (overlay S S S I)])     ;  1
          (above
           (beside I I I I I I I I I I I I I I)
           (beside I C C C C C I B B B B A A I)
           (beside I C C C C C I B B B B A A I)
           (beside I C C C C C I B B B B A A I)
           (beside I C C C C C C B B B B A A I)
           (beside I I C C I I I I I I I I I I))))


;; =============================================================================
;; Examples:
(define SIMPLE-ROOM ;        ^  v   >   <   u    d
  (shared ([-1-  (container #f #f -2-  #f  #f   #f (cube WALL-IMG -1-))]
           [-2-  (container #f #f  #f -1- -3-   #f (cube WALL-IMG -1-))]
           [-3-  (container #f #f -4-  #f  #f  -2- (cube WALL-IMG -1-))]
           [-4-  (container #f #f  #f -3- -5-   #f (cube WALL-IMG -1-))]
           [-5-  (container #f #f -6-  #f  #f  -4- (cube WALL-IMG -1-))]
           [-6-  (container #f #f -7- -5- -11-  #f (cube WALL-IMG -1-))]
           [-7-  (container #f #f -8- -6-  #f   #f (cube WALL-IMG -1-))]
           [-8-  (container #f #f  #f -7- -9-   #f (cube WALL-IMG -1-))]
           [-9-  (container #f #f  #f  #f -10- -8- (cube WALL-IMG -1-))]
           [-10- (container #f #f  #f  #f  #f  -9- (cube WALL-IMG -1-))]
           [-11- (container #f #f  #f  #f  #f  -6- (player PLAYER-IMG -1-))])
    -11-))

;; =============================================================================
;; Functions:

;; Node -> Image
;; render the whole node network, ignoring the nodes above/below

;; template as function composition
(define (render-node n)
  (define set-of-points (node->sop n))
  (define (sop->img sop)
    (cond [(set-empty? sop) MTS]
          [else
           (define fst (set-first sop))
           (define x (* SIZE (pos-x fst)))
           (define y (* SIZE (pos-y fst)))
           (place-image NODE-IMG
                        x y
                        (sop->img (set-rest sop)))]))
  (sop->img set-of-points))

;; Node -> Image
;; render the whole node network, including some nodes above/below

;; template as function composition
(define (render-node+ n)
  (define above (render-node (node-u n)))
  (define middle (render-node n))
  (define below (render-node (node-d n)))
  (define S (rectangle (image-width above)
                       (image-height above)
                       120
                       "black"))
  (overlay above S middle S below))

;; Node -> (setof Pos)
;; produce positions of all nodes, placing node at the center of the screen
(define (node->sop n)
  ;; template as full graph traversal + visited,
  ;; result so far and compound todo accumulators
  ;; all encapsulated
  
  ;; todo is (setof WLE); a worklist accumulator
  ;; rsf is (setof Pos); result so far accumulator
  (local [;; Path is Pos
          ;; interp. a path to a node from the central node
          ;; it is represented as a displacement vector

          (struct wle (node path))
          ;; WLE (Worklist Entry) is (wle Node Path)
          ;; interp. a node to check and a path leading to it
          
          (define (node->lop n path todo rsf)
            (cond [(or (false? n)
                       (set-member? rsf path))
                   (node->lop/todo todo rsf)]
                  [else
                   (define brothers (list (wle (node-n n) (move-pos path  0 -1))
                                          (wle (node-s n) (move-pos path  0  1))
                                          (wle (node-e n) (move-pos path  1  0))
                                          (wle (node-w n) (move-pos path -1  0))))
                   (define on-screen (filter (λ(x) (pos-on-screen? (wle-path x))) brothers))
                   (node->lop/todo (append todo on-screen)
                                   (set-add rsf path))]))

          (define (node->lop/todo todo rsf)
            (cond [(empty? todo) rsf]
                  [else
                   (define fst (first todo))
                   (define node (wle-node fst))
                   (define path (wle-path fst))
                   (node->lop node path (rest todo) rsf)]))]
    (node->lop n CTR (list) (set))))

;; =============================================================================
;; Utilities:

;; Pos Number Number -> Pos
;; move the position by dx, dy
(define (move-pos p dx dy)
  (pos (+ (pos-x p) dx)
       (+ (pos-y p) dy)))

;; Pos -> Boolean
;; produce true if the node is on the screen, false otherwise
(define (pos-on-screen? p)
  (define x (pos-x p))
  (define y (pos-y p))
  (and (<= 0 x WIDTH)
       (<= 0 y HEIGHT)))

;; =============================================================================
;; Tests:

(module+ test
  (require rackunit)

  (define single-node (node #f #f #f
                            #f #f #f))
  (define node-on-node 
    (shared ([-1- (node #f  #f  #f  #f  #f  -2-)]
             [-2- (node #f  #f  #f  #f  -1- #f)]) -1-))
  (define node-under-node 
    (shared ([-1- (node #f  #f  #f  #f  -2- #f)]
             [-2- (node #f  #f  #f  #f  #f  -1-)]) -1-))
  (define nodes+
    (shared ([-1- (node -5- -3- -2- -4- #f #f)]
             [-2- (node #f  #f  #f  -1- #f #f)]
             [-3- (node -1- #f  #f  #f  #f #f)]
             [-4- (node #f  #f  -1- #f  #f #f)]
             [-5- (node #f  -1- #f  #f  #f #f)]) -1-))

  ;; testing functions
  (check-equal? (render-node #f) MTS)
  #;#;#;#;
  (check-equal? (render-node single-node)
                (overlay NODE-IMG MTS))
  (check-equal? (render-node node-on-node)
                (overlay NODE-IMG MTS))
  (check-equal? (render-node node-under-node)
                (overlay NODE-IMG MTS))
  (check-equal? (render-node nodes+)
                (overlay (beside NODE-IMG
                                 (above NODE-IMG NODE-IMG NODE-IMG)
                                 NODE-IMG)
                         MTS))
  ;(check-equal? (render-node NODE-SQUARE) MTS) ;; !!!
  ;(check-equal? (render-node NODE-HARD) MTS) ;; !!!

  (check-equal? (node->sop single-node)     (set CTR))
  (check-equal? (node->sop node-on-node)    (set CTR))
  (check-equal? (node->sop node-under-node) (set CTR))
  (check-equal? (node->sop nodes+) (set (move-pos CTR 0 -1)
                                        (move-pos CTR 0 +1)
                                        (move-pos CTR +1 0)
                                        (move-pos CTR -1 0)
                                        CTR))
  (check-equal? (node->sop NODE-SQUARE) (set CTR
                                             (move-pos CTR 1 0)
                                             (move-pos CTR 1 1)
                                             (move-pos CTR 0 1)))
  (check-equal? (node->sop NODE-HARD) (set CTR
                                           (move-pos CTR  0 -1)
                                           (move-pos CTR  0 +1)
                                           (move-pos CTR +1  0)
                                           (move-pos CTR -1  0)
                                           (move-pos CTR +1 -1)
                                           (move-pos CTR +1 -2)
                                           (move-pos CTR +2 -2)
                                           (move-pos CTR +3 -2)
                                           (move-pos CTR +3 -1)))
  
  ;; testing utilities
  (check-equal? (move-pos (pos 23   40) 0     0) (pos 23           40))
  (check-equal? (move-pos (pos 55   40) -23   0) (pos (+ 55 -23)   40))
  (check-equal? (move-pos (pos 0   -40) 0   -43) (pos 0            (+ -40 -43)))
  (check-equal? (move-pos (pos 1000 40) 9.2  pi) (pos (+ 1000 9.2) (+ pi 40)))

  (check-equal? (pos-on-screen? (pos 0     0))            #t)
  (check-equal? (pos-on-screen? (pos 0     HEIGHT))       #t)
  (check-equal? (pos-on-screen? (pos WIDTH 0))            #t)
  (check-equal? (pos-on-screen? (pos WIDTH HEIGHT))       #t)
  (check-equal? (pos-on-screen? (pos 1     HEIGHT))       #t)
  (check-equal? (pos-on-screen? (pos WIDTH 2))            #t)
  (check-equal? (pos-on-screen? (pos (- WIDTH 1) HEIGHT)) #t)
  (check-equal? (pos-on-screen? CTR)                      #t)
  (check-equal? (pos-on-screen? (pos -1     0))                 #f)
  (check-equal? (pos-on-screen? (pos 0 (+ 1 HEIGHT)))           #f)
  (check-equal? (pos-on-screen? (pos (+ 1 WIDTH) -1))           #f)
  (check-equal? (pos-on-screen? (pos (+ 1 WIDTH) (- HEIGHT 1))) #f)

  "all tests run")