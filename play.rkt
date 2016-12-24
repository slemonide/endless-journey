#lang racket
(require 2htdp/image)
(require 2htdp/universe)

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
;;  w is the node to the West
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

;; Thing is one of:
;;  - false
;;  - (thing Image Container)
(struct thing (img) #:transparent)
;; interp. anything that can be rendered and placed in a node
;; false means no thing

;; Cube is (material Image Container)
(struct cube thing () #:transparent)
;; interp. a cube occupies a whole node, it is not transparent
;; if @ is a player and # is a node, then, when viewed from the top:
;;   - rows smaller than -2 are not rendered
;;   - rows greater than  2 are not rendered
;;   - for row -2, no shaders are applied
;;   - for row -1, one shade is applied
;;   - for row  0, two shades are applied
;;   - for row  1, three shades are applied
;;   - for row  2, four shades are applied

;; -3 #######################################
;; -2 #                               #######
;; -1 ##                            ###     #<- |
;;  0 ###   @                       |       #   | view range
;;  1 ##########                 ############   |
;;  2 ###############        ################   |
;;  3 #################    ##################<- |
;;  4 #######################################

;; Entity is (entity Image Container)
(struct entity thing () #:transparent)
;; interp. an entity does not occupy a whole cube, can be transparent
;; on the following illustration, @, H, | and _ are entities
;; only entities from row -2 to row 2 are rendered, and they are not shaded
;; higher entities overlap lower entities

;; -3 #H#####################################
;; -2 #H                              #######
;; -1 ##                            ###     #<-| view range
;;  0 ###   @                       |  _    #  |
;;  1 ##########                /######H#####  |
;;  2 ###############H      /##########H#####  |
;;  3 #################\  /##########     ###<-|
;;  4 #######################################

;;

;; Player is (player Image Container)
(struct player entity ())
;; interp. a player

;; World is (world Image Container)
(struct world player ())
;; interp. a world with a player in the center

;; Utilities:

;; Pos is (pos Number Number Number)
(struct pos (x y z) #:transparent)
;; interp. a point in 3d-space, with origin at the upper left corer of the
;; screen, x, y, z are measured in the chunks of SIZE pixels
;; x goes to the right of the screen, y down the screen and z out of the screen

;; =============================================================================
;; Constants:

;; Rendering:
(define SIZE 32)
(define SHADE (square SIZE 120 "black"))
(define NODE-IMG (square SIZE "solid" "blue"))
;; HEIGHT and WIDTH are measured in chunks of SIZE pixels
(define HEIGHT (quotient (* 10 32) SIZE))
(define WIDTH (quotient (* 10 32) SIZE))
(define MTS (rectangle (* WIDTH SIZE) (* HEIGHT SIZE) 0 "white"))
(define CTR (pos (quotient WIDTH 2) (quotient HEIGHT 2) 0))

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

;; World Generation
(define STARTING-CONTAINER (shared ([-1- (container #f #f #f #f  #f -2- (player PLAYER-IMG))]
                                    [-2- (container #f #f #f #f -1-  #f (cube WALL-IMG))])
                             -1-))
(define BLOCK-CHANCE 2)

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
;; Functions:

;; =============================================================================
;; Universe:

;; World -> World
;; start the world with (play)
(define (play)
  (big-bang (initialize-world)    ; -> World
            (to-draw render)      ; World -> Image
            (on-key handle-key))) ; World KeyEvent -> World

;; -> World
;; produce the initial world
(define (initialize-world)
  (grow-world STARTING-CONTAINER))

;; World -> Image
;; render the world
;; !!!
(define (render w) MTS)

;; World KeyEvent -> World
;; handle key-events
;; !!!
(define (handle-key w ke) w) ;stub

;; =============================================================================
;; World Generation:

;; Container -> Container
;; grow the world until it stops fitting on the screen
;; ASSUME: given container exists (not #f)
(define (grow-world c0)
  ;; template as graph traversal with todo and visited accumulators
  ;; and a mutable rsf accumulator (c0)

  ;; todo: (listof (Container . Pos)); containers to be traversed
  ;;                                   and their positions on the screen
  ;; visited: (listof Pos); list of positions of containers already traversed
  (local [(define (grow-world cont path todo visited)
            (cond [(or (and (not (false? cont))
                            (cube? (container-contents cont)))
                       (not (pos-on-screen? path))
                       (member path visited))
                   (grow-world/todo todo visited)]
                  [(false? cont)
                   (define new-cont (build-container path))
                   (define children
                     (list (cons (node-n new-cont) (move-pos path  0 -1  0))
                           (cons (node-s new-cont) (move-pos path  0  1  0))
                           (cons (node-e new-cont) (move-pos path  1  0  0))
                           (cons (node-w new-cont) (move-pos path -1  0  0))
                           (cons (node-u new-cont) (move-pos path  0  0 -1))
                           (cons (node-d new-cont) (move-pos path  0  0  1))))
                   (grow-world/todo (append todo children) (cons path visited))]
                  [else
                   (define children
                     (list (cons (node-n cont) (move-pos path  0 -1  0))
                           (cons (node-s cont) (move-pos path  0  1  0))
                           (cons (node-e cont) (move-pos path  1  0  0))
                           (cons (node-w cont) (move-pos path -1  0  0))
                           (cons (node-u cont) (move-pos path  0  0 -1))
                           (cons (node-d cont) (move-pos path  0  0  1))))
                   (grow-world/todo (append todo children) (cons path visited))]))

          (define (grow-world/todo todo visited)
            (cond [(empty? todo) c0]
                  [else
                   (define fst (first todo))
                   (define cont (car fst))
                   (define path (cdr fst))
                   (grow-world cont path (rest todo) visited)]))

          ;; Path -> Container
          ;; put something on the place of container, connecting it to
          ;; the nearby containers and connecting them to the new container
          (define (build-container path)
            ;; template as search and random choice
            (define n-node (graph-ref c0 (move-pos path  0 -1  0)))
            (define s-node (graph-ref c0 (move-pos path  0  1  0)))
            (define e-node (graph-ref c0 (move-pos path  1  0  0)))
            (define w-node (graph-ref c0 (move-pos path -1  0  0)))
            (define u-node (graph-ref c0 (move-pos path  0  0 -1)))
            (define d-node (graph-ref c0 (move-pos path  0  0  1)))
            (define contents (if (= (random BLOCK-CHANCE) 0)
                                 (cube WALL-IMG)
                                 #f))
            (define new-node (container n-node s-node e-node
                                        w-node u-node d-node
                                        contents))
            (safe-set set-node-s! n-node new-node)
            (safe-set set-node-n! s-node new-node)
            (safe-set set-node-w! e-node new-node)
            (safe-set set-node-e! w-node new-node)
            (safe-set set-node-d! u-node new-node)
            (safe-set set-node-u! d-node new-node)
            
            new-node)

          (define (safe-set setter a b)
            (when (not (false? a))
              (setter a b)))]
    (grow-world c0 CTR (list) (list))))

;; =============================================================================
;; Rendering:

;; Container -> Image
;; render the whole container network that would fit on the screen
;; template as function composition
(define (render-container n)  
  ;; (listof (Image . Pos)) Image -> Image
  (define (lop->img sop img)
    (cond [(empty? sop) img]
          [else
           (define fst (first sop))
           (define f-img (car fst))
           (define f-pos (cdr fst))
           (define x (* SIZE (pos-x f-pos)))
           (define y (* SIZE (pos-y f-pos)))
           (place-image f-img
                        x y
                        (lop->img (rest sop) img))]))

  (define list-of-points (set->list (container->sop n)))
  (define layer1 (filter (λ (i) (=  2 (pos-z (cdr i)))) list-of-points))
  (define layer2 (filter (λ (i) (=  1 (pos-z (cdr i)))) list-of-points))
  (define layer3 (filter (λ (i) (=  0 (pos-z (cdr i)))) list-of-points))
  (define layer4 (filter (λ (i) (= -1 (pos-z (cdr i)))) list-of-points))
  (define layer5 (filter (λ (i) (= -2 (pos-z (cdr i)))) list-of-points))
  
  (define img1 (lop->img layer1 MTS))
  (define img2 (lop->img layer2 img1))
  (define img3 (lop->img layer3 img2))
  (define img4 (lop->img layer4 img3))
  (define img5 (lop->img layer5 img4))
  img5)

;; Container -> (setof (Image . Pos))
;; produce positions and textures of all nodes that can fit on the screen,
;; placing node at the center of the screen
(define (container->sop c)
  ;; template as graph traversal + visited,
  ;; result so far and compound todo accumulators
  ;; all encapsulated
  
  ;; todo is (setof WLE); a worklist accumulator
  ;; rsf is (setof (Image .Pos)); result so far accumulator
  (local [;; Path is Pos
          ;; interp. a path to a node from the central node
          ;; it is represented as a displacement vector

          (struct wle (cont path))
          ;; WLE (Worklist Entry) is (wle Node Path)
          ;; interp. a node to check and a path leading to it
          
          (define (node->lop c path todo rsf)
            (cond [(or (false? c)
                       (false? (container-contents c))
                       (not (pos-on-screen? path))
                       (set-member? (set-map rsf cdr) path))
                   (node->lop/todo todo rsf)]
                  [else
                   (define brothers (list (wle (node-n c) (move-pos path  0 -1  0))
                                          (wle (node-s c) (move-pos path  0  1  0))
                                          (wle (node-e c) (move-pos path  1  0  0))
                                          (wle (node-w c) (move-pos path -1  0  0))
                                          (wle (node-u c) (move-pos path  0  0 -1))
                                          (wle (node-d c) (move-pos path  0  0  1))))
                   (define thing (container-contents c))
                   (define t-image (apply-shades thing path))
                   (define new-result (cons t-image path))
                   (node->lop/todo (append todo brothers)
                                   (set-add rsf new-result))]))

          (define (node->lop/todo todo rsf)
            (cond [(empty? todo) rsf]
                  [else
                   (define fst (first todo))
                   (define cont (wle-cont fst))
                   (define path (wle-path fst))
                   (node->lop cont path (rest todo) rsf)]))

          ;; Thing Path -> Image
          (define (apply-shades thing path)
            (define img (thing-img thing))
            (cond [(cube? thing)
                   (define z (pos-z path))
                   (case z
                     [(-1) img]
                     [( 0) (overlay SHADE img)]
                     [( 1) (overlay SHADE SHADE img)]
                     [( 2) (overlay SHADE SHADE SHADE img)]
                     [( 3) (overlay SHADE SHADE SHADE SHADE img)]
                     [else (error "point is too far")])]
                  [else img]))]
    (node->lop c CTR (list) (set))))

;; =============================================================================
;; Utilities:

;; Pos Number Number Number -> Pos
;; move the position by dx, dy, dz
(define (move-pos p dx dy dz)
  (pos (+ (pos-x p) dx)
       (+ (pos-y p) dy)
       (+ (pos-z p) dz)))

;; Pos Pos -> Boolean
;; produce true if p1 and p2 denote the same point
(define (pos=? p1 p2)
  (and (= (pos-x p1) (pos-x p2))
       (= (pos-y p1) (pos-y p2))
       (= (pos-z p1) (pos-z p2))))

;; Pos -> Boolean
;; produce true if the node is on the screen and not further than two blocks
;; from the plane of the screen, false otherwise
(define (pos-on-screen? p)
  (define x (pos-x p))
  (define y (pos-y p))
  (define z (pos-z p))
  (and (<=  0 x WIDTH)
       (<=  0 y HEIGHT)
       (<= -1 z 3)))

;; Container Pos -> Container
;; given a central node in a network, return the node at the requested position
(define (graph-ref ctr p)
  ;; template as graph traversal + visited,
  ;; and compound todo accumulators
  ;; all encapsulated
  
  ;; todo: (listof (Container . Pos)); a worklist accumulator
  ;; visited: (listof Pos); positions already traversed
  (local [(define (fn-for-container c path todo visited)
            (cond [(or (false? c)
                       (not (pos-on-screen? path))
                       (member path visited))
                   (fn-for-todo todo visited)]
                  [(pos=? p path) c]
                  [else
                   (define brothers (list (cons (node-n c) (move-pos path  0 -1  0))
                                          (cons (node-s c) (move-pos path  0  1  0))
                                          (cons (node-e c) (move-pos path  1  0  0))
                                          (cons (node-w c) (move-pos path -1  0  0))
                                          (cons (node-u c) (move-pos path  0  0 -1))
                                          (cons (node-d c) (move-pos path  0  0  1))))
                   (fn-for-todo (append todo brothers) (cons path visited))]))

          (define (fn-for-todo todo visited)
            (cond [(empty? todo) #f]
                  [else
                   (define fst (first todo))
                   (define cont (car fst))
                   (define path (cdr fst))
                   (fn-for-container cont path (rest todo) visited)]))]
    (fn-for-container ctr CTR (list) (list))))

;; =============================================================================
;; Tests:
; CO
;  (build-container (move-pos CTR -1 0 0)) (render-container c0) -- fails
;(define c0 (container #f #f #f #f #f #f (entity PLAYER-IMG)))
;(build-container (move-pos CTR 0 0 -1))
;(render-container c0)

(module+ test
  (require rackunit)

  ;; Some Useful Constants
  (define shades ;             ^  v   >   <   u    d
    (shared ([-1-  (container #f #f -2-  #f  #f   #f (cube WALL-IMG))]
             [-2-  (container #f #f  #f -1- -3-   #f (cube WALL-IMG))]
             [-3-  (container #f #f -4-  #f  #f  -2- (cube WALL-IMG))]
             [-4-  (container #f #f  #f -3- -5-   #f (cube WALL-IMG))]
             [-5-  (container #f #f -6-  #f  #f  -4- (cube WALL-IMG))]
             [-6-  (container #f #f -7- -5- -11-  #f (cube WALL-IMG))]
             [-7-  (container #f #f -8- -6-  #f   #f (cube WALL-IMG))]
             [-8-  (container #f #f  #f -7- -9-   #f (cube WALL-IMG))]
             [-9-  (container #f #f  #f  #f -10- -8- (cube WALL-IMG))]
             [-10- (container #f #f  #f  #f  #f  -9- (cube WALL-IMG))]
             [-11- (container #f #f  #f  #f  #f  -6- (player PLAYER-IMG))])
      -11-))

  (define single-cont1 (container #f #f #f #f #f #f (cube WALL-IMG)))
  (define single-cont2 (container #f #f #f #f #f #f (entity PLAYER-IMG)))
  
  (define cont-on-cont
    (shared ([-1- (container #f  #f  #f  #f  #f  -2- (entity PLAYER-IMG))]
             [-2- (container #f  #f  #f  #f  -1- #f  (cube WALL-IMG))])
      -1-))
  (define nodes+
    (shared ([-1- (container -5- -3- -2- -4- #f #f (cube WALL-IMG))]
             [-2- (container #f  #f  #f  -1- #f #f (cube WALL-IMG))]
             [-3- (container -1- #f  #f  #f  #f #f (cube WALL-IMG))]
             [-4- (container #f  #f  -1- #f  #f #f (cube WALL-IMG))]
             [-5- (container #f  -1- #f  #f  #f #f (cube WALL-IMG))])
      -1-))

  ;; World Generation

  (check-equal? (grow-world single-cont1) single-cont1)
  (check-not-false (node-n (grow-world single-cont2)))
  (check-not-false (node-s (grow-world single-cont2)))
  (check-not-false (node-e (grow-world single-cont2)))
  (check-not-false (node-w (grow-world single-cont2)))
  (check-not-false (node-u (grow-world single-cont2)))
  (check-not-false (node-d (grow-world single-cont2)))
  (check-equal? (grow-world nodes+) nodes+)

  #|
  ;; initialize-world
  (check-true (player? (container-contents (initialize-world)))
              "central node has to be a player")
  (check-true (cube? (container-contents (node-d (initialize-world))))
              "node below player has to be a cube")
  (check-true (player? (container-contents (node-u (node-d (initialize-world)))))
              "all nodes have to be mutually referential")
  (check-true (player? (container-contents (node-d (node-u (initialize-world)))))
              "all nodes have to be mutually referential")
  (check-true (player? (container-contents (node-e (node-w (initialize-world)))))
              "all nodes have to be mutually referential")
  (check-true (player? (container-contents (node-w (node-e (initialize-world)))))
              "all nodes have to be mutually referential")
  (check-true (player? (container-contents (node-n (node-s (initialize-world)))))
              "all nodes have to be mutually referential")
  (check-true (player? (container-contents (node-s (node-n (initialize-world)))))
              "all nodes have to be mutually referential")
|#
  
  ;; Rendering

  ;; render-container
  (check-equal? (render-container single-cont1)
                (place-image (overlay SHADE WALL-IMG)
                             (* SIZE (pos-x CTR)) (* SIZE (pos-y CTR))
                             MTS))
  (check-equal? (render-container single-cont2)
                (place-image PLAYER-IMG
                             (* SIZE (pos-x CTR)) (* SIZE (pos-y CTR))
                             MTS))
  (check-equal? (render-container nodes+)
                (place-image (overlay SHADE WALL-IMG)
                             (* SIZE (pos-x CTR)) (* SIZE (pos-y CTR))
                             (place-image (overlay SHADE WALL-IMG)
                                          (* SIZE (+ (pos-x CTR) -1)) (* SIZE (pos-y CTR))
                                          (place-image (overlay SHADE WALL-IMG)
                                                       (* SIZE (+ (pos-x CTR) 1)) (* SIZE (pos-y CTR))
                                                       (place-image (overlay SHADE WALL-IMG)
                                                                    (* SIZE (pos-x CTR)) (* SIZE (+ (pos-y CTR) -1))
                                                                    (place-image (overlay SHADE WALL-IMG)
                                                                                 (* SIZE (pos-x CTR)) (* SIZE (+ (pos-y CTR) 1))
                                                                                 MTS))))))
  (check-equal? (render-container shades)
                (place-image WALL-IMG
                             (* SIZE (+ (pos-x CTR) 2)) (* SIZE (pos-y CTR))
                             (place-image (overlay SHADE SHADE WALL-IMG)
                                          (* SIZE (+ (pos-x CTR) 1)) (* SIZE (pos-y CTR))
                                          (place-image (overlay PLAYER-IMG SHADE SHADE WALL-IMG)
                                                       (* SIZE (pos-x CTR)) (* SIZE (pos-y CTR))
                                                       (place-image (overlay SHADE SHADE WALL-IMG)
                                                                    (* SIZE (+ (pos-x CTR) -1)) (* SIZE (pos-y CTR))
                                                                    (place-image (overlay SHADE SHADE SHADE WALL-IMG)
                                                                                 (* SIZE (+ (pos-x CTR) -2)) (* SIZE (pos-y CTR))
                                                                                 MTS))))))
  
  ;; container->sop
  (check-equal? (container->sop single-cont1)
                (set (cons (overlay SHADE WALL-IMG) CTR)))
  (check-equal? (container->sop single-cont2) (set (cons PLAYER-IMG CTR)))
  (check-equal? (container->sop cont-on-cont)
                (set (cons PLAYER-IMG CTR)
                     (cons (overlay SHADE SHADE WALL-IMG) (move-pos CTR 0 0 1))))
  (check-equal? (container->sop nodes+)
                (set (cons (overlay SHADE WALL-IMG) (move-pos CTR 0 -1 0))
                     (cons (overlay SHADE WALL-IMG) (move-pos CTR 0 +1 0))
                     (cons (overlay SHADE WALL-IMG) (move-pos CTR +1 0 0))
                     (cons (overlay SHADE WALL-IMG) (move-pos CTR -1 0 0))
                     (cons (overlay SHADE WALL-IMG) CTR)))
  
  ;; ================================================
  ;; testing utilities
  (check-equal? (move-pos (pos 23   40 23) 0     0 0)   (pos 23           40 23))
  (check-equal? (move-pos (pos 55   40 33) -23   0 0)   (pos (+ 55 -23)   40 33))
  (check-equal? (move-pos (pos 0   -40 -2) 0   -43 0)   (pos 0            (+ -40 -43) -2))
  (check-equal? (move-pos (pos 1000 40 pi) 9.2  pi 0)   (pos (+ 1000 9.2) (+ pi 40) pi))
  (check-equal? (move-pos (pos 23   40 23) 0     0 22)  (pos 23           40 (+ 22 23)))
  (check-equal? (move-pos (pos 55   40 33) -23   0 -2)  (pos (+ 55 -23)   40 (+ -2 33)))
  (check-equal? (move-pos (pos 0   -40 -2) 0   -43 0.1) (pos 0            (+ -40 -43) (+ 0.1 -2)))
  (check-equal? (move-pos (pos 1000 40 pi) 9.2  pi 0.5) (pos (+ 1000 9.2) (+ pi 40) (+ 0.5 pi)))

  (check-true (pos=? (pos 0 0 0) (pos 0 0 0)))
  (check-true (pos=? (pos 11 -2 3) (pos 11 -2 3)))
  (check-true (pos=? (pos 0 32 43) (pos 0 32 43)))
  (check-false (pos=? (pos -310   74 82) (pos   54   4 -456)))
  (check-false (pos=? (pos   20  460 60) (pos -564 654    2)))
  (check-false (pos=? (pos  320 -320 76) (pos  -43   2    5)))

  ;; !!! Simplify check-expects
  (check-true (pos-on-screen? (pos 0     0         0)))
  (check-true (pos-on-screen? (pos 0     HEIGHT    0)))
  (check-true (pos-on-screen? (pos WIDTH 0         0)))
  (check-true (pos-on-screen? (pos WIDTH HEIGHT    0)))
  (check-true (pos-on-screen? (pos 1     HEIGHT    0)))
  (check-true (pos-on-screen? (pos WIDTH 2         0)))
  (check-true (pos-on-screen? (pos (- WIDTH 1) HEIGHT 0)))
  (check-true (pos-on-screen? CTR))
  (check-false (pos-on-screen? (pos -1     0        0)))
  (check-false (pos-on-screen? (pos 0 (+ 1 HEIGHT)  0)))
  (check-false (pos-on-screen? (pos (+ 1 WIDTH) -1  0)))
  (check-false (pos-on-screen? (pos (+ 1 WIDTH) (- HEIGHT 1) 0)))
  (check-true (pos-on-screen? (move-pos CTR 0 0 1)))
  (check-true (pos-on-screen? (move-pos CTR 0 0 2)))
  (check-true (pos-on-screen? (move-pos CTR 0 0 -1)))
  (check-false (pos-on-screen? (move-pos CTR 0 0 -2)))
  (check-true (pos-on-screen? (move-pos CTR 0 0 3)))
  (check-false (pos-on-screen? (move-pos CTR 0 0 -3)))
  (check-false (pos-on-screen? (move-pos CTR 0 0 332)))
  (check-false (pos-on-screen? (move-pos CTR 0 0 -312)))

  (check-equal? (graph-ref single-cont1 CTR) single-cont1)
  (check-equal? (graph-ref single-cont2 CTR) single-cont2)
  (check-equal? (graph-ref nodes+ CTR) nodes+)
  (check-equal? (graph-ref nodes+ (move-pos CTR  1  0 0)) (node-e nodes+))
  (check-equal? (graph-ref nodes+ (move-pos CTR  1  3 0)) #f)
  (check-equal? (graph-ref nodes+ (move-pos CTR -1  0 0)) (node-w nodes+))
  (check-equal? (graph-ref nodes+ (move-pos CTR  0  1 0)) (node-s nodes+))
  (check-equal? (graph-ref nodes+ (move-pos CTR  0 -1 0)) (node-n nodes+))
  (check-equal? (graph-ref nodes+ (move-pos CTR  1  1 0)) #f)
  
  "all tests run")