(ql:quickload "cl-utilities" :silent t)
(ql:quickload "str" :silent t)
(ql:quickload "iterate" :silent t)
(ql:quickload "bt-semaphore" :silent t)

(defpackage #:tech.corder.aoc2022
  (:use :cl :cl-utilities :iterate))

(in-package #:tech.corder.aoc2022)

(defun read-input-file (filename)
  (with-open-file (file (concatenate 'string "input/" filename))
    (loop for line = (read-line file nil)
          while line
          collect line)))

(defun take (n seq) (subseq seq 0 n))

(defun sum (seq) (apply #'+ seq))

(defun day01 ()
  (let ((elves
          (loop for group in (split-sequence-if
                              #'str:empty?
                              (read-input-file "day01.txt"))
                collect (mapcar #'parse-integer group))))
    (labels
        ((part1 () (loop for elf in elves maximize (sum elf)))
         (part2 () (sum (take 3 (sort (mapcar #'sum elves) #'>)))))
      (cons (part1) (part2)))))

(defun day02 ()
  (let ((rounds
          (loop for round in (read-input-file "day02.txt")
                collect (let* ((line (split-sequence #\Space round))
                               (them (first line))
                               (us (second line)))
                          (cons (str:string-case them
                                  ("A" 'rock) ("B" 'paper) ("C" 'scissors))
                                (str::string-case us
                                  ("X" 'x) ("Y" 'y) ("Z" 'z))))))
        (move-scores '((rock . 1) (paper . 2) (scissors . 3)))
        (result-scores '((win . 6) (draw . 3) (lose . 0))))
    (labels ((result (them us)
               (cond
                 ((eq them us) 'draw)
                 ((eq them 'rock) (if (eq us 'paper) 'win 'lose))
                 ((eq them 'paper) (if (eq us 'scissors) 'win 'lose))
                 ((eq them 'scissors) (if (eq us 'rock) 'win 'lose))))
             (score (move result)
               (+ (cdr (assoc move move-scores))
                  (cdr (assoc result result-scores))))
             (xyz->rps (xyz) (case xyz (x 'rock) (y 'paper) (z 'scissors)))
             (part1 ()
               (let ((rounds (mapcar
                              (lambda (round)
                                (cons (car round) (xyz->rps (cdr round))))
                              rounds)))
                 (loop for round in rounds
                       sum (let* ((them (car round))
                                  (us (cdr round))
                                  (result (result them us)))
                             (score us result)))))
             (get-move (them result)
               (case result
                 (draw them)
                 (win (case them
                        (rock 'paper)
                        (paper 'scissors)
                        (scissors 'rock)))
                 (lose (case them
                         (rock 'scissors)
                         (paper 'rock)
                         (scissors 'paper)))))
             (xyz->ldw (xyz)
               (case xyz (x 'lose) (y 'draw) (z 'win)))
             (part2 ()
               (let ((rounds (mapcar
                              (lambda (round)
                                (cons (car round) (xyz->ldw (cdr round))))
                              rounds)))
                 (loop for round in rounds
                       sum (let* ((them (car round))
                                  (result (cdr round))
                                  (us (get-move them result)))
                             (score us result))))))
      (cons (part1) (part2)))))

(defun string-intersection (a b &optional acc)
  (let ((a (sort a #'char<))
        (b (sort b #'char<)))
    (if (or (= (length a) 0) (= (length b) 0))
        (nreverse acc)
        (let ((a0 (char a 0))
              (b0 (char b 0))
              (a1 (subseq a 1))
              (b1 (subseq b 1)))
          (cond
            ((char= a0 b0)
             (string-intersection
              a1 b1 (if (and acc (char= a0 (car acc)))
                        acc
                        (cons a0 acc))))
            ((char< a0 b0) (string-intersection a1 b acc))
            ((char> a0 b0) (string-intersection a b1 acc)))))))

(defun group (n list &optional acc)
  (cond
    ((null list) (nreverse acc))
    ((<= (length list) n) (nreverse (cons list acc)))
    (t (group n (subseq list n) (cons (subseq list 0 n) acc)))))

(defun day03 ()
  (let ((rucksacks (read-input-file "day03.txt")))
    (labels
        ((priority (char)
           (cond
             ((char<= #\a char #\z) (1+ (- (char-code char) (char-code #\a))))
             ((char<= #\A char #\Z) (1+ (+ 26 (- (char-code char) (char-code #\A)))))))
         (part1 ()
           (let ((sacks
                   (mapcar
                    (lambda (sack)
                      (let* ((length (length sack))
                             (split (/ length 2)))
                        (cons (subseq sack 0 split)
                              (subseq sack split))))
                    rucksacks)))
             (loop for sack in sacks
                   sum (let ((int (string-intersection (car sack) (cdr sack))))
                         (priority (car int))))))
         (part2 ()
           (loop for group in (group 3 rucksacks)
                 sum (priority
                      (car (string-intersection
                            (car group)
                            (coerce
                             (string-intersection
                              (cadr group)
                              (caddr group))
                             'string)))))))
      (cons (part1) (part2)))))

(defun day04 ()
  (let ((assignments
          (mapcar
           (lambda (line)
             (mapcar (lambda (a) (mapcar #'parse-integer (str:split #\- a)))
                     (str:split #\, line)))
           (read-input-file "day04.txt"))))
    (labels
        ((fully-contains? (s1 s2)
           (or (and (<= (first s1) (first s2)) (<= (second s2) (second s1)))
               (and (<= (first s2) (first s1)) (<= (second s1) (second s2)))))
         (overlap? (s1 s2)
           (or (and (<= (first s1) (first s2)) (<= (first s2) (second s1)))
               (and (<= (first s2) (first s1)) (<= (first s1) (second s2)))))
         (part1 ()
           (loop for pair in assignments
                 count (fully-contains? (first pair) (second pair))))
         (part2 ()
           (loop for pair in assignments
                 count (overlap? (first pair) (second pair)))))
      (cons (part1) (part2)))))

(defun transpose (seq)
  (loop for i from 0 below (length (first seq))
        collect (loop for s in seq
                      collect (elt s i))))

(defun day05 ()
  (let* ((input (read-input-file "day05.txt"))
         (crates-string
           (let ((crates))
             (loop for line in input
                   do (if (/= 0 (length line))
                          (push line crates)
                          (return crates)))))
         (instructions (loop for line in input
                             if (str:starts-with? "move" line)
                               collect line))
         (crate-lines (subseq crates-string 1)))
    (labels ((parse-crates ()
               (mapcar
                (lambda (line) (remove-if (lambda (c) (char= #\_ c)) line))
                (mapcar #'nreverse
                        (transpose
                         (loop for line in crate-lines
                               collect (mapcar (lambda (crate) (elt crate 1))
                                               (str:split " "
                                                          (str:replace-all
                                                           "    "
                                                           " [_]"
                                                           line))))))))
             (parse-instruction (instruction)
               (destructuring-bind
                   (move count from source to destination)
                   (str:split " " instruction)
                 (declare (ignore move from to))
                 (mapcar #'parse-integer (list count source destination))))
             (get-answer-string (crates)
               (coerce (loop for stack in crates collect (first stack)) 'string))
             (part1 ()
               (let ((crates (parse-crates)))
                 (loop for instruction in instructions
                       do (destructuring-bind
                              (count src dst)
                              (parse-instruction instruction)
                            (loop for i from 0 below count
                                  do (push (pop (elt crates (1- src)))
                                           (elt crates (1- dst))))))
                 (get-answer-string crates)))
             (part2 ()
               (let ((crates (parse-crates)))
                 (loop for instruction in instructions
                       do (destructuring-bind
                              (count src dst)
                              (parse-instruction instruction)
                            (let ((acc))
                              (loop for i from 0 below count
                                    do (push (pop (elt crates (1- src))) acc))
                              (loop for i from 0 below count
                                    do (push (pop acc) (elt crates (1- dst)))))))
                 (get-answer-string crates))))
      (cons (part1) (part2)))))

(defun day06 ()
  (let ((input (first (read-input-file "day06.txt"))))
    (labels ((all-unique? (str)
               (let ((str (sort str #'char<)))
                 (loop for c across str
                       for i from 1 below (length str)
                       when (char= c (char str i))
                         do (return nil)
                       finally (return t))))
             (find-first-unique-substring (n string)
               (loop for i from n to (length string)
                     do (let ((substring (str:substring (- i n) i string)))
                          (when (all-unique? substring) (return i)))))
             (part1 () (find-first-unique-substring 4 input))
             (part2 () (find-first-unique-substring 14 input)))
      (cons (part1) (part2)))))

(defclass dir ()
  ((name :initarg :name :initform "" :accessor dir-name)
   (children :initarg :children :initform nil :accessor dir-children)
   (parent :initarg :parent :initform nil :accessor dir-parent)
   (files :initform nil :accessor dir-files)
   (size :initform 0 :accessor dir-size)))

(defun make-dir (&key name children parent)
  (make-instance 'dir :name name :children children :parent parent))

(defun day07 ()
  (let* ((input (read-input-file "day07.txt"))
         (parsed-commands
           (loop :for command :in input
                 :with temp := nil
                 :with parsed-commands := nil
                 :do (if (char= #\$ (char command 0))
                         (let ((command (rest (str:split #\Space command))))
                           (when temp
                             (push (nreverse temp) (car parsed-commands))
                             (setf (car parsed-commands) (nreverse (car parsed-commands)))
                             (setf temp nil))
                           (push command parsed-commands))
                         (push (str:split #\Space command) temp))
                 :finally (return (nreverse parsed-commands))))
         (dir-tree
           (loop :for command :in parsed-commands
                 :with root := (make-dir :name "/" :children nil :parent nil)
                 :with cwd := root
                 :do (str:string-case (first command)
                       ("ls" (loop :for output :in (second command)
                                   :do (unless (string= "dir" (first output))
                                         (push (cons (second output) (first output))
                                               (dir-files cwd)))))
                       ("cd" (let ((target (second command)))
                               (str:string-case target
                                 ("/" (setf cwd root))
                                 (".." (setf cwd (dir-parent cwd)))
                                 (:otherwise
                                  (let ((target-dir
                                          (first
                                           (member target
                                                   (dir-children cwd)
                                                   :test
                                                   (lambda (a b)
                                                     (string= a (dir-name b)))))))
                                    (when (null target-dir)
                                      (setf target-dir
                                            (make-dir :name target
                                                      :children nil
                                                      :parent cwd))
                                      (push target-dir (dir-children cwd)))
                                    (setf cwd target-dir)))))))
                 :finally (return root))))
    (labels
        ((calculate-sizes (dir)
           (let ((immediate-size (sum (mapcar #'(lambda (file) (parse-integer (cdr file)))
                                              (dir-files dir))))
                 (children-size (sum (mapcar #'calculate-sizes (dir-children dir)))))
             (setf (dir-size dir) (+ immediate-size children-size))))
         (find-dir-if (root test)
           (let ((child-results
                   (loop :for child :in (dir-children root)
                         :append (find-dir-if child test))))
             (if (funcall test root)
                 (cons root child-results)
                 child-results)))
         (part1 ()
           (calculate-sizes dir-tree)
           (sum (mapcar #'dir-size
                        (find-dir-if dir-tree
                                     #'(lambda (dir) (<= (dir-size dir) 100000))))))
         (part2 ()
           (calculate-sizes dir-tree)
           (let* ((total-space 70000000)
                  (needed-free-space 30000000)
                  (used-space (dir-size dir-tree))
                  (unused-space (- total-space used-space))
                  (space-to-clear (- needed-free-space unused-space))
                  (candidate-dirs (find-dir-if dir-tree
                                               #'(lambda (dir) (>= (dir-size dir) space-to-clear)))))
             (dir-size (car (sort candidate-dirs
                                  #'(lambda (dir-a dir-b)
                                      (< (dir-size dir-a) (dir-size dir-b)))))))))
      (cons (part1) (part2)))))

(defclass tree ()
  ((height :initarg :height :accessor tree-height :initform (error "no height"))
   (visible? :initform nil :accessor tree-visible?)))

(defun make-tree (height)
  (make-instance 'tree :height height))

(defmethod print-object ((obj tree) stream)
  (print-unreadable-object (obj stream :type nil :identity nil)
    (format stream "~a~a~a"
            (if (tree-visible? obj) " " "(")
            (tree-height obj)
            (if (tree-visible? obj) " " ")"))))

(defun day08 ()
  (let* ((input (read-input-file "day08.txt"))
         (trees (loop :for line :in input
                      :collect (loop :for tree :across line
                                     :collect (make-tree (digit-char-p tree))))))
    (labels ((from-left? (tree row col)
               (loop :for i :from 0 :below col
                     :always (< (tree-height (nth i (nth row trees)))
                                (tree-height tree))))
             (from-right? (tree row col)
               (loop :for i :from (1+ col) :below (length (first trees))
                     :always (< (tree-height (nth i (nth row trees)))
                                (tree-height tree))))
             (from-top? (tree row col)
               (loop :for i :from 0 :below row
                     :always (< (tree-height (nth col (nth i trees)))
                                (tree-height tree))))
             (from-bottom? (tree row col)
               (loop :for i :from (1+ row) :below (length trees)
                     :always (< (tree-height (nth col (nth i trees)))
                                (tree-height tree))))
             (is-visible? (tree row col)
               (or (from-left? tree row col)
                   (from-right? tree row col)
                   (from-top? tree row col)
                   (from-bottom? tree row col)))
             (count-visible (trees)
               (loop :for line :in trees
                     :sum (loop :for tree :in line
                                :count (tree-visible? tree))))
             (part1 ()
               (loop :for line :in trees
                     :for row :from 0
                     :do (loop :for tree :in line
                               :for col :from 0
                               :do (setf (tree-visible? tree)
                                         (is-visible? tree row col))))
               (count-visible trees))
             (to-left (tree row col)
               (loop :for i :from (1- col) :downto 0
                     :with score := 0
                     :do (incf score)
                     :when (>= (tree-height (nth i (nth row trees)))
                               (tree-height tree))
                       :do (loop-finish)
                     :finally (return score)))
             (to-right (tree row col)
               (loop :for i :from (1+ col) :below (length (first trees))
                     :with score := 0
                     :do (incf score)
                     :when (>= (tree-height (nth i (nth row trees)))
                               (tree-height tree))
                       :do (loop-finish)
                     :finally (return score)))
             (to-top (tree row col)
               (loop :for i :from (1- row) :downto 0
                     :with score := 0
                     :do (incf score)
                     :when (>= (tree-height (nth col (nth i trees)))
                               (tree-height tree))
                       :do (loop-finish)
                     :finally (return score)))
             (to-bottom (tree row col)
               (loop :for i :from (1+ row) :below (length trees)
                     :with score := 0
                     :do (incf score)
                     :when (>= (tree-height (nth col (nth i trees)))
                               (tree-height tree))
                       :do (loop-finish)
                     :finally (return score)))
             (calculate-score (tree row col)
               (* (to-left tree row col)
                  (to-right tree row col)
                  (to-top tree row col)
                  (to-bottom tree row col)))
             (part2 ()
               (loop :for line :in trees
                     :for row from 0
                     :maximize (loop :for tree :in line
                                     :for col :from 0
                                     :maximize (calculate-score tree row col)))))
      (cons (part1) (part2)))))

(defun print-arena (x-size y-size head tail)
  (iter (for y from (1- y-size) downto 0)
    (format t "~&")
    (iter (for x from 0 below x-size)
      (format t "~a" (cond
                       ((and (= (car head) x)
                             (= (cdr head) y)) "H")
                       ((and (= (car tail) x)
                             (= (cdr tail) y)) "T")
                       (:otherwise ".")))))
  (format t "~%~%"))

(defun print-arena-visited (x-size y-size visited)
  (iter (for y from (1- y-size) downto 0)
    (format t "~&")
    (iter (for x from 0 below x-size)
      (format t "~a" (if (gethash (cons x y) visited)
                         "#"
                         "."))))
  (format t "~%~%"))

(defun day09-part1 (input)
  (macrolet ((x (thing) `(car ,thing)) (y (thing) `(cdr ,thing)))
    (let ((head (cons 0 0)) (tail (cons 0 0))
          (visited (make-hash-table :test #'equal)))
      (iter (for (dir . count) in input)
        (iter (for i from 0 below count)
          (str:string-case dir
            ("R" (incf (x head))) ("L" (decf (x head)))
            ("U" (incf (y head))) ("D" (decf (y head))))
          (let ((dx (- (x head) (x tail)))
                (dy (- (y head) (y tail))))
            (when (or (< 1 (abs dx)) (< 1 (abs dy)))
              (incf (x tail) (signum dx))
              (incf (y tail) (signum dy))))
          (setf (gethash (cons (x tail) (y tail)) visited) t)))
      (iter (for (key value) in-hashtable visited)
        (count key)))))

(defun day09-part2 (input)
  (macrolet ((x (thing) `(car ,thing)) (y (thing) `(cdr ,thing)))
    (let ((knots (make-array 10
                             :initial-contents
                             (iter (for i from 0 to 9)
                               (collect (cons 0 0)))))
          (visited (make-hash-table :test #'equal)))
      (iter (for (dir . count) in input)
        (iter (for i from 0 below count)
          (str:string-case dir
            ("R" (incf (x (aref knots 0)))) ("L" (decf (x (aref knots 0))))
            ("U" (incf (y (aref knots 0)))) ("D" (decf (y (aref knots 0)))))
          (iter (for i from 1 below (length knots))
            (let* ((current-knot (aref knots i))
                   (previous-knot (aref knots (1- i)))
                   (dx (- (x previous-knot) (x current-knot)))
                   (dy (- (y previous-knot) (y current-knot))))
              (when (or (< 1 (abs dx)) (< 1 (abs dy)))
                (incf (x current-knot) (signum dx))
                (incf (y current-knot) (signum dy))))
            (setf (gethash (cons (x (aref knots (1- (length knots))))
                                 (y (aref knots (1- (length knots)))))
                           visited)
                  t))))
      (iter (for (key val) in-hashtable visited)
        (count 1)))))

(defun day09 ()
  (let ((input (mapcar
                #'(lambda (line)
                    (let ((split (str:split #\Space line)))
                      (cons (first split)
                            (parse-integer (second split)))))
                (read-input-file "day09.txt"))))
    (labels ((part1 () (day09-part1 input))
             (part2 () (day09-part2 input)))
      (cons (part1) (part2)))))

(defun day10 ()
  (let ((input (mapcar #'(lambda (line)
                           (let ((split (str:split #\Space line)))
                             (cons (first split)
                                   (when (second split)
                                     (parse-integer (second split))))))
                       (read-input-file "day10.txt"))))
    (labels ((part1 ()
               (let ((x 1) (tick 0) (signals nil))
                 (iter (for (inst . arg) in input)
                   (incf tick)
                   (when (zerop (mod (+ 20 tick) 40))
                     (push (* tick x) signals))
                   (when (string= inst "addx")
                     (incf tick)
                     (when (zerop (mod (+ 20 tick) 40))
                       (push (* tick x) signals))
                     (incf x arg)))
                 (sum signals)))
             (part2 ()
               (let ((x 1) (tick 0) (screen (make-array 40 :initial-element #\.))
                     (output nil))
                 (labels ((print-line ()
                            (push (format nil "~&~a" (coerce screen 'string)) output))
                          (draw ()
                            (let ((position (mod tick 40)))
                              (if (> 2 (abs (- position x)))
                                  (setf (aref screen position) #\#)
                                  (setf (aref screen position) #\.))
                              (when (and (not (zerop tick)) (zerop position))
                                (print-line)))))
                   (iter (for (inst . arg) in input)
                     (draw)
                     (incf tick)
                     (when (string= inst "addx")
                       (draw) (incf tick) (incf x arg)))
                    (print-line))
                 (nreverse output))))
      (cons (part1) (part2)))))

(defclass monkey ()
  ((index :initarg :index :accessor monkey-index)
   (count :initform 0 :accessor monkey-count)
   (items :initarg :items :accessor monkey-items)
   (operation :initarg :operation :accessor monkey-operation)
   (test :initarg :test :accessor monkey-test)
   (dividend :initarg :dividend :accessor monkey-dividend)
   (true :initarg :true :accessor monkey-true)
   (false :initarg :false :accessor monkey-false)))

(defmethod print-object ((monkey monkey) stream)
  (print-unreadable-object (monkey stream :type nil :identity nil)
    (with-slots (index items)
        monkey
      (format stream "Monkey ~a: ~a" index items))))

(defun day11 (&key (debug nil))
  (labels ((split-line (line)
             (remove-if (lambda (s) (string= s ""))
                        (str:split #\Space (str:replace-all
                                            "," "" line))))
           (parse-items (items)
             (let ((split (split-line items)))
               (destructuring-bind (_ __ &rest items)
                   split
                 (declare (ignore _ __))
                 (mapcar #'parse-integer items))))
           (parse-operation (operation)
             (let ((split (split-line operation)))
               (destructuring-bind (_ new = old op operand)
                   split
                 (declare (ignore _ new = old))
                 (let ((f (str:string-case op
                            ("*" #'*)
                            ("+" #'+))))
                   (if (string= operand "old")
                       #'(lambda (val) (funcall f val val))
                       #'(lambda (val) (funcall f val (parse-integer operand))))))))
           (parse-test (test)
             (let ((split (split-line test)))
               (destructuring-bind (_ divisible by x)
                   split
                 (declare (ignore _ divisible by))
                 #'(lambda (y) (zerop (mod y (parse-integer x)))))))
           (parse-dividend (test)
             (let ((split (split-line test)))
               (parse-integer (car (last split)))))
           (parse-what-happens (what)
             (let ((split (split-line what)))
               (destructuring-bind (_ t/f throw to monkey x)
                   split
                 (declare (ignore _ t/f throw to monkey))
                 (parse-integer x))))
           (parse-input (input)
             (let* ((parsed (split-sequence "" input :test #'string=))
                    (output (make-array (length parsed) :initial-element nil)))
               (iter (for monkey in parsed) (for index from 0)
                 (destructuring-bind (header items op test true false)
                     monkey
                   (declare (ignore header))
                   (setf (aref output index)
                         (make-instance 'monkey
                                        :index index
                                        :items (parse-items items)
                                        :operation (parse-operation op)
                                        :test (parse-test test)
                                        :dividend (parse-dividend test)
                                        :true (parse-what-happens true)
                                        :false (parse-what-happens false)))))
               output)))
    (labels ((part1 ()
               (let ((monkeys (parse-input (read-input-file "day11.txt"))))
                 (iter (for round from 1 to 20)
                   (when debug (format t "~&Round ~a:" round))
                   (iter (for monkey in-vector monkeys)
                     (when debug (format t "~&  Monkey ~a:" (monkey-index monkey)))
                     (iter (for item in (monkey-items monkey))
                       (incf (monkey-count monkey))
                       (when debug (format t "~&    Monkey inspects item with level ~a." item))
                       (setf item (funcall (monkey-operation monkey) item))
                       (when debug (format t "~&      Worry level becomes ~a." item))
                       (setf item (floor (/ item 3)))
                       (when debug (format t "~&      Monkey gets bored. Level becomes ~a." item))
                       (let* ((test-result (funcall (monkey-test monkey) item))
                              (target-monkey (if test-result
                                                 (monkey-true monkey)
                                                 (monkey-false monkey))))
                         (if test-result
                             (when debug (format t "~&      Level passes. Thrown to ~a."
                                                 target-monkey))
                             (when debug (format t "~&      Level doesn't pass. Thrown to ~a."
                                                 target-monkey)))
                         (let ((new-items (append
                                           (monkey-items (aref monkeys target-monkey))
                                           (list item))))
                           (setf (monkey-items (aref monkeys target-monkey))
                                 new-items))))
                     (setf (monkey-items monkey) nil)))
                 (let ((top-two-monkeys (take 2
                                              (sort monkeys
                                                    #'(lambda (a b)
                                                        (> (monkey-count a)
                                                           (monkey-count b)))))))
                   (* (monkey-count (aref top-two-monkeys 0))
                      (monkey-count (aref top-two-monkeys 1))))))
             (part2 ()
               (let* ((monkeys (parse-input (read-input-file "day11.txt")))
                      (modulus (iter (for monkey in-vector monkeys)
                                 (multiply (monkey-dividend monkey)))))
                 (iter (for round from 1 to 10000)
                   (when (and debug (zerop (mod round 1000)))
                     (format t "~&Round ~a:" round))
                   (iter (for monkey in-vector monkeys)
                     (when debug (format t "~&  Monkey ~a:" (monkey-index monkey)))
                     (iter (for item in (monkey-items monkey))
                       (incf (monkey-count monkey))
                       (when debug (format t "~&    Monkey inspects item with level ~a." item))
                       (setf item (funcall (monkey-operation monkey) item))
                       (when debug (format t "~&      Worry level becomes ~a." item))
                       (setf item (mod item modulus))
                       (when debug (format t "~&      Monkey gets bored. Level becomes ~a." item))
                       (let* ((test-result (funcall (monkey-test monkey) item))
                              (target-monkey (if test-result
                                                 (monkey-true monkey)
                                                 (monkey-false monkey))))
                         (if test-result
                             (when debug (format t "~&      Level passes. Thrown to ~a."
                                                 target-monkey))
                             (when debug (format t "~&      Level doesn't pass. Thrown to ~a."
                                                 target-monkey)))
                         (let ((new-items (append
                                           (monkey-items (aref monkeys target-monkey))
                                           (list item))))
                           (setf (monkey-items (aref monkeys target-monkey))
                                 new-items))))
                     (setf (monkey-items monkey) nil)))
                 (let ((top-two-monkeys (take 2
                                              (sort monkeys
                                                    #'(lambda (a b)
                                                        (> (monkey-count a)
                                                           (monkey-count b)))))))
                   (* (monkey-count (aref top-two-monkeys 0))
                      (monkey-count (aref top-two-monkeys 1)))))))
      (cons (part1) (part2)))))

(defparameter *infinity* 1e10)
(defclass square ()
  ((elevation :initarg :elevation :accessor elevation)
   (distance :initform *infinity* :accessor distance)))

(defmethod print-object ((square square) stream)
  (with-slots (elevation distance) square
    (format stream "~a:~a" elevation distance)))

(defun day12 ()
  (let* ((input (read-input-file "day12.txt"))
         (n-rows (length input))
         (n-cols (length (first input)))
         (start nil)
         (end nil)
         (map (make-array
               `(,n-rows ,n-cols)
               :initial-contents
               (iter (for line in input) (for y from 0)
                 (let ((start-pos (position #\S line))
                       (end-pos (position #\E line)))
                   (when start-pos (setf start (cons y start-pos)))
                   (when end-pos (setf end (cons y end-pos))))
                 (collect
                     (iter (for char in-string line)
                       (collect
                           (make-instance 'square
                                          :elevation (cond
                                                       ((char= char #\S) #\a)
                                                       ((char= char #\E) #\z)
                                                       (:otherwise char))))))))))
    (macrolet ((node (rc) `(aref map (car ,rc) (cdr ,rc))))
      (labels ((neighbors (rc)
                 (let ((e0 (elevation (node rc)))
                       (neighbors nil))
                   (iter (for dr from -1 to 1)
                     (append
                      (iter (for dc from -1 to 1)
                        (when (= 1 (+ (abs dr) (abs dc)))
                          (let* ((r0 (car rc)) (c0 (cdr rc))
                                 (r1 (+ r0 dr)) (c1 (+ c0 dc)))
                            (when (and (<= 0 r1) (<= 0 c1)
                                       (< r1 n-rows) (< c1 n-cols))
                              (let ((e1 (elevation (node (cons r1 c1)))))
                                (when (or (char>= e1 e0)
                                          (= 1 (- (char-code e0)
                                                  (char-code e1))))
                                  (push (cons r1 c1) neighbors)))))))))
                   (nreverse neighbors)))
               (populate-distances ()
                 (let ((unvisited (make-hash-table :test #'equal))
                       (current end))
                   (iter (for row from 0 below n-rows)
                     (iter (for col from 0 below n-cols)
                       (setf (gethash (cons row col) unvisited) t)))
                   (setf (distance (node end)) 0)
                   (iter (until (zerop (hash-table-count unvisited)))
                     (setf current (iter (for (key _) in-hashtable unvisited)
                                     (finding key minimizing
                                              (distance (node key)))))
                     (let ((neighbors (remove-if-not
                                       #'(lambda (n) (gethash n unvisited))
                                       (neighbors current))))
                       (iter (for neighbor in neighbors)
                         (setf (distance (node neighbor))
                               (min (distance (node neighbor))
                                    (1+ (distance (node current)))))))
                     (remhash current unvisited))))
               (part1 ()
                 (populate-distances)
                 (distance (node start)))
               (part2 ()
                 (iter (for r from 0 below n-rows)
                   (minimize
                    (iter (for c from 0 below n-cols)
                      (when (char= #\a (elevation (node (cons r c))))
                        (minimize (distance (node (cons r c))))))))))
        (cons (part1) (part2))))))

(defun day13 ()
  (labels ((parse-packet (packet)
             (let ((parsed
                     (str:replace-all
                      "," " "
                      (str:replace-all
                       "]" ")"
                       (str:replace-all "[" "(" packet)))))
               (read-from-string parsed)))
           (parse-pair (pair)
             (list (parse-packet (first pair))
                   (parse-packet (second pair)))))
    (let ((input (mapcar #'parse-pair
                         (split-sequence "" (read-input-file "day13.txt")
                                         :test #'string=))))
      (labels ((right-order? (left right &optional (depth 0))
                 (cond ((and (null left) (null right) 'maybe))
                       ((and (null left) (not (null right))) t)
                       ((and (not (null left)) (null right)) nil)
                       ((and (atom left) (atom right))
                        (cond ((< left right) t)
                              ((> left right) nil)
                              (:otherwise 'maybe)))
                       ((and (listp left) (listp right))
                        (let* ((l1 (first left)) (r1 (first right))
                               (result (right-order? l1 r1 (1+ depth))))
                          (cond
                            ((eq result t) t)
                            ((eq result nil) nil)
                            ((eq result 'maybe)
                             (right-order? (rest left)
                                           (rest right)
                                           (1+ depth))))))
                       ((and (atom left) (listp right))
                        (right-order? (list left) right))
                       ((and (listp left) (atom right))
                        (right-order? left (list right)))))
               (part1 ()
                 (iter (for pair in input) (for index from 1)
                   (when (right-order? (first pair) (second pair))
                     (summing index))))
               (part2 ()
                 (let* ((divider1 (list (list 2)))
                        (divider2 (list (list 6)))
                        (packets (append
                                  (list divider1 divider2)
                                  (iter (for pair in input) (appending pair)))))
                   (let* ((sorted (sort packets #'right-order?))
                          (pos1 (position divider1 sorted :test #'equal))
                          (pos2 (position divider2 sorted :test #'equal)))
                     (* (1+ pos1) (1+ pos2))))))
        (cons (part1) (part2))))))

(defun day14 ()
  (let* ((x-max 0) (y-max 0)
         (input (iter (for line in (read-input-file "day14.txt"))
                  (let ((split (str:split-omit-nulls "->" line)))
                    (collect (iter (for point in split)
                               (let* ((point (str:split "," point))
                                      (x (parse-integer (first point)))
                                      (y (parse-integer (second point))))
                                 (setf x-max (max x-max x)
                                       y-max (max y-max y))
                                 (collect (cons x y)))))))))
    (macrolet ((x (point) `(car ,point)) (y (point) `(cdr ,point)))
      (labels ((create-map ()
                 (let ((map (make-array (list (* 2 x-max) (* 2 y-max))
                                        :initial-element nil)))
                   (setf (aref map 500 0) 'source)
                   (iter (for path in input)
                     (iter (for i from 0 below (1- (length path)))
                       (let* ((this-point (nth i path))
                              (next-point (nth (1+ i) path))
                              (start-x (min (x this-point) (x next-point)))
                              (end-x (max (x this-point) (x next-point)))
                              (start-y (min (y this-point) (y next-point)))
                              (end-y (max (y this-point) (y next-point))))
                         (if (= (x this-point) (x next-point))
                             (iter (for y from start-y to end-y)
                               (setf (aref map (x this-point) y) 'rock))
                             (iter (for x from start-x to end-x)
                               (setf (aref map x (y this-point)) 'rock))))))
                   map))
               (part1 ()
                 (let ((map (create-map)))
                   (iter (for n-sand from 1 to 1000)
                     (let ((sand (cons 500 0)))
                       (until
                        (iter (for step from 0)
                          (cond
                            ((= (y sand) y-max) (return t))
                            ((eq nil (aref map (x sand) (1+ (y sand)))) (incf (y sand)))
                            ((eq nil (aref map (1- (x sand)) (1+ (y sand))))
                             (progn (incf (y sand)) (decf (x sand))))
                            ((eq nil (aref map (1+ (x sand)) (1+ (y sand))))
                             (progn (incf (y sand)) (incf (x sand))))
                            (:otherwise (progn
                                          (setf (aref map (x sand) (y sand))
                                                'sand)
                                          (return))))))))
                   (iter (for x from 0 to x-max)
                     (sum (iter (for y from 0 to y-max)
                            (counting (eq 'sand (aref map x y))))))))
               (print-map (map)
                 (iter (for y from 0 to 11)
                   (format t "~&~2,'0d " y)
                   (iter (for x from 485 to 515)
                     (format t "~a"
                             (if (= 11 y)
                                 #\#
                                 (case (aref map x y)
                                   (sand #\o)
                                   (rock #\#)
                                   (t #\.)))))))
               (part2 ()
                 (let ((map (create-map)))
                   (iter (for n-sand from 1 to 32000)
                     (until (eq 'sand (aref map 500 0)))
                     (let ((sand (cons 500 0)))
                       (iter (for step from 0)
                         (cond
                           ((= (y sand) (1+ y-max))
                            (progn
                              (setf (aref map (x sand) (y sand)) 'sand)
                              (return)))
                           ((eq nil (aref map (x sand) (1+ (y sand)))) (incf (y sand)))
                           ((eq nil (aref map (1- (x sand)) (1+ (y sand))))
                            (progn (incf (y sand)) (decf (x sand))))
                           ((eq nil (aref map (1+ (x sand)) (1+ (y sand))))
                            (progn (incf (y sand)) (incf (x sand))))
                           (:otherwise (progn
                                         (setf (aref map (x sand) (y sand))
                                               'sand)
                                         (return)))))))
                   (iter (for x from 0 below (* 2 x-max))
                     (sum (iter (for y from 0 below (* 2 y-max))
                            (counting (eq 'sand (aref map x y)))))))))
        (cons (part1) (part2))))))

(defun day15 ()
  (macrolet ((x (place) `(car ,place)) (y (place) `(cdr ,place)))
    (let* ((input (iter (for line in (read-input-file "day15.txt"))
                    (let* ((split (str:split " " line))
                           (sensor-x-string (nth 2 split))
                           (sensor-y-string (nth 3 split))
                           (beacon-x-string (nth 8 split))
                           (beacon-y-string (nth 9 split))
                           (sensor
                             (cons
                              (parse-integer
                               (str:substring 2 (1- (length sensor-x-string))
                                              sensor-x-string))
                              (parse-integer
                               (str:substring 2 (1- (length sensor-y-string))
                                              sensor-y-string))))
                           (beacon
                             (cons
                              (parse-integer
                               (str:substring 2 (1- (length beacon-x-string))
                                              beacon-x-string))
                              (parse-integer
                               (str:substring 2 (length beacon-y-string)
                                              beacon-y-string)))))
                      (collect (cons sensor beacon))))))
      (labels ((manhattan-distance (point-a point-b)
                 (+ (abs (- (x point-a) (x point-b)))
                    (abs (- (y point-a) (y point-b)))))
               (part1 ()
                 (let ((map (make-hash-table :test #'equal)))
                   (iter (for (sensor . beacon) in input) (for index from 0)
                     (format t "~&Sensor: ~a~&" index)
                     (setf (gethash sensor map) 'S
                           (gethash beacon map) 'B)
                     (let ((distance (manhattan-distance sensor beacon)))
                       (iter (for dy
                                  from (- distance)
                                  to distance)
                         (when (= 2000000 (+ (y sensor) dy))
                           (iter (for dx
                                      from (- (abs dy) distance)
                                      to (- distance (abs dy)))
                             (let ((x (+ dx (x sensor)))
                                   (y (+ dy (y sensor))))
                               (when (not (gethash (cons x y) map))
                                 (setf (gethash (cons x y) map) 'X))))))))
                   (iter (for (key val) in-hashtable map)
                     (when (= (y key) 2000000)
                       (count (eq 'X val))))))
               (distance (x0 y0 x1 y1)
                 (declare (type fixnum x0 y0 x1 y1))
                 (+ (abs (- x1 x0)) (abs (- y1 y0))))
               (scan (start end)
                 (declare (optimize (safety 0) (speed 3)))
                 (declare (ftype (function (fixnum fixnum fixnum fixnum) fixnum)
                                 distance))
                 (let ((y-start start)
                       (y-end end)
                       (max-coord 4000000)
                       (sensors (make-array (length input)))
                       (distress nil))
                   (iter (for (sensor . beacon) in input) (for index from 0)
                     (setf (aref sensors index)
                           (cons sensor (manhattan-distance sensor beacon))))
                   (iter (for y from y-start to end)
                     (declare (type fixnum y))
                     (until distress)
                     (when (zerop (mod y 100))
                       (format t "~&Scanning line ~a" y))
                     (let ((x 0))
                       (declare (type fixnum x))
                       (iter
                         (until (or distress (>= x max-coord)))
                         (iter (for ((s-x . s-y) . beacon-distance)
                                    in-vector sensors)
                           (declare (type fixnum s-x s-y beacon-distance))
                           (let ((current-distance (distance s-x s-y x y)))
                             (when (<= current-distance beacon-distance)
                               (when (< x s-x)
                                 (incf x (* 2 (- s-x x))))
                               (return)))
                           (finally (setf distress (cons x y))))
                         (incf x))))
                   distress))
               (part2 ()
                 (let ((chunk-size 5000)
                       (starting-line 0)
                       (ending-line 4000000))
                   (let ((distress nil)
                         (threads
                           (iter (for start from starting-line
                                      below ending-line by chunk-size)
                             (let ((end (+ start chunk-size)))
                               (collect (bt:make-thread
                                         (lambda () (scan start end))))))))
                     (iter (for thread in threads)
                       (let ((result (bt:join-thread thread)))
                         (when result
                           (push result distress))))
                     (car distress)))))
        (cons (part1) (part2))))))

(defun main ()
  (format t "~&Day 01: ~a" (day01))
  (format t "~&Day 02: ~a" (day02))
  (format t "~&Day 03: ~a" (day03))
  (format t "~&Day 04: ~a" (day04))
  (format t "~&Day 05: ~a" (day05))
  (format t "~&Day 06: ~a" (day06))
  (let ((*print-circle* t))
    (format t "~&Day 07: ~a" (day07)))
  (format t "~&Day 08: ~a" (day08))
  (format t "~&Day 09: ~a" (day09))
  (let ((o (day10)))
    (format t "~&Day 10: ~a~&~a" (car o) (cdr o)))
  (format t "~&Day 11: ~a" (day11))
  (format t "~&Day 12: ~a" (day12))
  (format t "~&Day 13: ~a" (day13))
  (format t "~&Day 14: ~a~&" (day14))
  #+nil(format t "~&Day 15: ~a" (day15)))

(main)
