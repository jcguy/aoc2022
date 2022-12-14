(ql:quickload "cl-utilities" :silent t)
(ql:quickload "str" :silent t)

(defpackage #:tech.corder.aoc2022
  (:use :cl :cl-utilities))

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

(defun main ()
  (format t "~&Day 01: ~a" (day01))
  (format t "~&Day 02: ~a" (day02))
  (format t "~&Day 03: ~a" (day03))
  (format t "~&Day 04: ~a" (day04))
  (format t "~&Day 05: ~a" (day05))
  (format t "~&Day 06: ~a" (day06))
  (let ((*print-circle* t))
    (format t "~&Day 07: ~a" (day07)))
  (format t "~&Day 08: ~a" (day08)))

(main)
