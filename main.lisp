(ql:quickload "cl-utilities")
(ql:quickload "str")

(defpackage #:tech.corder.aoc2022
  (:use :cl :cl-utilities))

(in-package #:tech.corder.aoc2022)

(defun read-input-file (filename)
  (with-open-file (file filename)
    (loop for line = (read-line file nil)
          while line
          collect line)))

(defun day01 ()
  (let ((elves (split-sequence
                ""
                (read-input-file "day01.txt")
                :test #'string-equal)))
    (labels ((part1 ()
               (loop for elf in elves
                     maximize (apply #'+ (mapcar #'parse-integer elf))))
             (part2 ()
               (apply #'+
                      (subseq
                       (sort
                        (loop for elf in elves
                              collect (apply #'+ (mapcar #'parse-integer elf)))
                        #'>)
                       0 3))))
      (cons (part1) (part2)))))

(defun day02 ()
  (let ((rounds
          (mapcar (lambda (round)
                    (let ((them (car round))
                          (us (cadr round)))
                      (cons (str:string-case them
                              ("A" 'rock)
                              ("B" 'paper)
                              ("C" 'scissors))
                            (str::string-case us
                              ("X" 'x)
                              ("Y" 'y)
                              ("Z" 'z)))))
                  (loop for round in (read-input-file "day02.txt")
                        collect (split-sequence #\Space round))))
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
             (part1 ()
               (let ((rounds (mapcar
                              (lambda (round)
                                (let ((them (car round))
                                      (us (cdr round)))
                                  (cons them
                                        (case us
                                          (x 'rock)
                                          (y 'paper)
                                          (z 'scissors)))))
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
             (part2 ()
               (let ((rounds (mapcar
                              (lambda (round)
                                (let ((them (car round))
                                      (result (cdr round)))
                                  (cons them
                                        (case result
                                          (x 'lose)
                                          (y 'draw)
                                          (z 'win)))))
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
    (cond
      ((or (= (length a) 0) (= (length b) 0)) acc)
      (t (let ((a0 (char a 0))
               (b0 (char b 0))
               (a1 (subseq a 1))
               (b1 (subseq b 1)))
           (cond
             ((char= a0 b0)
              (string-intersection a1
                                   b1
                                   (if (and acc (char= a0 (car acc)))
                                       acc
                                       (cons a0 acc))))
             ((char< a0 b0)
              (string-intersection a1
                                   b
                                   acc))
             ((char> a0 b0)
              (string-intersection a
                                   b1
                                   acc))))))))

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
           (let ((sacks (mapcar (lambda (sack)
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
           (lambda (line) (mapcar (lambda (a) (mapcar #'parse-integer (str:split #\- a)))
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

(defun day05 ()
  (let ((input (read-input-file "day05.txt")))
    input))

(defun main ()
  (format t "Day 01: ~a~%" (day01))
  (format t "Day 02: ~a~%" (day02))
  (format t "Day 03: ~a~%" (day03))
  (format t "Day 04: ~a~%" (day04)))

(main)
