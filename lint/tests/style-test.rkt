#lang racket/base

(require rackunit
         resyntax/base
         resyntax/default-recommendations
         "../style.rkt")

(define (rule-names suite)
  (map object-name (refactoring-suite-rules suite)))

(define default-names (rule-names default-recommendations))
(define project-names (rule-names project-style))

(test-case "the excluded rules are default rules, so a rename upstream fails here"
  (for ([name (in-list excluded-rule-names)])
    (check-not-false (memq name default-names) (format "~a is not a default rule" name))))

(test-case "project-style drops exactly the excluded rules"
  (for ([name (in-list excluded-rule-names)])
    (check-false (memq name project-names) (format "~a is still in project-style" name)))
  (check-equal? (length project-names)
                (- (length default-names) (length excluded-rule-names)))
  (check-not-false (memq 'nested-if-to-cond project-names))
  (check-not-false (memq 'let-to-define project-names)))

(test-case "project-style keeps the default analyzers"
  (check-equal? (refactoring-suite-analyzers project-style)
                (refactoring-suite-analyzers default-recommendations)))
