#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [project-style refactoring-suite?]
  [excluded-rule-names (listof symbol?)]))

(require racket/list
         resyntax/base
         resyntax/default-recommendations)

;; These two rewrite a flat cond or if into when/unless stacks (AGENTS.md).
(define excluded-rule-names
  '(always-throwing-cond-to-when always-throwing-if-to-when))

(define project-style
  (refactoring-suite
   #:name 'project-style
   #:rules (filter-not (λ (rule) (memq (object-name rule) excluded-rule-names))
                       (refactoring-suite-rules default-recommendations))
   #:analyzers (refactoring-suite-analyzers default-recommendations)))
