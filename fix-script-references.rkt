#lang racket

(require racket/gui)

;;
;; Racket script for resolving missing script link references in Unity3d
;;
;; Author: Isak Andersson
;;
;; Workflow method:
;;   1. Make full backup of all your shit in case something goes wrong (it shouldn't but you
;;     should be better safe than sorry)
;;   2. Create a new empty scene
;;   3. Create empty object
;;   4. Add component for the MonoBehaviour you want to resolve the missing script references
;;     for.
;;   5. SAVE THE SCENE
;;   6. Locate the old meta file for the script you want to resolve (you better have it in
;;     version control or backed up somewhere!!)
;;   7. Execute this script with racket on the command line, pass
;;     + the meta file as the first argument to the command
;;     + the scene with the empty with the component on it as the second argument
;;     + the director{y,ies} you want to search through **recursively** for scene filses and
;;       prefabs to resolve the links in.
;;   8. When the script asks you if it should proceed, check so that what it's asking you
;;     somewhat makes sense.
;;   9. REPEAT from step 4! Be really careful to not forget to:
;;     + SAVE THE SCENE
;;     + PASS THE CORRECT CORRESPONDING META FILE TO THE COMPONENT IN YOUR SCENE
;;
;; LICENSE:
;;   + For humans: https://creativecommons.org/publicdomain/zero/1.0/
;;   + For suits: https://creativecommons.org/publicdomain/zero/1.0/legalcode.txt
;;
;; ぐらく いん で きっちん!
;;

(define argv (current-command-line-arguments))

(define (usage)
  (printf "usage: <original-meta> <scene-with-only-new-component> <subdirs...>~n"))

(when (> 3 (vector-length argv)) (usage) (exit))

(define (string->complete-path str)
  (path->complete-path (string->path str)))

(define original-meta (string->complete-path (vector-ref argv 0)))
(define new-scene (string->complete-path (vector-ref argv 1)))

(define (exists-or-exit file)
  (when (not (file-exists? file))
    (eprintf "~a does not exist, please specify a valid file~n" file)
    (usage)
    (exit)))

(exists-or-exit original-meta)
(exists-or-exit new-scene)

(define argv-rest (vector-drop argv 2))
(define subdirs
  (for/vector ([subdir-string argv-rest])
    (path->complete-path (string->path subdir-string))))

(for ([subdir (in-vector subdirs)])
  (unless (directory-exists? subdir)
    (eprintf "Provided subdirectory: ~a does not exists" subdir)
    (exit)))

(define (read-entire-file path)
  (define input (open-input-file (path->string path)))
  (define content (port->string input))
  (close-input-port input)
  content)

(define original-meta-txt (read-entire-file original-meta))
(define original-meta-regex #px"guid: ([[:alnum:]]+)")
(define original-match-result (regexp-match original-meta-regex original-meta-txt))
(unless original-match-result
  (printf "Could not find guid in provided meta file ~n")
  (exit))
(unless (= 2 (vector-length (list->vector original-match-result)))
  (printf "Either too many or too few guids in meta file~n")
  (exit))

(define old-guid (list-ref original-match-result 1))

(define new-scene-txt (read-entire-file new-scene))
(define new-scene-regex #px"m_Script: \\{fileID: (-?[[:digit:]]+), guid: ([[:alnum:]]+)")
(define match-result (regexp-match new-scene-regex new-scene-txt))
(unless match-result
  (printf "Could not find fileID and guid in provided scene file~n")
  (newline)
  (exit))
(unless (= 3 (vector-length (list->vector match-result)))
  (printf "Either too many of too few fileIDs and guids in provided scene file~n")
  (exit))

(define new-file-id (list-ref match-result 1))
(define new-guid (list-ref match-result 2))

(define user-undecided #t)
(define (prompt-yes-no)
  (when user-undecided
      (begin
        (printf "I'm going to replace guid ~a with ~a and set the file ID to ~a, is this ok?(y/n): " old-guid new-guid new-file-id)
        (flush-output)
        (let ([yes-or-no (read)])
          (when (eq? yes-or-no 'n) (exit))
          (when (eq? yes-or-no 'y) (set! user-undecided #f)))
        (prompt-yes-no))))
(prompt-yes-no)
(printf "Okay proceeding...~n")

(define replacer-regex (pregexp (format "fileID: -?[[:digit:]]+, guid: ~a" old-guid)))
(define replacement-string (format "fileID: ~a, guid: ~a" new-file-id new-guid))

(for ([directory (in-vector subdirs)])
  (for ([unity-file (in-directory directory)] #:when (regexp-match? "\\.unity$|\\.prefab$" unity-file))
    (define file-content (read-entire-file unity-file))
    (define replaced-content (regexp-replace* replacer-regex file-content replacement-string))
    (unless (string=? replaced-content file-content)
      (printf "Found old reference(s) in ~a, replacing..~n" unity-file)
      (define output-file-port (open-output-file unity-file #:exists 'replace))
      (write-string replaced-content output-file-port)
      (close-output-port output-file-port))))

(printf "Done!~n")
