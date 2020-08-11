#lang s-exp rosette

(require racket/draw (rename-in racket/async-channel [async-channel-put channel-put]))
(require "../core/wallingford.rkt")
(require "../applications/geothings.rkt")
(require "abstract-reactive-thing.rkt")
(require "../compiled-reactive/compiled-reactive-thing.rkt")

(provide viewer% make-viewer)

(define viewer%
  (class compiled-reactive-thing%
    (init thing dc time-display fps-display sleep-time)
    (define my-thing thing)
    (define my-dc dc)
    (define my-time-display time-display)
    (define my-fps-display fps-display)
    (define my-sleep-time sleep-time)
    (define last-frame-time (current-inexact-milliseconds))
    (super-new)
    
    (define/override (match-thread-receive r)
      (match r
        [(list 'update-view-syncd ch)
         (send-syncd my-thing advance-time-syncd)
         (refresh-helper)
         (channel-put ch null)]
        [(list 'update-sampling)
         (if (member 'pull (send my-thing get-sampling))
             (start-pull)
             (stop-pull))]
        [(list 'thing-changed)   ; not sure if this needs to be synchd ....
         ; This is for push notification.  The thing should already have advanced time to the right time.
         (refresh-helper)]
        [(list 'refresh-syncd ch)
         (refresh-helper)
         (channel-put ch null)]
        [_
         (super match-thread-receive r)]))
    
    ; helper functions
    (define (refresh-helper)
      (cond [running
             (send-thing my-thing show my-dc)
             (send my-time-display set-label (seconds->string (send my-thing seconds)))
             (let ([dt (- (current-inexact-milliseconds) last-frame-time)])
               (cond [(> dt 0)
                      (send my-fps-display set-label (string-append "FPS: " (~r (/ 1000 dt) #:precision 0)))])
               (set! last-frame-time (current-inexact-milliseconds)))]
            [else
             (send my-dc clear)
             (send my-time-display set-label " ")
             (send my-fps-display set-label " ")]))
    ; seconds->string is to get around Racket's seeming inability to print a float formatted to have
    ; exactly one decimal place (?!!)
    (define (seconds->string s)
      (let* ([i (inexact->exact (round (* 10 s)))]
             [whole-part (quotient i 10)]
             [decimal-part (remainder i 10)])
        (string-append "Time: " (number->string whole-part) "." (number->string decimal-part))))
    (define running #f)
    (define/public (get-my-thing)
      my-thing)
    (define/public (watch)
      (set! running #t)
      (send my-thing watched-by this)
      ; trying to send this syncd deadlocks ???
      ; (send-syncd my-thing watched-by-syncd this)
      (refresh-helper)
      ; if my-thing wants pull sampling, set up a thread to poll every my-sleep-time seconds
      ; until the 'stop' button is pushed
      (cond [(member 'pull (send my-thing get-sampling))
             (start-pull)]))
    (define/public (unwatch)
      (set! running #f)
      (send my-thing unwatched-by this)
      ; trying to send this syncd deadlocks ???
      ; (send-syncd my-thing unwatched-by-syncd this)
      (send-syncd this refresh-syncd))
    (define pull-thread null)
    (define (start-pull) ; start a pull thread, if it's not already running
      (cond [(or (null? pull-thread) (thread-dead? pull-thread))
             (set! pull-thread  (thread (lambda ()
                                          (let loop ()
                                            (send-syncd this update-view-syncd)
                                            ; later: take account of compute time
                                            (sleep my-sleep-time)
                                            (if running (loop) (void))))))]))
    (define (stop-pull)  ; stop the pull thread if it's currently running
      ; If there is always a start-pull before a stop-pull, we don't actually need to test if pull-thread is null ...
      ; but this seems more robust.
      (if (null? pull-thread) #f (kill-thread pull-thread)))))



; Derive a new canvas (a drawing window) class to handle events
(define my-canvas%
  (class canvas%
    (define myviewer null)  ; hack - initialize myviewer later to avoid initialization cycle
    (define/public (set-viewer v) 
      (set! myviewer v))
    ; Catch mouse events (moving or button up or button down) and send them directly on to the thing I'm viewing
    (define/override (on-event event)
      (cond [(or (send event button-changed?) (send event moving?))
             (let ([thing (send myviewer get-my-thing)]
                   [x (send event get-x)]
                   [y (send event get-y)]
                   [state (cond [(send event button-down?) 'going-down]
                                [(send event button-up?) 'going-up]
                                [(send event get-left-down) 'down]
                                [else 'up])])
               (send-thing thing mouse-event null x y state))]))
    ; Call the superclass init, passing on all init args
    (super-new)))

; make a viewer on a reactive-thing r
(define (make-viewer r #:title [title "A viewer"] #:sleep-time [sleep-time 0.1])
  (define frame (new frame%
                     [label title]
                     [width 600]
                     [height 600]
                     [x 150]
                     [y 100]))
  (define panel (new vertical-panel% [parent frame]))
  (define canv (new my-canvas% [parent panel]
                    [paint-callback
                     (lambda (canvas dc)
                       (send dc set-pen "black" 1 'solid)
                       (send dc set-brush "black" 'solid)
                       (send-syncd v refresh-syncd))]))
  (define dc (send canv get-dc))
  (define controls (new horizontal-panel% [parent panel] [alignment '(center center)]))
  (send controls stretchable-height #f)
  (new button% [parent controls]
       [label "Watch"]
       ; Callback procedure for a button click:
       [callback (lambda (button event)
                   (send v watch))])
  (new button% [parent controls]
       [label "Unwatch"]
       ; Callback procedure for a button click:
       [callback (lambda (button event)
                   (send v unwatch))])
  ; start the label off as blank, with enough blanks to accommodate any reasonable time
  (define td (new message% [parent controls] [label (make-string 30 #\space)]))
  (define fps (new message% [parent controls] [label (make-string 30 #\space)]))
  ; make a viewer and start it up
  (define v (new viewer% [thing r] [dc dc] [time-display td] [fps-display fps] [sleep-time sleep-time]))
  (send canv set-viewer v)
  (send frame show #t)
  (send dc clear)
  (send v watch))