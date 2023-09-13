(import  (chibi) (scheme base) (chibi io) (chibi trace) (chibi match) (srfi 27) (srfi 151) (srfi 143) (scheme bytevector) (srfi 18))

;; constants
(define num-keys 16)
(define num-v-registers 16)
(define total-ram 4096)
(define stack-size 16)
(define fontset-size 80)
(define pc-start #x200)
(define timer-max 255)
(define chip8-ram-start-addr #x000)
(define chip8-ram-end-addr #x1FF)
(define program-start-addr #x200)
(define program-end-addr #xFFF)
(define screen-width 64)
(define screen-height 32)

;; variables
(define ram (make-bytevector 4096 0))
(define v-registers (make-bytevector 16))
(define i-register 0)
(define delay-timer-register 0)
(define sound-timer-register 0)
(define program-counter pc-start)
(define stack-pointer 0)
(define stack (make-bytevector 16))
(define screen (make-bytevector (* screen-width screen-height)))
(define any-key-pressed? #f)
(define cpu-timer 0)
(define redraw-screen? #f)

;; debugging variables
(define debug #f)
(define debug-draw-counter 0)

;; debugging functions
(define (print-current-draw-counter)
  (display "total draws: ")
  (display debug-draw-counter)
  (newline))

(define (display-regs)
  (do ((i 0 (+ i 1)))
      ((= i 16) 'finished)
    (display "V Reg ")
    (display (decimal->hex i))
    (display ": 0x")
    (display (decimal->hex (bytevector-u8-ref v-registers i)))
    (newline))
    (display "  I Reg: 0x")
    (display (decimal->hex i-register))
    (newline)
    (display " PC Reg: 0x")
    (display (decimal->hex program-counter))
    (newline)
    (display " SP Reg: 0x")
    (display (decimal->hex stack-pointer))
    (newline)
    (display " DT Reg: 0x")
    (display (decimal->hex delay-timer-register))
    (newline)
    (display " ST Reg: 0x")
    (display (decimal->hex sound-timer-register))
    (newline))

(define (display-screen-pixels)
  (do ((i 0 (+ i 1)))
      ((= i (* screen-width screen-height)) 'finished)
    (display (bytevector-u8-ref screen i))
    (when (equal? (modulo i screen-width) 63)
      (newline))))

(define (display-opcode opcode)
  (display "Current Opcode: 0x")
  (display (decimal->hex opcode))
  (newline)
  (display "full opcode: ")
  (display opcode)
  (newline))

;; helper functions
(define (decimal->hex n)
  (define (digit->hex digit)
    (cond ((< digit 10) (number->string digit))
          ((= digit 10) "A")
          ((= digit 11) "B")
          ((= digit 12) "C")
          ((= digit 13) "D")
          ((= digit 14) "E")
          ((= digit 15) "F")))
  (if (< n 16)
      (digit->hex n)
      (string-append
       (decimal->hex (quotient n 16))
       (digit->hex (remainder n 16)))))

(define (wrapping-sub x y)
  (if (< x y)
      (- 256 (- y x))
      (- x y)))

(define (load-rom filename)
  (let ((filebs (file->bytevector filename)))
    (bytevector-copy! filebs 0 ram program-counter (bytevector-length filebs))))

(define (reset!)
  (set! ram (make-bytevector 4096))
  (set! v-registers (make-bytevector 16))
  (set! i-register 0)
  (set! delay-timer-register 0)
  (set! sound-timer-register 0)
  (set! program-counter pc-start)
  (set! stack-pointer 0)
  (set! stack (make-bytevector 16))
  (set! redraw-screen? #f))

(define (increment-cpu-timer!)
  (set! cpu-timer (+ cpu-timer 1)))

(define (reset-cpu-timer!)
  (set! cpu-timer 0))

(define (decrement-timers!)
  (when (> delay-timer-register 0)
    (set! delay-timer-register (- delay-timer-register 1)))
  (when (> sound-timer-register 0)
    (set! sound-timer-register (- sound-timer-register 1))))

(define (fetch-opcode)
  (bytevector-u16-ref ram program-counter (endianness big)))

(define (increment-program-counter!)
  (set! program-counter (+ program-counter 2)))

(define (increment-program-counter-twice!)
  (set! program-counter (+ program-counter 4)))

(define (decrement-program-counter!)
  (set! program-counter (- program-counter 2)))

(define (increment-stack-pointer!)
  (set! stack-pointer (+ stack-pointer 1)))

;; instructions
(define (CLS)
  "Clear the display."
  (when debug (display "CLS\n"))
  (set! redraw-screen? #t)
  (bytevector-fill! screen 0)
  (increment-program-counter!))

(define (RET)
  "Return from a subroutine."
  (when debug (display "RET\n"))
  (set! program-counter (bytevector-u16-ref stack stack-pointer (endianness big)))
  (set! stack-pointer (- stack-pointer 1))
  (increment-program-counter!))

(define (SYS nnn)
  "Jump to a machine code routine at nnn."
  (when debug (display "SYS\n"))
  (set! program-counter nnn))

(define (JP nnn)
  "Jump to location nnn."
  (when debug (display "JP\n"))
  (set! program-counter nnn))

(define (CALL nnn)
  "Call subroutine at nnn."
  (when debug (display "CALL\n"))
  (increment-stack-pointer!)
  (bytevector-u16-set! stack stack-pointer program-counter (endianness big))
  (set! program-counter nnn))

(define (SE-x-kk x kk)
  "Skip next instruction if Vx = kk."
  (when debug (display "SE\n"))
  (if (equal? (bytevector-u8-ref v-registers x) kk)
      (increment-program-counter-twice!)
      (increment-program-counter!)))

(define (SNE x kk)
  "Skip next instruction if Vx = kk."
  (when debug (display "SNE\n"))
  (if (not (equal? (bytevector-u8-ref v-registers x) kk))
      (increment-program-counter-twice!)
      (increment-program-counter!)))

(define (SE-Vx-Vy x y)
  "Skip next instruction if Vx = Vy"
  (when debug (display "SE-Vx-Vy\n"))
  (if (equal?
       (bytevector-u8-ref v-registers x)
       (bytevector-u8-ref v-registers y))
      (increment-program-counter-twice!)
      (increment-program-counter!)))

(define (LD-Vx-Vy x kk)
  "Put the value kk into register Vx."
  (when debug (display "Load Vx Reg (6XNN)\n"))
  (bytevector-u8-set! v-registers x kk)
  (increment-program-counter!))

(define (ADD x kk)
  "Add kk to Vx."
  (when debug (display "ADD-Vx-imm\n"))
  (let ((Vx (bytevector-u8-ref v-registers x)))
    (bytevector-u8-set! v-registers x (fxand #xFF (+ Vx kk))))
  (increment-program-counter!))

(define (MOVE-Vx-Vy x y)
  "Store value of Vx in Vy."
  (when debug (display "MOVE-Vx-Vy\n"))
  (let ((Vy (bytevector-u8-ref v-registers y)))
    (bytevector-u8-set! v-registers x Vy))
    (increment-program-counter!))

(define (OR-Vx-Vy x y)
  "Store value of Vx OR Vy in Vx."
  (when debug (display "OR-Vx-Vy\n"))
  (let ((Vx (bytevector-u8-ref v-registers x))
        (Vy (bytevector-u8-ref v-registers y)))
    (bytevector-u8-set! v-registers x (fxior Vx Vy)))
  (increment-program-counter!))

(define (AND-Vx-Vy x y)
  "Store value of Vx AND Vy in Vx."
  (when debug (display "AND-Vx-Vy\n"))
  (let ((Vx (bytevector-u8-ref v-registers x))
        (Vy (bytevector-u8-ref v-registers y)))
    (bytevector-u8-set! v-registers x (fxand Vx Vy)))
  (increment-program-counter!))

(define (XOR-Vx-Vy x y)
  "Store value of Vx XOR Vy in Vx."
  (when debug (display "XOR-Vx-Vy\n"))
  (let ((Vx (bytevector-u8-ref v-registers x))
        (Vy (bytevector-u8-ref v-registers y)))
    (bytevector-u8-set! v-registers x (fxxor Vx Vy)))
  (increment-program-counter!))

(define (LD-I nnn)
  "Set the value of I register to value nnn."
  (when debug (display "LD-I "))
  (when debug (display nnn))
  (when debug (display "\n"))
  (set! i-register nnn)
  (increment-program-counter!))

(define (RND x kk)
  "Set Vx to random integer from 0-255 AND kk."
  (when debug (display "RND\n"))
  (let* ((random-number (random-integer 255))
        (to-set (fxand random-number kk)))
    (bytevector-u8-set! v-registers x to-set)
    (increment-program-counter!)))

(define (ADD-Vx-Vy x y)
  "Store value of Vx + Vy in Vx."
  (when debug (display "ADD-Vx-Vy\n"))
  (let* ((current-value (bytevector-u8-ref v-registers x))
         (Vy (bytevector-u8-ref v-registers y))
         (sum (+ current-value Vy))
         (lower-summed-bits (fxand sum #xFF))
         (carry (fxand sum #x100)))
    (bytevector-u8-set! v-registers x lower-summed-bits)
    (if (not (zero? carry))
    (bytevector-u8-set! v-registers #xF (if (= carry 256) 1 0)))
    (increment-program-counter!)))

(define (SUB x y)
  "Store value of Vx - Vy in Vx."
  (when debug (display "SUB\n"))
  (let* ((Vx (bytevector-u8-ref v-registers x))
       (Vy (bytevector-u8-ref v-registers y))
       (not-borrow? (> Vx Vy)))
    (if not-borrow?
      (bytevector-u8-set! v-registers #xF 1)
      (bytevector-u8-set! v-registers #xF 0))
    (bytevector-u8-set! v-registers x (wrapping-sub Vx Vy))
    (increment-program-counter!)))

(define (SHR x)
  "Store value of Vx shifted right by 1 in Vx."
  (when debug (display "SHR\n"))
  (let* ((Vx (bytevector-u8-ref v-registers x))
         (lsb (fxand Vx #x01))
         (new-Vx (fxarithmetic-shift-right Vx 1)))
    (bytevector-u8-set! v-registers #xF lsb)
    (bytevector-u8-set! v-registers x new-Vx)
    (increment-program-counter!)))

(define (SUBN x y)
  "Store value of Vy - Vx in Vx."
  (when debug (display "SUB\n"))
  (let* ((Vx (bytevector-u8-ref v-registers x))
         (Vy (bytevector-u8-ref v-registers y))
         (not-borrow? (> Vy Vx)))
    (if not-borrow?
        (bytevector-u8-set! v-registers #xF 1)
        (bytevector-u8-set! v-registers #xF 0))
    (bytevector-u8-set! v-registers x (wrapping-sub Vy Vx))
    (increment-program-counter!)))

(define (SHL x)
  "Store value of Vx shifted left by 1 in Vx."
  (when debug (display "SNE\n"))
  (let* ((msb (fxand Vx #x80))
         (Vx (bytevector-u8-ref v-registers x))
         (new-Vx (fxarithmetic-shift-left Vx 1)))
    (if (equal? msb 1)
        (bytevector-u8-set! v-registers #xF 1)
        (bytevector-u8-set! v-registers #xF 0))
    (bytevector-u8-set! v-registers x new-Vx)
    (increment-program-counter!)))

(define (SNE-Vx-Vy x y)
  "Skip next instruction if Vx != Vy."
  (when debug (display "SNE-Vx-Vy\n"))
  (let ((Vx (bytevector-u8-ref v-registers x))
        (Vy (bytevector-u8-ref v-registers y)))
    (if (not (equal? Vx Vy))
        (increment-program-counter-twice!)
        (increment-program-counter!))))

(define (JP-V0 nnn)
  "Jump to location nnn + V0."
  (when debug (display "JP-V0\n"))
  (let ((V0 (bytevector-u8-ref v-registers 0)))
    (set! program-counter (+ nnn V0))))

(define (DRW x y n)
  "Draw sprite at location (Vx, Vy) with height n."
  (let* ((Vx (bytevector-u8-ref v-registers x))
         (Vy (bytevector-u8-ref v-registers y))
         (sprite-height n)
         (pixel #f))
    (when debug (display "DRW\n"))
    (bytevector-u8-set! v-registers #xF 0)
    (do ((y-coordinate 0 (+ y-coordinate 1)))
        ((equal? y-coordinate sprite-height) 'finished)
      (set! pixel (bytevector-u8-ref ram (+ i-register y-coordinate)))
      (do ((x-coordinate 0 (+ x-coordinate 1)))
          ((equal? x-coordinate 8) 'finished)
        (let* ((existing-index
                (+
                 (* screen-width (modulo (+ Vy y-coordinate) screen-height))
                 (modulo (+ Vx x-coordinate) screen-width)))
               (existing-value
                (begin
                  
                  (bytevector-u8-ref screen existing-index))))
          (when
              (not (zero? (fxand pixel (fxarithmetic-shift-right #x80 x-coordinate))))
              (when
                  (equal? existing-value 1)
                  (bytevector-u8-set! v-registers #xF 1))
              (bytevector-u8-set! screen existing-index (fxxor existing-value 1))))))
  (set! redraw-screen? #t)
  (increment-program-counter!)))

(define (SKP x)
  "Skip next instruction if key with value of Vx is pressed."
  (when debug (display "SKP\n"))
  (let ((Vx (bytevector-u8-ref v-registers x)))
    (if (key-pressed? Vx)
        (increment-program-counter-twice!)
        (increment-program-counter!))
        ))

(define (SKNP x)
  "Skip next instruction if key with value of Vx is not pressed."
  (when debug (display "SKNP\n"))
  (let ((Vx (bytevector-u8-ref v-registers x)))
    (if (not (key-pressed? Vx))
        (increment-program-counter-twice!)
        (increment-program-counter!))))


(define (LD-Vx-DT x)
  "Set Vx = delay timer value."
  (when debug (display "LD-Vx-DT\n"))
  (let ((Vx (bytevector-u8-ref v-registers x)))
    (bytevector-u8-set! v-registers x delay-timer-register)
    (increment-program-counter!)))

(define (LD-Vx-K x)
  "Wait for key press and store value of key in Vx."
  (when debug (display "LD-Vx-K\n"))
  (set! any-key-pressed? #f)
  (let ((Vx (bytevector-u8-ref v-registers x)))
    (do ((i 0 (+ i 1)))
        ((= i num-keys) 'finished)
      (when
        (key-pressed? i)
        (bytevector-u8-set! v-registers x i)
        (set! any-key-pressed? #t))))
    (when any-key-pressed?
      (increment-program-counter!)))

(define (LD-DT-Vx x)
  "Set delay timer to value of Vx."
  (when debug (display "LD-DT-Vx\n"))
  (let ((Vx (bytevector-u8-ref v-registers x)))
    (set! delay-timer-register Vx)
    (increment-program-counter!)))

(define (LD-ST-Vx x)
  "Set sound timer to value of Vx."
  (when debug (display "LD-ST-Vx\n"))
  (let ((Vx (bytevector-u8-ref v-registers x)))
    (set! sound-timer-register Vx)
    (increment-program-counter!)))

(define (ADD-I-Vx x)
  "Set I = I + Vx."
  (when debug (display "ADD-I-Vx\n"))
  (let ((Vx (bytevector-u8-ref v-registers x)))
    (set! i-register (fx+ i-register Vx))
    (increment-program-counter!)))

(define (LD-F-Vx x)
  "Set I = location of sprite for digit Vx."
  (when debug (display "LD-F-Vx\n"))
  (let ((Vx (bytevector-u8-ref v-registers x)))
    (set! i-register (fx* Vx 5))
    (increment-program-counter!)))

(define (LD-B-Vx x)
  "Store BCD representation of Vx in memory locations I, I+1, and I+2."
  (when debug (display "LD-B-Vx\n"))
  (let* ((Vx (bytevector-u8-ref v-registers x))
         (hundreds (fxquotient Vx 100))
         (tens (fxquotient (modulo Vx 100) 10))
         (ones (fxremainder (modulo Vx 100) 10)))
    (bytevector-u8-set! ram i-register hundreds)
    (bytevector-u8-set! ram (+ i-register 1) tens)
    (bytevector-u8-set! ram (+ i-register 2) ones)
    (increment-program-counter!)))

(define (LD-Vx-I x)
  "Store registers V0 through Vx in memory starting at location I."
  (when debug (display "LD-Vx-I\n"))
  (let ((Vx (bytevector-u8-ref v-registers x)))
    (do ((i 0 (+ i 1)))
        ((= i Vx) 'finished)
      (bytevector-u8-set! v-registers i (bytevector-u8-ref ram (+ i-register i))))
    (increment-program-counter!)))

(define (execute-opcode opcode)
  "Perform one CPU operation."
  (let* ((nibble-1 (fxarithmetic-shift-right opcode 12))
         (nibble-2 (fxarithmetic-shift-right (fxand opcode #x0F00) 8))
         (nibble-3 (fxarithmetic-shift-right (fxand opcode #x00F0) 4))
         (nibble-4 (fxand opcode #x000F))
         (nnn (fxand opcode #x0FFF))
         (x nibble-2)
         (y nibble-3)
         (kk (fxand opcode #x00FF))
         (n nibble-4))
    (let ((niblist (list nibble-1 nibble-2 nibble-3 nibble-4)))
      (match niblist
      ((#x00 #x00 #x0E #x00) (CLS))
      ((#x00 #x00 #x0E #x0E) (RET))
      ((#x01 _ _ _) (JP nnn))
      ((#x02 _ _ _) (CALL nnn))
      ((#x03 _ _ _) (SE-x-kk x kk))
      ((#x04 _ _ _) (SNE x kk))
      ((#x05 _ _ _) (SE-Vx-Vy x kk))
      ((#x06 _ _ _) (LD-Vx-Vy x kk))
      ((#x07 _ _ _) (ADD x kk))
      ((#x08 _ _ #x00) (MOVE-Vx-Vy x y))
      ((#x08 _ _ #x01) (OR-Vx-Vy x y))
      ((#x08 _ _ #x02) (AND-Vx-Vy x y))
      ((#x08 _ _ #x03) (XOR-Vx-Vy x y))
      ((#x08 _ _ #x04) (ADD-Vx-Vy x y))
      ((#x08 _ _ #x05) (SUB x y))
      ((#x08 _ _ #x06) (SHR x))
      ((#x08 _ _ #x07) (SUBN x))
      ((#x08 _ _ #x0E) (SHL x))
      ((#x09 _ _ #x00) (SNE-Vx-Vy x y))
      ((#x0A _ _ _) (LD-I nnn))
      ((#x0B _ _ _) (JP-V0 nnn))
      ((#x0C _ _ _) (RND x kk))
      ((#x0D _ _ _) (DRW x y n))
      ((#x0E _ #x09 #x0E) (SKP x))
      ((#x0E _ #x0A #x01) (SKNP x))
      ((#x0F _ #x00 #x07) (LD-Vx-DT x))
      ((#x0F _ #x00 #x0A) (LD-Vx-K x))
      ((#x0F _ #x01 #x05) (LD-DT-Vx x))
      ((#x0F _ #x01 #x08) (LD-ST-Vx x))
      ((#x0F _ #x01 #x0E) (ADD-I-Vx x))
      ((#x0F _ #x02 #x09) (LD-F-Vx x))
      ((#x0F _ #x03 #x03) (LD-B-Vx x))
      ((#x0F _ #x05 #x05) (LD-I-Vx x))
      ((#x0F _ #x06 #x05) (LD-Vx-I x))
      (else niblist)))))

(define (run-program)
  "Start the main program loop."
  (define (loop-input)
    (when
        (and (is-running?) (is-paused?))
      (process-user-input)
      (loop-input)))
  (when (is-running?)
    (let ((opcode (fetch-opcode)))
      (increment-cpu-timer!)
      (process-user-input)
      (when
        (and (is-running?) (is-paused?))
        (process-user-input)
        (loop-input))
      (when (> cpu-timer 10)
        (reset-cpu-timer!)
        (decrement-timers!))
      (execute-opcode opcode)
      (when debug
        (display-opcode opcode)
        (display-regs))
      (when redraw-screen?
          (set! redraw-screen? #f)
          (when debug (print-current-draw-counter))
          (set! debug-draw-counter (+ debug-draw-counter 1))
          (when debug (display-screen-pixels))
          (buffer-graphics screen)
          (draw-graphics))
      (when (not debug)
      (thread-sleep! 0.001)) ;; debugging is slow enough
      (run-program))))

(load-rom "brick.ch8")
(run-program)
