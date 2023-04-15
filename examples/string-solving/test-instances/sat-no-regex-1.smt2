(set-logic ALL)
(declare-fun X () String)
(assert (= X "a"))
(check-sat)

; X = a
