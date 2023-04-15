(set-logic ALL)
(declare-fun X () String)
(declare-fun Y () String)
(assert (= (str.++ X "a") (str.++ "b" Y)))
(check-sat)

; Xa = bY
