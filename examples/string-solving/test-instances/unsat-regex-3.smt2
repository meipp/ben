(set-logic ALL)
(declare-fun X () String)
(declare-fun Y () String)
(assert (str.in_re X (re.++ (str.to_re "a") (re.* (str.to_re "b")))))
(assert (str.in_re Y (re.union (str.to_re "a") (re.* (str.to_re "b")))))
(assert (= (str.++ X "b") (str.++ Y X)))
(check-sat)

; X: /ab*/
; Y: /a|b*/
; Xb = YX

; cvc5 hangs up
