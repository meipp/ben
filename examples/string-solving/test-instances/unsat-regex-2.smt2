(set-logic ALL)
(declare-fun X () String)
(declare-fun Y () String)
(assert (str.in_re X (re.* (re.union (str.to_re "a") (str.to_re "b")))))
(assert (str.in_re Y (re.++ (re.* (str.to_re "b")) (str.to_re "a"))))
(assert (= (str.++ X "b" Y) (str.++ Y X)))
(check-sat)

; X: /(a|b)*/
; Y: /b*a/
; XbY = YX
