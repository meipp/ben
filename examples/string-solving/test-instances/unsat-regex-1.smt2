(set-logic ALL)
(declare-fun X () String)
(assert (str.in_re X (re.++ (re.* (str.to_re "a")) (str.to_re "b"))))
(assert (= X "aaa"))
(check-sat)

; X: /a*b/
; X = aaa
