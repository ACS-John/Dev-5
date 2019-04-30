pr newpage
dim x$*64
x$='12345678901234567'
do
	pr f str$(lc+=1)&',2,c': 'take '&str$(takeCount+=1)
	rin f str$(lc+=1)&',2,17/pic(-----------------),T[textboxes],300': x$
	pr  f str$(lc+=1)&',2,17/pic(-----------------),T[textboxes]': x$
	!							'  5,38,17/#PIC(-----------------),T[textboxes],300
	! rin f str$(lc+=1)&',2,17/#fmt(R9999999999999999),T[textboxes],300': x$
	! pr  f str$(lc+=1)&',2,17/#fmt(R9999999999999999),T[textboxes]': x$
loop until fkey