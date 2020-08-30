option base 0
dim coin(10)
mat coin=(0)
! 0 means face up
! 1 means face down
one: !
step_size=1

two: !
finger=0

three: !
pr step_size
! flip the coin
if finger<=udim(mat coin) then
	if coin(finger)=1 then
		coin(finger)=0
	else ! coin(finger)=0
		coin(finger)=1
	end if
end if

four: !
finger+=step_size

five: !

if finger<=udim(mat coin) then goto three

six: !
step_size+=1

seven: !
if step_size<=10 then goto two
eight: !
for item=0 to udim(mat coin)
	pr 'coin '&str$(item)&' is '&str$(coin(item))
next item
pr 'done.'