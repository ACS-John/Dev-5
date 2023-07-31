! Replace S:\acsGL\CoverLetterPrint
! -- pr Cover Letter

autoLibrary
on error goto Ertn
fnTop(program$)
dim dat$*20
fndat(dat$)
open #hCompany=fnH: 'Name=[Q]\GLmstr\Company.h[cno],Shr',i,i,r
dim tb$*32
read #hCompany,using 'form pos 195,C 30',rec=1: tb$
close #hCompany:
tb$='('&trim$(tb$)&')'
tempx=val(fnactpd$) conv ignore
actpd$=fn_actPdName$(tempx)
open #hCoverLetterDesign=fnH: 'Name=[Q]\GLmstr\ACGLCovF.h[cno],Shr',display,input ioerr Xit
! on fkey 5 goto DONE
fnOpenPrn
do ! READ_ACGLCOVF
	dim ln$*8800
	linput #hCoverLetterDesign: ln$ eof DONE ioerr DONE
	for j2=1 to len(rtrm$(ln$))
		if ln$(j2:j2)='@' then
			if ln$(j2+1:j2+1)='1' then
				ln$(j2:j2+1)=fnpedat$&ln$(j2+2:132-len(fnpedat$))
			else if ln$(j2+1:j2+1)='2' then
					ln$(j2:j2+1)=rtrm$(dat$)&ln$(j2+2:132-len(rtrm$(dat$)))
			else if ln$(j2+1:j2+1)='3' then
				ln$(j2:j2+1)=rtrm$(actpd$)&ln$(j2+2:132-len(rtrm$(actpd$)))
			end if
		end if
	next j2
	pr #255: tab(10);ln$
loop

DONE: !
	close #hCoverLetterDesign:
	fnClosePrn
goto Xit

Xit: fnXit

def fn_actPdName$(tempx; ___,return$)
	if tempx= 1 then return$='one'
	if tempx= 2 then return$='two'
	if tempx= 3 then return$='three'
	if tempx= 4 then return$='four'
	if tempx= 5 then return$='five'
	if tempx= 6 then return$='six'
	if tempx= 7 then return$='seven'
	if tempx= 8 then return$='eight'
	if tempx= 9 then return$='nine'
	if tempx=10 then return$='ten'
	if tempx=11 then return$='eleven'
	if tempx=12 then return$='twelve'
	if tempx=13 then return$='thirteen'
	if tempx=14 then return$='fourteen'
	fn_actPdName$=return$
fnend


include: ertn

