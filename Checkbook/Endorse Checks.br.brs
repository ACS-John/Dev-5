! pr endorsment on back of check
autoLibrary
fnTop(program$)
on error goto Ertn
fnTos
fnLbl(1,1,'Number of Endorsements:',25,1,0)
fnTxt(1,28,6,0,0,'30',0,'You can guess. Too many will only cause you to have to cancel print.')
resp$(1)=''
fnLbl(2,1,'Bank Account:',25,1,0)
fnTxt(2,28,15,0,0,'30',0,'Enter your bank account number if you want it shown on the back of the check.')
resp$(2)=''
fnCmdKey('&Next',1,1,0,'Proceed with printing.')
fnCmdKey('&Cancel',5,0,1,'Cancel printing any check endorsments.')
ckey=fnAcs(mat resp$) ! endorse check
if ckey=5 then goto Xit
endorsements=val(resp$(1))
bank=val(resp$(2))
fnOpenPrn
for j=1 to endorsements
	pr #255,using 'form pos 1,cc 30,skip 1,cc 30': 'For Deposit Only',env$('cnam')(1:30)
	if bank>0 then pr #255,using 'form pos 1,cc 30': 'Account: '&str$(bank)
	pr #255: newpage
next j
fnClosePrn
goto Xit
Xit: fnXit
include: ertn
