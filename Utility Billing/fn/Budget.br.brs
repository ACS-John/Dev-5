def library fnOpenBudTransInput
	autoLibrary
	fnOpenBudTransInput=fn_openBudTrans( 1)
fnend
def library fnOpenBudTrans
	autoLibrary
	fnOpenBudTrans=fn_openBudTrans
fnend
def fn_openBudTrans(; inputOnly, ___,returnN)
	if ~exists('[Q]\UBmstr\BudTransIdx1.h[cno]') or ~exists('[Q]\UBmstr\BudTransIdx2.h[cno]') then
		fn_budgetReIndex
	end if
	if inputOnly then
		! open #returnN: 'Name=[Q]\UBmstr\BudTrans.h[cno],Shr,Use,RecL=149',i,i,r
		open #hBudTrans1=returnN=fnH: 'Name=[Q]\UBmstr\BudTrans.h[cno],kfName=[Q]\UBmstr\BudTransIdx1.h[cno],Shr',i,i,k
		open #hBudTrans2=hUnpassed=fnH: 'Name=[Q]\UBmstr\BudTrans.h[cno],kfName=[Q]\UBmstr\BudTransIdx2.h[cno],Shr',i,i,k
	else 
		! open #returnN: 'Name=[Q]\UBmstr\BudTrans.h[cno],Shr,Use,RecL=149',i,outi,r
		open #hBudTrans1=returnN=fnH: 'Name=[Q]\UBmstr\BudTrans.h[cno],kfName=[Q]\UBmstr\BudTransIdx1.h[cno],Use,RecL=149,KPs=1,KLn=10,Shr',i,outi,k
		open #hBudTrans2=returnHbudgetTrans2=fnH: 'Name=[Q]\UBmstr\BudTrans.h[cno],kfName=[Q]\UBmstr\BudTransIdx2.h[cno],Use,RecL=149,KPs=1/11,KLn=10/4,Shr',i,outi,k
	end if
	fn_openBudTrans=returnN
fnend
def library fnHbudgetTrans2
	fnHbudgetTrans2=returnHbudgetTrans2
fnend
def library fnCloseBudTrans
	close #hBudTrans1: ioerr ignore
	close #hBudTrans2: ioerr ignore
	hBudTrans1=hBudTrans2=0
	returnHbudgetTrans2=0
fnend

def library fnOpenBudMstrInput
	autoLibrary
	fnOpenBudMstrInput=fn_openBudMstr( 1)
fnend
def library fnOpenBudMstr(; inputOnly)
	autoLibrary
	fnOpenBudMstr=fn_openBudMstr( inputOnly)
fnend
def fn_openBudMstr(; inputOnly, ___,returnN)
	if exists('[Q]\UBmstr\BudMstr.h[cno]') then
		if ~exists('[Q]\UBmstr\BudIdx1.h[cno]') then
			fn_budgetReIndex
		end if
		if inputOnly then
			open #hBudMstr1=returnN=fnH: 'Name=[Q]\UBmstr\BudMstr.h[cno],KFName=[Q]\UBmstr\BudIdx1.h[cno],Shr',i,i,k ioerr BudMstrOpenFail
		else 
			open #hBudMstr1=returnN=fnH: 'Name=[Q]\UBmstr\BudMstr.h[cno],KFName=[Q]\UBmstr\BudIdx1.h[cno],Shr',i,outIn,k ioerr BudMstrOpenFail
		end if
	else
		BudMstrOpenFail: !
		if env$('acsDeveloper')<>'' and exists('[Q]\UBmstr\BudMstr.h[cno]') then
			pr 'open budmstr err=';err;' on line ';line
			pause
		end if
		returnN=0
	end if
	fn_openBudMstr=returnN
fnend
def library fnCloseBudMstr
	close #hBudMstr1: ioerr ignore
	! close #hBudMstr2: ioerr ignore
	hBudMstr1=hBudMstr2=0
fnend


def library fnBudgetReIndex(; cno)
	autoLibrary
	fnBudgetReIndex=fn_budgetReIndex
fnend
def fn_budgetReIndex(; cno)
	if ~cno then cno=val(env$('cno'))
	fnIndex('[Q]\UBmstr\BudMstr.h'&str$(cno), '[Q]\UBmstr\BudIdx1.h'&str$(cno),'1 10')
	fnIndex('[Q]\UBmstr\BudTrans.h'&str$(cno), '[Q]\UBmstr\BudTransIdx1.h'&str$(cno),'1 10')
	fnIndex('[Q]\UBmstr\BudTrans.h'&str$(cno), '[Q]\UBmstr\BudTransIdx2.h'&str$(cno),'1/11 10/4')
fnend

def library fnBudgetTransMatchingRecords(z$*10,mat btRec; option$*128,___,returnN,x$*10,unpaidOnly,DatePaidCur)
	!  returnN = matching Records Count and collect all matching record numbers into mat btRec
	mat btRec(0)
	restore #hBudTrans1,key=>z$: nokey QuickCountFinis
	if pos(lwrc$(' '&option$&' '),' unpaidonly ')>0 then unpaidOnly=1
	do
		dim bt1(14,2)
		read #hBudTrans1,using FbudgetTrans: x$,mat bt1 eof QuickCountFinis
		FbudgetTrans: form pos 1,c 10,2*pd 4,24*pd 5.2,2*pd 4
		DatePaidCur=bt1(14,1)
		if z$=x$ and (~unpaidOnly or DatePaidCur<=0) then
			fnAddOneN(mat btRec,rec(hBudTrans1))
			returnN+=1
		end if
	loop while z$=x$
	QuickCountFinis: !
	fnBudgetTransMatchingRecords=returnN
fnend

def library fnCustomerBudgetEnable(x1$*10; ___,returnN,z$*10)
	dim ba(13)
	mat ba=(0)
	read #hBudMstr1,using 'form pos 1,C 10,PD 4,12*PD 5.2',key=x1$: z$,mat ba nokey ignore
	if sum(mat ba(2:11))>0 then returnN=1
	fnCustomerBudgetEnable=returnN
fnend

